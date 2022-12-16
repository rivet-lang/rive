# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

import os

from ..sym import TypeKind
from ..token import Kind, OVERLOADABLE_OPERATORS_STR, NO_POS
from .. import ast, sym, type, prefs, colors, report, utils

from . import ir
from .c import CGen

def prefix_type(tt):
    prefix = ""
    if isinstance(tt, type.Ptr):
        _t = tt
        while isinstance(_t, type.Ptr):
            prefix += "ptr_"
            if tt.is_mut:
                prefix += "mut_"
            _t = _t.typ
        prefix += prefix_type(tt.typ)
    elif isinstance(tt, type.Ref):
        prefix += "ref_"
        if tt.is_mut:
            prefix += "mut_"
        prefix += prefix_type(tt.typ)
    elif isinstance(tt, type.Optional):
        prefix += "opt_" + prefix_type(tt.typ)
    return prefix

def mangle_type(typ):
    if isinstance(typ, type.Fn):
        s = "fn_"
        if typ.is_unsafe:
            s += "unsafe_"
        if typ.is_extern:
            s += f"extern_{typ.abi}_"
        if typ.is_method:
            s += "m_"
        if typ.self_is_mut:
            s += "_sm_"
        elif typ.self_is_ref:
            s += "_sr_"
        if typ.is_variadic:
            s += "_v_"
        s += f"_args{len(typ.args)}"
        return s
    return f"{prefix_type(typ)}{mangle_symbol(typ.symbol())}"

def mangle_symbol(s):
    if len(s.mangled_name) > 0:
        return s.mangled_name
    res = []
    root = s
    while True:
        if s.is_universe:
            break
        if isinstance(s, sym.Mod):
            s.mangled_name = "".join([
                f"{len(n)}{n}" for n in s.name.split(".")
            ])
            res.insert(0, s.mangled_name)
        elif isinstance(s, sym.Type):
            if s.kind == TypeKind.Tuple:
                name = "Tuple_"
                for i, tt in enumerate(s.info.types):
                    name += mangle_type(tt)
                    if i < len(s.info.types) - 1:
                        name += "_"
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == TypeKind.Vec:
                res.insert(0, "7runtime3Vec")
                s.mangled_name = "_R7runtime3Vec"
            elif s.kind == TypeKind.Array:
                name = f"Array_{mangle_type(s.info.elem_typ)}_{s.info.size}"
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == TypeKind.String:
                res.insert(0, "7runtime6string")
                s.mangled_name = "_R7runtime6string"
            else:
                res.insert(0, f"{len(s.name)}{s.name}")
        elif s.name in OVERLOADABLE_OPERATORS_STR:
            name = OVERLOADABLE_OPERATORS_STR[s.name]
            name = f"{len(name)}{name}"
            res.insert(0, name)
            s.mangled_name = name
        else:
            res.insert(0, f"{len(s.name)}{s.name}")
        if s.parent == None:
            break
        else:
            s = s.parent
    res.insert(0, "_R")

    if isinstance(root, sym.Fn):
        if root.is_method:
            res.append("M")
        else:
            res.append("F")

    root.mangled_name = "".join(res)
    return root.mangled_name

class TestInfo:
    def __init__(self, name, func):
        self.name = name
        self.func = func

    def __lt__(self, other):
        return self.name < other.name

class Codegen:
    def __init__(self, comp):
        self.comp = comp
        self.out_rir = ir.RIRFile(self.comp.prefs.mod_name)
        self.void_types = (self.comp.void_t, self.comp.never_t)

        self.sf = None

        self.init_global_vars_fn = None
        self.cur_fn = None
        self.cur_fn_is_main = False
        self.cur_fn_ret_typ = self.comp.void_t
        self.cur_fn_defer_stmts = []

        self.inside_trait = False
        self.inside_test = False

        self.generated_string_literals = {}
        self.generated_opt_res_types = []
        self.generated_array_returns = []
        self.generated_tests = []

        self.loop_entry_label = ""
        self.loop_exit_label = ""
        self.while_continue_expr = None

    def gen_source_files(self, source_files):
        self.gen_types()
        # generate 'init_string_lits_fn' function
        self.init_string_lits_fn = ir.FnDecl(
            False, [], False, "_R7runtime16init_string_litsF", [], False,
            ir.VOID_T, False
        )
        self.out_rir.decls.append(self.init_string_lits_fn)

        # generate '_R7runtime12init_globalsF' function
        self.init_global_vars_fn = ir.FnDecl(
            False, [], False, "_R7runtime12init_globalsF", [], False, ir.VOID_T,
            False
        )
        self.out_rir.decls.append(self.init_global_vars_fn)

        for mod in self.comp.universe.syms:
            if isinstance(mod, sym.Mod):
                self.gen_mod_attrs(mod.name, mod.attrs)
        for source_file in source_files:
            self.sf = source_file
            self.gen_decls(source_file.decls)

        # generate '_R12drop_globalsZ' function
        g_fn = ir.FnDecl(
            False, [], False, "_R7runtime12drop_globalsF", [], False, ir.VOID_T,
            False
        )
        self.out_rir.decls.append(g_fn)

        # generate 'main' fn
        argc = ir.Ident(ir.INT_T, "_argc")
        argv = ir.Ident(ir.CHAR_T.ptr().ptr(), "_argv")
        main_fn = ir.FnDecl(
            False, [], False, "main", [argc, argv], False, ir.INT_T, False
        )
        if self.comp.prefs.build_mode == prefs.BuildMode.Test:
            self.cur_fn = main_fn
            self.cur_fn.add_call("_R7runtime16init_string_litsF")
            test_runner = ir.Ident(ir.TEST_RUNNER_T, "_test_runner")
            main_fn.alloca(test_runner)
            tests_field = ir.Selector(ir.VEC_T, test_runner, ir.Name("tests"))
            main_fn.store(
                ir.Selector(ir.U64_T, test_runner, ir.Name("ok_tests")),
                ir.IntLit(ir.U64_T, "0")
            )
            main_fn.store(
                ir.Selector(ir.U64_T, test_runner, ir.Name("fail_tests")),
                ir.IntLit(ir.U64_T, "0")
            )
            main_fn.store(
                ir.Selector(ir.U64_T, test_runner, ir.Name("skip_tests")),
                ir.IntLit(ir.U64_T, "0")
            )
            tests_vec = ir.Selector(
                ir.VEC_T.ptr(True), test_runner, ir.Name("tests")
            )
            test_t = ir.TEST_T.ptr()
            gtests_array = []
            self.generated_tests.sort()
            for i, gtest in enumerate(self.generated_tests):
                test_value = ir.Ident(ir.TEST_T, f"test_value_{i}")
                main_fn.alloca(test_value)
                main_fn.store(
                    ir.Selector(ir.U8_T, test_value, ir.Name("result")),
                    ir.IntLit(ir.U8_T, "0")
                )
                main_fn.store(
                    ir.Selector(ir.STRING_T, test_value, ir.Name("err_pos")),
                    ir.Ident(ir.STRING_T, "_R7runtime12empty_string")
                )
                main_fn.store(
                    ir.Selector(ir.STRING_T, test_value, ir.Name("err_msg")),
                    ir.Ident(ir.STRING_T, "_R7runtime12empty_string")
                )
                main_fn.store(
                    ir.Selector(
                        ir.STRING_T.ptr(True), test_value, ir.Name("name")
                    ), self.gen_string_lit(gtest.name)
                )
                main_fn.store(
                    ir.Selector(
                        ir.Function(test_t, ir.VOID_T), test_value,
                        ir.Name("func")
                    ), ir.Name(gtest.func)
                )
                gtests_array.append(test_value)
            test_t_size, _ = self.comp.type_size(
                type.Type(self.comp.universe["runtime"]["Test"])
            )
            main_fn.store(
                tests_field,
                ir.Inst(
                    ir.InstKind.Call, [
                        ir.Name("_R7runtime3Vec19from_array_no_allocF"),
                        ir.ArrayLit(
                            ir.Array(ir.TEST_T, str(len(self.generated_tests))),
                            gtests_array
                        ),
                        ir.IntLit(ir.USIZE_T, str(test_t_size)),
                        ir.IntLit(ir.USIZE_T, str(len(self.generated_tests)))
                    ]
                )
            )
            main_fn.add_call(
                "_R7runtime4mainF", [
                    argc,
                    ir.Inst(ir.InstKind.Cast, [argv, ir.U8_T.ptr().ptr()]),
                    ir.Inst(ir.InstKind.GetRef, [test_runner])
                ]
            )
        else:
            main_fn.add_call(
                "_R7runtime4mainF", [
                    argc,
                    ir.Inst(ir.InstKind.Cast, [argv, ir.U8_T.ptr().ptr()]),
                    ir.Name(
                        f"_R{len(self.comp.prefs.mod_name)}{self.comp.prefs.mod_name}4mainF"
                    )
                ]
            )
        main_fn.add_ret(ir.IntLit(ir.INT_T, "0"))
        self.out_rir.decls.append(main_fn)

        if report.ERRORS == 0:
            if self.comp.prefs.emit_rir:
                with open(f"{self.comp.prefs.mod_name}.rir", "w+") as f:
                    f.write(str(self.out_rir).strip())
            if self.comp.prefs.target_backend == prefs.Backend.C:
                CGen(self.comp).gen(self.out_rir)
            if self.comp.prefs.build_mode == prefs.BuildMode.Test:
                if os.system(self.comp.prefs.mod_output) == 0:
                    os.remove(self.comp.prefs.mod_output)

    def gen_mod_attrs(self, mod_name, attrs):
        mod_folder = os.path.join(prefs.RIVET_DIR, "obj", mod_name)
        if attrs == None:
            return
        for attr in attrs.attrs:
            if attr.name == "c_compile":
                if not os.path.exists(mod_folder):
                    os.mkdir(mod_folder)
                cfile = os.path.realpath(attr.args[0].expr.lit)
                objfile = os.path.join(
                    mod_folder,
                    f"{os.path.basename(cfile)}.{self.comp.prefs.get_obj_postfix()}.o"
                )
                self.comp.prefs.objects_to_link.append(objfile)
                msg = f"c_compile: compiling object for C file `{cfile}`..."
                if os.path.exists(objfile):
                    if os.path.getmtime(objfile) < os.path.getmtime(cfile):
                        msg = f"c_compile: {objfile} is older than {cfile}, rebuilding..."
                    else:
                        continue
                self.comp.vlog(msg)
                args = [
                    self.comp.prefs.target_backend_compiler, cfile, "-m64" if
                    self.comp.prefs.target_bits == prefs.Bits.X64 else "-m32",
                    "-O3" if self.comp.prefs.build_mode
                    == prefs.BuildMode.Release else "-g",
                    f'-L{os.path.dirname(cfile)}', "-c", "-o", objfile,
                ]
                res = utils.execute(*args)
                if res.exit_code != 0:
                    utils.error(
                        f"error while compiling the object file `{objfile}`:\n{res.err}"
                    )
        if report.ERRORS > 0:
            self.abort()

    def gen_decls(self, decls):
        for decl in decls:
            self.gen_decl(decl)

    def gen_decl(self, decl):
        self.cur_fn_defer_stmts = []
        if isinstance(decl, ast.ExternDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.LetDecl):
            for l in decl.lefts:
                is_extern = decl.is_extern and decl.abi != sym.ABI.Rivet
                name = l.name if is_extern else mangle_symbol(l.sym)
                typ = self.ir_type(l.typ)
                self.out_rir.globals.append(
                    ir.GlobalVar(decl.vis.is_pub(), is_extern, typ, name)
                )
                if not decl.is_extern:
                    self.cur_fn = self.init_global_vars_fn
                    self.cur_fn.store(
                        ir.Ident(typ, name),
                        self.gen_expr_with_cast(l.typ, decl.right)
                    )
        elif isinstance(decl, ast.EnumDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.TraitDecl):
            self.inside_trait = True
            self.gen_decls(decl.decls)
            self.inside_trait = False
        elif isinstance(decl, ast.ClassDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.StructDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.ExtendDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.FnDecl):
            if self.inside_trait and not decl.has_body:
                return
            if decl.is_main and (
                self.comp.prefs.build_mode == prefs.BuildMode.Test
                or self.comp.prefs.mod_type != prefs.ModType.Bin
            ):
                return
            args = []
            if decl.is_method:
                self_typ = self.ir_type(decl.self_typ)
                if decl.self_is_mut and not decl.self_typ.symbol().is_boxed():
                    self_typ = self_typ.ptr()
                args.append(ir.Ident(self_typ, "self"))
            for arg in decl.args:
                arg_typ = self.ir_type(arg.typ)
                if arg.is_mut:
                    arg_typ = arg_typ.ptr()
                args.append(ir.Ident(arg_typ, arg.name))
            ret_typ = self.ir_type(decl.ret_typ)
            arr_ret_struct = ""
            if isinstance(ret_typ, ir.Array):
                # In C functions cannot return an array, so we create a special
                # struct for this.
                if self.comp.prefs.target_backend == prefs.Backend.C:
                    name = f"ArrayReturn{len(self.generated_array_returns)}"
                    name = f"_R{len(name)}{name}"
                    if name not in self.generated_array_returns:
                        arr_ret_struct = name
                        self.out_rir.structs.append(
                            ir.Struct(False, name, [ir.Field("arr", ret_typ)])
                        )
                        self.generated_array_returns.append(name)
                    ret_typ = ir.Type(name)
            fn_decl = ir.FnDecl(
                decl.vis.is_pub(), decl.attrs, decl.is_extern
                and not decl.has_body, decl.sym.name if decl.is_extern
                and not decl.has_body else mangle_symbol(decl.sym), args,
                decl.is_variadic and decl.is_extern, ret_typ,
                decl.ret_typ == self.comp.never_t
            )
            self.cur_fn = fn_decl
            self.cur_fn.arr_ret_struct = arr_ret_struct
            self.cur_fn_is_main = decl.is_main
            self.cur_fn_ret_typ = decl.ret_typ
            for defer_stmt in decl.defer_stmts:
                defer_stmt.flag_var = self.cur_fn.local_name()
                self.cur_fn.alloca(
                    ir.Ident(ir.BOOL_T, defer_stmt.flag_var),
                    ir.IntLit(ir.BOOL_T, "0")
                )
            self.gen_stmts(decl.stmts)
            self.gen_defer_stmts()
            if str(fn_decl.ret_typ) == "_R7Result__R4void":
                self.cur_fn.add_ret(self.result_void(decl.ret_typ))
            elif str(fn_decl.ret_typ) != "void" and not (
                len(fn_decl.instrs) > 0
                and isinstance(fn_decl.instrs[-1], ir.Inst)
                and fn_decl.instrs[-1].kind == ir.InstKind.Ret
            ):
                self.cur_fn.add_ret(self.default_value(decl.ret_typ))
            if decl.is_extern and not decl.has_body:
                self.out_rir.externs.append(fn_decl)
            else:
                self.out_rir.decls.append(fn_decl)
        elif isinstance(decl, ast.DestructorDecl):
            self_typ = self.ir_type(decl.self_typ)
            if decl.self_is_mut and decl.self_typ.sym.kind != TypeKind.Class:
                self_typ = self_typ.ptr()
            self_arg = ir.Ident(self_typ, "self")
            dtor_fn = ir.FnDecl(
                False, [], False, f"{mangle_type(decl.self_typ)}6_dtor_",
                [self_arg], False, ir.VOID_T, False
            )
            self.cur_fn = dtor_fn
            for defer_stmt in decl.defer_stmts:
                defer_stmt.flag_var = self.cur_fn.local_name()
                self.cur_fn.alloca(
                    ir.Ident(ir.BOOL_T, defer_stmt.flag_var),
                    ir.IntLit(ir.BOOL_T, "0")
                )
            self.gen_stmts(decl.stmts)
            self.gen_defer_stmts()
            self.out_rir.decls.append(dtor_fn)
        elif isinstance(decl, ast.TestDecl):
            if self.comp.prefs.build_mode == prefs.BuildMode.Test:
                if not self.sf.sym.is_root:
                    return # skip non-root module tests
                self.inside_test = True
                test_name = utils.smart_quote(decl.name, True)
                test_func = f"__test{len(self.generated_tests)}__"
                test_func = f"_R{len(test_func)}{test_func}"
                test_fn = ir.FnDecl(
                    False, [], False, test_func,
                    [ir.Ident(ir.TEST_T.ptr(), "test")], False, ir.VOID_T, False
                )
                self.cur_fn = test_fn
                self.gen_stmts(decl.stmts)
                self.generated_tests.append(TestInfo(test_name, test_func))
                self.out_rir.decls.append(test_fn)
                self.inside_test = False

    def gen_stmts(self, stmts):
        for stmt in stmts:
            self.gen_stmt(stmt)

    def gen_stmt(self, stmt):
        if isinstance(stmt, ast.ForStmt):
            old_entry_label = self.loop_entry_label
            old_exit_label = self.loop_exit_label
            iterable_sym = stmt.iterable.typ.symbol()
            self.loop_entry_label = self.cur_fn.local_name()
            body_label = self.cur_fn.local_name()
            self.loop_exit_label = self.cur_fn.local_name()
            self.cur_fn.add_comment("for in stmt")
            idx_name = stmt.index.name if stmt.index else self.cur_fn.local_name(
            )
            iterable = self.gen_expr(stmt.iterable)
            self.cur_fn.try_alloca(
                ir.USIZE_T, idx_name, ir.IntLit(ir.USIZE_T, "0")
            )
            idx = ir.Ident(ir.USIZE_T, idx_name)
            self.cur_fn.add_label(self.loop_entry_label)
            if iterable_sym.kind == TypeKind.Array:
                len_ = ir.IntLit(ir.USIZE_T, iterable_sym.info.size.lit)
            else:
                len_ = ir.Selector(ir.USIZE_T, iterable, ir.Name("len"))
            self.cur_fn.add_cond_br(
                ir.Inst(ir.InstKind.Cmp, [ir.Name("<"), idx, len_]), body_label,
                self.loop_exit_label
            )
            self.cur_fn.add_label(body_label)
            value_t_ir = self.ir_type(iterable_sym.info.elem_typ)
            value_is_ref_or_is_mut = stmt.value.is_ref or stmt.value.is_mut
            value_t_is_boxed = isinstance(
                value_t_ir, ir.Pointer
            ) and value_t_ir.is_managed
            if iterable_sym.kind == TypeKind.Array:
                value = ir.Inst(
                    ir.InstKind.GetElementPtr, [iterable, idx], value_t_ir
                )
            else:
                value = ir.Selector(ir.VOID_PTR_T, iterable, ir.Name("ptr"))
                value = ir.Inst(
                    ir.InstKind.Add, [
                        ir.Inst(
                            ir.InstKind.Cast,
                            [value, value_t_ir.ptr(value_t_is_boxed)]
                        ), idx
                    ]
                )
            if value_is_ref_or_is_mut and not isinstance(
                value_t_ir, ir.Pointer
            ):
                value_t_ir = ir.Pointer(value_t_ir)
            if not value_is_ref_or_is_mut:
                value = ir.Inst(ir.InstKind.LoadPtr, [value])
            unique_ir_name = self.cur_fn.unique_name(stmt.value.name)
            self.cur_fn.try_alloca(value_t_ir, unique_ir_name, value)
            stmt.scope.update_ir_name(stmt.value.name, unique_ir_name)
            self.gen_stmt(stmt.stmt)
            self.cur_fn.add_inst(ir.Inst(ir.InstKind.Inc, [idx]))
            self.cur_fn.add_br(self.loop_entry_label)
            self.cur_fn.add_label(self.loop_exit_label)
            self.loop_entry_label = old_entry_label
            self.loop_exit_label = old_exit_label
        elif isinstance(stmt, ast.WhileStmt):
            old_entry_label = self.loop_entry_label
            old_exit_label = self.loop_exit_label
            self.cur_fn.add_comment(f"while stmt (is_inf: {stmt.is_inf})")
            self.loop_entry_label = self.cur_fn.local_name()
            body_label = self.cur_fn.local_name()
            self.loop_exit_label = self.cur_fn.local_name()
            self.while_continue_expr = stmt.continue_expr
            self.cur_fn.add_label(self.loop_entry_label)
            if stmt.is_inf:
                cond = ir.IntLit(self.comp.bool_t, "1")
                self.cur_fn.add_br(body_label)
            else:
                if isinstance(stmt.cond, ast.GuardExpr):
                    gexpr = self.gen_expr_with_cast(
                        stmt.cond.expr.typ, stmt.cond.expr
                    )
                    if stmt.cond.is_result:
                        cond = ir.Inst(
                            InstKind.BooleanNot,
                            [ir.Selector(ir.BOOL_T, gexpr, ir.Name("is_err"))]
                        )
                        var_t = self.ir_type(stmt.cond.expr.typ.typ)
                        self.cur_fn.try_alloca(
                            var_t, stmt.cond.vars[0],
                            ir.Selector(var_t, gexpr, ir.Name("value"))
                        )
                    else:
                        if isinstance(stmt.cond.typ, (type.Ref, type.Ptr)):
                            cond = ir.Inst(
                                ir.InstKind.Cmp, [
                                    ir.Name("!="), gexpr,
                                    ir.NilLit(ir.VOID_PTR_T)
                                ]
                            )
                            self.cur_fn.try_alloca(
                                self.ir_type(stmt.cond.expr.typ.typ),
                                stmt.cond.vars[0], gexpr
                            )
                        else:
                            cond = ir.Inst(
                                ir.InstKind.BooleanNot, [
                                    ir.Selector(
                                        ir.BOOL_T, gexpr, ir.Name("is_nil")
                                    )
                                ]
                            )
                            self.cur_fn.try_alloca(
                                self.ir_type(stmt.cond.expr.typ.typ),
                                stmt.cond.vars[0],
                                ir.Selector(
                                    self.ir_type(stmt.cond.expr.typ.typ), gexpr,
                                    ir.Name("value")
                                )
                            )
                    if stmt.cond.has_cond:
                        gcond = self.gen_expr_with_cast(
                            self.comp.bool_t, stmt.cond.cond
                        )
                        self.cur_fn.add_cond_br(
                            gcond, body_label, self.loop_exit_label
                        )
                else:
                    cond = self.gen_expr_with_cast(self.comp.bool_t, stmt.cond)
                if isinstance(cond, ir.IntLit) and cond.lit == "1":
                    self.cur_fn.add_br(body_label)
                else:
                    self.cur_fn.add_cond_br(
                        cond, body_label, self.loop_exit_label
                    )
            gen_stmt = True
            if isinstance(cond, ir.IntLit) and cond.lit == "0":
                self.cur_fn.add_comment("skip while stmt (cond: false)")
                gen_stmt = False
            self.cur_fn.add_label(body_label)
            if gen_stmt:
                self.gen_stmt(stmt.stmt)
                if stmt.has_continue_expr:
                    self.gen_expr(stmt.continue_expr)
                self.cur_fn.add_comment(
                    f"while stmt (goto to `{self.loop_entry_label}` for continue)"
                )
                self.cur_fn.add_br(self.loop_entry_label)
            self.cur_fn.add_label(self.loop_exit_label)
            self.loop_entry_label = old_entry_label
            self.loop_exit_label = old_exit_label
            self.while_continue_expr = None
        elif isinstance(stmt, ast.LetStmt):
            if len(stmt.lefts) == 1:
                left = stmt.lefts[0]
                left_ir_typ = self.ir_type(left.typ)
                ident = ir.Ident(
                    left_ir_typ,
                    self.cur_fn.local_name()
                    if left.name == "_" else self.cur_fn.unique_name(left.name)
                )
                stmt.scope.update_ir_name(left.name, ident.name)
                if isinstance(left_ir_typ, ir.Array):
                    self.cur_fn.alloca(ident)
                    val = self.gen_expr_with_cast(left.typ, stmt.right, ident)
                else:
                    self.cur_fn.alloca(
                        ident, self.gen_expr_with_cast(left.typ, stmt.right)
                    )
            else:
                right = self.gen_expr(stmt.right)
                for i, left in enumerate(stmt.lefts):
                    left_ir_typ = self.ir_type(left.typ)
                    ident = ir.Ident(
                        left_ir_typ,
                        self.cur_fn.local_name() if left.name == "_" else
                        self.cur_fn.unique_name(left.name)
                    )
                    stmt.scope.update_ir_name(left.name, ident.name)
                    if isinstance(left_ir_typ, ir.Array):
                        size, _ = self.comp.type_size(left.typ)
                        self.cur_fn.alloca(ident)
                        self.cur_fn.add_call(
                            "_R7runtime8mem_copyF", [
                                ident,
                                ir.Selector(
                                    left_ir_typ, right, ir.Name(f"f{i}")
                                ),
                                ir.IntLit(ir.USIZE_T, str(size))
                            ]
                        )
                    else:
                        self.cur_fn.alloca(
                            ident,
                            ir.Selector(left_ir_typ, right, ir.Name(f"f{i}"))
                        )
        elif isinstance(stmt, ast.DeferStmt):
            self.cur_fn.store(
                ir.Ident(ir.BOOL_T, stmt.flag_var), ir.IntLit(ir.BOOL_T, "1")
            )
            self.cur_fn_defer_stmts.append(stmt)
        elif isinstance(stmt, ast.ExprStmt):
            _ = self.gen_expr(stmt.expr)

    def gen_expr_with_cast(self, expected_typ_, expr, custom_tmp = None):
        expected_typ = self.ir_type(expected_typ_)
        res_expr = self.gen_expr(expr, custom_tmp)

        if isinstance(res_expr, ir.IntLit) and self.comp.is_int(expected_typ_):
            res_expr.typ = expected_typ
        elif isinstance(res_expr,
                        ir.FloatLit) and self.comp.is_float(expected_typ_):
            res_expr.typ = expected_typ
        elif self.comp.is_comptime_number(
            expr.typ
        ) and self.comp.is_number(expected_typ_):
            res_expr.typ = expected_typ_

        if isinstance(res_expr.typ,
                      ir.Pointer) and str(res_expr.typ) != "*void":
            if isinstance(expected_typ, ir.Pointer):
                if not expected_typ.is_managed:
                    nr_level_expected = expected_typ.nr_level()
                    nr_level = res_expr.typ.nr_level()
                    if nr_level > nr_level_expected:
                        while nr_level > nr_level_expected:
                            if isinstance(
                                res_expr.typ, ir.Pointer
                            ) and res_expr.typ.is_managed:
                                break
                            res_expr = ir.Inst(
                                ir.InstKind.LoadPtr, [res_expr],
                                res_expr.typ.typ
                            )
                            nr_level -= 1
                    elif nr_level < nr_level_expected:
                        while nr_level < nr_level_expected:
                            res_expr = ir.Inst(
                                ir.InstKind.GetRef, [res_expr],
                                res_expr.typ.ptr()
                            )
                            nr_level += 1
            elif not res_expr.typ.is_managed:
                res_expr = ir.Inst(
                    ir.InstKind.LoadPtr, [res_expr], res_expr.typ.typ
                )
        elif isinstance(
            expected_typ, ir.Pointer
        ) and not expected_typ.is_managed and str(res_expr.typ) != "void":
            nr_level_expected = expected_typ.nr_level()
            nr_level = res_expr.typ.nr_level(
            ) if isinstance(res_expr.typ, ir.Pointer) else 0
            while nr_level < nr_level_expected:
                res_expr = ir.Inst(
                    ir.InstKind.GetRef, [res_expr], res_expr.typ.ptr()
                )
                nr_level += 1

        if hasattr(expr, "expected_typ"):
            expr_typ = expr.expected_typ
        else:
            expr_typ = expr.typ
        expr_sym = expr.typ.symbol()
        expected_sym = expected_typ_.symbol()
        if expected_sym.kind == TypeKind.Trait and expected_typ_ != expr_typ:
            res_expr = self.trait_value(res_expr, expr_typ, expected_typ_)
        elif expected_sym.kind == TypeKind.Class and expr_sym.is_subtype_of(
            expected_sym
        ) and expr_sym != expected_sym:
            res_expr = self.class_upcast(res_expr, expr_typ, expected_typ_)

        # wrap optional value
        if isinstance(expected_typ_, type.Optional
                      ) and not isinstance(expected_typ, ir.Pointer):
            if isinstance(res_expr, ir.NilLit):
                res_expr = self.optional_nil(expected_typ_)
            elif not isinstance(res_expr, ir.Skip
                                ) and not isinstance(expr_typ, type.Optional):
                res_expr = self.optional_value(expected_typ_, res_expr)

        return res_expr

    def gen_expr(self, expr, custom_tmp = None):
        if isinstance(expr, ast.ParExpr):
            return self.gen_expr(expr.expr)
        elif isinstance(expr, ast.NilLiteral):
            return ir.NilLit(ir.VOID_PTR_T)
        elif isinstance(expr, ast.BoolLiteral):
            return ir.IntLit(ir.BOOL_T, str(int(expr.lit)))
        elif isinstance(expr, ast.CharLiteral):
            if expr.is_byte:
                return ir.IntLit(ir.U8_T, str(utils.bytestr(expr.lit).buf[0]))
            else:
                return ir.RuneLit(ir.RUNE_T, expr.lit)
        elif isinstance(expr, ast.IntegerLiteral):
            return ir.IntLit(self.ir_type(expr.typ), expr.lit)
        elif isinstance(expr, ast.FloatLiteral):
            return ir.FloatLit(ir.F64_T, expr.lit)
        elif isinstance(expr, ast.StringLiteral):
            escaped_val = utils.smart_quote(expr.lit, expr.is_raw)
            size = utils.bytestr(expr.lit).len
            if expr.is_bytestr:
                return ir.ArrayLit(
                    self.ir_type(expr.typ), [
                        ir.IntLit(ir.U8_T, str(b))
                        for b in list(utils.bytestr(escaped_val).buf)
                    ]
                )
            if expr.typ == self.comp.string_t:
                return self.gen_string_lit(
                    escaped_val,
                    utils.bytestr(expr.lit).len
                )
            return ir.StringLit(escaped_val, str(size))
        elif isinstance(expr, ast.EnumLiteral):
            enum_sym = expr.typ.symbol()
            if expr.is_instance:
                return self.advanced_enum_value(
                    enum_sym, expr.sym.name, expr.value_arg
                )
            return ir.IntLit(
                self.ir_type(enum_sym.info.underlying_typ), str(expr.sym.value)
            )
        elif isinstance(expr, (ast.SelfExpr, ast.BaseExpr)):
            self_typ = self.ir_type(expr.typ)
            if expr.is_mut and not expr.typ.symbol().is_boxed():
                self_typ = self_typ.ptr()
            return ir.Ident(self_typ, "self")
        elif isinstance(expr, ast.Ident):
            if isinstance(expr.sym, sym.Const):
                return self.gen_const(expr.sym)
            elif isinstance(expr.sym, sym.Var):
                ir_typ = self.ir_type(expr.typ)
                if expr.sym.is_extern:
                    return ir.Ident(
                        ir_typ,
                        mangle_symbol(expr.sym)
                        if expr.sym.abi == sym.ABI.Rivet else expr.sym.name
                    )
                return ir.Ident(ir_typ, mangle_symbol(expr.sym))
            elif isinstance(expr.sym, sym.Fn):
                return ir.Ident(self.ir_type(expr.typ), mangle_symbol(expr.sym))
            # runtime object
            i_typ = self.ir_type(expr.typ)
            if (expr.obj.level == sym.ObjLevel.Arg and expr.obj.is_mut) or (
                expr.obj.is_hidden_ref and not isinstance(i_typ, ir.Pointer)
            ):
                i_typ = i_typ.ptr()
            return ir.Ident(i_typ, expr.obj.ir_name)
        elif isinstance(expr, ast.BuiltinCallExpr):
            if expr.name == "vec":
                typ_sym = expr.typ.symbol()
                if len(expr.args) == 2:
                    return self.empty_vec(typ_sym, self.gen_expr(expr.args[1]))
                return self.empty_vec(typ_sym)
            elif expr.name == "as":
                arg1 = expr.args[1]
                ir_typ = self.ir_type(expr.typ)
                res = self.gen_expr_with_cast(arg1.typ, arg1)
                if isinstance(res, ir.IntLit):
                    if self.comp.is_int(ir_typ) or expr.typ == self.comp.bool_t:
                        res.typ = ir_typ
                        return res
                typ_sym = arg1.typ.symbol()
                expr_typ_sym = expr.typ.symbol()
                if typ_sym.kind == TypeKind.Class and typ_sym.kind == expr_typ_sym.kind:
                    if typ_sym == expr_typ_sym:
                        return res
                    if typ_sym.is_subtype_of(expr_typ_sym): # up-casting
                        return self.class_upcast(res, arg1.typ, expr.typ)
                    if expr_typ_sym.is_subtype_of(typ_sym): # down-casting
                        return self.class_downcast(res, arg1.typ, expr.typ)
                elif typ_sym.kind == TypeKind.Enum and typ_sym.info.is_advanced_enum:
                    tmp = self.cur_fn.local_name()
                    self.cur_fn.try_alloca(
                        ir_typ, tmp,
                        ir.Inst(
                            ir.InstKind.LoadPtr, [
                                ir.Inst(
                                    ir.InstKind.Cast, [
                                        ir.Selector(
                                            ir.VOID_PTR_T, res, ir.Name("obj")
                                        ),
                                        ir_typ.ptr(True)
                                    ]
                                )
                            ]
                        )
                    )
                    return ir.Ident(ir_typ, tmp)
                return ir.Inst(ir.InstKind.Cast, [res, ir_typ], ir_typ)
            elif expr.name in ("size_of", "align_of"):
                size, align = self.comp.type_size(expr.args[0].typ)
                if expr.name == "size_of":
                    return ir.IntLit(ir.USIZE_T, str(size))
                return ir.IntLit(ir.USIZE_T, str(align))
            elif expr.name in ("addr_of", "addr_of_mut"):
                value = self.gen_expr(expr.args[0])
                return ir.Inst(ir.InstKind.GetRef, [value], value.typ.ptr())
            elif expr.name == "assert":
                msg_ = f"`{expr.args[0]}`"
                msg = utils.smart_quote(msg_, False)
                if self.inside_test:
                    tmp_id = ir.Ident(ir.TEST_T.ptr(), "test")
                    pos = utils.smart_quote(str(expr.pos), False)
                    self.cur_fn.add_call(
                        "_R7runtime11assert_testF", [
                            self.gen_expr(expr.args[0]),
                            self.gen_string_lit(msg,
                                                utils.bytestr(msg_).len),
                            self.gen_string_lit(
                                pos,
                                utils.bytestr(str(expr.pos)).len
                            ), tmp_id
                        ]
                    )
                    l1 = self.cur_fn.local_name()
                    l2 = self.cur_fn.local_name()
                    self.cur_fn.add_cond_br(
                        ir.Selector(ir.BOOL_T, tmp_id, ir.Name("early_return")),
                        l1, l2
                    )
                    self.cur_fn.add_label(l1)
                    self.cur_fn.add_ret_void()
                    self.cur_fn.add_label(l2)
                else:
                    self.cur_fn.add_call(
                        "_R7runtime6assertF",
                        [self.gen_expr(expr.args[0]),
                         self.gen_string_lit(msg)]
                    )
            elif expr.name in ("ptr_add", "ptr_diff"):
                return ir.Inst(
                    ir.InstKind.Add
                    if expr.name == "ptr_add" else ir.InstKind.Sub,
                    [self.gen_expr(expr.args[0]),
                     self.gen_expr(expr.args[1])]
                )
            elif expr.name == "unreachable":
                self.panic("entered unreachable code")
            elif expr.name == "breakpoint":
                if self.comp.prefs.build_mode != prefs.BuildMode.Release:
                    self.cur_fn.breakpoint()
        elif isinstance(expr, ast.TupleLiteral):
            expr_sym = expr.typ.symbol()
            tmp = ir.Ident(self.ir_type(expr.typ), self.cur_fn.local_name())
            self.cur_fn.alloca(tmp)
            for i, elem in enumerate(expr.exprs):
                elem_typ = expr_sym.info.types[i]
                field_expr = self.gen_expr_with_cast(elem_typ, elem)
                self.cur_fn.store(
                    ir.Selector(elem_typ, tmp, ir.Name(f"f{i}")), field_expr
                )
            return tmp
        elif isinstance(expr, ast.AssignExpr):
            left = None
            if isinstance(expr.left, ast.Ident):
                if expr.left.name == "_":
                    return ir.Inst(
                        ir.InstKind.Cast,
                        [self.gen_expr(expr.right), ir.VOID_T]
                    )
                else:
                    left = self.gen_expr_with_cast(expr.left.typ, expr.left)
            elif isinstance(expr.left, ast.SelectorExpr):
                if expr.left.is_indirect:
                    left = ir.Inst(
                        ir.InstKind.LoadPtr, [self.gen_expr(expr.left.left)]
                    )
                else:
                    left = self.gen_expr_with_cast(expr.left.typ, expr.left)
            elif isinstance(expr.left, ast.IndexExpr):
                left_ir_typ = self.ir_type(expr.left.left_typ)
                left_sym = expr.left.left_typ.symbol()
                sym_is_class = left_sym.is_boxed()
                if left_sym.kind == TypeKind.Vec and expr.op == Kind.Assign:
                    rec = self.gen_expr_with_cast(
                        expr.left.left_typ, expr.left.left
                    )
                    if not isinstance(left_ir_typ, ir.Pointer):
                        rec = ir.Inst(ir.InstKind.GetRef, [rec])
                    expr_right = self.gen_expr_with_cast(
                        expr.right.typ, expr.right
                    )
                    val_sym = expr.right.typ.symbol()
                    self.cur_fn.add_call(
                        "_R7runtime3Vec3setM", [
                            rec,
                            self.gen_expr(expr.left.index),
                            ir.Inst(ir.InstKind.GetRef, [expr_right])
                        ]
                    )
                    return
                left = ir.Inst(ir.InstKind.LoadPtr, [self.gen_expr(expr.left)])
            if left == None:
                return
            expr_left_typ_ir = self.ir_type(expr.left.typ)
            expr_left_sym = expr.left.typ.symbol()
            if expr.op == Kind.Assign:
                if isinstance(expr_left_typ_ir, ir.Array):
                    self.gen_expr_with_cast(left.typ, stmt.right, ident)
                else:
                    value = self.gen_expr_with_cast(expr.left.typ, expr.right)
                    self.cur_fn.store(left, value)
            else:
                single_op = expr.op.single()
                right = self.gen_expr_with_cast(expr.left.typ, expr.right)
                if expr_left_sym.kind in (TypeKind.Class, TypeKind.Struct
                                          ) and expr_left_sym.exists(
                                              str(single_op)
                                          ):
                    ov_m = OVERLOADABLE_OPERATORS_STR[str(single_op)]
                    left_operand = left
                    right_operand = right
                    if not isinstance(expr_left_typ_ir, ir.Pointer):
                        left_operand = ir.Inst(
                            ir.InstKind.GetRef, [left_operand]
                        )
                        right_operand = ir.Inst(
                            ir.InstKind.GetRef, [right_operand]
                        )
                    self.cur_fn.store(
                        left,
                        ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name(
                                    f"{mangle_symbol(expr_left_sym)}{len(ov_m)}{ov_m}M"
                                ), left_operand, right_operand
                            ]
                        )
                    )
                elif op_kind := ir.get_ir_op(single_op):
                    self.cur_fn.store(left, ir.Inst(op_kind, [left, right]))
                else:
                    assert False
        elif isinstance(expr, ast.Block):
            self.gen_stmts(expr.stmts)
            if expr.is_expr:
                return self.gen_expr(expr.expr)
            return ir.Skip()
        elif isinstance(expr, ast.CallExpr):
            if expr.is_ctor:
                typ_sym = expr.typ.symbol()
                if typ_sym.kind == TypeKind.Trait:
                    value = expr.args[0].expr
                    return self.trait_value(
                        self.gen_expr_with_cast(value.typ, value), value.typ,
                        expr.typ
                    )
                elif typ_sym.kind == TypeKind.Enum:
                    return self.advanced_enum_value(
                        typ_sym, expr.left.field_name, expr.args[0].expr,
                        custom_tmp = custom_tmp
                    )
                if custom_tmp:
                    tmp = custom_tmp
                elif typ_sym.is_boxed():
                    tmp = self.boxed_instance(
                        mangle_symbol(typ_sym), typ_sym.id
                    )
                else:
                    tmp = ir.Ident(
                        self.ir_type(expr.typ), self.cur_fn.local_name()
                    )
                    self.cur_fn.alloca(tmp)
                initted_fields = []
                type_fields = typ_sym.full_fields()
                for i, f in enumerate(expr.args):
                    if f.is_named:
                        for ff in type_fields:
                            if ff.name == f.name:
                                field = f
                                break
                    else:
                        field = type_fields[i]
                    initted_fields.append(field.name)
                    self.cur_fn.store(
                        ir.Selector(
                            self.ir_type(field.typ), tmp, ir.Name(field.name)
                        ), self.gen_expr_with_cast(field.typ, f.expr)
                    )
                if expr.has_spread_expr:
                    spread_val = self.gen_expr_with_cast(
                        expr.spread_expr.typ, expr.spread_expr
                    )
                else:
                    spread_val = None
                for f in typ_sym.full_fields():
                    if f.name in initted_fields:
                        continue
                    if f.typ.symbol().kind == TypeKind.Array:
                        continue
                    f_typ = self.ir_type(f.typ)
                    sltor = ir.Selector(f_typ, tmp, ir.Name(f.name))
                    if f.has_def_expr:
                        value = self.gen_expr_with_cast(f.typ, f.def_expr)
                    elif expr.has_spread_expr:
                        value = ir.Selector(f_typ, spread_val, ir.Name(f.name))
                    else:
                        value = self.default_value(f.typ)
                    self.cur_fn.store(
                        ir.Selector(f_typ, tmp, ir.Name(f.name)), value
                    )
                return tmp
            args = []
            is_trait_call = False
            if not expr.sym:
                raise Exception(f"expr.sym is None [ {expr} ] at {expr.pos}")
            if expr.sym.is_method:
                left_sym = expr.sym.self_typ.symbol()
                if left_sym.kind == TypeKind.Trait:
                    is_trait_call = True
                    self_expr = ir.Inst(
                        ir.InstKind.LoadPtr, [
                            self.gen_expr_with_cast(
                                expr.sym.self_typ, expr.left.left
                            )
                        ]
                    )
                    args.append(
                        ir.Selector(
                            ir.VOID_PTR_T,
                            ir.Inst(
                                ir.InstKind.LoadPtr, [
                                    ir.Inst(
                                        ir.InstKind.GetElementPtr, [
                                            ir.Name(
                                                mangle_symbol(left_sym) +
                                                "4VTBL"
                                            ),
                                            ir.Selector(
                                                self.comp.usize_t, self_expr,
                                                ir.Name("_id")
                                            )
                                        ]
                                    )
                                ]
                            ), ir.Name(expr.sym.name)
                        )
                    )
                    args.append(
                        ir.Selector(ir.VOID_PTR_T, self_expr, ir.Name("obj"))
                    )
            if not is_trait_call:
                if expr.is_closure:
                    name = self.gen_expr_with_cast(expr.left.typ, expr.left)
                elif expr.sym.is_extern and expr.sym.abi != sym.ABI.Rivet and not expr.sym.has_body:
                    name = ir.Name(expr.sym.name)
                else:
                    name = ir.Name(mangle_symbol(expr.sym))
                args.append(name)
                if expr.sym.is_method:
                    self_expr = self.gen_expr_with_cast(
                        expr.sym.self_typ, expr.left.left
                    )
                    is_boxed = expr.sym.self_typ.symbol().is_boxed()
                    if (
                        expr.sym.self_is_mut or expr.sym.self_is_ref
                    ) and not is_boxed:
                        self_expr = ir.Inst(ir.InstKind.GetRef, [self_expr])
                    args.append(self_expr)
                    if left_sym.kind == TypeKind.Vec:
                        expr.sym = self.comp.vec_sym[expr.sym.name]
            args_len = expr.sym.args_len()
            for i, arg in enumerate(expr.args):
                if expr.sym.is_variadic and i == args_len:
                    break
                fn_arg = expr.sym.args[i]
                arg_typ = fn_arg.typ
                if fn_arg.is_mut:
                    arg_typ = type.Ptr(arg_typ)
                args.append(self.gen_expr_with_cast(arg_typ, arg.expr))
            if expr.has_spread_expr:
                args.append(
                    self.gen_expr_with_cast(
                        expr.spread_expr.typ, expr.spread_expr
                    )
                )
            elif expr.sym.is_variadic:
                variadic_count = len(expr.args) - args_len
                if expr.sym.is_extern:
                    for i in range(args_len, len(expr.args)):
                        arg = expr.args[i]
                        args.append(self.gen_expr_with_cast(arg.typ, arg.expr))
                else:
                    var_arg = expr.sym.args[-1]
                    if variadic_count == 1 and len(expr.args
                                                   ) > 0 and isinstance(
                                                       expr.args[-1].expr.typ,
                                                       type.Variadic
                                                   ):
                        arg = expr.args[-1]
                        args.append(self.gen_expr_with_cast(arg.typ, arg.expr))
                    elif variadic_count > 0:
                        vargs = []
                        for i in range(args_len, len(expr.args)):
                            vargs.append(
                                self.gen_expr_with_cast(
                                    var_arg.typ.typ, expr.args[i].expr
                                )
                            )
                        args.append(self.variadic_args(vargs, var_arg.typ.typ))
                    else:
                        args.append(self.empty_vec(var_arg.typ.symbol()))
            inst = ir.Inst(ir.InstKind.Call, args)
            if expr.sym.ret_typ in self.void_types:
                self.cur_fn.add_inst(inst)
            else:
                is_void_value = expr.typ in self.void_types
                tmp = "" if custom_tmp else self.cur_fn.local_name()
                if isinstance(expr.sym.ret_typ, type.Array):
                    size, _ = self.comp.type_size(expr.sym.ret_typ)
                    if custom_tmp:
                        id = custom_tmp
                    else:
                        id = ir.Ident(self.ir_type(expr.sym.ret_typ), tmp)
                        self.cur_fn.alloca(id)
                    self.cur_fn.add_call(
                        "_R7runtime8mem_copyF", [
                            id,
                            ir.Selector(
                                self.ir_type(expr.sym.ret_typ), inst,
                                ir.Name("arr")
                            ),
                            ir.IntLit(ir.USIZE_T, str(size))
                        ]
                    )
                elif expr.sym.is_method and expr.sym.name == "pop" and left_sym.kind == TypeKind.Vec:
                    ret_typ = self.ir_type(expr.sym.ret_typ)
                    value = ir.Inst(ir.InstKind.Cast, [inst, ret_typ.ptr()])
                    if custom_tmp:
                        self.cur_fn.store(
                            custom_tmp, ir.Inst(ir.InstKind.LoadPtr, [value])
                        )
                    else:
                        self.cur_fn.try_alloca(
                            ret_typ, tmp, ir.Inst(ir.InstKind.LoadPtr, [value])
                        )
                else:
                    if custom_tmp:
                        self.cur_fn.store(custom_tmp, inst)
                    else:
                        self.cur_fn.try_alloca(
                            self.ir_type(expr.sym.ret_typ), tmp, inst
                        )
                if expr.has_err_handler():
                    err_handler_is_void = (
                        not expr.err_handler.is_propagate
                    ) and expr.err_handler.expr.typ in self.void_types
                    res_value = ir.Ident(self.ir_type(expr.sym.ret_typ), tmp)
                    panic_l = self.cur_fn.local_name()
                    else_value = "" if err_handler_is_void else self.cur_fn.local_name(
                    )
                    exit_l = "" if expr.err_handler.is_propagate else self.cur_fn.local_name(
                    )
                    if err_handler_is_void:
                        self.cur_fn.add_cond_br(
                            ir.Selector(
                                ir.BOOL_T, res_value, ir.Name("is_err")
                            ), panic_l, exit_l
                        )
                    else:
                        self.cur_fn.add_cond_br(
                            ir.Selector(
                                ir.BOOL_T, res_value, ir.Name("is_err")
                            ), panic_l, else_value
                        )
                    self.cur_fn.add_label(panic_l)
                    if expr.err_handler.is_propagate:
                        if self.cur_fn_is_main:
                            self.cur_fn.add_call(
                                "_R7runtime11error_panicF", [
                                    ir.Selector(
                                        self.ir_type(self.comp.error_t),
                                        res_value, ir.Name("err")
                                    )
                                ]
                            )
                        else:
                            if self.inside_test:
                                pos = utils.smart_quote(str(expr.pos), False)
                                self.cur_fn.add_call(
                                    "_R7runtime19test_error_returnedF", [
                                        ir.Selector(
                                            self.ir_type(self.comp.error_t),
                                            res_value, ir.Name("err")
                                        ),
                                        self.gen_string_lit(pos),
                                        ir.Ident(ir.TEST_T, "test")
                                    ]
                                )
                                self.cur_fn.add_ret_void()
                            else:
                                tmp2 = ir.Ident(
                                    self.ir_type(self.cur_fn_ret_typ),
                                    self.cur_fn.local_name()
                                )
                                self.cur_fn.alloca(tmp2)
                                self.cur_fn.store(
                                    ir.Selector(
                                        ir.BOOL_T, tmp2, ir.Name("is_err")
                                    ), ir.IntLit(ir.BOOL_T, "1")
                                )
                                self.cur_fn.store(
                                    ir.Selector(
                                        self.ir_type(self.comp.error_t), tmp2,
                                        ir.Name("err")
                                    ),
                                    ir.Selector(
                                        self.ir_type(self.comp.error_t),
                                        res_value, ir.Name("err")
                                    )
                                )
                                self.cur_fn.add_ret(tmp2)
                        self.cur_fn.add_label(else_value)
                        if is_void_value:
                            return ir.Skip()
                        return ir.Selector(
                            self.ir_type(expr.sym.ret_typ.typ), res_value,
                            ir.Name("value")
                        )
                    else: # `catch`
                        if expr.err_handler.has_varname():
                            self.cur_fn.try_alloca(
                                self.ir_type(self.comp.error_t),
                                expr.err_handler.varname,
                                ir.Selector(
                                    self.ir_type(self.comp.error_t), res_value,
                                    ir.Name("err")
                                )
                            )
                        if err_handler_is_void:
                            _ = self.gen_expr_with_cast(
                                expr.sym.ret_typ.typ, expr.err_handler.expr
                            )
                            self.cur_fn.add_label(exit_l)
                            return ir.Selector(
                                self.ir_type(expr.typ), res_value,
                                ir.Name("value")
                            )
                        tmp2 = ir.Ident(
                            self.ir_type(expr.sym.ret_typ.typ),
                            self.cur_fn.local_name()
                        )
                        self.cur_fn.alloca(
                            tmp2,
                            self.gen_expr_with_cast(
                                expr.sym.ret_typ.typ, expr.err_handler.expr
                            )
                        )
                        self.cur_fn.add_br(exit_l)
                        self.cur_fn.add_label(else_value)
                        self.cur_fn.store(
                            tmp2,
                            ir.Selector(
                                self.ir_type(expr.typ), res_value,
                                ir.Name("value")
                            )
                        )
                        self.cur_fn.add_label(exit_l)
                        return tmp2
                return ir.Ident(self.ir_type(expr.sym.ret_typ), tmp)
        elif isinstance(expr, ast.SelectorExpr):
            if expr.is_symbol_access:
                if isinstance(expr.field_sym, sym.Const):
                    return self.gen_const(expr.field_sym)
                elif isinstance(
                    expr.left_sym, sym.Type
                ) and expr.left_sym.kind == TypeKind.Enum:
                    if v := expr.left_sym.info.get_variant(expr.field_name):
                        return ir.IntLit(
                            self.ir_type(expr.left_sym.info.underlying_typ),
                            str(v.value)
                        )
                elif isinstance(expr.left_sym, sym.Fn):
                    return ir.Ident(
                        self.ir_type(expr.typ), mangle_symbol(expr.left_sym)
                    )
                elif isinstance(
                    expr.field_sym, sym.Var
                ) and expr.field_sym.is_extern and expr.field_sym.abi != sym.ABI.Rivet:
                    return ir.Ident(self.ir_type(expr.typ), expr.field_sym.name)
                return ir.Ident(
                    self.ir_type(expr.typ), mangle_symbol(expr.field_sym)
                )
            left_sym = expr.left_typ.symbol()
            left = self.gen_expr_with_cast(expr.left_typ, expr.left)
            ir_left_typ = self.ir_type(expr.left.typ)
            ir_typ = self.ir_type(expr.typ)
            if expr.is_indirect:
                tmp = self.cur_fn.local_name()
                self.cur_fn.try_alloca(
                    ir_typ, tmp, ir.Inst(ir.InstKind.LoadPtr, [left])
                )
                return ir.Ident(ir_typ, tmp)

            if expr.is_nilcheck:
                panic_l = self.cur_fn.local_name()
                exit_l = self.cur_fn.local_name()
                if isinstance(ir_left_typ, ir.Pointer):
                    self.cur_fn.add_cond_br(
                        ir.Inst(
                            ir.InstKind.Cmp,
                            [ir.Name("=="), left,
                             ir.NilLit(ir.VOID_PTR_T)]
                        ), panic_l, exit_l
                    )
                    self.cur_fn.add_label(panic_l)
                    self.panic(f"attempt to use nil value (`{expr.left}`)")
                    self.cur_fn.add_label(exit_l)
                    return left
                else:
                    self.cur_fn.add_cond_br(
                        ir.Selector(ir.BOOL_T, left, ir.Name("is_nil")),
                        panic_l, exit_l
                    )
                    self.cur_fn.add_label(panic_l)
                    self.panic(f"attempt to use nil value (`{expr.left}`)")
                    self.cur_fn.add_label(exit_l)
                    return ir.Selector(ir_typ, left, ir.Name("value"))
            elif isinstance(left, ir.StringLit):
                if expr.field_name == "ptr":
                    return ir.StringLit(left.lit, left.len)
                elif expr.field_name == "len":
                    return ir.IntLit(ir.USIZE_T, str(left.len))
            elif left_sym.kind == TypeKind.Array and expr.field_name == "len":
                return ir.IntLit(ir.USIZE_T, str(left_sym.info.size))
            return ir.Selector(
                ir_typ, left,
                ir.Name(
                    f"f{expr.field_name}" if left_sym.kind ==
                    TypeKind.Tuple else expr.field_name
                )
            )
        elif isinstance(expr, ast.VecLiteral):
            typ_sym = expr.typ.symbol()
            if len(expr.elems) == 0:
                if expr.is_arr:
                    return self.default_value(expr.typ)
                tmp = ir.Ident(self.ir_type(expr.typ), self.cur_fn.local_name())
                self.cur_fn.alloca(tmp, self.empty_vec(typ_sym))
                return tmp
            elem_typ = typ_sym.info.elem_typ
            size, _ = self.comp.type_size(elem_typ)
            elems = []
            for i, elem in enumerate(expr.elems):
                element = self.gen_expr_with_cast(elem_typ, elem)
                elems.append(element)
            arr_lit = ir.ArrayLit(self.ir_type(elem_typ), elems)
            if expr.is_arr:
                if custom_tmp:
                    size, _ = self.comp.type_size(expr.typ)
                    self.cur_fn.add_call(
                        "_R7runtime8mem_copyF",
                        [custom_tmp, arr_lit,
                         ir.IntLit(ir.USIZE_T, str(size))]
                    )
                    return ir.Skip()
                return arr_lit
            tmp = ir.Ident(self.ir_type(expr.typ), self.cur_fn.local_name())
            self.cur_fn.alloca(
                tmp,
                ir.Inst(
                    ir.InstKind.Call, [
                        ir.Name("_R7runtime3Vec10from_arrayF"), arr_lit,
                        ir.IntLit(ir.USIZE_T, str(size)),
                        ir.IntLit(ir.USIZE_T, str(len(elems)))
                    ]
                )
            )
            return tmp
        elif isinstance(expr, ast.IndexExpr):
            s = expr.left.typ.symbol()
            left = self.gen_expr_with_cast(expr.left_typ, expr.left)
            if isinstance(expr.index, ast.RangeExpr):
                if expr.index.has_start:
                    start = self.gen_expr(expr.index.start)
                else:
                    start = ir.IntLit(ir.USIZE_T, "0")
                if expr.index.has_end:
                    end = self.gen_expr(expr.index.end)
                else:
                    if s.kind == TypeKind.Array:
                        end = ir.IntLit(ir.USIZE_T, s.info.size.lit)
                    elif isinstance(left, ir.StringLit):
                        end = ir.IntLit(ir.USIZE_T, left.len)
                    else:
                        end = None
                tmp = self.cur_fn.local_name()
                if s.kind == TypeKind.String:
                    if end == None:
                        inst = ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name("_R7runtime6string10slice_fromM"), left,
                                start
                            ]
                        )
                    else:
                        inst = ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name("_R7runtime6string5sliceM"), left,
                                start, end
                            ]
                        )
                elif s.kind == TypeKind.Vec:
                    if end == None:
                        inst = ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name("_R7runtime3Vec10slice_fromM"), left,
                                start
                            ]
                        )
                    else:
                        inst = Inst(
                            InstKind.Call, [
                                ir.Name("_R7runtime3Vec5sliceM"), left, start,
                                end
                            ]
                        )
                else:
                    size, _ = self.comp.type_size(s.info.elem_typ)
                    if end == None:
                        inst = ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name("_R7runtime16array_slice_fromF"),
                                ir.Inst(ir.InstKind.GetRef, [left]),
                                ir.IntLit(ir.USIZE_T, str(size)),
                                ir.IntLit(ir.USIZE_T, s.info.size.lit), start
                            ]
                        )
                    else:
                        inst = ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name("_R7runtime11array_sliceF"),
                                ir.Inst(ir.InstKind.GetRef, [left]),
                                ir.IntLit(ir.USIZE_T, str(size)),
                                ir.IntLit(ir.USIZE_T, s.info.size.lit), start,
                                end
                            ]
                        )
                self.cur_fn.try_alloca(self.ir_type(expr.typ), tmp, inst)
                return ir.Ident(self.ir_type(expr.typ), tmp)
            idx = self.gen_expr(expr.index)
            if isinstance(s.info, sym.ArrayInfo):
                self.cur_fn.add_call(
                    "_R7runtime11array_indexF",
                    [ir.IntLit(ir.USIZE_T, s.info.size.lit), idx]
                )
            tmp = self.cur_fn.local_name()
            expr_typ_ir = self.ir_type(expr.typ)
            if s.kind == TypeKind.String:
                self.cur_fn.try_alloca(
                    expr_typ_ir, tmp,
                    ir.Inst(
                        ir.InstKind.Call,
                        [ir.Name("_R7runtime6string2atM"), left, idx],
                        expr_typ_ir
                    )
                )
                return ir.Ident(expr_typ_ir, tmp)
            elif s.kind == TypeKind.Vec:
                expr_typ_ir2 = expr_typ_ir.ptr()
                value = ir.Inst(
                    ir.InstKind.Cast, [
                        ir.Inst(
                            ir.InstKind.Call,
                            [ir.Name("_R7runtime3Vec3getM"), left, idx]
                        ), expr_typ_ir2
                    ], expr_typ_ir2
                )
                if expr.is_ref:
                    expr_typ_ir = expr_typ_ir.ptr()
                else:
                    value = ir.Inst(ir.InstKind.LoadPtr, [value])
            else:
                if not s.is_boxed():
                    expr_typ_ir = expr_typ_ir.ptr()
                value = ir.Inst(ir.InstKind.GetElementPtr, [left, idx])
            self.cur_fn.try_alloca(expr_typ_ir, tmp, value)
            return ir.Ident(expr_typ_ir, tmp)
        elif isinstance(expr, ast.UnaryExpr):
            right = self.gen_expr_with_cast(expr.right_typ, expr.right)
            if expr.op == Kind.Amp:
                tmp = self.cur_fn.local_name()
                self.cur_fn.try_alloca(
                    self.ir_type(expr.typ), tmp,
                    ir.Inst(ir.InstKind.GetRef, [right])
                )
                return ir.Ident(self.ir_type(expr.typ), tmp)

            # runtime calculation
            if expr.op == Kind.Bang:
                kind = ir.InstKind.BooleanNot
            elif expr.op == Kind.BitNot:
                kind = ir.InstKind.BitNot
            else:
                kind = ir.InstKind.Neg
            tmp = self.cur_fn.local_name()
            self.cur_fn.try_alloca(
                self.ir_type(expr.typ), tmp, ir.Inst(kind, [right])
            )
            return ir.Ident(self.ir_type(expr.typ), tmp)
        elif isinstance(expr, ast.BinaryExpr):
            expr_left_typ = expr.left.typ
            expr_right_typ = expr.right.typ
            if isinstance(expr_left_typ, type.Optional):
                if expr.op in (Kind.KwIs, Kind.KwNotIs):
                    left = self.gen_expr_with_cast(expr_left_typ, expr.left)
                    if isinstance(expr_left_typ.typ, (type.Ref, type.Ptr)):
                        op = "==" if expr.op == Kind.KwIs else "!="
                        return ir.Inst(
                            ir.InstKind.Cmp,
                            [op, left, ir.NilLit(ir.VOID_PTR_T)], ir.BOOL_T
                        )
                    val = ir.Selector(ir.BOOL_T, left, ir.Name("is_nil"))
                    if expr.op == Kind.KwNotIs:
                        val = ir.Inst(ir.InstKind.BooleanNot, [val], ir.BOOL_T)
                    return val
                elif expr.op == Kind.OrElse:
                    expr_typ = expr_left_typ
                    is_not_never = expr.right.typ != self.comp.never_t
                    left = self.gen_expr_with_cast(expr_typ, expr.left)
                    is_nil_label = self.cur_fn.local_name()
                    is_not_nil_label = self.cur_fn.local_name()
                    exit_label = self.cur_fn.local_name(
                    ) if is_not_never else ""
                    if isinstance(expr_typ.typ, type.Ref):
                        cond = ir.Inst(
                            ir.InstKind.Cmp,
                            [Name("=="), left, NoneLiteral()]
                        )
                    else:
                        cond = ir.Selector(ir.BOOL_T, left, ir.Name("is_nil"))
                    tmp = ir.Ident(
                        self.ir_type(expr_typ.typ), self.cur_fn.local_name()
                    )
                    self.cur_fn.alloca(tmp)
                    self.cur_fn.add_cond_br(
                        cond, is_nil_label, is_not_nil_label
                    )
                    self.cur_fn.add_label(is_nil_label)
                    right = self.gen_expr_with_cast(expr_typ.typ, expr.right)
                    if is_not_never:
                        self.cur_fn.store(tmp, right)
                        self.cur_fn.add_br(exit_label)
                    self.cur_fn.add_label(is_not_nil_label)
                    if isinstance(expr_typ.typ, type.Ref):
                        self.cur_fn.store(tmp, left)
                    else:
                        self.cur_fn.store(
                            tmp,
                            ir.Selector(expr_typ.typ, left, ir.Name("value"))
                        )
                    if is_not_never:
                        self.cur_fn.add_label(exit_label)
                    return tmp
            elif expr.op in (Kind.KwAnd, Kind.KwOr):
                left = self.gen_expr_with_cast(expr_left_typ, expr.left)
                tmp = ir.Ident(
                    self.ir_type(self.comp.bool_t), self.cur_fn.local_name()
                )
                self.cur_fn.alloca(tmp, left)
                left_l = self.cur_fn.local_name()
                exit_l = self.cur_fn.local_name()
                if expr.op == Kind.KwAnd:
                    self.cur_fn.add_cond_br(left, left_l, exit_l)
                else:
                    self.cur_fn.add_cond_br(left, exit_l, left_l)
                self.cur_fn.add_label(left_l)
                self.cur_fn.store(
                    tmp, self.gen_expr_with_cast(expr_left_typ, expr.right)
                )
                self.cur_fn.add_label(exit_l)
                return tmp
            elif expr.op in (Kind.KwIs, Kind.KwNotIs):
                left = self.gen_expr_with_cast(expr_left_typ, expr.left)
                tmp = self.cur_fn.local_name()
                kind = "==" if expr.op == Kind.KwIs else "!="
                left_sym = expr_left_typ.symbol()
                expr_right_sym = expr_right_typ.symbol()
                if left_sym.kind == TypeKind.Enum:
                    if left_sym.info.is_advanced_enum:
                        cmp = ir.Inst(
                            ir.InstKind.Cmp, [
                                ir.Name(kind),
                                ir.Selector(ir.USIZE_T, left, ir.Name("_id")),
                                ir.IntLit(
                                    ir.USIZE_T, str(expr.right.sym.value)
                                )
                            ]
                        )
                    else:
                        cmp = ir.Inst(
                            ir.InstKind.Cmp, [
                                ir.Name(kind), left,
                                ir.IntLit(
                                    ir.USIZE_T, str(expr.right.sym.value)
                                )
                            ]
                        )
                elif left_sym.kind == TypeKind.Trait:
                    cmp = ir.Inst(
                        ir.InstKind.Cmp, [
                            ir.Name(kind),
                            ir.Selector(ir.USIZE_T, left, ir.Name("_id")),
                            ir.IntLit(
                                ir.USIZE_T,
                                str(left_sym.info.indexof(expr_right_sym))
                            )
                        ]
                    )
                else:
                    cmp = ir.Inst(
                        ir.InstKind.Cmp, [
                            ir.Name(kind),
                            ir.Selector(ir.USIZE_T, left, ir.Name("_id")),
                            ir.IntLit(ir.USIZE_T, str(expr_right_sym.id))
                        ]
                    )
                self.cur_fn.try_alloca(self.ir_type(expr.typ), tmp, cmp)
                return ir.Ident(self.ir_type(expr.typ), tmp)
            elif expr.op in (Kind.KwIn, Kind.KwNotIn):
                expr_left_typ = self.comp.comptime_number_to_type(expr_left_typ)
                left = self.gen_expr_with_cast(expr_left_typ, expr.left)
                right = self.gen_expr_with_cast(expr.right.typ, expr.right)
                left_sym = expr_left_typ.symbol()
                right_sym = expr.right.typ.symbol()
                contains_method = f"contains_{right_sym.id}"
                full_name = f"_R7runtime3Vec{len(contains_method)}{contains_method}"
                if not right_sym.info.has_contains_method:
                    right_sym.info.has_contains_method = True
                    self_id = ir.Ident(ir.VEC_T.ptr(True), "self")
                    elem_id = ir.Ident(self.ir_type(expr_left_typ), "_elem_")
                    contains_decl = ir.FnDecl(
                        False, [], False, full_name, [self_id, elem_id], False,
                        ir.BOOL_T, False
                    )
                    inc_v = ir.Ident(ir.USIZE_T, contains_decl.local_name())
                    contains_decl.alloca(inc_v, ir.IntLit(ir.USIZE_T, "0"))
                    cond_l = contains_decl.local_name()
                    body_l = contains_decl.local_name()
                    ret_l = contains_decl.local_name()
                    continue_l = contains_decl.local_name()
                    exit_l = contains_decl.local_name()
                    contains_decl.add_br(cond_l)
                    contains_decl.add_label(cond_l)
                    contains_decl.add_cond_br(
                        ir.Inst(
                            ir.InstKind.Cmp, [
                                ir.Name("<"), inc_v,
                                ir.Selector(
                                    ir.USIZE_T, self_id, ir.Name("len")
                                )
                            ]
                        ), body_l, exit_l
                    )
                    contains_decl.add_label(body_l)
                    cur_elem = ir.Inst(
                        ir.InstKind.LoadPtr, [
                            ir.Inst(
                                ir.InstKind.Cast, [
                                    ir.Inst(
                                        ir.InstKind.Call, [
                                            ir.Name("_R7runtime3Vec7raw_getM"),
                                            self_id, inc_v
                                        ]
                                    ),
                                    self.ir_type(expr_left_typ).ptr()
                                ]
                            )
                        ]
                    )
                    right_kind = right_sym.info.elem_typ.symbol().kind
                    if right_kind.is_primitive() or right_kind == TypeKind.Enum:
                        cond = ir.Inst(
                            ir.InstKind.Cmp, [ir.Name("=="), cur_elem, elem_id]
                        )
                    else:
                        cond = ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name(f"{mangle_symbol(left_sym)}4_eq_M"),
                                cur_elem, elem_id
                            ]
                        )
                    contains_decl.add_cond_br(cond, ret_l, continue_l)
                    contains_decl.add_label(ret_l)
                    contains_decl.add_ret(ir.IntLit(ir.BOOL_T, "1"))
                    contains_decl.add_label(continue_l)
                    contains_decl.add_inst(ir.Inst(ir.InstKind.Inc, [inc_v]))
                    contains_decl.add_br(cond_l)
                    contains_decl.add_label(exit_l)
                    contains_decl.add_ret(ir.IntLit(ir.BOOL_T, "0"))
                    self.out_rir.decls.append(contains_decl)
                call = ir.Inst(
                    ir.InstKind.Call, [ir.Name(full_name), right, left],
                    ir.BOOL_T
                )
                if expr.op == Kind.KwNotIn:
                    call = ir.Inst(ir.InstKind.BooleanNot, [call], ir.BOOL_T)
                return call

            left = self.gen_expr_with_cast(expr_left_typ, expr.left)
            right = self.gen_expr_with_cast(expr_right_typ, expr.right)

            # runtime calculation
            tmp = self.cur_fn.local_name()
            typ_sym = expr_left_typ.symbol()
            if expr.op.is_overloadable_op() and typ_sym.kind in (
                TypeKind.Array, TypeKind.Vec, TypeKind.String, TypeKind.Struct,
                TypeKind.Class
            ) and not isinstance(expr_left_typ, type.Ptr):
                if typ_sym.kind == TypeKind.Array:
                    if expr.op == Kind.Eq:
                        name = "_R7runtime8array_eqF"
                    elif expr.op == Kind.Ne:
                        name = "_R7runtime8array_neF"
                    size, _ = self.comp.type_size(expr_left_typ)
                    self.cur_fn.try_alloca(
                        self.ir_type(expr.typ), tmp,
                        ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name(name), left, right,
                                ir.IntLit(ir.USIZE_T, str(size))
                            ]
                        )
                    )
                else:
                    op_method = OVERLOADABLE_OPERATORS_STR[str(expr.op)]
                    sym_is_class = typ_sym.is_boxed()
                    self.cur_fn.try_alloca(
                        self.ir_type(expr.typ), tmp,
                        ir.Inst(
                            ir.InstKind.Call, [
                                ir.Name(
                                    mangle_symbol(typ_sym) +
                                    f"{len(op_method)}{op_method}M"
                                ), left if sym_is_class else
                                ir.Inst(ir.InstKind.GetRef, [left]),
                                right if sym_is_class else
                                ir.Inst(ir.InstKind.GetRef, [right])
                            ]
                        )
                    )
                return ir.Ident(expr.typ, tmp)
            if expr.op.is_relational():
                kind = str(expr.op)
                self.cur_fn.try_alloca(
                    self.ir_type(expr.typ), tmp,
                    ir.Inst(ir.InstKind.Cmp, [ir.Name(kind), left, right])
                )
            elif expr.op in (Kind.Div, Kind.Mod):
                is_div = expr.op == Kind.Div
                kind = ir.InstKind.Div if is_div else ir.InstKind.Mod
                self.cur_fn.try_alloca(
                    self.ir_type(expr.typ), tmp, ir.Inst(kind, [left, right])
                )
            else:
                if op_kind := ir.get_ir_op(expr.op):
                    self.cur_fn.try_alloca(
                        self.ir_type(expr.typ), tmp,
                        ir.Inst(op_kind, [left, right])
                    )
                else:
                    assert False
            return ir.Ident(self.ir_type(expr.typ), tmp)
        elif isinstance(expr, ast.IfExpr):
            is_void_value = expr.typ in self.void_types
            if len(expr.branches) == 2 and not is_void_value:
                b2 = expr.branches[1]
                if b2.is_else:
                    b1 = expr.branches[0]
                    cond = self.gen_expr_with_cast(expr.typ, b1.cond)
                    if isinstance(cond, ir.IntLit):
                        if cond.lit == "1":
                            # use first value if cond is true
                            return self.gen_expr_with_cast(expr.typ, b1.expr)
                        else:
                            # use second value instead
                            return self.gen_expr_with_cast(expr.typ, b2.expr)
            gen_branch = True
            exit_label = self.cur_fn.local_name()
            else_label = self.cur_fn.local_name(
            ) if expr.has_else else exit_label
            tmp = ir.Ident(
                self.ir_type(expr.typ),
                self.cur_fn.local_name() if not is_void_value else ""
            )
            if not is_void_value:
                self.cur_fn.alloca(tmp)
            self.cur_fn.add_comment(f"if expr (end: {exit_label})")
            next_branch = ""
            for i, b in enumerate(expr.branches):
                if not gen_branch:
                    break
                self.cur_fn.add_comment(f"if branch (is_else: {b.is_else})")
                if b.is_else:
                    self.cur_fn.add_label(else_label)
                else:
                    if isinstance(b.cond, ast.GuardExpr):
                        gexpr = self.gen_expr_with_cast(
                            b.cond.expr.typ, b.cond.expr
                        )
                        if b.cond.is_result:
                            cond = ir.Inst(
                                ir.InstKind.BooleanNot, [
                                    ir.Selector(
                                        ir.BOOL_T, gexpr, ir.Name("is_err")
                                    )
                                ]
                            )
                            var_t = self.ir_type(b.cond.expr.typ.typ)
                            self.cur_fn.try_alloca(
                                var_t, b.cond.vars[0],
                                ir.Selector(var_t, gexpr, ir.Name("value"))
                            )
                        else:
                            if isinstance(b.cond.typ, (type.Ref, type.Ptr)):
                                cond = ir.Inst(
                                    ir.InstKind.Cmp, [
                                        ir.Name("!="), gexpr,
                                        ir.NilLit(ir.VOID_PTR_T)
                                    ]
                                )
                                self.cur_fn.try_alloca(
                                    self.ir_type(b.cond.expr.typ.typ),
                                    b.cond.vars[0], gexpr
                                )
                            else:
                                cond = ir.Inst(
                                    ir.InstKind.BooleanNot, [
                                        ir.Selector(
                                            ir.BOOL_T, gexpr, ir.Name("is_nil")
                                        )
                                    ]
                                )
                                self.cur_fn.try_alloca(
                                    self.ir_type(b.cond.expr.typ.typ),
                                    b.cond.vars[0],
                                    ir.Selector(
                                        self.ir_type(b.cond.expr.typ.typ),
                                        gexpr, ir.Name("value")
                                    )
                                )
                    else:
                        cond = self.gen_expr_with_cast(self.comp.bool_t, b.cond)
                    branch_label = self.cur_fn.local_name()
                    if i == len(expr.branches) - 1:
                        next_branch = exit_label
                    elif i + 1 == len(expr.branches
                                      ) - 1 and expr.branches[i + 1].is_else:
                        next_branch = else_label
                    else:
                        next_branch = self.cur_fn.local_name()
                    if isinstance(cond, ir.IntLit) and cond.lit == "1":
                        gen_branch = False
                    else:
                        self.cur_fn.add_cond_br(cond, branch_label, next_branch)
                        self.cur_fn.add_label(branch_label)
                        if isinstance(b.cond, ast.GuardExpr):
                            if b.cond.has_cond:
                                gcond = self.gen_expr_with_cast(
                                    self.comp.bool_t, b.cond.cond
                                )
                                self.cur_fn.add_cond_br(
                                    gcond, branch_label, next_branch
                                )
                if is_void_value:
                    self.gen_expr_with_cast(
                        expr.typ, b.expr
                    ) # ignore void value
                else:
                    self.cur_fn.store(
                        tmp, self.gen_expr_with_cast(expr.typ, b.expr)
                    )
                if gen_branch:
                    self.cur_fn.add_comment(
                        "if expr branch (goto to other branch)"
                    )
                    self.cur_fn.add_br(exit_label)
                if len(next_branch) > 0 and next_branch != else_label:
                    self.cur_fn.add_label(next_branch)
            if gen_branch:
                self.cur_fn.add_label(exit_label)
            if not is_void_value:
                return tmp
        elif isinstance(expr, ast.SwitchExpr):
            is_void_value = expr.typ in self.void_types
            exit_switch = self.cur_fn.local_name()
            self.cur_fn.add_comment(f"switch expr (end: {exit_switch})")
            tmp = ir.Ident(
                self.ir_type(expr.expected_typ),
                self.cur_fn.local_name() if not is_void_value else ""
            )
            if not is_void_value:
                self.cur_fn.alloca(tmp)
            is_guard_expr = isinstance(expr.expr, ast.GuardExpr)
            if is_guard_expr:
                gexpr = self.gen_expr_with_cast(expr.expr.typ, expr.expr.expr)
                if expr.expr.is_result:
                    cond = ir.Selector(ir.BOOL_T, gexpr, ir.Name("is_err"))
                    var_t = self.ir_type(expr.expr.typ)
                    self.cur_fn.try_alloca(
                        var_t, expr.expr.vars[0],
                        ir.Selector(var_t, gexpr, ir.Name("value"))
                    )
                else:
                    if isinstance(expr.expr.typ, (type.Ref, type.Ptr)):
                        cond = ir.Inst(
                            ir.InstKind.Cmp,
                            [ir.Name("!="), gexpr,
                             ir.NilLit(ir.VOID_PTR_T)]
                        )
                        self.cur_fn.try_alloca(
                            self.ir_type(expr.expr.typ), expr.expr.vars[0],
                            gexpr
                        )
                    else:
                        cond = ir.Selector(ir.BOOL_T, gexpr, ir.Name("is_nil"))
                        self.cur_fn.try_alloca(
                            self.ir_type(expr.expr.typ), expr.expr.vars[0],
                            ir.Selector(
                                self.ir_type(expr.expr.typ), gexpr,
                                ir.Name("value")
                            )
                        )
                self.cur_fn.add_cond_single_br(cond, exit_switch)
                if expr.expr.has_cond:
                    self.cur_fn.add_cond_single_br(
                        ir.Inst(
                            ir.InstKind.BooleanNot, [
                                self.gen_expr_with_cast(
                                    self.comp.bool_t, expr.expr.cond
                                )
                            ]
                        ), exit_switch
                    )
                switch_expr = ir.Ident(
                    self.ir_type(expr.expr.typ), expr.expr.vars[0]
                )
            else:
                switch_expr = self.gen_expr_with_cast(expr.expr.typ, expr.expr)
            for b in expr.branches:
                b_label = "" if b.is_else else self.cur_fn.local_name()
                b_exit = exit_switch if b.is_else else self.cur_fn.local_name()
                if not b.is_else:
                    self.cur_fn.add_comment(
                        f"switch expr patterns (len: {len(b.pats)})"
                    )
                for i, p in enumerate(b.pats):
                    next_pat = self.cur_fn.local_name(
                    ) if i < len(b.pats) - 1 else b_exit
                    tmp2 = self.cur_fn.local_name()
                    if expr.is_typeswitch:
                        if p.typ.sym.kind == TypeKind.Trait:
                            value_idx = ir.IntLit(
                                ir.USIZE_T,
                                str(
                                    expr.expected_typ.symbol().indexof(
                                        p.typ.sym
                                    )
                                )
                            )
                        elif p.typ.sym.kind == TypeKind.Enum:
                            value_idx = ir.IntLit(ir.USIZE_T, str(p.sym.value))
                        else:
                            value_idx = ir.IntLit(ir.USIZE_T, str(p.typ.sym.id))
                        if p.typ.sym.kind == TypeKind.Enum and not p.typ.sym.info.is_advanced_enum:
                            self.cur_fn.try_alloca(
                                ir.BOOL_T, tmp2,
                                ir.Inst(
                                    ir.InstKind.Cmp,
                                    [ir.Name("=="), switch_expr, value_idx]
                                )
                            )
                        else:
                            self.cur_fn.try_alloca(
                                ir.BOOL_T, tmp2,
                                ir.Inst(
                                    ir.InstKind.Cmp, [
                                        ir.Name("=="),
                                        ir.Selector(
                                            self.ir_type(expr.expr.typ),
                                            switch_expr, ir.Name("_id")
                                        ), value_idx
                                    ]
                                )
                            )
                        if b.has_var and i == 0:
                            var_t = self.ir_type(b.var_typ)
                            var_t2 = var_t.ptr(
                            ) if b.var_is_mut or not isinstance(
                                var_t, ir.Pointer
                            ) else var_t
                            if expr.expr.typ.sym.kind == TypeKind.Enum:
                                val = ir.Inst(
                                    ir.InstKind.Cast, [
                                        ir.Selector(
                                            ir.VOID_PTR_T, switch_expr,
                                            ir.Name("obj")
                                        ), var_t2
                                    ]
                                )
                                if not (
                                    b.var_is_mut or (
                                        isinstance(var_t, ir.Pointer)
                                        and var_t.is_managed
                                    )
                                ):
                                    val = ir.Inst(ir.InstKind.LoadPtr, [val])
                                if b.var_is_mut and not isinstance(
                                    var_t, ir.Pointer
                                ):
                                    var_t = var_t.ptr(True)
                            else:
                                val = ir.Inst(
                                    ir.InstKind.Cast, [switch_expr, var_t]
                                )
                            self.cur_fn.try_alloca(var_t, b.var_name, val)
                    else:
                        p_conv = self.gen_expr_with_cast(p.typ, p)
                        p_typ_sym = p.typ.symbol()
                        if p_typ_sym.kind.is_primitive(
                        ) or p_typ_sym.kind == TypeKind.Enum:
                            inst = ir.Inst(
                                ir.InstKind.Cmp,
                                [ir.Name("=="), switch_expr, p_conv]
                            )
                        else:
                            inst = ir.Inst(
                                ir.InstKind.Call, [
                                    ir
                                    .Name(f"{mangle_symbol(p_typ_sym)}4_eq_M"),
                                    switch_expr, p_conv,
                                ]
                            )
                        self.cur_fn.try_alloca(ir.BOOL_T, tmp2, inst)
                    self.cur_fn.add_cond_br(
                        ir.Ident(ir.BOOL_T, tmp2), b_label, next_pat
                    )
                    if i < len(b.pats) - 1:
                        self.cur_fn.add_label(next_pat)
                if not b.is_else:
                    self.cur_fn.add_label(b_label)
                    if b.has_cond:
                        self.cur_fn.add_cond_single_br(
                            ir.Inst(
                                ir.InstKind.BooleanNot, [
                                    self.gen_expr_with_cast(
                                        self.comp.bool_t, b.cond
                                    )
                                ]
                            ), b_exit
                        )
                if is_void_value:
                    self.gen_expr_with_cast(
                        expr.expected_typ, b.expr
                    ) # ignore void value
                else:
                    self.cur_fn.store(
                        tmp, self.gen_expr_with_cast(expr.expected_typ, b.expr)
                    )
                self.cur_fn.add_br(exit_switch)
                if not b.is_else:
                    self.cur_fn.add_label(b_exit)
            self.cur_fn.add_label(exit_switch)
            if not is_void_value:
                return tmp
        elif isinstance(expr, ast.BranchExpr):
            if expr.op == Kind.KwContinue:
                if self.while_continue_expr != None and not isinstance(
                    self.while_continue_expr, ast.EmptyExpr
                ):
                    self.gen_expr(self.while_continue_expr)
                self.cur_fn.add_br(self.loop_entry_label)
            else:
                self.cur_fn.add_br(self.loop_exit_label)
            return ir.Skip()
        elif isinstance(expr, ast.ReturnExpr):
            wrap_result = isinstance(self.cur_fn_ret_typ, type.Result)
            ret_typ = self.cur_fn_ret_typ.typ if wrap_result else self.cur_fn_ret_typ
            if self.inside_test:
                self.cur_fn.store(
                    ir.Selector(
                        ir.U8_T, ir.Ident(ir.TEST_T.ptr(), "test"),
                        ir.Name("result")
                    ), ir.IntLit(ir.U8_T, "1")
                )
                self.gen_defer_stmts()
                self.cur_fn.add_ret_void()
            elif expr.has_expr:
                is_array = self.cur_fn_ret_typ.symbol().kind == TypeKind.Array
                expr_ = self.gen_expr_with_cast(ret_typ, expr.expr)
                if is_array and self.comp.prefs.target_backend == prefs.Backend.C:
                    size, _ = self.comp.type_size(ret_typ)
                    tmp = ir.Ident(
                        ir.Type(self.cur_fn.arr_ret_struct),
                        self.cur_fn.local_name()
                    )
                    self.cur_fn.alloca(tmp)
                    self.cur_fn.add_call(
                        "_R7runtime8mem_copyF", [
                            ir.Selector(
                                self.ir_type(ret_typ), tmp, ir.Name("arr")
                            ), expr_,
                            ir.IntLit(ir.USIZE_T, str(size))
                        ]
                    )
                    expr_ = tmp
                if wrap_result:
                    if expr.expr.typ.symbol().is_subtype_of(
                        self.comp.error_t.sym
                    ):
                        expr_ = self.result_error(self.cur_fn_ret_typ, expr_)
                    else:
                        expr_ = self.result_value(self.cur_fn_ret_typ, expr_)
                self.gen_defer_stmts(
                    wrap_result,
                    ir.Selector(ir.BOOL_T, expr_, ir.Name("is_err"))
                )
                self.cur_fn.add_ret(expr_)
            elif wrap_result:
                self.gen_defer_stmts()
                self.cur_fn.add_ret(self.result_void(self.cur_fn_ret_typ))
            else:
                self.gen_defer_stmts()
                self.cur_fn.add_ret_void()
            return ir.Skip()
        else:
            raise Exception(expr.__class__, expr.pos)
        return ir.Skip()

    def gen_defer_stmts(self, gen_errdefer = False, last_ret_was_err = None):
        for i in range(len(self.cur_fn_defer_stmts) - 1, -1, -1):
            defer_stmt = self.cur_fn_defer_stmts[i]
            if defer_stmt.is_errdefer and not gen_errdefer:
                continue
            defer_start = self.cur_fn.local_name()
            defer_end = self.cur_fn.local_name()
            self.cur_fn.add_comment(
                f"defer_stmt (start: {defer_start}, end: {defer_end}, is_errdefer: {defer_stmt.is_errdefer})"
            )
            self.cur_fn.add_cond_br(
                ir.Ident(ir.BOOL_T, defer_stmt.flag_var), defer_start, defer_end
            )
            self.cur_fn.add_label(defer_start)
            if defer_stmt.is_errdefer:
                self.cur_fn.add_cond_single_br(
                    ir.Inst(ir.InstKind.BooleanNot, [last_ret_was_err]),
                    defer_end
                )
            self.gen_expr(defer_stmt.expr)
            self.cur_fn.add_label(defer_end)

    def gen_const(self, const_sym):
        if const_sym.has_evaled_expr:
            const_sym.has_ir_expr = True
            const_sym.ir_expr = self.gen_expr_with_cast(
                const_sym.typ, const_sym.evaled_expr
            )
            return const_sym.ir_expr
        if const_sym.has_ir_expr:
            return const_sym.ir_expr
        const_sym.has_ir_expr = True
        const_sym.ir_expr = self.gen_expr_with_cast(
            const_sym.typ, const_sym.expr
        )
        return const_sym.ir_expr

    def result_void(self, typ):
        tmp = ir.Ident(self.ir_type(typ), self.cur_fn.local_name())
        self.cur_fn.alloca(tmp)
        self.cur_fn.store(
            ir.Selector(ir.BOOL_T, tmp, ir.Name("is_err")),
            ir.IntLit(ir.BOOL_T, "0")
        )
        return tmp

    def result_value(self, typ, value):
        tmp = ir.Ident(self.ir_type(typ), self.cur_fn.local_name())
        self.cur_fn.alloca(tmp)
        self.cur_fn.store(
            ir.Selector(ir.BOOL_T, tmp, ir.Name("is_err")),
            ir.IntLit(ir.BOOL_T, "0")
        )
        self.cur_fn.store(
            ir.Selector(
                self.ir_type(self.cur_fn_ret_typ.typ), tmp, ir.Name("value")
            ), value
        )
        return tmp

    def result_error(self, typ, expr):
        tmp = ir.Ident(self.ir_type(typ), self.cur_fn.local_name())
        self.cur_fn.alloca(tmp)
        self.cur_fn.store(
            ir.Selector(ir.BOOL_T, tmp, ir.Name("is_err")),
            ir.IntLit(ir.BOOL_T, "1")
        )
        if str(expr.typ) != "+_R5Error":
            expr = ir.Inst(
                ir.InstKind.Cast, [expr, self.ir_type(self.comp.error_t)]
            )
        self.cur_fn.store(
            ir.Selector(self.ir_type(self.comp.error_t), tmp, ir.Name("err")),
            expr
        )
        return tmp

    def optional_value(self, typ, value):
        tmp = ir.Ident(self.ir_type(typ), self.cur_fn.local_name())
        self.cur_fn.alloca(tmp)
        self.cur_fn.store(
            ir.Selector(ir.BOOL_T, tmp, ir.Name("is_nil")),
            ir.IntLit(ir.BOOL_T, "0")
        )
        self.cur_fn.store(
            ir.Selector(self.ir_type(typ.typ), tmp, ir.Name("value")), value
        )
        return tmp

    def optional_nil(self, typ):
        tmp = ir.Ident(self.ir_type(typ), self.cur_fn.local_name())
        self.cur_fn.alloca(tmp)
        self.cur_fn.store(
            ir.Selector(ir.BOOL_T, tmp, ir.Name("is_nil")),
            ir.IntLit(ir.BOOL_T, "1")
        )
        return tmp

    def panic(self, msg):
        self.cur_fn.add_call(
            "_R7runtime13process_panicF", [
                self.gen_string_lit(utils.smart_quote(msg, False)),
                self.empty_vec(self.comp.universe["[runtime.ToString]"])
            ]
        )

    def variadic_args(self, vargs, var_arg_typ_):
        if len(vargs) == 0:
            return self.empty_vec(var_arg_typ_.typ.symbol())
        elem_size, _ = self.comp.type_size(var_arg_typ_)
        return ir.Inst(
            ir.InstKind.Call, [
                ir.Name("_R7runtime3Vec19from_array_no_allocF"),
                ir.ArrayLit(self.ir_type(var_arg_typ_), vargs),
                ir.IntLit(ir.USIZE_T, str(elem_size)),
                ir.IntLit(ir.USIZE_T, str(len(vargs)))
            ]
        )

    def default_value(self, typ, custom_tmp = None):
        if isinstance(typ, (type.Ptr, type.Ref)):
            return ir.NilLit(ir.VOID_PTR_T)
        if isinstance(typ, type.Optional):
            return self.optional_nil(typ)
        if typ == self.comp.rune_t:
            return ir.RuneLit("\\0")
        elif typ in (
            self.comp.bool_t, self.comp.i8_t, self.comp.i16_t, self.comp.i32_t,
            self.comp.i64_t, self.comp.u8_t, self.comp.u16_t, self.comp.u32_t,
            self.comp.u64_t, self.comp.isize_t, self.comp.usize_t
        ):
            return ir.IntLit(self.ir_type(typ), "0")
        elif typ in (self.comp.f32_t, self.comp.f64_t):
            return ir.FloatLit(self.ir_type(typ), "0.0")
        elif typ == self.comp.string_t:
            return ir.Ident(ir.STRING_T, "_R7runtime12empty_string")
        elif isinstance(typ, type.Result):
            if typ.typ == self.comp.void_t:
                return self.result_void(typ)
            return self.result_value(typ, self.default_value(typ.typ))
        typ_sym = typ.symbol()
        if typ_sym.kind == TypeKind.Array:
            return ir.ArrayLit(
                self.ir_type(typ), [self.default_value(typ_sym.info.elem_typ)]
            )
        elif typ_sym.kind == TypeKind.Vec:
            return self.empty_vec(typ_sym)
        elif typ_sym.kind == TypeKind.Enum:
            return ir.IntLit(self.ir_type(typ_sym.info.underlying_typ), "0")
        elif typ_sym.kind == TypeKind.Tuple:
            tmp = ir.Ident(typ, self.cur_fn.local_name())
            self.cur_fn.alloca(tmp)
            for i, typ in enumerate(typ_sym.info.types):
                self.cur_fn.store(
                    ir.Selector(self.ir_type(typ), tmp, ir.Name(f"f{i}")),
                    self.default_value(typ)
                )
            return tmp
        elif typ_sym.kind == TypeKind.Class:
            if custom_tmp:
                tmp = custom_tmp
            else:
                tmp = self.boxed_instance(mangle_symbol(typ_sym), typ_sym.id)
            for f in typ_sym.full_fields():
                if f.typ.symbol().kind == TypeKind.Array:
                    continue
                sltor = ir.Selector(self.ir_type(f.typ), tmp, ir.Name(f.name))
                if f.has_def_expr:
                    val = self.gen_expr_with_cast(f.typ, f.def_expr, sltor)
                else:
                    val = self.default_value(f.typ)
                    self.cur_fn.store(sltor, val)
            return tmp
        elif typ_sym.kind == TypeKind.Struct:
            if custom_tmp:
                tmp = custom_tmp
            else:
                tmp = ir.Ident(self.ir_type(typ), self.cur_fn.local_name())
                self.cur_fn.alloca(tmp)
            for f in typ_sym.full_fields():
                if f.typ.symbol().kind == TypeKind.Array:
                    continue
                sltor = ir.Selector(self.ir_type(f.typ), tmp, ir.Name(f.name))
                if f.has_def_expr:
                    val = self.gen_expr_with_cast(f.typ, f.def_expr, sltor)
                else:
                    val = self.default_value(f.typ)
                    self.cur_fn.store(sltor, val)
            return tmp
        return None

    def empty_vec(self, typ_sym, cap = None):
        elem_typ = typ_sym.info.elem_typ
        size, _ = self.comp.type_size(elem_typ)
        return ir.Inst(
            ir.InstKind.Call, [
                ir.Name("_R7runtime3Vec3newF"),
                ir.IntLit(ir.USIZE_T, str(size)), cap
                or ir.IntLit(ir.USIZE_T, "0")
            ]
        )

    def gen_string_lit(self, lit, size = None):
        size = size or utils.bytestr(lit).len
        if size == 0:
            return ir.Ident(ir.STRING_T.ptr(True), "_R7runtime12empty_string")
        lit_hash = hash(lit)
        if lit_hash in self.generated_string_literals:
            return ir.Ident(
                ir.STRING_T.ptr(True), self.generated_string_literals[lit_hash]
            )
        tmp = self.boxed_instance(
            "_R7runtime6string", 19,
            custom_name = f"STR_LIT{len(self.generated_string_literals)}"
        )
        self.out_rir.globals.append(
            ir.GlobalVar(False, False, ir.STRING_T.ptr(True), tmp.name)
        )
        self.init_string_lits_fn.store(
            ir.Selector(ir.U8_T.ptr(), tmp, ir.Name("ptr")),
            ir.StringLit(lit, size)
        )
        self.init_string_lits_fn.store(
            ir.Selector(ir.USIZE_T, tmp, ir.Name("len")),
            ir.IntLit(ir.USIZE_T, str(size))
        )
        self.init_string_lits_fn.store(
            ir.Selector(ir.BOOL_T, tmp, ir.Name("is_ref")),
            ir.IntLit(ir.BOOL_T, "1")
        )
        self.generated_string_literals[lit_hash] = tmp.name
        return tmp

    def boxed_instance(self, name, id, not_id = False, custom_name = None):
        tmp = ir.Ident(
            ir.Type(name).ptr(True), custom_name or self.cur_fn.local_name()
        )
        to_fn = self.init_string_lits_fn if custom_name else self.cur_fn
        inst = ir.Inst(
            ir.InstKind.Call, [
                ir.Name("_R7runtime14internal_allocF"),
                ir.Name(f"sizeof({name})")
            ]
        )
        if custom_name:
            to_fn.store(tmp, inst)
        else:
            to_fn.alloca(tmp, inst)
        to_fn.store(
            ir.Selector(ir.USIZE_T, tmp, ir.Name("_rc")),
            ir.IntLit(ir.USIZE_T, "1")
        )
        if not not_id:
            to_fn.store(
                ir.Selector(ir.USIZE_T, tmp, ir.Name("_id")),
                ir.IntLit(ir.USIZE_T, str(id))
            )
        return tmp

    def trait_value(self, value, value_typ, trait_typ):
        value_sym = self.comp.comptime_number_to_type(value_typ).symbol()
        trait_sym = trait_typ.symbol()
        size, _ = self.comp.type_size(value_typ)
        tmp = self.boxed_instance(mangle_symbol(trait_sym), 0, True)
        is_ptr = isinstance(value.typ, ir.Pointer)
        if not is_ptr:
            value = ir.Inst(ir.InstKind.GetRef, [value])
        self.cur_fn.store(
            ir.Selector(ir.VOID_PTR_T, tmp, ir.Name("obj")),
            value if is_ptr else ir.Inst(
                ir.InstKind.Call, [
                    ir.Name("_R7runtime12internal_dupF"), value,
                    ir.IntLit(ir.Name("usize"), str(size))
                ]
            )
        )
        self.cur_fn.store(
            ir.Selector(ir.USIZE_T, tmp, ir.Name("_id")),
            ir.IntLit(ir.USIZE_T, str(trait_sym.info.indexof(value_sym)))
        )
        return tmp

    def advanced_enum_value(
        self, enum_sym, variant_name, value, custom_tmp = None
    ):
        if custom_tmp:
            tmp = custom_tmp
        else:
            tmp = self.boxed_instance(
                mangle_symbol(enum_sym), enum_sym.id, True
            )
        usize_t = ir.USIZE_T
        variant_info = enum_sym.info.get_variant(variant_name)
        self.cur_fn.store(
            ir.Selector(usize_t, tmp, ir.Name("_id")),
            ir.IntLit(usize_t, variant_info.value)
        )
        if variant_info.has_typ and not isinstance(value, ast.EmptyExpr):
            arg0 = self.gen_expr_with_cast(variant_info.typ, value)
            size, _ = self.comp.type_size(variant_info.typ)
            value = ir.Inst(
                ir.InstKind.Call, [
                    ir.Name("_R7runtime12internal_dupF"),
                    ir.Inst(ir.InstKind.GetRef, [arg0]),
                    ir.IntLit(usize_t, str(size))
                ]
            )
        else:
            value = ir.NilLit(ir.VOID_PTR_T)
        self.cur_fn.store(ir.Selector(usize_t, tmp, ir.Name("obj")), value)
        return tmp

    def class_upcast(self, value, value_typ, class_typ):
        value_sym = self.comp.comptime_number_to_type(value_typ).symbol()
        class_sym = class_typ.symbol()
        class_typ_ir = self.ir_type(class_typ)
        return ir.Inst(ir.InstKind.Cast, [value, class_typ_ir], class_typ_ir)

    def class_downcast(self, value, value_typ, class_typ):
        value_sym = self.comp.comptime_number_to_type(value_typ).symbol()
        class_sym = class_typ.symbol()
        class_typ_ir = self.ir_type(class_typ)
        self.cur_fn.add_call(
            "_R7runtime14class_downcastF", [
                ir.Selector(ir.USIZE_T, value, ir.Name("_id")),
                ir.IntLit(ir.USIZE_T, str(class_sym.id))
            ]
        )
        return ir.Inst(ir.InstKind.Cast, [value, class_typ_ir], class_typ_ir)

    def ir_type(self, typ):
        if isinstance(typ, type.Result):
            name = f"_R7Result_{mangle_type(typ.typ)}"
            if name not in self.generated_opt_res_types:
                is_void = typ.typ in self.void_types
                self.out_rir.structs.append(
                    ir.Struct(
                        False, name, [
                            ir.Field(
                                "value",
                                ir.U8_T if is_void else self.ir_type(typ.typ)
                            ),
                            ir.Field("is_err", ir.BOOL_T),
                            ir.Field("err", self.ir_type(self.comp.error_t))
                        ]
                    )
                )
                self.generated_opt_res_types.append(name)
            return ir.Type(name)
        elif isinstance(typ, type.Optional):
            if isinstance(typ.typ, (type.Ref, type.Ptr)):
                return self.ir_type(typ.typ)
            name = f"_R9Optional_{mangle_type(typ.typ)}"
            if name not in self.generated_opt_res_types:
                is_void = typ.typ in self.void_types
                self.out_rir.structs.append(
                    ir.Struct(
                        False, name, [
                            ir.Field(
                                "value",
                                ir.U8_T if is_void else self.ir_type(typ.typ)
                            ),
                            ir.Field("is_nil", ir.BOOL_T)
                        ]
                    )
                )
                self.generated_opt_res_types.append(name)
            return ir.Type(name)
        elif isinstance(typ, type.Fn):
            args = []
            for arg in typ.args:
                args.append(self.ir_type(arg.typ))
            return ir.Function(args, self.ir_type(typ.ret_typ))
        elif isinstance(typ, type.Tuple):
            return ir.Type(mangle_symbol(typ.symbol()))
        elif isinstance(typ, type.Array):
            return ir.Array(self.ir_type(typ.typ), typ.size)
        elif isinstance(typ, type.Vec):
            return ir.VEC_T.ptr(True)
        elif isinstance(typ, (type.Ptr, type.Ref)):
            return ir.Pointer(self.ir_type(typ.typ))
        typ_sym = typ.symbol()
        if typ_sym.kind == TypeKind.Vec:
            return ir.VEC_T.ptr(True)
        elif typ_sym.kind == TypeKind.Array:
            return ir.Array(
                self.ir_type(typ_sym.info.elem_typ), typ_sym.info.size
            )
        elif typ_sym.kind == TypeKind.Never:
            return ir.VOID_T
        elif typ_sym.kind == TypeKind.Nil:
            return ir.VOID_PTR_T
        elif typ_sym.kind == TypeKind.Enum:
            if typ_sym.info.is_advanced_enum:
                return ir.Type(mangle_symbol(typ_sym)).ptr(True)
            return ir.Type(str(typ_sym.info.underlying_typ))
        elif typ_sym.kind.is_primitive():
            return ir.Type(typ_sym.name)
        res = ir.Type(mangle_symbol(typ_sym))
        if typ_sym.is_boxed():
            return res.ptr(True)
        return res

    def gen_types(self):
        type_symbols = self.sort_type_symbols(
            self.get_type_symbols(self.comp.universe)
        )
        for ts in type_symbols:
            if ts.kind == TypeKind.Tuple:
                fields = []
                for i, f in enumerate(ts.info.types):
                    fields.append(ir.Field(f"f{i}", self.ir_type(f)))
                self.out_rir.structs.append(
                    ir.Struct(False, mangle_symbol(ts), fields)
                )
            elif ts.kind == TypeKind.Enum:
                # TODO: in the self-hosted compiler calculate the enum value here
                # not in register nor resolver.
                if ts.info.is_advanced_enum:
                    self.out_rir.structs.append(
                        ir.Struct(
                            ts.vis.is_pub(), mangle_symbol(ts), [
                                ir.Field("_rc", ir.USIZE_T),
                                ir.Field("_id", ir.USIZE_T),
                                ir.Field("obj", ir.VOID_PTR_T)
                            ]
                        )
                    )
            elif ts.kind == TypeKind.Trait:
                if ts.info.has_objects:
                    ts_name = mangle_symbol(ts)
                    self.out_rir.structs.append(
                        ir.Struct(
                            False, ts_name, [
                                ir.Field("_id", ir.USIZE_T),
                                ir.Field("_rc", ir.USIZE_T),
                                ir.Field("obj", ir.VOID_PTR_T)
                            ]
                        )
                    )
                    # Virtual table
                    vtbl_name = f"{ts_name}4Vtbl"
                    static_vtbl_name = f"{ts_name}4VTBL"
                    fields = []
                    for m in ts.syms:
                        if isinstance(m, sym.Fn):
                            proto = m.typ()
                            proto.args.insert(
                                0,
                                sym.Arg(
                                    "self", m.self_is_mut,
                                    type.Ptr(self.comp.void_t), None, False,
                                    NO_POS
                                )
                            )
                            fields.append(ir.Field(m.name, self.ir_type(proto)))
                    self.out_rir.structs.append(
                        ir.Struct(False, vtbl_name, fields)
                    )
                    funcs = []
                    for its in ts.info.implements:
                        map = {}
                        for m in ts.syms:
                            if isinstance(m, sym.Fn):
                                if ts_method := its.find(m.name):
                                    map[m.name] = mangle_symbol(ts_method)
                                else:
                                    map[m.name] = mangle_symbol(m)
                        funcs.append(map)
                    if len(funcs) > 0:
                        self.out_rir.decls.append(
                            ir.VTable(
                                vtbl_name, static_vtbl_name, ts_name,
                                len(ts.info.implements), funcs
                            )
                        )
            elif ts.kind in (TypeKind.Class, TypeKind.String, TypeKind.Vec):
                fields = [
                    ir.Field("_rc", ir.USIZE_T),
                    ir.Field("_id", ir.USIZE_T)
                ]
                for f in ts.full_fields():
                    fields.append(ir.Field(f.name, self.ir_type(f.typ)))
                self.out_rir.structs.append(
                    ir.Struct(False, mangle_symbol(ts), fields)
                )
            elif ts.kind == TypeKind.Struct:
                fields = []
                for f in ts.full_fields():
                    fields.append(ir.Field(f.name, self.ir_type(f.typ)))
                self.out_rir.structs.append(
                    ir.Struct(ts.info.is_opaque, mangle_symbol(ts), fields)
                )

    def get_type_symbols(self, root):
        ts = []
        for s in root.syms:
            if isinstance(s, sym.Type):
                if s.kind not in (TypeKind.Vec, TypeKind.Alias,
                                  TypeKind.Never) and not s.kind.is_primitive():
                    ts.append(s)
            ts += self.get_type_symbols(s)
        return ts

    def sort_type_symbols(self, tss):
        dg = utils.DepGraph()
        typ_names = []
        for ts in tss:
            ts.mangled_name = mangle_symbol(ts)
            typ_names.append(ts.mangled_name)
        for ts in tss:
            field_deps = []
            if ts.kind == TypeKind.Array:
                dsym = ts.info.elem_typ.symbol()
                dep = mangle_symbol(dsym)
                if dep in typ_names:
                    field_deps.append(dep)
            elif ts.kind == TypeKind.Vec:
                dep = mangle_symbol(ts.info.elem_typ.symbol())
                if dep in typ_names:
                    field_deps.append(dep)
            elif ts.kind == TypeKind.Tuple:
                for f in ts.info.types:
                    dep = mangle_symbol(f.symbol())
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
            elif ts.kind == TypeKind.Class:
                if ts.info.base:
                    dep = mangle_symbol(ts.info.base)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
                for f in ts.fields:
                    dep = mangle_symbol(f.typ.symbol())
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
            elif ts.kind == TypeKind.Struct:
                for base in ts.info.bases:
                    dep = mangle_symbol(base)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
                for f in ts.fields:
                    dep = mangle_symbol(f.typ.symbol())
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
            dg.add(ts.mangled_name, field_deps)
        dg_sorted = dg.resolve()
        if not dg_sorted.acyclic:
            utils.error(
                "codegen: the following types form a dependency cycle:\n" +
                dg_sorted.display_cycles()
            )
        types_sorted = []
        for node in dg_sorted.nodes:
            for ts in tss:
                if ts.mangled_name == node.name:
                    types_sorted.append(ts)
        return types_sorted
