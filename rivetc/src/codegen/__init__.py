# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os

from ..sym import TypeKind
from ..token import Kind, OVERLOADABLE_OPERATORS_STR
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
    return f"{prefix_type(typ)}{mangle_symbol(typ.symbol())}"

def mangle_symbol(s):
    if len(s.mangled_name) > 0:
        return s.mangled_name
    res = []
    root = s
    while True:
        if s.is_universe:
            break
        if isinstance(s, sym.Type):
            if s.kind.is_primitive():
                name = str(s.kind)
                name = f"{len(name)}{name}"
                res.insert(0, name)
            elif s.kind == sym.TypeKind.Tuple:
                name = "Tuple_"
                for i, tt in enumerate(s.info.types):
                    name += mangle_type(tt)
                    if i < len(s.info.types) - 1:
                        name += "_"
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == sym.TypeKind.Vec:
                res.insert(0, "4core3Vec")
                s.mangled_name = "_R4core3Vec"
            elif s.kind == sym.TypeKind.Array:
                name = f"Array_{mangle_type(s.info.elem_typ)}_{s.info.size}"
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == sym.TypeKind.Enum:
                if isinstance(root, sym.Fn):
                    name = s.name
                else:
                    name = mangle_type(s.info.underlying_typ)
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == sym.TypeKind.String:
                res.insert(0, "4core6String")
                s.mangled_name = "_R4core6String"
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

class Codegen:
    def __init__(self, comp):
        self.comp = comp
        self.out_rir = ir.RIRFile(self.comp.prefs.pkg_name)
        self.void_types = (self.comp.void_t, self.comp.never_t)

        self.sf = None

        self.cur_fn = None
        self.cur_fn_is_main = False

        self.generated_tests = []
        self.generated_opt_res_types = []

        self.loop_entry_label = ""
        self.loop_exit_label = ""

    def gen_source_files(self, source_files):
        self.gen_types()
        for source_file in source_files:
            self.sf = source_file
            self.gen_decls(source_file.decls)

        # generate 'main' fn
        argc = ir.Ident(ir.Type("int"), "_argc")
        argv = ir.Ident(ir.Type("char").ptr().ptr(), "_argv")
        main_fn = ir.FnDecl(
            False, [], False, "main",
            [
                argc, argv
            ], False, ir.Type("int")
        )
        if self.comp.prefs.build_mode==prefs.BuildMode.Test:
            for gtest in self.generated_tests:
                main_fn.add_call(gtest, [])
        else:
            main_fn.add_call(
                f"_R{len(self.comp.prefs.pkg_name)}{self.comp.prefs.pkg_name}4mainF", [argc, ir.Inst(ir.InstKind.Cast, [
                    ir.Type("u8").ptr().ptr(), argv
                ])]
            )
        main_fn.add_ret(ir.IntLit(ir.Type("i32"), "0"))
        self.out_rir.decls.append(main_fn)

        if report.ERRORS == 0:
            if self.comp.prefs.emit_rir:
                with open(f"{self.comp.prefs.pkg_name}.rir", "w+") as f:
                    f.write(str(self.out_rir).strip())
            if self.comp.prefs.target_backend == prefs.Backend.C:
                # self.check_pkg_attrs()
                CGen(self.comp).gen(self.out_rir)

    def check_pkg_attrs(self):
        pkg_folder = os.path.join(
            prefs.RIVET_DIR, "objs", self.comp.prefs.pkg_name
        )
        for attr in self.pkg_attrs.attrs:
            if attr.name == "c_compile":
                if not os.path.exists(pkg_folder):
                    os.mkdir(pkg_folder)
                cfile = os.path.realpath(attr.args[0].expr.lit)
                objfile = os.path.join(
                    pkg_folder,
                    f"{os.path.basename(cfile)}.{self.get_postfix()}.o"
                )
                self.comp.prefs.objects_to_link.append(objfile)
                msg = f"c_compile: compiling object for C file `{cfile}`..."
                if os.path.exists(objfile):
                    if os.path.getmtime(objfile) < os.path.getmtime(cfile):
                        msg = f"c_compile: {objfile} is older than {cfile}, rebuilding..."
                    else:
                        continue
                self.vlog(msg)
                args = [
                    self.comp.prefs.backend_compiler, cfile, "-m64" if
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

    def get_postfix(self):
        postfix = str(self.comp.prefs.target_os).lower()
        postfix += "-"
        postfix += str(self.comp.prefs.target_arch).lower()
        postfix += "-"
        postfix += str(self.comp.prefs.target_bits).lower()
        postfix += "-"
        postfix += str(self.comp.prefs.target_endian).lower()
        postfix += "-"
        postfix += str(self.comp.prefs.target_backend).lower()
        postfix += "-"
        if self.comp.prefs.build_mode == prefs.BuildMode.Debug:
            postfix += "debug"
        else:
            postfix += "release"
        postfix += f"-{self.comp.prefs.backend_compiler}"
        return postfix

    def gen_decls(self, decls):
        for decl in decls:
            self.gen_decl(decl)

    def gen_decl(self, decl):
        if isinstance(decl, ast.ExternDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.ModDecl):
            if decl.is_inline:
                self.gen_decls(decl.decls)
        elif isinstance(decl, ast.LetDecl):
            pass
        elif isinstance(decl, ast.EnumDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.TraitDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.ClassDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.StructDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.ExtendDecl):
            self.gen_decls(decl.decls)
        elif isinstance(decl, ast.FnDecl):
            fn_decl = ir.FnDecl(
                decl.vis.is_pub(), decl.attrs, decl.is_extern, mangle_symbol(decl.sym),
                [ir.Ident(self.ir_type(arg.typ), arg.name) for arg in decl.args], False, self.ir_type(decl.ret_typ)
            )
            self.cur_fn=fn_decl
            if decl.is_extern:
                self.out_rir.externs.append(fn_decl)
            else:
                self.out_rir.decls.append(fn_decl)
        elif isinstance(decl, ast.DestructorDecl):
            pass
        elif isinstance(decl, ast.TestDecl):
            if self.comp.prefs.build_mode==prefs.BuildMode.Test:
                test_name =f"__test{len(self.generated_tests)}__"
                test_name = f"_R{len(test_name)}{test_name}"
                test_fn = ir.FnDecl(
                    False, [], False, test_name,
                    [], False, ir.Type("_R7Result__R4void")
                )
                self.cur_fn=test_fn
                self.generated_tests.append(test_name)
                self.out_rir.decls.append(test_fn)

    def ir_type(self, typ):
        if isinstance(typ, type.Result):
            name = f"_R7Result_{mangle_type(typ.typ)}"
            if name not in self.generated_opt_res_types:
                is_void = typ.typ in self.void_types
                self.out_rir.types.append(
                    ir.Struct(
                        False, name, [
                            ir.Field(
                                "value",
                                ir.Type("u8")
                                if is_void else self.ir_type(typ.typ)
                            ),
                            ir.Field("is_err", ir.Type("bool")),
                            ir.Field("err", ir.Type("_R4core5Error"))
                        ]
                    )
                )
                self.generated_opt_res_types.append(name)
            return ir.Type(name)
        elif isinstance(typ, type.Optional):
            if isinstance(typ.typ, type.Ref):
                return ir.Pointer(self.ir_type(typ.typ))
            name = f"_R9Optional_{mangle_type(typ.typ)}"
            if name not in self.generated_opt_res_types:
                is_void = typ.typ in self.void_types
                self.out_rir.types.append(
                    ir.Struct(
                        False, name, [
                            ir.Field(
                                "value",
                                ir.Type("u8") if is_void else self.ir_type(typ.typ)
                            ),
                            ir.Field("is_none", ir.Type("bool"))
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
            return ir.Array(typ.typ, typ.size)
        elif isinstance(typ, type.Vec):
            return ir.Type("_R4core3Vec")
        elif isinstance(typ, (type.Ptr, type.Ref)):
            return ir.Pointer(self.ir_type(typ.typ))
        typ_sym = typ.symbol()
        if typ_sym.kind == TypeKind.Never:
            return ir.Type("void")
        elif typ_sym.kind.is_primitive():
            return ir.Type(typ_sym.name)
        res = ir.Type(mangle_symbol(typ_sym))
        if typ_sym.kind == TypeKind.Class:
            return res.ptr()
        return res

    def gen_types(self):
        type_symbols = self.sort_type_symbols(
            self.get_type_symbols(self.comp.universe)
        )
        for ts in type_symbols:
            if ts.kind == sym.TypeKind.Tuple:
                fields = list()
                for i, f in enumerate(ts.info.types):
                    fields.append(ir.Field(f"f{i}", self.ir_type(f)))
                self.out_rir.types.append(
                    ir.Struct(False, mangle_symbol(ts), fields)
                )
            elif ts.kind == sym.TypeKind.Enum:
                for i, v in enumerate(ts.info.values):
                    v.value = i
            elif ts.kind == sym.TypeKind.Trait:
                if ts.info.has_objects:
                    ts_name = mangle_symbol(ts)
                    self.out_rir.types.append(
                        ir.Struct(
                            False, ts_name, [
                                ir.Field("obj", ir.Pointer("void")),
                                ir.Field("idx", ir.Type("usize"))
                            ]
                        )
                    )
                    # Virtual table
                    vtbl_name = f"{ts_name}4Vtbl"
                    static_vtbl_name = f"{ts_name}4VTBL"
                    fields = list()
                    for m in ts.syms:
                        if isinstance(m, sym.Fn):
                            fields.append(
                                ir.Field(
                                    m.name, self.ir_type(m.typ())
                                )
                            )
                    self.out_rir.types.append(
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
                    self.out_rir.decls.append(
                        ir.VTable(
                            vtbl_name, static_vtbl_name, ts_name,
                            len(ts.info.implements), funcs
                        )
                    )
            elif ts.kind == sym.TypeKind.Class:
                fields = list()
                if ts.info.base:
                    fields.append(
                        ir.Field(
                            "base",
                            ir.Type(mangle_symbol(ts.info.base)).ptr()
                        )
                    )
                for f in ts.fields:
                    fields.append(
                        ir.Field(f.name, self.ir_type(f.typ))
                    )
                fields.append(ir.Field("_rc", ir.Type("usize")))
                self.out_rir.types.append(
                    ir.Struct(
                        False, mangle_symbol(ts), fields
                    )
                )
            elif ts.kind == sym.TypeKind.Struct:
                fields = list()
                for base in ts.info.bases:
                    fields.append(
                        ir.Field(base.name, ir.Type(mangle_symbol(base)))
                    )
                for f in ts.fields:
                    fields.append(
                        ir.Field(f.name, self.ir_type(f.typ))
                    )
                self.out_rir.types.append(
                    ir.Struct(
                        ts.info.is_opaque,
                        mangle_symbol(ts), fields
                    )
                )

    def get_type_symbols(self, root):
        ts = list()
        for s in root.syms:
            if isinstance(s, sym.Type):
                ts.append(s)
                ts += self.get_type_symbols(s)
            elif isinstance(s, sym.Pkg):
                ts += self.get_type_symbols(s)
            elif isinstance(s, sym.Mod):
                ts += self.get_type_symbols(s)
        return ts

    def sort_type_symbols(self, tss):
        dg = utils.DepGraph()
        typ_names = list()
        for ts in tss:
            if ts.kind in (sym.TypeKind.Alias, sym.TypeKind.Never):
                continue
            ts.mangled_name = mangle_symbol(ts)
            typ_names.append(ts.mangled_name)
        for ts in tss:
            if ts.kind in (sym.TypeKind.Alias, sym.TypeKind.Never):
                continue
            field_deps = list()
            if ts.kind == sym.TypeKind.Array:
                dsym = ts.info.elem_typ.symbol()
                dep = mangle_symbol(dsym)
                if dep in typ_names:
                    field_deps.append(dep)
            elif ts.kind == sym.TypeKind.Vec:
                dsym = ts.info.elem_typ.symbol()
                dep = mangle_symbol(dsym)
                if dep in typ_names:
                    field_deps.append(dep)
            elif ts.kind == sym.TypeKind.Tuple:
                for f in ts.info.types:
                    dsym = f.symbol()
                    dep = mangle_symbol(dsym)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f, type.Ref
                    ):
                        continue
                    field_deps.append(dep)
            elif ts.kind == sym.TypeKind.Class:
                if ts.info.base:
                    dep = mangle_symbol(ts.info.base)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
                for f in ts.fields:
                    dsym = f.typ.symbol()
                    dep = mangle_symbol(dsym)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
            elif ts.kind == sym.TypeKind.Struct:
                for base in ts.info.bases:
                    dep = mangle_symbol(base)
                    if dep not in typ_names or dep in field_deps or isinstance(
                        f.typ, type.Optional
                    ):
                        continue
                    field_deps.append(dep)
                for f in ts.fields:
                    dsym = f.typ.symbol()
                    dep = mangle_symbol(dsym)
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
        types_sorted = list()
        for node in dg_sorted.nodes:
            for ts in tss:
                if ts.mangled_name == node.name:
                    types_sorted.append(ts)
        return types_sorted
