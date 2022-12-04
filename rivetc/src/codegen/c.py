# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

import os

from .. import prefs, utils

from .ir import InstKind
from . import ir, c_headers

MIN_INT64 = -9223372036854775808

# NOTE: some of the words in `C_RESERVED` are not reserved in C, but are
# in C++, thus need escaping too. `small` should not be needed, but see
# https://stackoverflow.com/questions/5874215/what-is-rpcndr-h
C_RESERVED = [
    'auto', 'bool', 'case', 'char', 'complex', 'default', 'delete', 'do',
    'double', 'export', 'float', 'goto', 'inline', 'int', 'long', 'namespace',
    'new', 'register', 'restrict', 'short', 'signed', 'sizeof', 'small',
    'static', 'typedef', 'typename', 'union', 'unix', 'unsigned', 'void',
    'volatile', 'template', 'far', 'near', 'huge'
]

def c_escape(kw):
    if kw in C_RESERVED:
        return f"_ri_{kw}"
    return kw

class CGen:
    def __init__(self, comp):
        self.comp = comp
        self.typedefs = utils.Builder()
        self.structs = utils.Builder()
        self.protos = utils.Builder()
        self.globals = utils.Builder()
        self.out = utils.Builder()

    def gen(self, out_rir):
        self.gen_structs(out_rir.structs)
        self.gen_externs(out_rir.externs)
        self.gen_globals(out_rir.globals)
        self.gen_decls(out_rir.decls)

        c_file = f"{self.comp.prefs.mod_name}.ri.c"
        with open(c_file, "w+") as out:
            out.write(c_headers.HEADER)
            out.write(str(self.typedefs).strip() + "\n\n")
            out.write(str(self.structs).strip() + "\n\n")
            out.write(str(self.protos).strip() + "\n\n")
            out.write(str(self.globals).strip() + "\n\n")
            out.write(str(self.out).strip())

        args = [
            self.comp.prefs.target_backend_compiler, "-o",
            self.comp.prefs.mod_output, "-Werror", "-fno-builtin",
            "-m64" if self.comp.prefs.target_bits == prefs.Bits.X64 else "-m32",
        ]
        if self.comp.prefs.build_mode == prefs.BuildMode.Release:
            args.append("-flto")
            args.append("-O3")
        else:
            args.append("-g3")
        if self.comp.prefs.target_os == prefs.OS.Windows:
            args.append(f"-municode")

        for l in self.comp.prefs.library_path:
            args.append(f"-L{l}")
        for l in self.comp.prefs.libraries_to_link:
            args.append(f"-l{l}")
        for obj in self.comp.prefs.objects_to_link:
            args.append(obj)

        args.append(c_file)
        self.comp.vlog(f"C compiler arguments: {' '.join(args)}")

        res = utils.execute(*args)
        if res.exit_code == 0:
            if not self.comp.prefs.keep_c:
                os.remove(c_file)
        else:
            utils.error(
                f"error while compiling the output C file `{c_file}`:\n{res.err}"
            )

    def write(self, txt):
        self.out.write(txt)

    def writeln(self, txt = ""):
        self.out.writeln(txt)

    def gen_structs(self, structs):
        for s in structs:
            self.typedefs.writeln(f"typedef struct {s.name} {s.name};")
            if not s.is_opaque:
                self.structs.writeln(f"struct {s.name} {{")
                for i, f in enumerate(s.fields):
                    self.structs.write("  ")
                    self.structs.write(self.gen_type(f.typ, f.name))
                    if not isinstance(f.typ, (ir.Array, ir.Function)):
                        self.structs.write(f" {f.name}")
                    if i < len(s.fields) - 1:
                        self.structs.writeln(";")
                    else:
                        self.structs.writeln(";")
                self.structs.writeln("};")
            self.structs.writeln()

    def gen_externs(self, externs):
        for extern_fn in externs:
            self.gen_fn_decl(extern_fn)

    def gen_globals(self, globals):
        for g in globals:
            if not g.is_pub:
                self.globals.write("RIVET_LOCAL ")
            if g.is_extern:
                self.globals.write("extern ")
            self.globals.write(self.gen_type(g.typ))
            self.globals.write(" ")
            self.globals.write(g.name)
            self.globals.writeln(";")

    def gen_decls(self, decls):
        for decl in decls:
            if isinstance(decl, ir.FnDecl):
                self.gen_fn_decl(decl)
            else:
                self.globals.writeln(
                    f"static {decl.structure} {decl.name}[{decl.implement_nr}] = {{"
                )
                for i, ft in enumerate(decl.funcs):
                    self.globals.writeln('  {')
                    for f, impl in ft.items():
                        self.globals.writeln(f'    .{f} = (void*){impl}')
                    self.globals.write("  }")
                    if i < len(decl.funcs) - 1:
                        self.globals.writeln(",")
                    else:
                        self.globals.writeln()
                self.globals.writeln("};")
            self.writeln()

    def gen_fn_decl(self, decl):
        if decl.is_never:
            if not decl.is_extern:
                self.write("RIVET_NEVER ")
            self.protos.write("RIVET_NEVER ")
        if not decl.is_extern:
            if decl.is_pub:
                self.write("RIVET_EXPORT ")
                self.protos.write("RIVET_EXPORT ")
            else:
                self.write("RIVET_LOCAL ")
                self.protos.write("RIVET_LOCAL ")
        ret_typ = self.gen_type(decl.ret_typ)
        self.protos.write(f"{ret_typ} {decl.name}(")
        if not decl.is_extern:
            self.write(f"{ret_typ} {decl.name}(")
        if len(decl.args) == 0:
            if not decl.is_extern:
                self.write("void")
            self.protos.write("void")
        else:
            for i, arg in enumerate(decl.args):
                arg_typ = self.gen_type(arg.typ, arg.name)
                self.protos.write(arg_typ)
                if not decl.is_extern:
                    self.write(arg_typ)
                if not isinstance(arg.typ, (ir.Array, ir.Function)):
                    self.protos.write(f" {arg.name}")
                    if not decl.is_extern:
                        self.write(f" {arg.name}")
                if i < len(decl.args) - 1:
                    self.protos.write(", ")
                    if not decl.is_extern:
                        self.write(", ")
            if decl.is_variadic:
                if len(decl.args) > 0:
                    if not decl.is_extern:
                        self.write(", ")
                    self.protos.write(", ")
                if not decl.is_extern:
                    self.write("...")
                self.protos.write("...")
        self.protos.writeln(");")
        if not decl.is_extern:
            self.writeln(") {")
            self.gen_instrs(decl.instrs)
            if decl.is_never:
                self.writeln("  while (1);")
            self.writeln("}")

    def gen_instrs(self, insts):
        for inst in insts:
            if isinstance(inst, ir.Label):
                self.writeln()
            else:
                self.write("  ")
            if isinstance(inst, ir.Skip):
                continue # skip
            elif isinstance(inst, ir.Comment):
                self.writeln(f"/* {inst.text} */")
            elif isinstance(inst, ir.Label):
                self.writeln(f"{inst.label}: {{}}")
            elif isinstance(inst, ir.Inst):
                self.gen_inst(inst)
                if inst.kind == InstKind.DbgStmtLine:
                    self.writeln()
                elif inst.kind != InstKind.Nop:
                    self.writeln(";")

    def gen_inst(self, inst):
        if inst.kind == InstKind.Nop:
            self.write("/* NOP */")
        elif inst.kind == InstKind.Alloca:
            name = inst.args[0].name
            typ = inst.args[0].typ
            self.write_type(typ, name)
            if not isinstance(typ, (ir.Function, ir.Array)):
                self.write(" ")
                self.gen_expr(inst.args[0])
            if len(inst.args) == 2:
                self.write(" = ")
                self.gen_expr(inst.args[1])
        elif inst.kind in (InstKind.Store, InstKind.StorePtr):
            arg0 = inst.args[0]
            arg1 = inst.args[1]
            if inst.kind == InstKind.StorePtr:
                self.write("(*(")
            self.gen_expr(arg0)
            if inst.kind == InstKind.StorePtr:
                self.write("))")
            self.write(" = ")
            self.gen_expr(arg1)
        elif inst.kind == InstKind.LoadPtr:
            self.write("(*(")
            self.gen_expr(inst.args[0])
            self.write("))")
        elif inst.kind == InstKind.GetElementPtr:
            self.write("(")
            self.gen_expr(inst.args[0])
            arg1 = inst.args[1]
            if not (isinstance(arg1, ir.IntLit) and arg1.lit == "0"):
                self.write(" + ")
                self.gen_expr(arg1)
            self.write(")")
        elif inst.kind == InstKind.GetRef:
            arg0 = inst.args[0]
            if isinstance(
                arg0, (ir.Ident, ir.Selector, ir.ArrayLit)
            ) or (isinstance(arg0, ir.Inst) and arg0.kind == InstKind.LoadPtr):
                if isinstance(arg0, ir.ArrayLit):
                    self.gen_expr(arg0)
                else:
                    self.write("(&")
                    self.gen_expr(arg0)
                    self.write(")")
            else:
                self.write(f"(({self.gen_type(arg0.typ)}[]){{ ")
                self.gen_expr(arg0)
                self.write(" })")
        elif inst.kind == InstKind.Cast:
            self.write("((")
            self.gen_expr(inst.args[1])
            self.write(")(")
            self.gen_expr(inst.args[0])
            self.write("))")
        elif inst.kind == InstKind.Cmp:
            self.gen_expr(inst.args[1])
            self.write(" ")
            self.gen_expr(inst.args[0])
            self.write(" ")
            self.gen_expr(inst.args[2])
        elif inst.kind == InstKind.Select:
            self.write("(")
            self.gen_expr(inst.args[0])
            self.write(") ? (")
            self.gen_expr(inst.args[1])
            self.write(") : (")
            self.gen_expr(inst.args[2])
            self.write(")")
        elif inst.kind == InstKind.DbgStmtLine:
            self.write(f'#line {inst.args[1].name} "{inst.args[0].name}"')
        elif inst.kind == InstKind.Breakpoint:
            self.write("RIVET_BREAKPOINT()")
        elif inst.kind in (
            InstKind.Add, InstKind.Sub, InstKind.Mult, InstKind.Div,
            InstKind.Mod, InstKind.BitAnd, InstKind.BitOr, InstKind.BitXor,
            InstKind.Lshift, InstKind.Rshift, InstKind.BooleanAnd,
            InstKind.BooleanOr
        ):
            self.gen_expr(inst.args[0])
            if inst.kind == InstKind.Add: self.write(" + ")
            elif inst.kind == InstKind.Sub: self.write(" - ")
            elif inst.kind == InstKind.Mult: self.write(" * ")
            elif inst.kind == InstKind.Div: self.write(" / ")
            elif inst.kind == InstKind.Mod: self.write(" % ")
            elif inst.kind == InstKind.BitAnd: self.write(" & ")
            elif inst.kind == InstKind.BitOr: self.write(" | ")
            elif inst.kind == InstKind.BitXor: self.write(" ^ ")
            elif inst.kind == InstKind.Lshift: self.write(" << ")
            elif inst.kind == InstKind.Rshift: self.write(" >> ")
            elif inst.kind == InstKind.BooleanAnd: self.write(" && ")
            elif inst.kind == InstKind.BooleanOr: self.write(" || ")
            self.gen_expr(inst.args[1])
        elif inst.kind in (InstKind.Inc, InstKind.Dec):
            self.gen_expr(inst.args[0])
            if inst.kind == InstKind.Inc:
                self.write(" += 1")
            else:
                self.write(" -= 1")
        elif inst.kind in (InstKind.BitNot, InstKind.BooleanNot, InstKind.Neg):
            if inst.kind == InstKind.BooleanNot:
                self.write("!(")
            elif inst.kind == InstKind.Neg:
                self.write("-")
            else:
                self.write("~")
            self.gen_expr(inst.args[0])
            if inst.kind == InstKind.BooleanNot:
                self.write(")")
        elif inst.kind == InstKind.Br:
            if len(inst.args) == 1:
                self.write(f"goto {inst.args[0].name}")
            else:
                self.write("if (")
                self.gen_expr(inst.args[0])
                self.write(f") goto {inst.args[1].name}")
                if len(inst.args) == 3:
                    self.write(f"; else goto {inst.args[2].name}")
        elif inst.kind == InstKind.Call:
            self.gen_expr(inst.args[0])
            self.write("(")
            args = inst.args[1:]
            for i, arg in enumerate(args):
                self.gen_expr(arg)
                if i < len(args) - 1:
                    self.write(", ")
            self.write(")")
        elif inst.kind == InstKind.Ret:
            self.write("return")
            if len(inst.args) == 1:
                self.write(" ")
                self.gen_expr(inst.args[0])
        else:
            raise Exception(inst) # unreachable

    def gen_expr(self, expr):
        if isinstance(expr, ir.Skip):
            self.write("/* <skip> */")
        elif isinstance(expr, ir.Inst):
            self.gen_inst(expr)
        elif isinstance(expr, ir.NilLit):
            self.write("NULL")
        elif isinstance(expr, ir.IntLit):
            if expr.value() == MIN_INT64:
                # NOTE: `-9223372036854775808` is wrong because C compilers
                # parse literal values without sign first, and `9223372036854775808`
                # overflows `i64`, hence the consecutive subtraction by `1`.
                self.write("(-9223372036854775807L - 1)")
            else:
                self.write(expr.lit)
                if expr.typ.name.endswith("64"
                                          ) or expr.typ.name.endswith("size"):
                    if expr.typ.name.startswith("u"):
                        self.write("U")
                    self.write("L")
        elif isinstance(expr, ir.FloatLit):
            self.write(expr.lit)
            if expr.typ.name == "f32":
                self.write("F")
        elif isinstance(expr, ir.RuneLit):
            self.write(f"L'{expr.lit}'")
        elif isinstance(expr, ir.StringLit):
            self.write(f'(u8*)"{expr.lit}"')
        elif isinstance(expr, ir.ArrayLit):
            self.write("(")
            self.write_type(expr.typ)
            if not isinstance(expr.typ, ir.Array):
                self.write("[]")
            self.write("){ ")
            for i, e in enumerate(expr.elems):
                self.gen_expr(e)
                if i < len(expr.elems) - 1:
                    self.write(", ")
            self.write(" }")
        elif isinstance(expr, ir.Ident):
            self.write(c_escape(expr.name))
        elif isinstance(expr, ir.Name):
            self.write(c_escape(expr.name))
        elif isinstance(expr, ir.Selector):
            self.gen_expr(expr.left)
            if isinstance(expr.left.typ, ir.Pointer):
                self.write("->")
            else:
                self.write(".")
            self.write(c_escape(expr.name.name))
        else:
            self.write(self.gen_type(expr))

    def write_type(self, typ, wrap = ""):
        self.write(self.gen_type(typ, wrap))

    def gen_type(self, typ, wrap = ""):
        if isinstance(typ, ir.Pointer):
            return f"{self.gen_type(typ.typ, wrap)}*"
        elif isinstance(typ, ir.Array):
            sizes = []
            p_typ = typ
            is_arr_of_fns = False
            while isinstance(p_typ, ir.Array):
                sizes.append(p_typ.size)
                if isinstance(p_typ.typ, ir.Function):
                    is_arr_of_fns = True
                    p_typ = p_typ.typ
                    break
                p_typ = p_typ.typ
            if is_arr_of_fns and len(sizes) > 0:
                sizes.reverse()
                return self.gen_type(
                    p_typ, f"{wrap}{''.join([f'[{s}]' for s in sizes])}"
                )
            sb = utils.Builder()
            sb.write(self.gen_type(typ.typ, wrap))
            if not isinstance(typ.typ, ir.Array):
                sb.write(f" {wrap}")
            sb.write(f"[{typ.size}]")
            return str(sb)
        elif isinstance(typ, ir.Function):
            sb = utils.Builder()
            sb.write(self.gen_type(typ.ret_typ))
            sb.write(" (*")
            if len(wrap) > 0:
                sb.write(wrap)
            sb.write(")(")
            if len(typ.args) == 0:
                sb.write("void")
            else:
                for i, arg in enumerate(typ.args):
                    sb.write(self.gen_type(arg))
                    if i < len(typ.args) - 1:
                        sb.write(", ")
            sb.write(")")
            return str(sb)
        return str(typ)
