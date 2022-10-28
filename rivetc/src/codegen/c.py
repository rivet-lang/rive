# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os

from . import ir, c_headers
from .. import prefs, utils

# NOTE: some of the words in `C_RESERVED` are not reserved in C, but are
# in C++, thus need escaping too. `small` should not be needed, but see:
# https://stackoverflow.com/questions/5874215/what-is-rpcndr-h
C_RESERVED = [
    'auto', 'bool', 'case', 'char', 'complex', 'default', 'delete', 'do',
    'double', 'export', 'float', 'goto', 'inline', 'int', 'long', 'namespace',
    'new', 'register', 'restrict', 'short', 'signed', 'sizeof', 'small',
    'static', 'typedef', 'typename', 'union', 'unix', 'unsigned', 'void',
    'volatile', 'template'
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

        c_file = f"{self.comp.prefs.pkg_name}.ri.c"
        with open(c_file, "w+") as out:
            out.write(c_headers.HEADER)
            out.write(str(self.typedefs).strip() + "\n\n")
            out.write(str(self.structs).strip() + "\n\n")
            out.write(str(self.protos).strip() + "\n\n")
            out.write(str(self.globals).strip() + "\n\n")
            out.write(str(self.out).strip())

        args = [
            self.comp.prefs.target_backend_compiler, "-o",
            self.comp.prefs.pkg_output, "-fno-builtin", "-Werror",
            "-m64" if self.comp.prefs.target_bits == prefs.Bits.X64 else "-m32",
            *[f"-l{l}" for l in self.comp.prefs.libraries_to_link],
            *[f"-L{l}" for l in self.comp.prefs.library_path],
            *self.comp.prefs.objects_to_link, c_file
        ]
        if self.comp.prefs.build_mode == prefs.BuildMode.Release:
            args.append("-flto")
            args.append("-O3")
        else:
            args.append("-g")
        self.comp.vlog(f"C compiler options: {args}")

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
        pass

    def gen_globals(self, globals):
        pass

    def gen_decls(self, decls):
        for decl in decls:
            if isinstance(decl, ir.FnDecl):
                if decl.is_never:
                    self.write("RIVET_NEVER ")
                    self.protos.write("RIVET_NEVER ")
                if decl.is_pub:
                    self.write("RIVET_EXPORT ")
                    self.protos.write("RIVET_EXPORT ")
                else:
                    self.write("RIVET_LOCAL ")
                    self.protos.write("RIVET_LOCAL ")
                ret_typ=self.gen_type(decl.ret_typ)
                self.protos.write(f"{ret_typ} {decl.name}(")
                self.write(f"{ret_typ} {decl.name}(")
                if len(decl.args)==0:
                    self.write("void")
                    self.protos.write("void")
                else:
                    for i, arg in enumerate(decl.args):
                        arg_typ=self.gen_type(arg.typ, arg.name)
                        self.protos.write(arg_typ)
                        self.write(arg_typ)
                        if not isinstance(arg.typ, (ir.Array, ir.Function)):
                            self.protos.write(f" {arg.name}")
                            self.write(f" {arg.name}")
                        if i < len(decl.args) - 1:
                            self.protos.write(", ")
                            self.write(", ")
                self.protos.writeln(");")
                self.writeln(") {")
                if decl.is_never:
                    self.writeln("  while (1);")
                self.writeln("}")
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

    def write_type(self, typ, wrap = ""):
        self.write(self.gen_type(typ, wrap))

    def gen_type(self, typ, wrap = ""):
        if isinstance(typ, ir.Pointer):
            sb=utils.Builder()
            sb.write(self.gen_type(typ.typ, wrap))
            sb.write("*")
            return str(sb)
        elif isinstance(typ, ir.Array):
            sb=utils.Builder()
            sb.write(self.gen_type(typ.typ))
            if len(wrap) > 0:
                sb.write(f" {wrap}")
            sb.write(f"[{typ.size}]")
            return str(sb)
        elif isinstance(typ, ir.Function):
            sb=utils.Builder()
            sb.write(self.gen_type(typ.ret_typ))
            sb.write("(*")
            if len(wrap) > 0:
                sb.write(wrap)
            sb.write(")(")
            for i, arg in enumerate(typ.args):
                sb.write(self.gen_type(arg))
            sb.write(")")
            return str(sb)
        else:
            return str(typ)
