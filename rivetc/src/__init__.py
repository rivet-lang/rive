# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from os import path
import os, copy, glob

from . import (
    ast, sym, type, token, prefs, report, utils,

    # stages
    parser, register, resolver, checker, codegen
)

class Compiler:
    def __init__(self, args):
        #  `universe` is the mega-module where all the modules being
        #  compiled reside.
        self.universe = sym.universe()

        #  Primitive types.
        self.void_t = type.Type(self.universe[0])
        self.never_t = type.Type(self.universe[1])
        self.nil_t = type.Type(self.universe[2])
        self.bool_t = type.Type(self.universe[3])
        self.rune_t = type.Type(self.universe[4])
        self.i8_t = type.Type(self.universe[5])
        self.i16_t = type.Type(self.universe[6])
        self.i32_t = type.Type(self.universe[7])
        self.i64_t = type.Type(self.universe[8])
        self.isize_t = type.Type(self.universe[9])
        self.u8_t = type.Type(self.universe[10])
        self.u16_t = type.Type(self.universe[11])
        self.u32_t = type.Type(self.universe[12])
        self.u64_t = type.Type(self.universe[13])
        self.usize_t = type.Type(self.universe[14])
        self.untyped_int_t = type.Type(self.universe[15])
        self.untyped_float_t = type.Type(self.universe[16])
        self.f32_t = type.Type(self.universe[17])
        self.f64_t = type.Type(self.universe[18])
        self.string_t = type.Type(self.universe[19])
        self.error_t = type.Type(self.universe[20])

        self.prefs = prefs.Prefs(args)
        self.pointer_size = 8 if self.prefs.target_bits == prefs.Bits.X64 else 4

        self.runtime_mod = None
        self.vec_sym = None # from `runtime` module

        self.parsed_files = []
        self.source_files = []

        self.register = register.Register(self)
        self.resolver = resolver.Resolver(self)
        self.checker = checker.Checker(self)
        self.codegen = codegen.Codegen(self)

    def import_modules(self):
        for sf in self.parsed_files:
            for decl in sf.decls:
                if isinstance(decl, ast.ImportDecl):
                    mod = self.load_mod(
                        decl.path, decl.alias, sf.file, decl.pos
                    )
                    if mod.found:
                        if mod_sym_ := self.universe.find(mod.full_name):
                            mod_sym = mod_sym_ # module already imported
                        else:
                            mod_sym = sym.Mod(decl.vis, mod.full_name)
                            self.universe.add(mod_sym)
                            self.parsed_files += parser.Parser(self).parse_mod(
                                sf.sym.name, mod_sym, decl.vis, mod.files
                            )
                        decl.alias = mod.alias
                        decl.mod_sym = mod_sym
        self.resolve_deps()
        if report.ERRORS > 0:
            self.abort()

    def resolve_deps(self):
        g = self.import_graph()
        g_resolved = g.resolve()
        if self.prefs.is_verbose:
            utils.eprint("-----= resolved dependencies graph =-----")
            utils.eprint(g_resolved.display())
            utils.eprint("-----------------------------------------")
        cycles = g_resolved.display_cycles()
        if len(cycles) > 1:
            utils.error(
                f"import cycle detected between the following modules:\n{cycles}"
            )
        if self.prefs.is_verbose:
            utils.eprint("----------= imported modules =-----------")
            for node in g_resolved.nodes:
                utils.eprint(f" > {node.name}")
            utils.eprint("-----------------------------------------")
        for node in g_resolved.nodes:
            for fp in self.parsed_files:
                if not fp.sym:
                    continue
                if fp.sym.name == node.name:
                    self.source_files.append(fp)
        self.parsed_files.clear()

    def import_graph(self):
        g = utils.DepGraph()
        for fp in self.parsed_files:
            if not fp.sym:
                continue
            deps = []
            if fp.sym.name not in ["c.libc", "c", "runtime"]:
                deps.append("runtime")
            for d in fp.decls:
                if isinstance(d, ast.ImportDecl):
                    if not d.mod_sym:
                        continue # module not found
                    if d.mod_sym.name == fp.sym.name:
                        report.error("import cycle detected", d.pos)
                        continue
                    if fp.sym.name == "c" and d.mod_sym.name == "c.libc":
                        continue
                    deps.append(d.mod_sym.name)
            g.add(fp.sym.name, deps)
        return g

    def run(self):
        self.parsed_files += self.load_mod_and_parse(
            "runtime", "runtime", "", token.NO_POS
        )
        self.load_root_mod()
        self.import_modules()
        if not self.prefs.check_syntax:
            self.register.walk_files(self.source_files)
            if report.ERRORS > 0:
                self.abort()
            self.resolver.resolve_files(self.source_files)
            if report.ERRORS > 0:
                self.abort()
            self.checker.check_files(self.source_files)
            if report.ERRORS > 0:
                self.abort()
            if not self.prefs.check:
                self.codegen.gen_source_files(self.source_files)
                if report.ERRORS > 0:
                    self.abort()

    def load_root_mod(self):
        if path.isdir(self.prefs.input):
            files = self.filter_files(
                glob.glob(path.join(self.prefs.input, "*.ri"))
            )
            src_dir = path.join(self.prefs.input, "src")
            if path.isdir(src_dir): # support `src/` directory
                files += glob.glob(path.join(src_dir, "*.ri"))
        else:
            files = [self.prefs.input]
        if len(files) == 0:
            utils.error("no input received")
        root_sym = sym.Mod(sym.Vis.Priv, self.prefs.mod_name)
        root_sym.is_root = True
        self.universe.add(root_sym)
        self.parsed_files += parser.Parser(self).parse_mod(
            self.prefs.mod_name, root_sym, sym.Vis.Priv, files
        )

    def load_mod_and_parse(self, pathx, alias, file_path, pos):
        mod = self.load_mod(pathx, alias, file_path, pos)
        if mod.found:
            mod_sym = sym.Mod(sym.Vis.Priv, mod.full_name)
            self.universe.add(mod_sym)
            return parser.Parser(self).parse_mod(
                mod.full_name, mod_sym, sym.Vis.Priv, mod.files
            )
        return []

    def load_mod(self, pathx, alias, file_path, pos):
        found = False
        name = ""
        full_name = ""
        abspath = ""
        files = []
        is_super = pathx.startswith("../")
        if pathx.startswith("./") or is_super:
            pathx2 = pathx[3 if is_super else 2:]
            name = pathx2[pathx2.rfind("/") + 1:]
            old_wd = os.getcwd()
            dirname = path.abspath(path.dirname(file_path))
            os.chdir(dirname)
            if path.isdir(pathx):
                found = True
                abspath = path.abspath(pathx)
                mod_basedir = path.dirname(abspath)
                if mod_basedir.endswith("/src"):
                    mod_basedir = mod_basedir[:-4] # skip `src/`
                names = abspath[mod_basedir.rfind("/") + 1:].split("/")
                if "src" in names:
                    src_idx = names.index("src")
                    full_name = ".".join([
                        *names[:src_idx], *names[src_idx + 1:]
                    ])
                else:
                    full_name = ".".join(names)
            os.chdir(old_wd)
            if found:
                files = self.filter_files(
                    glob.glob(path.join(path.relpath(abspath), "*.ri"))
                )
        else:
            name = pathx[pathx.rfind("/") + 1:]
            full_name = pathx.replace("/", ".")
            for l in self.prefs.library_path:
                mod_path = path.relpath(path.join(l, pathx))
                if path.isdir(mod_path):
                    found = True
                    files = self.filter_files(
                        glob.glob(path.join(mod_path, "*.ri"))
                    )
                # support `src/` directory
                if pathx.count("/") > 0:
                    slash_idx = pathx.find("/") + 1
                    src_dir = path.join(
                        l, pathx[:slash_idx], "src", pathx[slash_idx:]
                    )
                else:
                    src_dir = path.join(mod_path, "src")
                if path.isdir(src_dir):
                    if not found: found = True
                    files = self.filter_files(
                        glob.glob(path.join(src_dir, "*.ri"))
                    )
                if found:
                    break
        if not found:
            report.error(f"module `{pathx}` not found", pos)
        elif len(files) == 0:
            report.error(f"module `{pathx}` contains no rivet files", pos)
        return ast.ImportedMod(
            found, name, name if len(alias) == 0 else alias, full_name, files
        )

    def filter_files(self, inputs):
        new_inputs = []
        for input in inputs:
            basename_input = path.basename(path.relpath(input))
            if basename_input.count('.') == 1:
                new_inputs.append(input)
                continue
            exts = basename_input[:-3].split('.')[1:]
            should_compile = True
            already_exts = []
            for ext in exts:
                if ext in already_exts:
                    error(f"{input}: duplicate special extension `{ext}`")
                already_exts.append(ext)
                if ext.startswith("d_") or ext.startswith("notd_"):
                    if ext.startswith("d_"):
                        should_compile = should_compile and ext[
                            2:] in self.prefs.flags
                    else:
                        should_compile = should_compile and ext[
                            5:] not in self.prefs.flags
                elif osf := prefs.OS.from_string(ext):
                    should_compile = should_compile and self.prefs.target_os == osf
                elif arch := prefs.Arch.from_string(ext):
                    should_compile = should_compile and self.prefs.target_arch == arch
                elif ext in ("x32", "x64"):
                    if ext == "x32":
                        should_compile = should_compile and self.prefs.target_bits == Bits.X32
                    else:
                        should_compile = should_compile and self.prefs.target_bits == Bits.X64
                elif ext in ("little_endian", "big_endian"):
                    if ext == "little_endian":
                        should_compile = should_compile and self.prefs.target_endian == Endian.Little
                    else:
                        should_compile = should_compile and self.prefs.target_endian == Endian.Big
                elif b := Backend.from_string(ext): # backends
                    should_compile = should_compile and self.prefs.target_backend == b
                else:
                    error(f"{input}: unknown special extension `{ext}`")
            if should_compile:
                new_inputs.append(input)
        return new_inputs

    # ========================================================

    def is_number(self, typ):
        return self.is_int(typ) or self.is_float(typ)

    def is_int(self, typ):
        return self.is_signed_int(typ) or self.is_unsigned_int(typ)

    def is_signed_int(self, typ):
        return typ in (
            self.i8_t, self.i16_t, self.i32_t, self.i64_t, self.isize_t,
            self.untyped_int_t
        )

    def is_unsigned_int(self, typ):
        return typ in (
            self.u8_t, self.u16_t, self.u32_t, self.u64_t, self.usize_t
        )

    def is_float(self, typ):
        return typ in (self.f32_t, self.f64_t, self.untyped_float_t)

    def untyped_to_type(self, typ):
        if typ == self.untyped_int_t:
            return self.i32_t
        elif typ == self.untyped_float_t:
            return self.f64_t
        return typ

    def num_bits(self, typ):
        if self.is_int(typ):
            return self.int_bits(typ)
        return self.float_bits(typ)

    def int_bits(self, typ):
        typ_sym = typ.symbol()
        if typ_sym.kind == sym.TypeKind.UntypedInt:
            return 75 # only for checker
        elif typ_sym.kind in (sym.TypeKind.Int8, sym.TypeKind.Uint8):
            return 8
        elif typ_sym.kind in (sym.TypeKind.Int16, sym.TypeKind.Uint16):
            return 16
        elif typ_sym.kind in (sym.TypeKind.Int32, sym.TypeKind.Uint32):
            return 32
        elif typ_sym.kind in (sym.TypeKind.Int64, sym.TypeKind.Uint64):
            return 64
        elif typ_sym.kind in (sym.TypeKind.Isize, sym.TypeKind.Usize):
            return 32 if self.prefs.target_bits == prefs.Bits.X32 else 64
        else:
            return -1

    def float_bits(self, typ):
        typ_sym = typ.symbol()
        if typ_sym.kind == sym.TypeKind.Float32:
            return 32
        elif typ_sym.kind in (sym.TypeKind.Float64, sym.TypeKind.UntypedFloat):
            return 64
        else:
            return -1

    # Returns the size and alignment (in bytes) of `typ`, similarly to
    # C's `sizeof(T)` and `_Alignof(T)`.
    def type_size(self, typ):
        if isinstance(typ, (type.Result, type.Optional)):
            return self.type_size(typ.typ)
        elif isinstance(typ, (type.Ptr, type.Ref)):
            return self.pointer_size, self.pointer_size
        elif isinstance(typ, type.Fn):
            return self.pointer_size, self.pointer_size
        return self.type_symbol_size(typ.symbol())

    def type_symbol_size(self, sy):
        if sy.size != -1:
            return sy.size, sy.align
        size, align = 0, 0
        if sy.kind in (
            sym.TypeKind.Placeholder, sym.TypeKind.Void, sym.TypeKind.Nil,
            sym.TypeKind.Never
        ):
            pass
        elif sy.kind == sym.TypeKind.Alias:
            size, align = self.type_size(sy.info.parent)
        elif sy.kind in (sym.TypeKind.Usize, sym.TypeKind.Isize):
            size, align = self.pointer_size, self.pointer_size
        elif sy.kind in (
            sym.TypeKind.Int8, sym.TypeKind.Uint8, sym.TypeKind.Bool
        ):
            size, align = 1, 1
        elif sy.kind in (sym.TypeKind.Int16, sym.TypeKind.Uint16):
            size, align = 2, 2
        elif sy.kind in (
            sym.TypeKind.Int32, sym.TypeKind.Uint32, sym.TypeKind.Rune,
            sym.TypeKind.Float32, sym.TypeKind.UntypedInt
        ):
            size, align = 4, 4
        elif sy.kind in (
            sym.TypeKind.Int64, sym.TypeKind.Uint64, sym.TypeKind.Float64,
            sym.TypeKind.UntypedFloat
        ):
            size, align = 8, 8
        elif sy.kind == sym.TypeKind.Enum:
            size, align = self.type_size(sy.info.underlying_typ)
        elif sy.kind == sym.TypeKind.Array:
            elem_size, elem_align = self.type_size(sy.info.elem_typ)
            size, align = int(sy.info.size.lit) * elem_size, elem_align
        elif sy.is_boxed():
            size, align = self.pointer_size, self.pointer_size
        elif sy.kind in (sym.TypeKind.Struct, sym.TypeKind.Tuple):
            total_size = 0
            max_alignment = 0
            types = sy.info.types if sy.kind == sym.TypeKind.Tuple else list(
                map(lambda it: it.typ, sy.full_fields())
            )
            for ftyp in types:
                field_size, alignment = self.type_size(ftyp)
                if alignment > max_alignment:
                    max_alignment = alignment
                total_size = utils.round_up(total_size, alignment) + field_size
            size = utils.round_up(total_size, max_alignment)
            align = max_alignment
        else:
            raise Exception(
                f"Compiler.type_size(): unsupported type `{sy.qualname()}`"
            )
        sy.size = size
        sy.align = align
        return size, align

    def evalue_pp_symbol(self, name, pos):
        #  operating systems
        if name in ("_LINUX_", "_WINDOWS_"):
            return self.prefs.target_os.equals_to_string(name)
        #  architectures
        elif name in ("_X86_", "_AMD64_"):
            return self.prefs.target_arch.equals_to_string(name)
        #  bits
        elif name in ("_x32_", "_x64_"):
            if name == "_x32_":
                return self.prefs.target_bits == prefs.Bits.X32
            return self.prefs.target_bits == prefs.Bits.X64
        #  endian
        elif name in ("_LITTLE_ENDIAN_", "_BIG_ENDIAN_"):
            if name == "_LITTLE_ENDIAN_":
                return self.prefs.target_endian == prefs.Endian.Little
            return self.prefs.target_endian == prefs.Endian.Big
        # build modes
        elif name in ("_DEBUG_", "_RELEASE_", "_TESTS_"):
            if name == "_DEBUG_":
                return self.prefs.build_mode == prefs.BuildMode.Debug
            elif name == "_RELEASE_":
                return self.prefs.build_mode == prefs.BuildMode.Release
            return self.prefs.build_mode == prefs.BuildMode.Test
        elif name.startswith("_") and name.endswith("_"):
            report.error(f"unknown runtime flag: `{name}`", pos)
            return False
        return name in self.prefs.flags

    # ========================================================

    def vlog(self, msg):
        if self.prefs.is_verbose:
            utils.eprint(">>", msg)

    def abort(self):
        if report.ERRORS == 1:
            msg = f"could not compile module `{self.prefs.mod_name}`, aborting due to previous error"
        else:
            msg = f"could not compile module `{self.prefs.mod_name}`, aborting due to {report.ERRORS} previous errors"
        if report.WARNS > 0:
            word = "warning" if report.WARNS == 1 else "warnings"
            msg += f"; {report.WARNS} {word} emitted"
        utils.error(msg)
        exit(1)
