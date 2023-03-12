# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

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
        self.none_t = type.Type(self.universe[2])
        self.bool_t = type.Type(self.universe[3])
        self.rune_t = type.Type(self.universe[4])
        self.int8_t = type.Type(self.universe[5])
        self.int16_t = type.Type(self.universe[6])
        self.int32_t = type.Type(self.universe[7])
        self.int64_t = type.Type(self.universe[8])
        self.isize_t = type.Type(self.universe[9])
        self.uint8_t = type.Type(self.universe[10])
        self.uint16_t = type.Type(self.universe[11])
        self.uint32_t = type.Type(self.universe[12])
        self.uint64_t = type.Type(self.universe[13])
        self.usize_t = type.Type(self.universe[14])
        self.comptime_int_t = type.Type(self.universe[15])
        self.comptime_float_t = type.Type(self.universe[16])
        self.float32_t = type.Type(self.universe[17])
        self.float64_t = type.Type(self.universe[18])
        self.string_t = type.Type(self.universe[19])
        self.anyptr_t = type.Ptr(self.void_t)
        self.mut_anyptr_t = type.Ptr(self.void_t, True)
        self.error_t = None # updated in register

        self.prefs = prefs.Prefs(args)
        self.pointer_size = 8 if self.prefs.target_bits == prefs.Bits.X64 else 4

        self.core_mod = None
        self.vec_sym = None # from `core` module
        self.error_sym = None # from `core` module

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
                            mod_sym = sym.Mod(False, mod.full_name)
                            self.universe.add(mod_sym)
                            self.parsed_files += parser.Parser(self).parse_mod(
                                mod_sym, mod.files
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
            if fp.sym.name not in ["c.libc", "c", "core"]:
                deps.append("core")
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
            "core", "core", "", token.NO_POS
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
        root_sym = sym.Mod(False, self.prefs.mod_name)
        root_sym.is_root = True
        self.universe.add(root_sym)
        self.parsed_files += parser.Parser(self).parse_mod(root_sym, files)

    def load_mod_and_parse(self, pathx, alias, file_path, pos):
        mod = self.load_mod(pathx, alias, file_path, pos)
        if mod.found:
            mod_sym = sym.Mod(False, mod.full_name)
            self.universe.add(mod_sym)
            return parser.Parser(self).parse_mod(mod_sym, mod.files)
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
            dirname = path.abspath(path.dirname(file_path))
            old_wd = os.getcwd()
            os.chdir(dirname)
            if path.isdir(pathx):
                found = True
                abspath = path.abspath(pathx)
                mod_basedir = path.dirname(abspath)
                if mod_basedir.endswith("/src"):
                    mod_basedir = mod_basedir[:-4] # skip `src/`
                if "/src" in mod_basedir and not mod_basedir.endswith("/src"):
                    first_part = mod_basedir[:mod_basedir.rfind("/")]
                    mod_basedir = mod_basedir[:first_part.rfind("/")]
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
            basename_input = path.basename(input)
            if basename_input.count('.') == 1:
                new_inputs.append(input)
                continue
            exts = basename_input[:-3].split('.')[1:]
            should_compile = False
            already_exts = []
            for ext in exts:
                if ext in already_exts:
                    error(f"{input}: duplicate special extension `{ext}`")
                    continue
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
                    break
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
            self.int8_t, self.int16_t, self.int32_t, self.int64_t, self.isize_t,
            self.comptime_int_t
        )

    def is_unsigned_int(self, typ):
        return typ in (
            self.uint8_t, self.uint16_t, self.uint32_t, self.uint64_t,
            self.usize_t
        )

    def is_float(self, typ):
        return typ in (self.float32_t, self.float64_t, self.comptime_float_t)

    def is_comptime_number(self, typ):
        return typ == self.comptime_int_t or typ == self.comptime_float_t

    def comptime_number_to_type(self, typ):
        if typ == self.comptime_int_t:
            return self.int32_t
        elif typ == self.comptime_float_t:
            return self.float64_t
        return typ

    def num_bits(self, typ):
        if self.is_int(typ):
            return self.int_bits(typ)
        return self.float_bits(typ)

    def int_bits(self, typ):
        typ_sym = typ.symbol()
        if typ_sym.kind == sym.TypeKind.ComptimeInt:
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
        elif typ_sym.kind in (sym.TypeKind.Float64, sym.TypeKind.ComptimeFloat):
            return 64
        else:
            return -1

    # Returns the size and alignment (in bytes) of `typ`, similarly to
    # C's `sizeof(T)` and `_Alignof(T)`.
    def type_size(self, typ):
        if isinstance(typ, (type.Result, type.Option)):
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
            sym.TypeKind.Placeholder, sym.TypeKind.Void, sym.TypeKind.None_,
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
            sym.TypeKind.Float32, sym.TypeKind.ComptimeInt
        ):
            size, align = 4, 4
        elif sy.kind in (
            sym.TypeKind.Int64, sym.TypeKind.Uint64, sym.TypeKind.Float64,
            sym.TypeKind.ComptimeFloat
        ):
            size, align = 8, 8
        elif sy.kind == sym.TypeKind.Enum:
            if sy.info.is_boxed_enum:
                size, align = self.pointer_size, self.pointer_size
            else:
                size, align = self.type_size(sy.info.underlying_typ)
        elif sy.kind == sym.TypeKind.Array:
            elem_size, elem_align = self.type_size(sy.info.elem_typ)
            size, align = int(sy.info.size.lit) * elem_size, elem_align
        elif sy.is_boxed():
            size, align = self.pointer_size, self.pointer_size
        elif sy.kind in (sym.TypeKind.Struct, sym.TypeKind.Tuple):
            if sy.kind == sym.TypeKind.Struct and sy.info.is_boxed:
                size, align = self.pointer_size, self.pointer_size
            else:
                total_size = 0
                max_alignment = 0
                types = sy.info.types if sy.kind == sym.TypeKind.Tuple else list(
                    map(lambda it: it.typ, sy.full_fields())
                )
                for ftyp in types:
                    field_size, alignment = self.type_size(ftyp)
                    if alignment > max_alignment:
                        max_alignment = alignment
                    total_size = utils.round_up(
                        total_size, alignment
                    ) + field_size
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
        # operating systems
        if name in ("_LINUX_", "_WINDOWS_"):
            return self.prefs.target_os.equals_to_string(name)
        # architectures
        elif name in ("_X86_", "_AMD64_"):
            return self.prefs.target_arch.equals_to_string(name)
        # bits
        elif name in ("_x32_", "_x64_"):
            if name == "_x32_":
                return self.prefs.target_bits == prefs.Bits.X32
            return self.prefs.target_bits == prefs.Bits.X64
        # endian
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
            report.error(f"unknown builtin flag: `{name}`", pos)
            return False
        return name in self.prefs.flags

    # ========================================================

    def vlog(self, msg):
        if self.prefs.is_verbose:
            utils.eprint(utils.bold(utils.green(">>")), msg)

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
