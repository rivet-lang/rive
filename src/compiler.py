# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from os import path
import os, copy, glob

from . import (ast, sym, type, token, prefs, report, utils,

               # stages
               parser)

class Compiler:
	def __init__(self, args):
		# `universe` is the mega-package where all the packages being
		# compiled reside.
		self.universe = sym.universe()

		# Primitive types.
		self.void_t = type.Type(self.universe[0])
		self.none_t = type.Type(self.universe[1])
		self.bool_t = type.Type(self.universe[2])
		self.rune_t = type.Type(self.universe[3])
		self.i8_t = type.Type(self.universe[4])
		self.i16_t = type.Type(self.universe[5])
		self.i32_t = type.Type(self.universe[6])
		self.i64_t = type.Type(self.universe[7])
		self.isize_t = type.Type(self.universe[8])
		self.u8_t = type.Type(self.universe[9])
		self.u16_t = type.Type(self.universe[10])
		self.u32_t = type.Type(self.universe[11])
		self.u64_t = type.Type(self.universe[12])
		self.usize_t = type.Type(self.universe[13])
		self.untyped_int_t = type.Type(self.universe[14])
		self.untyped_float_t = type.Type(self.universe[15])
		self.f32_t = type.Type(self.universe[16])
		self.f64_t = type.Type(self.universe[17])
		self.string_t = type.Type(self.universe[18])
		self.error_t = type.Type(self.universe[19])
		self.no_return_t = type.Type(self.universe[20])

		self.prefs = prefs.Prefs(args)

		self.pkg_deps = utils.PkgDeps()
		self.source_files = []

		self.pointer_size = 8 if self.prefs.target_bits == prefs.Bits.X64 else 4

		self.core_pkg = None
		self.string_class = None # from `core` package
		self.slice_struct = None # from `core` package
		self.error_struct = None # from `core` package

	def run(self):
		self.load_root_pkg()
		if report.ERRORS > 0:
			self.abort()
		self.source_files = self.pkg_deps.resolve()

		#if not self.prefs.check_syntax:
		#	self.resolver.resolve_files(self.source_files)
		#	if report.ERRORS > 0:
		#		self.abort()

		#	self.load_core_syms()

		#	self.checker.check_files(self.source_files)
		#	if report.ERRORS > 0:
		#		self.abort()

		#	if not self.prefs.check:
		#		unique_rir = self.ast2rir.convert(self.source_files)
		#		if report.ERRORS > 0:
		#			self.abort()
		#		if self.prefs.emit_rir:
		#			with open(f"{self.prefs.pkg_name}.rir", "w+") as f:
		#				f.write(str(unique_rir))
		#		else:
		#			self.check_pkg_attrs()
		#			if self.prefs.target_backend == prefs.Backend.C:
		#				self.cgen.gen(unique_rir)
		#				c_file = f"{self.prefs.pkg_name}.ri.c"
		#				self.cgen.write_to_file(c_file)
		#				args = [
		#				    self.prefs.ccompiler, c_file,
		#				    *self.prefs.objects_to_link, "-fno-builtin",
		#				    "-Werror", "-m64" if self.prefs.target_bits
		#				    == prefs.Bits.X64 else "-m32",
		#				    *[f"-l{l}" for l in self.prefs.libraries_to_link],
		#				    *[f"-L{l}" for l in self.prefs.library_path], "-o",
		#				    self.prefs.pkg_output,
		#				]
		#				if self.prefs.build_mode == prefs.BuildMode.Release:
		#					args.append("-flto")
		#					args.append("-O3")
		#				else:
		#					args.append("-g")
		#				self.vlog(f"C compiler options: {args}")
		#				res = utils.execute(*args)
		#				if res.exit_code == 0:
		#					if not self.prefs.keep_c:
		#						os.remove(c_file)
		#				else:
		#					utils.error(
		#					    f"error while compiling the output C file `{c_file}`:\n{res.err}"
		#					)

	def load_root_pkg(self):
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
		parser.Parser(self).parse_pkg(self.prefs.pkg_name, files)

	def load_pkg(self, pkg_name, pos):
		parser.Parser(self).parse_pkg(
		    pkg_name, self.get_pkg_files(pkg_name, pos)
		)

	def get_pkg_files(self, pkg_name, pos):
		files = []
		found = False
		for l in self.prefs.library_path:
			pkg_path = path.join(l, pkg_name)
			if path.exists(pkg_path):
				pkg_path = path.relpath(pkg_path)
				found = True
				if path.isdir(pkg_path):
					files = self.filter_files(
					    glob.glob(path.join(pkg_path, "*.ri"))
					)
					# support `src/` directory
					if path.isdir(path.join(pkg_path, "src")):
						files += self.filter_files(
						    glob.glob(path.join(pkg_path, "src", "*.ri"))
						)
				else:
					report.error(f"`{pkg_name}` is not a directory", pos)
				break
		if not found:
			report.error(f"package `{pkg_name}` not found", pos)
		elif len(files) == 0:
			report.error(f"package `{pkg_name}` contains no rivet files", pos)
		return files

	def filter_files(self, inputs):
		new_inputs = []
		for input in inputs:
			basename_input = path.basename(input)
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

	def load_core_syms(self):
		if core_pkg := self.universe.find("core"):
			self.core_pkg = core_pkg
			if string_class := self.core_pkg.find("string"):
				self.string_class = string_class
			else:
				utils.error("cannot find type `string` in package `core`")

			if slice_struct := self.core_pkg.find("_slice"):
				self.slice_struct = slice_struct
			else:
				utils.error("cannot find type `_slice` in package `core`")

			if error_struct := self.core_pkg.find("_error"):
				self.error_struct = error_struct
			else:
				utils.error("cannot find type `_error` in package `core`")
		else:
			utils.error("package `core` not found")

	def check_pkg_attrs(self):
		pkg_folder = os.path.join(prefs.RIVET_DIR, "objs", self.prefs.pkg_name)
		for attr in self.pkg_attrs.attrs:
			if attr.name == "c_compile":
				if not os.path.exists(pkg_folder):
					os.mkdir(pkg_folder)
				cfile = os.path.realpath(attr.args[0].expr.lit)
				objfile = os.path.join(
				    pkg_folder,
				    f"{os.path.basename(cfile)}.{self.get_postfix()}.o"
				)
				self.prefs.objects_to_link.append(objfile)
				msg = f"c_compile: compiling object for C file `{cfile}`..."
				if os.path.exists(objfile):
					if os.path.getmtime(objfile) < os.path.getmtime(cfile):
						msg = f"c_compile: {objfile} is older than {cfile}, rebuilding..."
					else:
						continue
				self.vlog(msg)
				args = [
				    self.prefs.ccompiler, cfile, "-m64"
				    if self.prefs.target_bits == prefs.Bits.X64 else "-m32",
				    "-O3" if self.prefs.build_mode == prefs.BuildMode.Release
				    else "-g", f'-L{os.path.dirname(cfile)}', "-c", "-o",
				    objfile,
				]
				res = utils.execute(*args)
				if res.exit_code != 0:
					utils.error(
					    f"error while compiling the object file `{objfile}`:\n{res.err}"
					)
			else:
				report.error(
				    f"unknown package attribute `{attr.name}`", attr.pos
				)
		if report.ERRORS > 0:
			self.abort()

	def get_postfix(self):
		postfix = str(self.prefs.target_os).lower()
		postfix += "-"
		postfix += str(self.prefs.target_arch).lower()
		postfix += "-"
		postfix += str(self.prefs.target_bits).lower()
		postfix += "-"
		postfix += str(self.prefs.target_endian).lower()
		postfix += "-"
		postfix += str(self.prefs.target_backend).lower()
		postfix += "-"
		if self.prefs.build_mode == prefs.BuildMode.Debug:
			postfix += "debug"
		else:
			postfix += "release"
		postfix += f"-{self.prefs.ccompiler}"
		return postfix

	# ========================================================

	def is_number(self, typ):
		return self.is_int(typ) or self.is_float(typ)

	def is_int(self, typ):
		return self.is_signed_int(typ) or self.is_unsigned_int(typ)

	def is_signed_int(self, typ):
		return typ in (
		    self.int8_t, self.int16_t, self.int32_t, self.int64_t, self.isize_t,
		    self.untyped_int_t
		)

	def is_unsigned_int(self, typ):
		return typ in (
		    self.u8_t, self.u16_t, self.u32_t, self.u64_t, self.usize_t
		)

	def is_float(self, typ):
		return typ in (self.float32_t, self.float64_t, self.untyped_float_t)

	def untyped_to_type(self, typ):
		if typ == self.untyped_int_t:
			return self.int32_t
		elif typ == self.untyped_float_t:
			return self.float64_t
		return typ

	def num_bits(self, typ):
		if self.is_int(typ):
			return self.int_bits(typ)
		return self.float_bits(typ)

	def int_bits(self, typ):
		typ_sym = typ.get_sym()
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
		typ_sym = typ.get_sym()
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
		return self.type_symbol_size(typ.get_sym())

	def type_symbol_size(self, sy):
		if sy.size != -1:
			return sy.size, sy.align
		size, align = 0, 0
		if sy.kind in (
		    sym.TypeKind.Placeholder, sym.TypeKind.Void, sym.TypeKind.None_,
		    sym.TypeKind.NoReturn, sym.TypeKind.TypeArg
		):
			pass
		elif sy.kind == sym.TypeKind.Alias:
			size, align = self.type_size(sy.info.parent)
		elif sy.kind in (sym.TypeKind.Usize, sym.TypeKind.Isize):
			size, align = self.pointer_size, self.pointer_size
		elif sy.kind in (
		    sym.TypeKind.Int8, sym.TypeKind.Uint8, sym.TypeKind.Bool,
		    sym.TypeKind.ErrType
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
		elif sy.kind == sym.TypeKind.Str:
			size, align = self.type_symbol_size(self.string_class)
		elif sy.kind == sym.TypeKind.Slice:
			size, align = self.type_symbol_size(self.slice_struct)
		elif sy.kind == sym.TypeKind.Trait:
			size, align = self.pointer_size * 2, self.pointer_size
		elif sy.kind == sym.TypeKind.Union:
			for vtyp in sy.info.variants:
				v_size, v_alignment = self.type_size(vtyp)
				if v_size > size:
					size = v_size
					align = v_alignment
			if not sy.info.is_c_union:
				# `tag: i32` field
				size += 4
		elif sy.kind in (sym.TypeKind.Struct, sym.TypeKind.Tuple):
			total_size = 0
			max_alignment = 0
			types = list(
			    map(lambda it: it.typ, sy.fields)
			) if sy.kind == sym.TypeKind.Struct else sy.info.types
			for ftyp in types:
				field_size, alignment = self.type_size(ftyp)
				if alignment > max_alignment:
					max_alignment = alignment
				total_size = self.round_up(total_size, alignment) + field_size
			size = self.round_up(total_size, max_alignment)
			align = max_alignment
		else:
			raise Exception(f"type_size(): unsupported type `{sy.qualname()}`")
		sy.size = size
		sy.align = align
		return size, align

	# Rounds the number `n` up to the next multiple `multiple`.
	# NOTE: `multiple` must be a power of 2.
	def round_up(self, n, multiple):
		return (n + multiple - 1) & (-multiple)

	def evalue_comptime_condition(self, cond):
		if isinstance(cond, ast.BoolLiteral):
			return cond.lit
		elif isinstance(cond, ast.Ident):
			if cond.is_comptime:
				report.error("invalid comptime condition", cond.pos)
			# operating systems
			elif cond.name in ("_LINUX_", "_WINDOWS_"):
				return self.prefs.target_os.equals_to_string(cond.name)
			# architectures
			elif cond.name in ("_AMD64_", "_i386_"):
				return self.prefs.target_arch.equals_to_string(cond.name)
			# bits
			elif cond.name in ("_x32_", "_x64_"):
				if cond.name == "_x32_":
					return self.prefs.target_bits == prefs.Bits.X32
				else:
					return self.prefs.target_bits == prefs.Bits.X64
			# endian
			elif cond.name in ("_LITTLE_ENDIAN_", "_BIG_ENDIAN_"):
				if cond.name == "_LITTLE_ENDIAN_":
					return self.prefs.target_endian == prefs.Endian.Little
				else:
					return self.prefs.target_endian == prefs.Endian.Big
			else:
				if cond.name.startswith("_") and cond.name.endswith("_"):
					report.error(f"unknown builtin flag: `{cond}`", cond.pos)
					return False
				return cond.name in self.prefs.flags
		elif isinstance(cond, ast.UnaryExpr):
			if cond.op == token.Kind.Bang:
				return not self.evalue_comptime_condition(cond.right)
			else:
				report.error(f"expected `!`, found token `{cond.op}`", cond.pos)
		elif isinstance(cond, ast.BinaryExpr):
			if cond.op in (token.Kind.KeyAnd, token.Kind.KeyOr):
				if cond.op == token.Kind.KeyAnd:
					return self.evalue_comptime_condition(
					    cond.left
					) and self.evalue_comptime_condition(cond.right)
				else:
					return self.evalue_comptime_condition(
					    cond.left
					) or self.evalue_comptime_condition(cond.right)
			else:
				report.error("invalid comptime condition", cond.pos)
		elif isinstance(cond, ast.ParExpr):
			return self.evalue_comptime_condition(cond.expr)
		else:
			report.error("invalid comptime condition", cond.pos)
		return False

	# ========================================================

	def vlog(self, msg):
		if self.prefs.is_verbose:
			utils.eprint(">>", msg)

	def abort(self):
		if report.ERRORS == 1:
			msg = f"could not compile package `{self.prefs.pkg_name}`, aborting due to previous error"
		else:
			msg = f"could not compile package `{self.prefs.pkg_name}`, aborting due to {report.ERRORS} previous errors"
		if report.WARNS > 0:
			word = "warning" if report.WARNS == 1 else "warnings"
			msg += f"; {report.WARNS} {word} emitted"
		utils.error(msg)
		exit(1)
