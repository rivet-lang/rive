# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .sym import TypeKind
from . import ast, sym, type, report, utils

class Register:
	def __init__(self, comp):
		self.comp = comp
		self.source_file = None
		self.abi = sym.ABI.Rivet
		self.sym = None

	def walk_files(self, source_files):
		for i, sf in enumerate(source_files):
			if self.comp.core_pkg == None and sf.sym.is_core_pkg():
				self.comp.core_pkg = sf.sym
			self.sym = sf.sym
			self.source_file = sf
			self.walk_decls(self.source_file.decls)

	def walk_decls(self, decls):
		for decl in decls:
			old_abi = self.abi
			old_sym = self.sym
			if isinstance(decl, ast.ExternDecl):
				self.walk_decls(decl.decls)
			elif isinstance(decl, ast.ModDecl):
				decl.sym = self.sym.add_or_get_mod(sym.Mod(decl.vis, decl.name))
				self.sym = decl.sym
				self.walk_decls(decl.decls)
			elif isinstance(decl, ast.ConstDecl):
				self.add_sym(
				    sym.Const(decl.vis, decl.name, decl.typ, decl.expr),
				    decl.pos
				)
			elif isinstance(decl, ast.LetDecl):
				for v in decl.lefts:
					try:
						self.source_file.sym.add(
						    sym.Var(
						        decl.vis, v.is_mut, decl.is_extern, v.name,
						        v.typ
						    )
						)
					except utils.CompilerError as e:
						report.error(e.args[0], v.pos)
			elif isinstance(decl, ast.TypeDecl):
				self.add_sym(
				    sym.Type(
				        decl.vis, decl.name, TypeKind.Alias,
				        info = sym.AliasInfo(decl.parent)
				    ), decl.pos
				)
			elif isinstance(decl, ast.TraitDecl):
				try:
					decl.sym = self.sym.add_and_return(
					    sym.Type(
					        decl.vis, decl.name, TypeKind.Trait,
					        info = sym.TraitInfo()
					    )
					)
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.ClassDecl):
				try:
					is_core_pkg = self.source_file.sym.is_core_pkg()
					if is_core_pkg and decl.name == "string":
						decl.sym = self.comp.string_t.sym
					elif is_core_pkg and decl.name == "Error":
						decl.sym = self.comp.error_t.sym
					else:
						decl.sym = self.sym.add_and_return(
						    sym.Type(decl.vis, decl.name, TypeKind.Class)
						)
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.StructDecl):
				try:
					is_core_pkg = self.source_file.sym.is_core_pkg()
					if is_core_pkg and decl.name == "None":
						decl.sym = self.comp.none_t.sym
					elif is_core_pkg and decl.name == "bool":
						decl.sym = self.comp.bool_t.sym
					elif is_core_pkg and decl.name == "rune":
						decl.sym = self.comp.rune_t.sym
					elif is_core_pkg and decl.name == "untyped_int":
						decl.sym = self.comp.untyped_int_t.sym
					elif is_core_pkg and decl.name == "untyped_float":
						decl.sym = self.comp.untyped_float_t.sym
					elif is_core_pkg and decl.name == "i8":
						decl.sym = self.comp.i8_t.sym
					elif is_core_pkg and decl.name == "i16":
						decl.sym = self.comp.i16_t.sym
					elif is_core_pkg and decl.name == "i32":
						decl.sym = self.comp.i32_t.sym
					elif is_core_pkg and decl.name == "i64":
						decl.sym = self.comp.i64_t.sym
					elif is_core_pkg and decl.name == "isize":
						decl.sym = self.comp.isize_t.sym
					elif is_core_pkg and decl.name == "u8":
						decl.sym = self.comp.u8_t.sym
					elif is_core_pkg and decl.name == "u16":
						decl.sym = self.comp.u16_t.sym
					elif is_core_pkg and decl.name == "u32":
						decl.sym = self.comp.u32_t.sym
					elif is_core_pkg and decl.name == "u64":
						decl.sym = self.comp.u64_t.sym
					elif is_core_pkg and decl.name == "usize":
						decl.sym = self.comp.usize_t.sym
					else:
						decl.sym = self.sym.add_and_return(
						    sym.Type(decl.vis, decl.name, TypeKind.Struct)
						)
						if is_core_pkg and decl.name == "Slice":
							self.comp.slice_sym = decl.sym
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.EnumDecl):
				try:
					info = sym.EnumInfo(decl.underlying_typ)
					for i, v in enumerate(decl.variants):
						if info.has_variant(v):
							report.error(
							    f"enum `{decl.name}` has duplicate variant `{v}`",
							    decl.pos
							)
							continue
						info.add_variant(v, i)
					decl.sym = self.sym.add_and_return(
					    sym.Type(
					        decl.vis, decl.name, TypeKind.Enum, info = info
					    )
					)
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.FieldDecl):
				if self.sym.has_field(decl.name):
					report.error(
					    f"{self.sym.typeof()} `{self.sym.name}` has duplicate field `{decl.name}`",
					    decl.pos
					)
				else:
					self.sym.fields.append(
					    sym.Field(
					        decl.name, decl.is_mut, decl.vis, decl.typ,
					        decl.has_def_expr, decl.def_expr
					    )
					)
			elif isinstance(decl, ast.ExtendDecl):
				if isinstance(decl.typ, type.Type):
					if decl.typ.sym != None:
						self.sym = decl.typ.sym
					elif isinstance(decl.typ.expr, ast.Ident):
						if typ_sym := self.sym.find(decl.typ.expr.name):
							self.sym = typ_sym
						else:
							self.sym = self.sym.add_and_return(
							    sym.Type(
							        sym.Vis.Priv, decl.typ.expr.name,
							        TypeKind.Placeholder
							    )
							)
					else:
						report.error(
						    f"invalid type `{decl.typ}` to extend", decl.pos
						)
						continue
					self.walk_decls(decl.decls)
				else:
					report.error(
					    f"invalid type `{decl.typ}` to extend", decl.pos
					)
			elif isinstance(decl, ast.FnDecl):
				try:
					decl.sym = self.sym.add_and_return(
					    sym.Fn(
					        self.abi, decl.vis, decl.is_extern, decl.is_unsafe,
					        decl.is_method, decl.is_variadic, decl.name,
					        decl.args, decl.ret_typ, decl.has_named_args,
					        decl.has_body, decl.name_pos, decl.self_is_mut
					    )
					)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.name_pos)
			elif isinstance(decl, ast.DestructorDecl):
				self.add_sym(
				    sym.Fn(
				        self.abi, sym.Vis.Priv, False, True, True, False,
				        "_dtor", [
				            sym.Arg(
				                "self", decl.self_is_mut, type.Type(self.sym),
				                None, False, decl.pos
				            )
				        ], self.comp.void_t, False, True, decl.pos,
				        decl.self_is_mut
				    ), decl.pos
				)
			self.abi = old_abi
			self.sym = old_sym

	def add_sym(self, sy, pos):
		try:
			self.sym.add(sy)
		except utils.CompilerError as e:
			report.error(e.args[0], pos)
