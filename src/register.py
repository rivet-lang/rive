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
		for sf in source_files:
			old_sym = self.sym
			if sf.sym:
				self.sym = sf.sym
			self.source_file = sf
			self.walk_decls(self.source_file.decls)
			self.sym = old_sym

	def walk_decls(self, decls):
		for decl in decls:
			old_abi = self.abi
			old_sym = self.sym
			if isinstance(decl, ast.ExternDecl):
				self.walk_decls(decl.protos)
			elif isinstance(decl, ast.ModDecl):
				decl.sym = self.sym.add_or_get_mod(sym.Mod(decl.vis, decl.name))
				self.sym = decl.sym
				self.walk_decls(decl.decls)
			elif isinstance(decl, ast.ConstDecl):
				self.add_sym(
				    sym.Const(decl.vis, decl.name, decl.typ, decl.expr),
				    decl.pos
				)
			elif isinstance(decl, ast.TypeDecl):
				self.add_sym(
				    sym.Type(decl.vis, decl.name, TypeKind.Alias), decl.pos
				)
			elif isinstance(decl, ast.ErrTypeDecl):
				self.add_sym(
				    sym.Type(decl.vis, decl.name, TypeKind.ErrType), decl.pos
				)
			elif isinstance(decl, ast.TraitDecl):
				try:
					decl.sym = self.sym.add_and_return(
					    sym.Type(decl.vis, decl.name, TypeKind.Trait)
					)
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.UnionDecl):
				try:
					decl.sym = self.sym.add_and_return(
					    sym.Type(decl.vis, decl.name, TypeKind.Union)
					)
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.ClassDecl):
				try:
					decl.sym = self.sym.add_and_return(
					    sym.Type(decl.vis, decl.name, TypeKind.Class)
					)
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.StructDecl):
				try:
					decl.sym = self.sym.add_and_return(
					    sym.Type(decl.vis, decl.name, TypeKind.Struct)
					)
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.EnumDecl):
				try:
					decl.sym = self.sym.add_and_return(
					    sym.Type(decl.vis, decl.name, TypeKind.Enum)
					)
					self.sym = decl.sym
					self.walk_decls(decl.decls)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.FieldDecl):
				if self.sym.has_field(decl.name):
					report.error(
					    f"{self.sym.kind} `{self.sym.name}` has duplicate field `{decl.name}`",
					    decl.pos
					)
				else:
					self.sym.fields.append(
					    sym.Field(
					        decl.name, decl.is_mut, decl.vis, decl.typ,
					        decl.has_def_expr, decl.def_expr
					    )
					)
			# TODO:
			#elif isinstance(decl, ast.ExtendDecl):
			#	self.walk_decls(decl.decls)
			elif isinstance(decl, ast.FnDecl):
				args = []
				for arg in decl.args:
					args.append(
					    sym.Arg(
					        arg.name, arg.typ, arg.def_expr, arg.has_def_expr,
					        arg.pos
					    )
					)
				try:
					self.sym.add_and_return(
					    sym.Fn(
					        self.abi, decl.vis, decl.is_extern, decl.is_unsafe,
					        decl.is_method, decl.is_variadic, decl.name, args,
					        decl.ret_typ, decl.has_named_args, decl.has_body,
					        decl.name_pos, decl.self_is_mut
					    )
					)
				except utils.CompilerError as e:
					report.error(e.args[0], decl.pos)
			elif isinstance(decl, ast.DestructorDecl):
				self.add_sym(
				    sym.Fn(
				        self.abi, sym.Vis.Priv, False, True, True, False,
				        f"_dtor_for_id_{self.sym.id}_", [
				            sym.Arg(
				                "self", type.Type(self.sym), None, False,
				                decl.pos
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
