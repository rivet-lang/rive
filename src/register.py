# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os, glob

from .ast import sym, type
from . import ast, parser, report, utils

class Register:
	def __init__(self, comp):
		self.comp = comp
		self.sf = None
		self.cur_sym = None
		self.errtype_nr = 1
		self.cur_fn_scope = None

	def visit_source_files(self, source_files):
		self.comp.pkg_sym = self.add_pkg(self.comp.prefs.pkg_name)
		self.cur_sym = self.comp.pkg_sym
		for sf in source_files:
			self.visit_source_file(sf)

	def add_pkg(self, name):
		idx = len(self.comp.universe.syms)
		self.comp.universe.add(sym.Pkg(ast.Visibility.Public, name))
		return self.comp.universe.syms[idx]

	def add_mod(self, vis, name):
		return self.cur_sym.add_or_extend_mod(sym.Mod(vis, name))

	def add_sym(self, sym, pos):
		try:
			self.cur_sym.add(sym)
		except utils.CompilerError as e:
			report.error(e.args[0], pos)

	def visit_source_file(self, sf):
		self.sf = sf
		old_cur_sym = self.cur_sym
		if sf.mod_sym:
			self.cur_sym = sf.mod_sym
		self.visit_decls(sf.decls)
		if sf.mod_sym:
			self.cur_sym = old_cur_sym

	def visit_decls(self, decls):
		for decl in decls:
			should_register = True
			if not decl.__class__ in (ast.TestDecl, ast.ExternPkg):
				if if_attr := decl.attrs.find("if"):
					should_register = self.comp.evalue_comptime_condition(
					    if_attr.args[0].expr
					)
					decl.attrs.if_check = should_register
			if isinstance(decl, ast.ExternPkg):
				continue # TODO(StunxFS): load external packages
			elif isinstance(decl, ast.ExternDecl):
				if should_register:
					self.visit_decls(decl.protos)
			elif isinstance(decl, ast.ModDecl):
				if should_register:
					old_sym = self.cur_sym
					self.cur_sym = self.add_mod(decl.vis, decl.name)
					decl.sym = self.cur_sym
					if decl.is_unloaded:
						path = os.path.join(
						    os.path.dirname(self.sf.file), decl.name
						)
						if os.path.exists(path):
							if os.path.isdir(path):
								files = self.comp.prefs.filter_files_list(
								    glob.glob(os.path.join(path, "*.ri"))
								)
								if len(files) == 0:
									report.error(
									    f"cannot import module `{decl.name}` (.ri files not found)",
									    decl.pos
									)
								else:
									self.comp.mod_sym = self.cur_sym
									parser.Parser(
									    self.comp
									).parse_extern_module_files(files)
									if report.ERRORS > 0:
										self.comp.abort()
									self.comp.mod_sym = None
							else:
								report.error(
								    f"cannot import module `{decl.name}` (is a file)",
								    decl.pos
								)
						else:
							report.error(
							    f"cannot import module `{decl.name}` (not found)",
							    decl.pos
							)
					else:
						self.visit_decls(decl.decls)
					self.cur_sym = old_sym
			elif isinstance(decl, ast.ConstDecl):
				if should_register:
					const_sym = sym.Const(
					    decl.vis, decl.name, decl.typ, decl.expr
					)
					self.add_sym(const_sym, decl.pos)
					self.visit_expr(decl.expr)
					decl.sym = const_sym
			elif isinstance(decl, ast.StaticDecl):
				if should_register:
					static_sym = sym.Static(
					    decl.vis, decl.is_mut, decl.is_extern, decl.name,
					    decl.typ
					)
					self.add_sym(static_sym, decl.pos)
					self.visit_expr(decl.expr)
					decl.sym = static_sym
			elif isinstance(decl, ast.TypeDecl):
				if should_register:
					self.add_sym(
					    sym.Type(
					        decl.vis, decl.name, sym.TypeKind.Alias,
					        info = sym.AliasInfo(decl.parent)
					    ), decl.pos
					)
			elif isinstance(decl, ast.ErrTypeDecl):
				if should_register:
					self.add_sym(
					    sym.Type(
					        decl.vis, decl.name, sym.TypeKind.ErrType,
					        info = sym.ErrTypeInfo(self.errtype_nr)
					    ), decl.pos
					)
					self.errtype_nr += 1
			elif isinstance(decl, ast.TraitDecl):
				ts = sym.Type(
				    decl.vis, decl.name, sym.TypeKind.Trait,
				    info = sym.TraitInfo()
				)
				old_cur_sym = self.cur_sym
				self.cur_sym = ts
				for d in decl.decls:
					self.visit_fn_decl(d)
				self.cur_sym = old_cur_sym
				self.add_sym(ts, decl.pos)
			elif isinstance(decl, ast.UnionDecl):
				if should_register:
					variants = []
					for v in decl.variants:
						if v in variants:
							report.error(
							    f"union `{decl.name}` has duplicate variant type `{v}`",
							    decl.pos
							)
						else:
							variants.append(v)
					decl.sym = sym.Type(
					    decl.vis, decl.name, sym.TypeKind.Union,
					    info = sym.UnionInfo(
					        variants, decl.attrs.has("c_union")
					    )
					)
					old_cur_sym = self.cur_sym
					self.cur_sym = decl.sym
					for d in decl.decls:
						if isinstance(d, ast.FnDecl):
							self.visit_fn_decl(d)
						else:
							report.error(
							    "expected associated function or method", d.pos
							)
					self.cur_sym = old_cur_sym
					self.add_sym(decl.sym, decl.pos)
			elif isinstance(decl, ast.StructDecl):
				if should_register:
					decl.sym = sym.Type(
					    decl.vis, decl.name, sym.TypeKind.Struct, list(),
					    sym.StructInfo(decl.is_opaque)
					)
					old_cur_sym = self.cur_sym
					self.cur_sym = decl.sym
					for d in decl.decls:
						if isinstance(d, ast.StructField):
							should_register_field = True
							if if_attr := d.attrs.find("if"):
								should_register_field = self.comp.evalue_comptime_condition(
								    if_attr.args[0].expr
								)
							if should_register_field:
								if decl.sym.has_field(d.name):
									report.error(
									    f"field `{d.name}` is already declared",
									    d.pos
									)
								else:
									decl.sym.fields.append(
									    sym.Field(
									        d.name, d.is_mut, d.vis, d.typ
									    )
									)
						elif isinstance(d, ast.FnDecl):
							self.visit_fn_decl(d)
						elif isinstance(d, ast.DestructorDecl):
							self_typ = type.Ref(type.Type(decl.sym))
							d.self_typ = self_typ
							d.scope.add(
							    sym.Object(True, "self", self_typ, True)
							)
							sym_fn = sym.Fn(
							    sym.ABI.Rivet, ast.Visibility.Public, False,
							    False, True, False, "0_dtor", [],
							    self.comp.void_t, False, True, d.pos, True, True
							)
							sym_fn.rec_typ = self_typ
							self.add_sym(sym_fn, decl.pos)
							for stmt in d.stmts:
								self.visit_stmt(stmt)
						else:
							report.error(
							    "expected associated function or method", d.pos
							)
					self.cur_sym = old_cur_sym
					self.add_sym(decl.sym, decl.pos)
			elif isinstance(decl, ast.EnumDecl):
				variants = []
				for v in decl.variants:
					if v in variants:
						report.error(
						    f"enum `{decl.name}` has duplicate variant `{v}`",
						    decl.pos
						)
					else:
						variants.append(v)
				decl.sym = sym.Type(
				    decl.vis, decl.name, sym.TypeKind.Enum, info = sym.EnumInfo(
				        decl.underlying_typ,
				        [sym.EnumVariant(v) for v in variants]
				    )
				)
				old_cur_sym = self.cur_sym
				self.cur_sym = decl.sym
				for d in decl.decls:
					if isinstance(d, ast.FnDecl):
						self.visit_fn_decl(d)
					else:
						report.error(
						    "expected associated function or method", d.pos
						)
				self.cur_sym = old_cur_sym
				self.add_sym(decl.sym, decl.pos)
			elif isinstance(decl, ast.ExtendDecl):
				if should_register:
					old_sym = self.cur_sym
					if isinstance(decl.typ, type.Type):
						if decl.typ._unresolved:
							if isinstance(decl.typ.expr, ast.Ident):
								if s := self.cur_sym.find(decl.typ.expr.name):
									if s.kind == sym.TypeKind.Alias and (
									    isinstance(s.info.parent, type.Type)
									    and s.info.parent.is_resolved()
									):
										self.cur_sym = s.info.parent.sym
									else:
										self.cur_sym = s
								else:
									# placeholder
									self.cur_sym = sym.Type(
									    ast.Visibility.Private,
									    decl.typ.expr.name,
									    sym.TypeKind.Placeholder
									)
									old_sym.add(self.cur_sym)
								for d in decl.decls:
									self.visit_fn_decl(d)
							else:
								report.error(
								    "cannot extend non-local types",
								    decl.typ.expr.pos
								)
						else:
							self.cur_sym = decl.typ.sym
							for d in decl.decls:
								self.visit_fn_decl(d)
					self.cur_sym = old_sym
			elif isinstance(decl, ast.TestDecl):
				self.cur_fn_scope = decl.scope
				for stmt in decl.stmts:
					self.visit_stmt(stmt)
				self.cur_fn_scope = None
			elif isinstance(decl, ast.FnDecl):
				self.cur_fn_scope = decl.scope
				if should_register:
					self.visit_fn_decl(decl, decl.abi)
				self.cur_fn_scope = None

	def visit_fn_decl(self, decl, abi = sym.ABI.Rivet):
		decl.sym = sym.Fn(
		    abi, decl.vis, decl.is_extern, decl.is_unsafe, decl.is_method,
		    decl.is_variadic, decl.name, decl.args, decl.ret_typ,
		    decl.has_named_args, decl.has_body, decl.name_pos, decl.self_is_mut,
		    decl.self_is_ref
		)
		decl.sym.is_main = decl.is_main
		if decl.is_method:
			self_typ = type.Type(self.cur_sym)
			if decl.self_is_ref:
				self_typ = type.Ref(self_typ, decl.self_is_mut)
			decl.self_typ = self_typ
			decl.sym.self_typ = self_typ
			decl.scope.add(sym.Object(False, "self", self_typ, True))
		self.add_sym(decl.sym, decl.name_pos)
		for arg in decl.args:
			try:
				decl.scope.add(sym.Object(False, arg.name, arg.typ, True))
			except utils.CompilerError as e:
				report.error(e.args[0], arg.pos)
			if arg.has_def_expr:
				self.visit_expr(arg.def_expr)
		for stmt in decl.stmts:
			self.visit_stmt(stmt)

	def visit_stmt(self, stmt):
		if isinstance(stmt, ast.LetStmt):
			self.register_variables(stmt.scope, stmt.lefts)
			self.visit_expr(stmt.right)
		elif isinstance(stmt, ast.AssignStmt):
			self.visit_expr(stmt.right)
		elif isinstance(stmt, ast.LabelStmt):
			if self.cur_fn_scope != None:
				try:
					self.cur_fn_scope.add(sym.Label(stmt.label))
				except utils.CompilerError as e:
					report.error(e.args[0], stmt.pos)
		elif isinstance(stmt, ast.ExprStmt):
			self.visit_expr(stmt.expr)
		elif isinstance(stmt, ast.WhileStmt):
			self.visit_expr(stmt.cond)
			self.visit_stmt(stmt.stmt)
		elif isinstance(stmt, ast.ForInStmt):
			for v in stmt.vars:
				try:
					stmt.scope.add(
					    sym.Object(False, v, self.comp.void_t, False)
					)
				except utils.CompilerError as e:
					report.error(e.args[0], stmt.pos)
			self.visit_stmt(stmt.stmt)

	def visit_expr(self, expr):
		if isinstance(expr, ast.BuiltinCallExpr):
			for a in expr.args:
				self.visit_expr(a)
		elif isinstance(expr, ast.CallExpr):
			self.visit_expr(expr.left)
			for a in expr.args:
				self.visit_expr(a.expr)
			if expr.has_err_handler():
				if expr.err_handler.has_varname():
					# register error value
					try:
						expr.err_handler.scope.add(
						    sym.Object(
						        False, expr.err_handler.varname,
						        self.comp.error_t, False
						    )
						)
					except utils.CompilerError as e:
						report.error(e.args[0], expr.err_handler.varname_pos)
				self.visit_expr(expr.err_handler.expr)
		elif isinstance(expr, ast.IfExpr):
			if expr.is_comptime:
				# evalue comptime if expression
				branch_idx = -1
				for idx, b in enumerate(expr.branches):
					cond_val = False
					if not b.is_else:
						cond_val = self.comp.evalue_comptime_condition(b.cond)
					if branch_idx == -1:
						if cond_val or b.is_else:
							branch_idx = idx
							if isinstance(b.expr, ast.Block):
								for stmt in b.expr.stmts:
									self.visit_stmt(stmt)
							break
				expr.branch_idx = branch_idx
			else:
				for b in expr.branches:
					if not b.is_else: self.visit_expr(b.cond)
					self.visit_expr(b.expr)
		elif isinstance(expr, ast.MatchExpr):
			self.visit_expr(expr.expr)
			for b in expr.branches:
				for p in b.pats:
					self.visit_expr(p)
				self.visit_expr(b.expr)
		elif isinstance(expr, ast.GuardExpr):
			for v in expr.vars:
				try:
					expr.scope.add(
					    sym.Object(False, v, self.comp.void_t, False)
					)
				except utils.CompilerError as e:
					report.error(e.args[0], expr.pos)
			self.visit_expr(expr.expr)
			if expr.has_cond:
				self.visit_expr(expr.cond)
		elif isinstance(expr, ast.TupleLiteral):
			for e in expr.exprs:
				self.visit_expr(e)
		elif isinstance(expr, ast.ArrayLiteral):
			for e in expr.elems:
				self.visit_expr(e)
		elif isinstance(expr, ast.StructLiteral):
			for f in expr.fields:
				self.visit_expr(f.expr)
		elif isinstance(expr, ast.UnaryExpr):
			self.visit_expr(expr.right)
		elif isinstance(expr, ast.BinaryExpr):
			self.visit_expr(expr.left)
			self.visit_expr(expr.right)
		elif isinstance(expr, ast.PostfixExpr):
			self.visit_expr(expr.left)
		elif isinstance(expr, ast.CastExpr):
			self.visit_expr(expr.expr)
		elif isinstance(expr, ast.IndexExpr):
			self.visit_expr(expr.left)
			self.visit_expr(expr.index)
		elif isinstance(expr, ast.RangeExpr):
			if expr.has_start: self.visit_expr(expr.start)
			if expr.has_end: self.visit_expr(expr.end)
		elif isinstance(expr, ast.SelectorExpr):
			self.visit_expr(expr.left)
		elif isinstance(expr, ast.PathExpr):
			self.visit_expr(expr.left)
		elif isinstance(expr, ast.ReturnExpr):
			self.visit_expr(expr.expr)
		elif isinstance(expr, ast.RaiseExpr):
			self.visit_expr(expr.expr)
		elif isinstance(expr, ast.Block):
			self.cur_fn_scope = expr.scope
			for stmt in expr.stmts:
				self.visit_stmt(stmt)
			if expr.is_expr: self.visit_expr(expr.expr)
			self.cur_fn_scope = None
		elif isinstance(expr, ast.ParExpr):
			self.visit_expr(expr.expr)

	def register_variables(self, scope, var_decls):
		for vd in var_decls:
			try:
				scope.add(sym.Object(vd.is_mut, vd.name, vd.typ, False))
			except utils.CompilerError as e:
				report.error(e.args[0], vd.pos)
