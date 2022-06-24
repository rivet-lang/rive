# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .token import Kind
from .ast import sym, type
from .ast.sym import Visibility
from . import ast, report, register, utils

class Resolver:
	def __init__(self, comp):
		self.comp = comp
		self.sf = None
		self.cur_sym = None

		self.inside_is_comparation = False

		self.self_sym = None

	def resolve_selective_import_symbols(self, symbols, path_sym):
		for isym in symbols:
			if isym.is_self:
				self.sf.imported_symbols[decl.alias] = path_sym
			else:
				if isym_ := path_sym.find(isym.name):
					self.check_visibility(isym_, isym.pos)
					self.sf.imported_symbols[isym.alias] = isym_
				else:
					report.error(
					    f"could not find `{isym.name}` in {path_sym.sym_kind()} `{path_sym.name}`",
					    isym.pos
					)

	def resolve_files(self, source_files):
		register.Register(self.comp).visit_source_files(source_files)
		if report.ERRORS > 0:
			return

		self.cur_sym = self.comp.pkg_sym
		for sf in source_files:
			self.resolve_file(sf)

	def resolve_file(self, sf):
		self.sf = sf
		self.resolve_decls(sf.decls)

	def resolve_decls(self, decls):
		for decl in decls:
			self.resolve_decl(decl)

	def resolve_decl(self, decl):
		should_check = True
		if not decl.__class__ in (
		    ast.TestDecl, ast.ExternPkg, ast.DestructorDecl
		):
			should_check = decl.attrs.if_check
		if isinstance(decl, ast.ImportDecl):
			if should_check:
				if isinstance(decl.path, (ast.Ident, ast.PkgExpr)):
					name = decl.path.name if isinstance(
					    decl.path, ast.Ident
					) else self.comp.prefs.pkg_name
					if len(decl.symbols) == 0:
						if isinstance(decl.path, ast.PkgExpr):
							report.error(
							    "invalid `import` declaration", decl.path.pos
							)
						elif _ := self.comp.pkg_sym.find(name):
							report.error(
							    f"use of undeclared external package `{name}`",
							    decl.path.pos
							)
							report.help(f"use `pkg::{name}` instead")
						else:
							report.error(
							    "expected symbol list after name", decl.path.pos
							)
					elif sym_info := self.comp.universe.find(name):
						self.resolve_selective_import_symbols(
						    decl.symbols, sym_info
						)
					else:
						report.error(
						    f"use of undeclared external package `{name}`",
						    decl.path.pos
						)
				elif isinstance(decl.path, ast.PathExpr):
					self.resolve_expr(decl.path)
					if not decl.path.has_error:
						if len(decl.symbols) == 0:
							self.sf.imported_symbols[decl.alias
							                         ] = decl.path.field_info
						else:
							self.resolve_selective_import_symbols(
							    decl.symbols, decl.path.field_info
							)
		if isinstance(decl, ast.ExternDecl):
			if should_check:
				self.resolve_decls(decl.protos)
		elif isinstance(decl, ast.ConstDecl):
			if should_check:
				self.resolve_type(decl.typ)
				self.resolve_expr(decl.expr)
		elif isinstance(decl, ast.StaticDecl):
			if should_check:
				self.resolve_type(decl.typ)
				self.resolve_expr(decl.expr)
		elif isinstance(decl, ast.ModDecl):
			if should_check:
				old_sym = self.cur_sym
				self.cur_sym = decl.sym
				self.resolve_decls(decl.decls)
				self.cur_sym = old_sym
		elif isinstance(decl, ast.TypeDecl):
			if should_check:
				self.resolve_type(decl.parent)
		elif isinstance(decl, ast.TraitDecl):
			if should_check:
				self.resolve_decls(decl.decls)
		elif isinstance(decl, ast.UnionDecl):
			if should_check:
				self.self_sym = decl.sym
				for v in decl.variants:
					self.resolve_type(v)
				self.resolve_decls(decl.decls)
				self.self_sym = None
		elif isinstance(decl, ast.EnumDecl):
			if should_check:
				self.self_sym = decl.sym
				self.resolve_decls(decl.decls)
				self.self_sym = None
		elif isinstance(decl, ast.StructDecl):
			if should_check:
				self.self_sym = decl.sym
				self.resolve_decls(decl.decls)
				self.self_sym = None
		elif isinstance(decl, ast.StructField):
			if should_check:
				self.resolve_type(decl.typ)
				if decl.has_def_expr:
					self.resolve_expr(decl.def_expr)
		elif isinstance(decl, ast.ExtendDecl):
			if should_check:
				if self.resolve_type(decl.typ):
					if decl.is_for_trait:
						self.resolve_type(decl.for_trait)
					self.self_sym = decl.typ.get_sym()
					if isinstance(
					    decl.typ, (type.Array, type.Slice, type.Tuple)
					):
						# TODO(StunxFS): better error messages
						s = decl.typ.get_sym()
						for d in decl.decls:
							if isinstance(d, ast.FnDecl):
								if d.is_method:
									self_typ = type.Type(self.self_sym)
									if d.self_is_ref:
										self_typ = type.Ref(
										    self_typ, d.self_is_mut
										)
									d.self_typ = self_typ
									if not d.scope.exists("self"):
										d.scope.add(
										    sym.Object(
										        False, "self", self_typ, True
										    )
										)
									try:
										d.sym = sym.Fn(
										    sym.ABI.Rivet, d.vis, d.is_extern,
										    d.is_unsafe, d.is_method, False,
										    d.name, d.args, d.ret_typ,
										    d.has_named_args, d.has_body,
										    d.name_pos, d.self_is_mut,
										    d.self_is_ref
										)
										d.sym.self_typ = self_typ
										s.add(d.sym)
									except utils.CompilerError as e:
										report.error(e.args[0], d.name_pos)
								else:
									report.error("expected method", d.name_pos)
							else:
								report.error("expected method", d.pos)
					self.resolve_decls(decl.decls)
					self.self_sym = None
		elif isinstance(decl, ast.TestDecl):
			self.resolve_stmts(decl.stmts)
		elif isinstance(decl, ast.FnDecl):
			if should_check:
				for arg in decl.args:
					self.resolve_type(arg.typ)
					if arg.has_def_expr: self.resolve_expr(arg.def_expr)
				self.resolve_type(decl.ret_typ)
				self.resolve_stmts(decl.stmts)
		elif isinstance(decl, ast.DestructorDecl):
			self.resolve_stmts(decl.stmts)

	def resolve_stmts(self, stmts):
		for stmt in stmts:
			self.resolve_stmt(stmt)

	def resolve_stmt(self, stmt):
		if isinstance(stmt, ast.LetStmt):
			for l in stmt.lefts:
				self.resolve_type(l.typ)
			self.resolve_expr(stmt.right)
		elif isinstance(stmt, ast.AssignStmt):
			self.resolve_expr(stmt.left)
			self.resolve_expr(stmt.right)
		elif isinstance(stmt, ast.ExprStmt):
			self.resolve_expr(stmt.expr)
		elif isinstance(stmt, ast.WhileStmt):
			self.resolve_expr(stmt.cond)
			self.resolve_stmt(stmt.stmt)
		elif isinstance(stmt, ast.ForInStmt):
			self.resolve_expr(stmt.iterable)
			self.resolve_stmt(stmt.stmt)

	def resolve_expr(self, expr):
		if isinstance(expr, ast.ParExpr):
			self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.Ident):
			self.resolve_ident(expr)
		elif isinstance(expr, ast.SelfExpr):
			if self_ := expr.scope.lookup("self"):
				expr.typ = self_.typ
			else:
				report.error("cannot find `self` in this scope", expr.pos)
		elif isinstance(expr, ast.SelfTyExpr):
			if self.self_sym != None:
				expr.typ = type.Type(self.self_sym)
			else:
				report.error("cannot resolve type for `Self`", expr.pos)
		elif isinstance(expr, ast.TypeNode):
			self.resolve_type(expr.typ)
		elif isinstance(expr, ast.TupleLiteral):
			for e in expr.exprs:
				self.resolve_expr(e)
		elif isinstance(expr, ast.ArrayLiteral):
			for e in expr.elems:
				self.resolve_expr(e)
		elif isinstance(expr, ast.StructLiteral):
			self.resolve_expr(expr.expr)
			for f in expr.fields:
				self.resolve_expr(f.expr)
		elif isinstance(expr, ast.UnaryExpr):
			self.resolve_expr(expr.right)
		elif isinstance(expr, ast.BinaryExpr):
			self.inside_is_comparation = expr.op in (Kind.KeyNotIs, Kind.KeyIs)
			self.resolve_expr(expr.left)
			self.resolve_expr(expr.right)
		elif isinstance(expr, ast.PostfixExpr):
			self.resolve_expr(expr.left)
		elif isinstance(expr, ast.CastExpr):
			self.resolve_expr(expr.expr)
			self.resolve_type(expr.typ)
		elif isinstance(expr, ast.IndexExpr):
			self.resolve_expr(expr.left)
			self.resolve_expr(expr.index)
		elif isinstance(expr, ast.RangeExpr):
			if expr.has_start: self.resolve_expr(expr.start)
			if expr.has_end: self.resolve_expr(expr.end)
		elif isinstance(expr, ast.SelectorExpr):
			self.resolve_expr(expr.left)
		elif isinstance(expr, ast.PathExpr):
			self.resolve_path_expr(expr)
		elif isinstance(expr, ast.BuiltinCallExpr):
			for a in expr.args:
				self.resolve_expr(a)
		elif isinstance(expr, ast.CallExpr):
			self.resolve_expr(expr.left)
			for a in expr.args:
				self.resolve_expr(a.expr)
			if expr.has_err_handler():
				self.resolve_expr(expr.err_handler.expr)
		elif isinstance(expr, ast.ReturnExpr):
			self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.RaiseExpr):
			self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.Block):
			for stmt in expr.stmts:
				self.resolve_stmt(stmt)
			if expr.is_expr: self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.IfExpr):
			if expr.is_comptime:
				if expr.branch_idx > -1:
					self.resolve_expr(expr.branches[expr.branch_idx].expr)
			else:
				for b in expr.branches:
					if not b.is_else: self.resolve_expr(b.cond)
					self.resolve_expr(b.expr)
		elif isinstance(expr, ast.MatchExpr):
			self.resolve_expr(expr.expr)
			for b in expr.branches:
				for p in b.pats:
					self.resolve_expr(p)
				self.resolve_expr(b.expr)

	def find_symbol(self, symbol, name, pos):
		if s := symbol.find(name):
			self.check_visibility(s, pos)
			return s
		elif isinstance(symbol, sym.Type) and symbol.kind == sym.TypeKind.Enum:
			if symbol.info.has_variant(name):
				return symbol
			else:
				report.error(
				    f"enum `{symbol.name}` has no variant `{name}`", pos
				)
				return None
		report.error(
		    f"could not find `{name}` in {symbol.sym_kind()} `{symbol.name}`",
		    pos
		)
		return None

	def resolve_ident(self, ident):
		if ident.name == "_":
			return # ignore special var
		elif ident.is_comptime:
			if not ast.is_comptime_constant(ident.name):
				report.error(
				    f"unknown comptime constant `{ident.name}`", ident.pos
				)
		elif obj := ident.scope.lookup(ident.name):
			if isinstance(obj, sym.Label):
				report.error("expected value, found label", ident.pos)
			else:
				ident.is_obj = True
				ident.obj = obj
				ident.typ = obj.typ
		elif s := self.cur_sym.find(ident.name):
			s.uses += 1
			ident.sym = s
		elif s := self.sf.find_imported_symbol(ident.name):
			s.uses += 1
			ident.sym = s
		else:
			report.error(f"cannot find `{ident.name}` in this scope", ident.pos)

	def resolve_path_expr(self, path):
		if path.is_global:
			path.left_info = self.comp.universe
			if field_info := self.find_symbol(
			    self.comp.universe, path.field_name, path.field_pos
			):
				field_info.uses += 1
				path.field_info = field_info
			else:
				path.has_error = True
		elif isinstance(path.left, ast.PkgExpr):
			path.left_info = self.comp.pkg_sym
			if field_info := self.find_symbol(
			    self.comp.pkg_sym, path.field_name, path.field_pos
			):
				field_info.uses += 1
				path.field_info = field_info
			else:
				path.has_error = True
		elif isinstance(path.left, ast.SuperExpr):
			if self.cur_sym.parent != None and not self.cur_sym.parent.is_universe:
				path.left_info = self.cur_sym.parent
				if field_info := self.find_symbol(
				    self.cur_sym.parent, path.field_name, path.field_pos
				):
					field_info.uses += 1
					path.field_info = field_info
				else:
					path.has_error = True
			else:
				report.error("current module has no parent", path.left.pos)
		elif isinstance(path.left, ast.SelfExpr):
			path.left_info = self.cur_sym
			if field_info := self.find_symbol(
			    self.cur_sym, path.field_name, path.field_pos
			):
				field_info.uses += 1
				path.field_info = field_info
			else:
				path.has_error = True
		elif isinstance(path.left, ast.Ident):
			if local_sym := self.cur_sym.find(path.left.name):
				path.left_info = local_sym
				if field_info := self.find_symbol(
				    local_sym, path.field_name, path.field_pos
				):
					field_info.uses += 1
					path.field_info = field_info
				else:
					path.has_error = True
			elif imported_sym := self.sf.find_imported_symbol(path.left.name):
				path.left_info = imported_sym
				if field_info := self.find_symbol(
				    imported_sym, path.field_name, path.field_pos
				):
					field_info.uses += 1
					path.field_info = field_info
				else:
					path.has_error = True
			elif package := self.comp.universe.find(path.left.name):
				path.left_info = package
				if field_info := self.find_symbol(
				    package, path.field_name, path.field_pos
				):
					field_info.uses += 1
					path.field_info = field_info
				else:
					path.has_error = True
			else:
				report.error(
				    f"use of undeclared external package `{path.left.name}`",
				    path.left.pos
				)
				path.has_error = True
		elif isinstance(path.left, ast.SelfTyExpr):
			if self.self_sym != None:
				path.left_info = self.self_sym
				if field_info := self.find_symbol(
				    self.self_sym, path.field_name, path.field_pos
				):
					field_info.uses += 1
					path.field_info = field_info
				else:
					path.has_error = True
			else:
				report.error("cannot resolve `Self`", path.left.pos)
		elif isinstance(path.left, ast.PathExpr):
			self.resolve_expr(path.left)
			if not path.left.has_error:
				path.left_info = path.left.field_info
				if field_info := self.find_symbol(
				    path.left.field_info, path.field_name, path.field_pos
				):
					field_info.uses += 1
					path.field_info = field_info
				else:
					path.has_error = True
		else:
			report.error("bad use of path expression", path.pos)
			path.has_error = True

	def check_visibility(self, sym, pos):
		if sym.vis == Visibility.Private and not self.cur_sym.has_access_to(
		    sym
		):
			report.error(f"{sym.sym_kind()} `{sym.name}` is private", pos)

	def disallow_errtype_use(self, kind, pos):
		if (not self.inside_is_comparation) and kind == sym.TypeKind.ErrType:
			report.error("cannot use error type as a normal type", pos)
			report.note(
			    "only inside `raise` statement or `is` comparation can be used"
			)

	def resolve_type(self, typ):
		if isinstance(typ, type.Ref):
			return self.resolve_type(typ.typ)
		elif isinstance(typ, type.Ptr):
			return self.resolve_type(typ.typ)
		elif isinstance(typ, type.Slice):
			if self.resolve_type(typ.typ):
				typ.resolve(self.comp.universe.add_or_get_slice(typ.typ))
				return True
		elif isinstance(typ, type.Array):
			if self.resolve_type(typ.typ):
				if typ_size := self.eval_size_expr(typ.size):
					if int(typ_size.lit) <= 0:
						report.error(
						    f"array size cannot be zero or negative (size: {typ_size.lit})",
						    typ.size.pos
						)
					typ.size = typ_size
					typ.resolve(
					    self.comp.universe.add_or_get_array(typ.typ, typ_size)
					)
					return True
				report.error(
				    "array size cannot use non-constant value", typ.size.pos
				)
		elif isinstance(typ, type.Tuple):
			res = False
			for t in typ.types:
				res = self.resolve_type(t)
			typ.resolve(self.comp.universe.add_or_get_tuple(typ.types))
			return res
		elif isinstance(typ, type.Fn):
			res = False
			for i in range(len(typ.args)):
				res = self.resolve_type(typ.args[i])
			res = self.resolve_type(typ.ret_typ)
			return res
		elif isinstance(typ, type.Optional):
			if not isinstance(typ.typ, (type.Ptr, type.Ref)):
				typ.sym = self.comp.universe.add_or_get_optional(typ.typ)
			return self.resolve_type(typ.typ)
		elif isinstance(typ, type.Result):
			typ.sym = self.comp.universe.add_or_get_result(typ.typ)
			return self.resolve_type(typ.typ)
		elif isinstance(typ, type.Type):
			if typ.is_resolved():
				return True # resolved
			if isinstance(typ.expr, ast.Ident):
				self.resolve_ident(typ.expr)
				if s := typ.expr.sym:
					if isinstance(s, sym.Type):
						pos = typ.expr.pos
						typ.resolve(s)
						if s.kind == sym.TypeKind.Alias: # unalias
							if self.resolve_type(s.info.parent):
								typ.unalias()
						s.uses += 1
						self.disallow_errtype_use(s.kind, pos)
						return True
					else:
						report.error(
						    f"expected type, found {s.sym_kind()}", typ.expr.pos
						)
				else:
					report.error(
					    f"cannot find type `{typ.expr.name}` in this scope",
					    typ.expr.pos
					)
			elif isinstance(typ.expr, ast.PathExpr):
				self.resolve_path_expr(typ.expr)
				if not typ.expr.has_error:
					if isinstance(typ.expr.field_info, sym.Type):
						pos = typ.expr.pos
						typ.resolve(typ.expr.field_info)
						if typ.expr.field_info.kind == sym.TypeKind.Alias: # unalias
							if self.resolve_type(
							    typ.expr.field_info.info.parent
							):
								typ.unalias()
						typ.sym.uses += 1
						self.disallow_errtype_use(typ.sym, pos)
						return True
					else:
						report.error(
						    f"expected type, found {typ.expr.field_info.sym_kind()}",
						    typ.expr.pos
						)
			elif isinstance(typ.expr, ast.SelfTyExpr):
				if self.self_sym != None:
					self.self_sym.uses += 1
					typ.resolve(self.self_sym)
				else:
					report.error("cannot resolve type for `Self`", typ.expr.pos)
			else:
				report.error(f"expected type, found {typ.expr}", typ.expr.pos)
		return False

	def eval_size_expr(self, expr):
		if isinstance(expr, ast.IntegerLiteral):
			return expr
		elif isinstance(expr, ast.ParExpr):
			return self.eval_size_expr(expr.expr)
		elif isinstance(expr, ast.BinaryExpr):
			if left := self.eval_size_expr(expr.left):
				if right := self.eval_size_expr(expr.right):
					il = int(left.lit)
					ir = int(right.lit)
					if expr.op == Kind.Plus:
						return ast.IntegerLiteral(str(il + ir), expr.pos)
					elif expr.op == Kind.Minus:
						return ast.IntegerLiteral(str(il - ir), expr.pos)
					elif expr.op == Kind.Mult:
						return ast.IntegerLiteral(str(il * ir), expr.pos)
					elif expr.op == Kind.Div:
						return ast.IntegerLiteral(str(il // ir), expr.pos)
					elif expr.op == Kind.Mod:
						return ast.IntegerLiteral(str(il % ir), expr.pos)
					elif expr.op == Kind.Amp:
						return ast.IntegerLiteral(str(il & ir), expr.pos)
					elif expr.op == Kind.Pipe:
						return ast.IntegerLiteral(str(il | ir), expr.pos)
					elif expr.op == Kind.Xor:
						return ast.IntegerLiteral(str(il ^ ir), expr.pos)
					elif expr.op == Kind.Lshift:
						return ast.IntegerLiteral(str(il << ir), expr.pos)
					elif expr.op == Kind.Rshift:
						return ast.IntegerLiteral(str(il >> ir), expr.pos)
		elif isinstance(expr, ast.Ident):
			if s := self.cur_sym.lookup(expr.name):
				if isinstance(s, sym.Const):
					if s.has_evaled_expr:
						return s.evaled_expr
					if evaled_expr := self.eval_size_expr(s.expr):
						s.evaled_expr = evaled_expr
						s.has_evaled_expr = True
						return s.evaled_expr
			else:
				report.error(
				    f"cannot find `{expr.name}` in this scope", expr.pos
				)
		elif isinstance(expr, ast.PathExpr):
			self.resolve_path_expr(expr)
			if not expr.has_error:
				if isinstance(expr.field_info, sym.Const):
					if expr.field_info.has_evaled_expr:
						return expr.field_info.evaled_expr
					if evaled_expr := self.eval_size_expr(expr.field_info.expr):
						expr.field_info.evaled_expr = evaled_expr
						expr.field_info.has_evaled_expr = True
						return expr.field_info.evaled_expr
		elif isinstance(expr, ast.BuiltinCallExpr):
			if expr.name in ("sizeof", "alignof"):
				size, align = self.comp.type_size(expr.args[0].typ)
				if expr.name == "sizeof":
					return ast.IntegerLiteral(str(size), expr.pos)
				else:
					return ast.IntegerLiteral(str(align), expr.pos)
		return None
