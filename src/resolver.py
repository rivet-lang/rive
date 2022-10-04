# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .sym import Vis
from .token import Kind
from . import ast, sym, type, report, utils

class Resolver:
	def __init__(self, comp):
		self.comp = comp
		self.source_file = None
		self.sym = None
		self.self_sym = None

	def resolve_files(self, source_files):
		for sf in source_files:
			self.sym = sf.sym
			self.source_file = sf
			self.resolve_decls(self.source_file.decls)

	def resolve_decls(self, decls):
		for decl in decls:
			old_sym = self.sym
			old_self_sym = self.self_sym
			if isinstance(decl, ast.UseDecl):
				if isinstance(decl.path, (ast.Ident, ast.PkgExpr)):
					name = decl.path.name if isinstance(
					    decl.path, ast.Ident
					) else self.source_file.pkg_name
					if len(decl.symbols) == 0:
						if isinstance(decl.path, ast.PkgExpr):
							report.error(
							    "invalid `use` declaration", decl.path.pos
							)
						elif _ := self.source_file.sym.find(name):
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
						self.resolve_selective_use_symbols(
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
							self.source_file.imported_symbols[
							    decl.alias] = decl.path.field_info
							if decl.vis.is_pub(): # reexport symbol
								self.source_file.sym.reexported_syms[
								    decl.alias] = decl.path.field_info
						else:
							self.resolve_selective_use_symbols(
							    decl.symbols, decl.path.field_info,
							    decl.vis.is_pub()
							)
			elif isinstance(decl, ast.ExternDecl):
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.ModDecl):
				if decl.is_inline:
					self.sym = decl.sym
					self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.ConstDecl):
				self.resolve_expr(decl.expr)
			elif isinstance(decl, ast.TypeDecl):
				self.resolve_type(decl.parent)
			elif isinstance(decl, ast.EnumDecl):
				self.self_sym = decl.sym
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.TraitDecl):
				self.self_sym = decl.sym
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.SumTypeDecl):
				self.self_sym = decl.sym
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.ClassDecl):
				self.self_sym = decl.sym
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.StructDecl):
				self.self_sym = decl.sym
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.FieldDecl):
				self.resolve_type(decl.typ)
				if decl.has_def_expr:
					self.resolve_expr(decl.def_expr)
			elif isinstance(decl, ast.ExtendDecl):
				self.self_sym = decl.typ.symbol()
				self.resolve_type(decl.typ)
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.FuncDecl):
				self.resolve_type(decl.ret_typ)
				for stmt in decl.stmts:
					self.resolve_stmt(stmt)
			elif isinstance(decl, ast.DestructorDecl):
				for stmt in decl.stmts:
					self.resolve_stmt(stmt)
			elif isinstance(decl, ast.TestDecl):
				for stmt in decl.stmts:
					self.resolve_stmt(stmt)
			self.sym = old_sym
			self.self_sym = old_self_sym

	def resolve_stmt(self, stmt):
		if isinstance(stmt, ast.AssignStmt):
			pass
		elif isinstance(stmt, ast.LetStmt):
			pass
		elif isinstance(stmt, ast.WhileStmt):
			self.resolve_expr(stmt.cond)
			self.resolve_stmt(stmt.stmt)
		elif isinstance(stmt, ast.ForInStmt):
			self.resolve_stmt(stmt.stmt)
		elif isinstance(stmt, ast.ExprStmt):
			self.resolve_expr(stmt.expr)

	def resolve_expr(self, expr):
		if isinstance(expr, ast.EmptyExpr):
			report.error("empty expression found", expr.pos)
			report.note("unexpected bug, please, report it")
		elif isinstance(expr, ast.TypeNode):
			self.resolve_type(expr.typ)
		elif isinstance(expr, ast.Ident):
			pass
		elif isinstance(expr, ast.SelfExpr):
			pass
		elif isinstance(expr, ast.SuperExpr):
			pass
		elif isinstance(expr, ast.SelfTyExpr):
			pass
		elif isinstance(expr, ast.TupleLiteral):
			for e in expr.exprs:
				self.resolve_expr(e)
		elif isinstance(expr, ast.ArrayLiteral):
			for e in expr.elems:
				self.resolve_expr(e)
		elif isinstance(expr, ast.CastExpr):
			self.resolve_type(expr.typ)
			self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.GuardExpr):
			for v in expr.vars:
				try:
					expr.scope.add(sym.Obj(False, v, self.comp.void_t, False))
				except utils.CompilerError as e:
					report.error(e.args[0], expr.pos)
			self.resolve_expr(expr.expr)
			if expr.has_cond:
				self.resolve_expr(expr.cond)
		elif isinstance(expr, ast.UnaryExpr):
			self.resolve_expr(expr.right)
		elif isinstance(expr, ast.BinaryExpr):
			self.resolve_expr(expr.left)
			self.resolve_expr(expr.right)
		elif isinstance(expr, ast.PostfixExpr):
			self.resolve_expr(expr.left)
		elif isinstance(expr, ast.ParExpr):
			self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.IndexExpr):
			self.resolve_expr(expr.left)
			self.resolve_expr(expr.index)
		elif isinstance(expr, ast.CallExpr):
			self.resolve_expr(expr.left)
			for arg in expr.args:
				self.resolve_expr(arg.expr)
			if expr.err_handler.has_expr:
				self.resolve_expr(expr.err_handler.expr)
		elif isinstance(expr, ast.BuiltinCallExpr):
			for arg in expr.args:
				self.resolve_expr(arg)
		elif isinstance(expr, ast.RangeExpr):
			if expr.has_start:
				self.resolve_expr(expr.start)
			if expr.has_end:
				self.resolve_expr(expr.end)
		elif isinstance(expr, ast.SelectorExpr):
			pass
		elif isinstance(expr, ast.PathExpr):
			self.resolve_path_expr(expr)
		elif isinstance(expr, ast.ReturnExpr):
			self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.RaiseExpr):
			self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.Block):
			for stmt in expr.stmts:
				self.resolve_stmt(stmt)
			if expr.is_expr:
				self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.IfExpr):
			pass
		elif isinstance(expr, ast.SwitchExpr):
			pass

	def find_symbol(self, symbol, name, pos):
		if s := symbol.find(name):
			self.check_vis(s, pos)
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
		    f"could not find `{name}` in {symbol.typeof()} `{symbol.name}`", pos
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
			return

		if obj := ident.scope.lookup(ident.name):
			if isinstance(obj, sym.Label):
				report.error("expected value, found label", ident.pos)
			else:
				ident.is_obj = True
				ident.obj = obj
				ident.typ = obj.typ
		elif s := self.source_file.sym.find(ident.name):
			s.uses += 1
			ident.sym = s
		elif s := self.source_file.find_imported_symbol(ident.name):
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
			pkg_sym = self.comp.universe.find(self.source_file.pkg_name)
			path.left_info = pkg_sym
			if field_info := self.find_symbol(
			    pkg_sym, path.field_name, path.field_pos
			):
				field_info.uses += 1
				path.field_info = field_info
			else:
				path.has_error = True
		elif isinstance(path.left, ast.SuperExpr):
			if self.source_file.sym.parent and not self.source_file.sym.parent.is_universe:
				path.left_info = self.source_file.sym.parent
				if field_info := self.find_symbol(
				    self.source_file.sym.parent, path.field_name, path.field_pos
				):
					field_info.uses += 1
					path.field_info = field_info
				else:
					path.has_error = True
			else:
				report.error("current module has no parent", path.left.pos)
		elif isinstance(path.left, ast.SelfExpr):
			path.left_info = self.source_file.sym
			if field_info := self.find_symbol(
			    self.source_file.sym, path.field_name, path.field_pos
			):
				field_info.uses += 1
				path.field_info = field_info
			else:
				path.has_error = True
		elif isinstance(path.left, ast.Ident):
			if local_sym := self.source_file.sym.find(path.left.name):
				path.left_info = local_sym
				if field_info := self.find_symbol(
				    local_sym, path.field_name, path.field_pos
				):
					field_info.uses += 1
					path.field_info = field_info
				else:
					path.has_error = True
			elif imported_sym := self.source_file.find_imported_symbol(
			    path.left.name
			):
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
		else:
			report.error("bad use of path expression", path.pos)
			path.has_error = True

	def resolve_type(self, typ):
		if isinstance(typ, type.Ref):
			return self.resolve_type(typ.typ)
		elif isinstance(typ, type.Ptr):
			return self.resolve_type(typ.typ)
		elif isinstance(typ, type.Variadic):
			if self.resolve_type(typ.typ):
				elem_sym = typ.typ.symbol()
				if elem_sym.kind == type.TypeKind.Trait:
					elem_sym.info.has_objects = True
				typ.resolve(self.comp.universe.add_or_get_slice(typ.typ, False))
				return True
		elif isinstance(typ, type.Array):
			if self.resolve_type(typ.typ):
				if typ_size := self.eval_size(typ.size):
					if int(typ_size.lit, 0) <= 0:
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
		elif isinstance(typ, type.Slice):
			if self.resolve_type(typ.typ):
				typ.resolve(
				    self.comp.universe.add_or_get_slice(typ.typ, typ.is_mut)
				)
				return True
		elif isinstance(typ, type.Tuple):
			res = False
			for t in typ.types:
				res = self.resolve_type(t)
			typ.resolve(self.comp.universe.add_or_get_tuple(typ.types))
			return res
		elif isinstance(typ, type.Func):
			res = False
			for i in range(len(typ.args)):
				res = self.resolve_type(typ.args[i])
			res = self.resolve_type(typ.ret_typ)
			return res
		elif isinstance(typ, type.Optional):
			if self.resolve_type(typ.typ):
				#if not isinstance(typ.typ, (type.Ptr, type.Ref)):
				#	typ.sym = self.comp.universe.add_or_get_optional(typ.typ)
				return True
			return False
		elif isinstance(typ, type.Result):
			if self.resolve_type(typ.typ):
				#typ.sym = self.comp.universe.add_or_get_result(typ.typ)
				return True
			return False
		elif isinstance(typ, type.Type):
			if typ.is_resolved():
				return True # resolved
			if isinstance(typ.expr, ast.Ident):
				self.resolve_ident(typ.expr)
				if typ.expr.sym != None:
					if isinstance(typ.expr.sym, sym.Type):
						pos = typ.expr.pos
						typ.resolve(typ.expr.sym)
						#if typ.expr.sym.kind == sym.TypeKind.Alias: # unalias
						#	if self.resolve_type(typ.expr.sym.info.parent):
						#		typ.unalias()
						typ_sym = typ.symbol()
						typ_sym.uses += 1
						return True
					else:
						report.error(
						    f"expected type, found {typ.expr.sym.typeof()}",
						    typ.expr.pos
						)
				elif typ.expr.is_obj:
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
						return True
					else:
						report.error(
						    f"expected type, found {typ.expr.field_info.typeof()}",
						    typ.expr.pos
						)
			elif isinstance(typ.expr, ast.SelfTyExpr):
				if self.self_sym != None:
					self.self_sym.uses += 1
					typ.resolve(self.self_sym)
					return True
				else:
					report.error("cannot resolve type for `Self`", typ.expr.pos)
			else:
				report.error(f"expected type, found {typ.expr}", typ.expr.pos)
		return False

	def eval_size(self, expr):
		if isinstance(expr, ast.IntegerLiteral):
			return expr
		elif isinstance(expr, ast.ParExpr):
			return self.eval_size(expr.expr)
		elif isinstance(expr, ast.BinaryExpr):
			if left := self.eval_size(expr.left):
				if right := self.eval_size(expr.right):
					il = int(left.lit, 0)
					ir = int(right.lit, 0)
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
			if s := self.source_file.sym.find(expr.name):
				if isinstance(s, sym.Const):
					if s.has_evaled_expr:
						return s.evaled_expr
					if evaled_expr := self.eval_size(s.expr):
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
					if evaled_expr := self.eval_size(expr.field_info.expr):
						expr.field_info.evaled_expr = evaled_expr
						expr.field_info.has_evaled_expr = True
						return expr.field_info.evaled_expr
		elif isinstance(expr, ast.BuiltinCallExpr):
			if expr.name in ("size_of", "align_of"):
				if self.resolve_type(expr.args[0].typ):
					size, align = self.comp.type_size(expr.args[0].typ)
					if expr.name == "size_of":
						return ast.IntegerLiteral(str(size), expr.pos)
					else:
						return ast.IntegerLiteral(str(align), expr.pos)
		return None

	def check_vis(self, sym, pos):
		if sym.vis == Vis.Priv and not self.source_file.sym.has_access_to(sym):
			report.error(f"{sym.typeof()} `{sym.name}` is private", pos)

	def check_imported_symbol(self, s, pos):
		if s.name in self.source_file.imported_symbols:
			report.error(f"{s.typeof()} `{s.name}` is already imported", pos)
		elif self.source_file.sym.find(s.name):
			report.error(
			    f"another symbol with the name `{s.name}` already exists", pos
			)
			report.help("you can use `as` to change the name of the import")

	def resolve_selective_use_symbols(self, symbols, path_sym, is_pub):
		for isym in symbols:
			if isym.is_self:
				self.source_file.imported_symbols[isym.alias] = path_sym
				if is_pub: # reexport symbol
					self.source_file.sym.reexported_syms[isym.alias] = path_sym
			else:
				if isym_ := path_sym.find(isym.name):
					self.check_vis(isym_, isym.pos)
					self.check_imported_symbol(isym_, isym.pos)
					self.source_file.imported_symbols[isym.alias] = isym_
					if is_pub: # reexport symbol
						self.source_file.sym.reexported_syms[isym.alias] = isym_
				else:
					report.error(
					    f"could not find `{isym.name}` in {path_sym.typeof()} `{path_sym.name}`",
					    isym.pos
					)
