# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import ast, sym, type, report, utils

class Checker:
	def __init__(self, comp):
		self.comp = comp
		self.source_file = None
		self.sym = None

	def check_files(self, source_files):
		for sf in source_files:
			self.sym = sf.sym
			self.source_file = sf
			self.check_decls(self.source_file.decls)

	def check_decls(self, decls):
		for decl in decls:
			old_sym = self.sym
			if isinstance(decl, ast.UseDecl):
				pass
			elif isinstance(decl, ast.ExternDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.ModDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.ConstDecl):
				pass
			elif isinstance(decl, ast.LetDecl):
				pass
			elif isinstance(decl, ast.TypeDecl):
				pass
			elif isinstance(decl, ast.ErrTypeDecl):
				pass
			elif isinstance(decl, ast.EnumDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.TraitDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.SumTypeDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.ClassDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.StructDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.FieldDecl):
				pass
			elif isinstance(decl, ast.ExtendDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.FuncDecl):
				self.check_stmts(decl.stmts)
			elif isinstance(decl, ast.DestructorDecl):
				self.check_stmts(decl.stmts)
			elif isinstance(decl, ast.TestDecl):
				self.check_stmts(decl.stmts)
			self.sym = old_sym

	def check_stmts(self, stmts):
		for stmt in stmts:
			self.check_stmt(stmt)

	def check_stmt(self, stmt):
		if isinstance(stmt, ast.LetStmt):
			right_typ = self.check_expr(stmt.right)
			for v in stmt.lefts:
				stmt.scope.update_typ(v.name, right_typ)
		elif isinstance(stmt, ast.AssignStmt):
			self.check_expr(stmt.left)
			self.check_expr(stmt.right)
		elif isinstance(stmt, ast.WhileStmt):
			self.check_expr(stmt.cond)
			self.check_stmt(stmt.stmt)
		elif isinstance(stmt, ast.ForInStmt):
			self.check_expr(stmt.iterable)
			self.check_stmt(stmt.stmt)
		elif isinstance(stmt, ast.ExprStmt):
			self.check_expr(stmt.expr)

	def check_expr(self, expr):
		if isinstance(expr, ast.EmptyExpr):
			pass # error raised in `Resolver`
		elif isinstance(expr, ast.TypeNode):
			return expr.typ
		elif isinstance(expr, ast.Ident):
			return expr.typ
		elif isinstance(expr, ast.SelfExpr):
			return expr.typ
		elif isinstance(expr, ast.SuperExpr):
			pass # TODO
		elif isinstance(expr, ast.SelfTyExpr):
			return type.Type(expr.sym)
		elif isinstance(expr, ast.TupleLiteral):
			for e in expr.exprs:
				self.check_expr(e)
		elif isinstance(expr, ast.ArrayLiteral):
			for e in expr.elems:
				self.check_expr(e)
		elif isinstance(expr, ast.CastExpr):
			self.check_expr(expr.expr)
			return expr.typ
		elif isinstance(expr, ast.GuardExpr):
			self.check_expr(expr.expr)
			if expr.has_cond:
				self.check_expr(expr.cond)
		elif isinstance(expr, ast.UnaryExpr):
			return self.check_expr(expr.right)
		elif isinstance(expr, ast.BinaryExpr):
			self.check_expr(expr.left)
			return self.check_expr(expr.right)
		elif isinstance(expr, ast.PostfixExpr):
			return self.check_expr(expr.left)
		elif isinstance(expr, ast.ParExpr):
			return self.check_expr(expr.expr)
		elif isinstance(expr, ast.IndexExpr):
			self.check_expr(expr.left)
			self.check_expr(expr.index)
		elif isinstance(expr, ast.CallExpr):
			#self.check_expr(expr.left)
			for arg in expr.args:
				self.check_expr(arg.expr)
			if expr.err_handler.has_expr:
				self.check_expr(expr.err_handler.expr)
		elif isinstance(expr, ast.BuiltinCallExpr):
			for arg in expr.args:
				self.check_expr(arg)
		elif isinstance(expr, ast.RangeExpr):
			if expr.has_start:
				self.check_expr(expr.start)
			if expr.has_end:
				self.check_expr(expr.end)
		elif isinstance(expr, ast.SelectorExpr):
			left_typ = self.check_expr(expr.left)
			left_sym=left_typ.symbol()
			if expr.is_indirect:
				return left_typ.typ
			elif expr.is_nonecheck:
				return left_typ.typ
			elif field := left_sym.find_field(expr.field_name):
				return field.typ
			else:
				report.error(f"type `{left_sym.name}` has no field `{expr.field_name}`",expr.pos)
		elif isinstance(expr, ast.PathExpr):
			return type.Type(expr.field_info)
		elif isinstance(expr, ast.ReturnExpr):
			if expr.has_expr:
				self.check_expr(expr.expr)
		elif isinstance(expr, ast.RaiseExpr):
			self.check_expr(expr.expr)
		elif isinstance(expr, ast.Block):
			for stmt in expr.stmts:
				self.check_stmt(stmt)
			if expr.is_expr:
				expr.typ=self.check_expr(expr.expr)
				return expr.typ
			return self.comp.void_t
		elif isinstance(expr, ast.IfExpr):
			if expr.is_comptime:
				return self.check_expr(expr.branches[expr.branch_idx].expr)
			else:
				for b in expr.branches:
					if not b.is_else:
						self.check_expr(b.cond)
					self.check_expr(b.expr)
		elif isinstance(expr, ast.SwitchExpr):
			for b in expr.branches:
				if not b.is_else:
					for pat in b.pats:
						self.check_expr(pat)
				self.check_expr(b.expr)
