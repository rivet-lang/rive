# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import ast, sym, type, report, utils

class Resolver:
	def __init__(self, comp):
		self.comp = comp
		self.source_file = None
		self.sym = None

	def resolve_files(self, source_files):
		for sf in source_files:
			self.sym = sf.sym
			self.source_file = sf
			self.resolve_decls(self.source_file.decls)

	def resolve_decls(self, decls):
		for decl in decls:
			old_sym = self.sym
			if isinstance(decl, ast.UseDecl):
				pass
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
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.TraitDecl):
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.UnionDecl):
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.ClassDecl):
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.StructDecl):
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.FieldDecl):
				self.resolve_type(decl.typ)
				if decl.has_def_expr:
					self.resolve_expr(decl.def_expr)
			elif isinstance(decl, ast.ExtendDecl):
				self.resolve_type(decl.typ)
				self.resolve_decls(decl.decls)
			elif isinstance(decl, ast.FnDecl):
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
			for e in expr.exprs:
				self.resolve_expr(e)
		elif isinstance(expr, ast.CastExpr):
			self.resolve_type(expr.typ)
			self.resolve_expr(expr.expr)
		elif isinstance(expr, ast.GuardExpr):
			pass
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
			pass
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

	def resolve_type(self,typ):
		pass
