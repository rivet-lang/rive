# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os, glob

from . import ast, sym, type, report, token, utils
from .token import Kind
from .lexer import Lexer

class Parser:
	def __init__(self, comp):
		self.comp = comp

		self.lexer = None
		self.prev_tok = None
		self.tok = None
		self.peek_tok = None
		self.last_err_pos = None

		self.pkg_name = ""
		self.pkg_deps = []
		self.mod_vis = sym.Vis.Priv
		self.mod_name = ""
		self.mod_deps = []

		self.file_path = ""
		self.file_dir = ""
		self.file_parent_sym = None
		self.file_sym = None

		self.scope = None

		self.inside_extern = False
		self.extern_abi = sym.ABI.Rivet
		self.inside_struct_or_class_decl = False
		self.inside_trait = False
		self.inside_block = False

	def parse_pkg(self, pkg_name, files):
		self.pkg_name = pkg_name
		if self.pkg_name in ("std", "tests"):
			self.pkg_deps.append("core")
		source_files = []
		for file in files:
			self.file_path = file
			self.file_dir = os.path.dirname(file)
			source_files.append(self.parse_file(file, True))
		self.comp.pkg_deps.add_pkg_deps(pkg_name, self.pkg_deps)
		self.comp.pkg_deps.add_pkg_mod_deps(pkg_name, pkg_name, self.mod_deps)
		self.comp.pkg_deps.add_source_files(pkg_name, source_files)

	def parse_mod(self, pkg_name, parent, vis, name, files):
		self.pkg_name = pkg_name
		self.mod_vis = vis
		self.mod_name = name
		self.file_parent_sym = parent
		source_files = []
		for file in files:
			self.file_path = file
			self.file_dir = os.path.dirname(file)
			source_files.append(self.parse_file(file, False))
		qualname = parent.qualname() + "::" + name
		self.comp.pkg_deps.add_pkg_mod_deps(pkg_name, qualname, self.mod_deps)
		self.comp.pkg_deps.add_source_files(qualname, source_files)

	def parse_file(self, file, from_pkg):
		self.lexer = Lexer.from_file(self.comp, file)
		if report.ERRORS > 0:
			return ast.SourceFile(file, [], self.pkg_name, None)
		self.advance(2)
		if from_pkg:
			if pkg_sym := self.comp.universe.find(self.pkg_name):
				sf_sym = pkg_sym
			else:
				sf_sym = self.comp.universe.add_and_return(
				    sym.Pkg(sym.Vis.Priv, self.pkg_name)
				)
		else:
			if mod_sym := self.file_parent_sym.find(self.mod_name):
				sf_sym = mod_sym
			else:
				sf_sym = self.file_parent_sym.add_and_return(
				    sym.Mod(self.mod_vis, self.mod_name)
				)
		self.file_sym = sf_sym
		return ast.SourceFile(
		    file, self.parse_decls(), self.pkg_name, self.file_sym
		)

	# ---- useful functions for working with tokens ----
	def next(self):
		self.prev_tok = self.tok
		self.tok = self.peek_tok
		self.peek_tok = self.lexer.next()

	def peek_token(self, n):
		return self.lexer.peek_token(n - 2)

	def advance(self, n):
		for _ in range(n):
			self.next()

	def accept(self, kind):
		if self.tok.kind == kind:
			self.next()
			return True
		return False

	def expect(self, kind):
		if self.accept(kind):
			return
		elif self.last_err_pos and self.last_err_pos.pos == self.tok.pos.pos:
			self.next() # avoid infinite output
			return
		self.last_err_pos = self.tok.pos
		kstr = str(kind)
		if token.is_key(kstr) or (len(kstr) > 0 and not kstr[0].isalpha()):
			kstr = f"`{kstr}`"
		report.error(f"expected {kstr}, found {self.tok} ", self.tok.pos)

	# ---- utilities ------------------
	def parse_name(self):
		lit = self.tok.lit
		self.expect(Kind.Name)
		return lit

	def open_scope(self):
		self.scope = sym.Scope(self.tok.pos.pos, self.scope)

	def close_scope(self):
		self.scope.end = self.tok.pos.pos
		self.scope = self.scope.parent

	# ---- declarations --------------
	def parse_doc_comment(self):
		pos = self.tok.pos
		lines = []
		while self.accept(Kind.DocComment):
			lines.append(self.prev_tok.lit)
		return ast.DocComment(lines, pos)

	def parse_abi(self):
		self.expect(Kind.Lparen)
		abi_pos = self.tok.pos
		abi = self.parse_name()
		self.expect(Kind.Rparen)
		if abi_f := sym.ABI.from_string(abi):
			return abi_f
		report.error(f"unknown ABI: `{abi}`", abi_pos)
		return sym.ABI.Rivet

	def parse_attrs(self, parse_pkg_attrs = False):
		attrs = ast.Attrs()
		while self.accept(Kind.Hash):
			if parse_pkg_attrs:
				self.expect(Kind.Bang)
			self.expect(Kind.Lbracket)
			while True:
				args = []
				pos = self.tok.pos
				if self.accept(Kind.KwUnsafe):
					attr_name = "unsafe"
				else:
					attr_name = self.parse_name()
				if self.accept(Kind.Lparen):
					while True:
						if self.tok.kind == Kind.Name and self.peek_tok.kind == Kind.Colon:
							name = self.parse_name()
							self.expect(Kind.Colon)
						else:
							name = ""
						expr = self.parse_expr()
						args.append(ast.AttrArg(name, expr))
						if not self.accept(Kind.Comma):
							break
					self.expect(Kind.Rparen)
				attrs.add(ast.Attr(attr_name, args, pos))
				if not self.accept(Kind.Semicolon):
					break
			self.expect(Kind.Rbracket)
		return attrs

	def parse_vis(self):
		if self.accept(Kind.KwPub):
			return sym.Vis.Pub
		elif self.accept(Kind.KwProt):
			return sym.Vis.Prot
		return sym.Vis.Priv

	def parse_decls(self):
		decls = []
		while self.tok.kind != Kind.EOF:
			decls.append(self.parse_decl())
		return decls

	def parse_decl(self):
		doc_comment = self.parse_doc_comment()
		attrs = self.parse_attrs()
		vis = self.parse_vis()
		pos = self.tok.pos
		if self.accept(Kind.KwUse):
			path = self.parse_expr()
			if isinstance(path, ast.Ident):
				alias = path.name
			elif isinstance(path, ast.PkgExpr):
				alias = self.comp.prefs.pkg_name
			elif isinstance(path, ast.PathExpr):
				alias = path.field_name
			else:
				report.error("expected name or path", path.pos)
				alias = ""
			symbols = list()
			if self.accept(Kind.KwAs):
				alias = self.parse_name()
			elif self.accept(Kind.Lbrace):
				while True:
					pos = self.tok.pos
					if self.accept(Kind.KwSelf):
						if self.accept(Kind.KwAs):
							alias = self.parse_name()
						symbols.append(ast.UseSymbol("", alias, True, pos))
					else:
						name = self.parse_name()
						alias = name
						if self.accept(Kind.KwAs):
							alias = self.parse_name()
						symbols.append(ast.UseSymbol(name, alias, False, pos))
					if not self.accept(Kind.Comma):
						break
				self.expect(Kind.Rbrace)
			self.expect(Kind.Semicolon)
			return ast.UseDecl(attrs, vis, path, alias, symbols)
		elif self.accept(Kind.KwExtern):
			if self.tok.kind == Kind.KwPkg and not isinstance(
			    self.file_sym, sym.Pkg
			):
				report.error(
				    "external packages can only be declared at the package level",
				    pos,
				)
			self.inside_extern = True
			if self.accept(Kind.KwPkg):
				# extern package
				if vis.is_pub():
					report.error(
					    "`extern pkg` declarations cannot be declared public",
					    pos
					)
				extern_pkg_pos = self.tok.pos
				extern_pkg = self.parse_name()
				self.expect(Kind.Semicolon)
				decl = ast.ExternPkg(extern_pkg, pos)
				if extern_pkg == self.pkg_name:
					report.error("a package cannot load itself", extern_pkg_pos)
				else:
					self.pkg_deps.append(extern_pkg)
					if not self.comp.universe.exists(extern_pkg):
						self.comp.load_pkg(extern_pkg, extern_pkg_pos)
			else:
				# extern function or var
				abi = self.parse_abi()
				protos = []
				if self.accept(Kind.Lbrace):
					if vis.is_pub():
						report.error(
						    "`extern` blocks cannot be declared public", pos
						)
					self.extern_abi = abi
					while True:
						protos.append(self.parse_decl())
						if self.tok.kind == Kind.Rbrace:
							break
					self.expect(Kind.Rbrace)
				elif self.accept(Kind.KwFn):
					protos.append(
					    self.parse_fn_decl(
					        doc_comment, attrs, vis,
					        attrs.has("unsafe") or abi != sym.ABI.Rivet, abi
					    )
					)
				else:
					report.error("invalid external declaration", pos)
				decl = ast.ExternDecl(attrs, abi, protos, pos)
			self.inside_extern = False
			return decl
		elif self.accept(Kind.KwMod):
			pos = self.tok.pos
			name = self.parse_name()
			decls = []
			is_inline = not self.accept(Kind.Semicolon)
			if is_inline:
				self.expect(Kind.Lbrace)
				while not self.accept(Kind.Rbrace):
					decls.append(self.parse_decl())
			else:
				mod_path = os.path.join(self.file_dir, name)
				if os.path.isdir(mod_path):
					files = self.comp.filter_files(
					    glob.glob(os.path.join(mod_path, "*.ri"))
					)
					if len(files) == 0:
						report.error(
						    f"module `{name}` contains no rivet files", pos
						)
					Parser(
					    self.comp
					).parse_mod(self.pkg_name, self.file_sym, vis, name, files)
					self.mod_deps.append(self.file_sym.qualname() + "::" + name)
				else:
					report.error(f"module `{name}` not found", pos)
			return ast.ModDecl(
			    doc_comment, attrs, name, vis, decls, is_inline, pos
			)
		elif self.accept(Kind.KwConst):
			pos = self.tok.pos
			name = self.parse_name()
			self.expect(Kind.Colon)
			typ = self.parse_type()
			self.expect(Kind.Assign)
			expr = self.parse_expr()
			self.expect(Kind.Semicolon)
			return ast.ConstDecl(doc_comment, attrs, vis, name, typ, expr, pos)
		elif self.accept(Kind.KwLet):
			# variable declarations
			pos = self.prev_tok.pos
			lefts = []
			if self.accept(Kind.Lparen):
				# multiple variables
				while True:
					lefts.append(self.parse_var_decl(True))
					if not self.accept(Kind.Comma):
						break
				self.expect(Kind.Rparen)
			else:
				lefts.append(self.parse_var_decl(True))
			if not self.inside_extern and self.accept(Kind.Assign):
				right = self.parse_expr()
			else:
				right = self.empty_expr()
			self.expect(Kind.Semicolon)
			return ast.LetDecl(
			    doc_comment, attrs, vis, self.inside_extern, lefts, right, pos
			)
		elif self.accept(Kind.KwType):
			pos = self.tok.pos
			name = self.parse_name()
			self.expect(Kind.Assign)
			parent = self.parse_type()
			self.expect(Kind.Semicolon)
			return ast.TypeDecl(doc_comment, attrs, vis, name, parent, pos)
		elif self.accept(Kind.KwTrait):
			pos = self.tok.pos
			name = self.parse_name()
			decls = []
			old_inside_trait = self.inside_trait
			self.inside_trait = True
			self.expect(Kind.Lbrace)
			while not self.accept(Kind.Rbrace):
				decls.append(self.parse_decl())
			self.inside_trait = old_inside_trait
			return ast.TraitDecl(doc_comment, attrs, vis, name, decls, pos)
		elif self.accept(Kind.KwClass):
			old_inside_struct_or_class_decl = self.inside_struct_or_class_decl
			self.inside_struct_or_class_decl = True
			pos = self.tok.pos
			name = self.parse_name()
			bases = []
			if self.accept(Kind.Colon):
				while True:
					bases.append(self.parse_type())
					if not self.accept(Kind.Comma):
						break
			decls = []
			self.expect(Kind.Lbrace)
			if self.tok.kind != Kind.Rbrace:
				while self.tok.kind != Kind.Rbrace:
					decls.append(self.parse_decl())
			self.expect(Kind.Rbrace)
			self.inside_struct_or_class_decl = old_inside_struct_or_class_decl
			return ast.ClassDecl(
			    doc_comment, attrs, vis, name, bases, decls, pos
			)
		elif self.accept(Kind.KwStruct):
			old_inside_struct_or_class_decl = self.inside_struct_or_class_decl
			self.inside_struct_or_class_decl = True
			pos = self.tok.pos
			name = self.parse_name()
			is_opaque = self.accept(Kind.Semicolon)
			bases = []
			decls = []
			if not is_opaque:
				if self.accept(Kind.Colon):
					while True:
						bases.append(self.parse_type())
						if not self.accept(Kind.Comma):
							break
				self.expect(Kind.Lbrace)
				if self.tok.kind != Kind.Rbrace:
					while self.tok.kind != Kind.Rbrace:
						decls.append(self.parse_decl())
				self.expect(Kind.Rbrace)
			self.inside_struct_or_class_decl = old_inside_struct_or_class_decl
			return ast.StructDecl(
			    doc_comment, attrs, vis, name, bases, decls, is_opaque, pos
			)
		elif (self.inside_struct_or_class_decl or
		      self.inside_trait) and self.tok.kind in (Kind.KwMut, Kind.Name):
			# fields
			is_mut = self.accept(Kind.KwMut)
			name = self.parse_name()
			self.expect(Kind.Colon)
			typ = self.parse_type()
			has_def_expr = self.accept(Kind.Assign)
			def_expr = None
			if has_def_expr:
				def_expr = self.parse_expr()
			self.expect(Kind.Semicolon)
			return ast.FieldDecl(
			    attrs, doc_comment, vis, is_mut, name, typ, def_expr,
			    has_def_expr, pos
			)
		elif self.accept(Kind.KwEnum):
			pos = self.tok.pos
			name = self.parse_name()
			underlying_typ = self.comp.i32_t
			if self.accept(Kind.Colon):
				underlying_typ = self.parse_type()
			self.expect(Kind.Lbrace)
			values = []
			decls = []
			while True:
				values.append(self.parse_name())
				if not self.accept(Kind.Comma):
					break
			if self.accept(Kind.Semicolon):
				while self.tok.kind != Kind.Rbrace:
					decls.append(self.parse_decl())
			self.expect(Kind.Rbrace)
			return ast.EnumDecl(
			    doc_comment, attrs, vis, name, underlying_typ, values, decls,
			    pos
			)
		elif self.accept(Kind.KwExtend):
			pos = self.prev_tok.pos
			typ = self.parse_type()
			bases = []
			if self.accept(Kind.Colon):
				while True:
					bases.append(self.parse_type())
					if not self.accept(Kind.Comma):
						break
			decls = []
			self.expect(Kind.Lbrace)
			while not self.accept(Kind.Rbrace):
				decls.append(self.parse_decl())
			return ast.ExtendDecl(attrs, typ, bases, decls, pos)
		elif self.accept(Kind.KwFn):
			return self.parse_fn_decl(
			    doc_comment, attrs, vis,
			    attrs.has("unsafe")
			    or (self.inside_extern and self.extern_abi != sym.ABI.Rivet),
			    self.extern_abi if self.inside_extern else sym.ABI.Rivet
			)
		elif self.inside_struct_or_class_decl and self.accept(Kind.BitNot):
			# destructor
			pos = self.prev_tok.pos
			self.expect(Kind.KwSelfTy)
			self.expect(Kind.Lparen)
			self_is_mut = self.accept(Kind.KwMut)
			self.expect(Kind.KwSelf)
			self.expect(Kind.Rparen)
			self.expect(Kind.Lbrace)
			self.open_scope()
			sc = self.scope
			stmts = []
			while not self.accept(Kind.Rbrace):
				stmts.append(self.parse_stmt())
			self.close_scope()
			return ast.DestructorDecl(self_is_mut, sc, stmts, pos)
		elif self.accept(Kind.KwTest):
			pos = self.prev_tok.pos
			name = self.tok.lit
			self.expect(Kind.String)
			self.open_scope()
			sc = self.scope
			stmts = []
			self.expect(Kind.Lbrace)
			while not self.accept(Kind.Rbrace):
				stmts.append(self.parse_stmt())
			self.close_scope()
			return ast.TestDecl(sc, name, stmts, pos)
		elif self.tok.kind != Kind.EOF:
			report.error(f"expected declaration, found {self.tok}", pos)
			self.next()
		return ast.EmptyDecl()

	def parse_fn_decl(self, doc_comment, attrs, vis, is_unsafe, abi):
		pos = self.tok.pos
		if self.tok.kind.is_overloadable_op():
			name = str(self.tok.kind)
			self.next()
		else:
			name = self.parse_name()

		args = []
		is_method = False
		is_variadic = False
		self_is_mut = False
		has_named_args = False

		self.open_scope()
		sc = self.scope
		self.expect(Kind.Lparen)
		if self.tok.kind != Kind.Rparen:
			# receiver (`self`|`mut self`)
			if self.tok.kind == Kind.KwSelf or (
			    self.tok.kind == Kind.KwMut
			    and self.peek_tok.kind == Kind.KwSelf
			):
				is_method = True
				self_is_mut = self.accept(Kind.KwMut)
				self.expect(Kind.KwSelf)
				if self.tok.kind != Kind.Rparen:
					self.expect(Kind.Comma)
			# arguments
			while self.tok.kind != Kind.Rparen:
				if self.inside_extern and self.accept(Kind.Ellipsis):
					is_variadic = True
					break
				else:
					arg_pos = self.tok.pos
					arg_is_mut = self.accept(Kind.KwMut)
					arg_name = self.parse_name()
					self.expect(Kind.Colon)
					arg_typ = self.parse_type()
					is_variadic = isinstance(arg_typ, type.Variadic)
					arg_expr = self.empty_expr()
					if self.accept(Kind.Assign):
						has_named_args = True
						arg_expr = self.parse_expr()
					args.append(
					    sym.Arg(
					        arg_name, arg_is_mut, arg_typ, arg_expr,
					        not isinstance(arg_expr, ast.EmptyExpr), arg_pos
					    )
					)
				if not self.accept(Kind.Comma):
					break
		self.expect(Kind.Rparen)

		is_result = self.accept(Kind.Bang)
		ret_typ = self.parse_type()
		if is_result:
			ret_typ = type.Result(ret_typ)

		stmts = []
		has_body = True
		if (self.inside_trait
		    or self.inside_extern) and self.accept(Kind.Semicolon):
			has_body = False
		else:
			self.expect(Kind.Lbrace)
			while not self.accept(Kind.Rbrace):
				stmts.append(self.parse_stmt())
		self.close_scope()
		return ast.FnDecl(
		    doc_comment, attrs, vis, self.inside_extern, is_unsafe, name, pos,
		    args, ret_typ, stmts, sc, has_body, is_method, self_is_mut,
		    has_named_args,
		    isinstance(self.file_sym, sym.Pkg)
		    and self.file_sym.name == self.comp.prefs.pkg_name
		    and name == "main", is_variadic, abi
		)

	# ---- statements --------------------------
	def parse_stmt(self):
		if self.accept(Kind.KwLet):
			# variable declarations
			pos = self.prev_tok.pos
			lefts = []
			if self.accept(Kind.Lparen):
				# multiple variables
				while True:
					lefts.append(self.parse_var_decl(False))
					if not self.accept(Kind.Comma):
						break
				self.expect(Kind.Rparen)
			else:
				lefts.append(self.parse_var_decl(False))
			self.expect(Kind.Assign)
			right = self.parse_expr()
			self.expect(Kind.Semicolon)
			return ast.LetStmt(self.scope, lefts, right, pos)
		elif self.accept(Kind.KwWhile):
			pos = self.prev_tok.pos
			is_inf = False
			if self.accept(Kind.Lparen):
				if self.tok.kind == Kind.KwLet:
					self.open_scope()
					cond = self.parse_guard_expr()
				else:
					cond = self.parse_expr()
				self.expect(Kind.Rparen)
			else:
				cond = ast.BoolLiteral(True, self.tok.pos)
				is_inf = True
			stmt = self.parse_stmt()
			if isinstance(cond, ast.GuardExpr):
				self.close_scope()
			return ast.WhileStmt(cond, stmt, is_inf, pos)
		elif self.accept(Kind.KwFor):
			pos = self.prev_tok.pos
			self.expect(Kind.Lparen)
			self.open_scope()
			sc = self.scope
			vars = []
			# single or multiple variables
			while True:
				vars.append(self.parse_name())
				if not self.accept(Kind.Comma):
					break
			self.expect(Kind.KwIn)
			iterable = self.parse_expr()
			if self.accept(Kind.DotDot): # range
				is_inclusive = self.accept(Kind.Assign)
				max = self.parse_expr()
				iterable = ast.RangeExpr(
				    iterable, max, is_inclusive, iterable.pos
				)
			self.expect(Kind.Rparen)
			stmt = self.parse_stmt()
			self.close_scope()
			return ast.ForInStmt(sc, vars, iterable, stmt, pos)
		expr = self.parse_expr()
		if self.tok.kind.is_assign():
			# assignment
			op = self.tok.kind
			self.next()
			right = self.parse_expr()
			self.expect(Kind.Semicolon)
			return ast.AssignStmt(expr, op, right, expr.pos)
		elif not ((self.inside_block and self.tok.kind == Kind.Rbrace)
		          or expr.__class__ in (ast.IfExpr, ast.SwitchExpr, ast.Block)):
			self.expect(Kind.Semicolon)
		return ast.ExprStmt(expr, expr.pos)

	def parse_var_decl(self, inside_global = False, support_typ = True):
		is_mut = self.accept(Kind.KwMut)
		pos = self.tok.pos
		name = self.parse_name()
		has_typ = False
		typ = self.comp.void_t
		if support_typ and self.accept(Kind.Colon):
			typ = self.parse_type()
			has_typ = True
		return ast.ObjDecl(is_mut, name, has_typ, typ, sym.ObjLevel.Local, pos)

	# ---- expressions -------------------------
	def parse_expr(self):
		return self.parse_or_expr()

	def parse_or_expr(self):
		left = self.parse_and_expr()
		while self.accept(Kind.KwOr):
			right = self.parse_and_expr()
			left = ast.BinaryExpr(left, Kind.KwOr, right, left.pos)
		return left

	def parse_and_expr(self):
		left = self.parse_equality_expr()
		while self.accept(Kind.KwAnd):
			right = self.parse_equality_expr()
			left = ast.BinaryExpr(left, Kind.KwAnd, right, left.pos)
		return left

	def parse_equality_expr(self):
		left = self.parse_relational_expr()
		while True:
			if self.tok.kind in [Kind.Eq, Kind.Ne]:
				op = self.tok.kind
				self.next()
				right = self.parse_relational_expr()
				left = ast.BinaryExpr(left, op, right, left.pos)
			else:
				break
		return left

	def parse_relational_expr(self):
		left = self.parse_shift_expr()
		while True:
			if self.tok.kind in [
			    Kind.Gt, Kind.Lt, Kind.Ge, Kind.Le, Kind.OrElse
			]:
				op = self.tok.kind
				self.next()
				right = self.parse_shift_expr()
				left = ast.BinaryExpr(left, op, right, left.pos)
			elif self.tok.kind in [Kind.KwIs, Kind.KwNotIs]:
				op = self.tok.kind
				self.next()
				pos = self.tok.pos
				right = ast.TypeNode(self.parse_type(), pos)
				left = ast.BinaryExpr(left, op, right, left.pos)
			else:
				break
		return left

	def parse_shift_expr(self):
		left = self.parse_additive_expr()
		while True:
			if self.tok.kind in [Kind.Lt, Kind.Gt]:
				op = Kind.Lshift if self.tok.kind == Kind.Lt else Kind.Rshift
				if self.tok.pos.pos + 1 == self.peek_tok.pos.pos:
					self.next()
					self.next()
					right = self.parse_additive_expr()
					left = ast.BinaryExpr(left, op, right, left.pos)
				else:
					break
			elif self.tok.kind in [Kind.Amp, Kind.Pipe, Kind.Xor]:
				op = self.tok.kind
				self.next()
				right = self.parse_additive_expr()
				left = ast.BinaryExpr(left, op, right, left.pos)
			else:
				break
		return left

	def parse_additive_expr(self):
		left = self.parse_multiplicative_expr()
		while True:
			if self.tok.kind in [Kind.Plus, Kind.Minus]:
				op = self.tok.kind
				self.next()
				right = self.parse_multiplicative_expr()
				left = ast.BinaryExpr(left, op, right, left.pos)
			else:
				break
		return left

	def parse_multiplicative_expr(self):
		left = self.parse_unary_expr()
		while True:
			if self.tok.kind in [Kind.Mul, Kind.Div, Kind.Mod]:
				op = self.tok.kind
				self.next()
				right = self.parse_unary_expr()
				left = ast.BinaryExpr(left, op, right, left.pos)
			else:
				break
		return left

	def parse_unary_expr(self):
		expr = self.empty_expr()
		if (self.tok.kind in [Kind.Amp, Kind.Bang, Kind.BitNot, Kind.Minus]):
			op = self.tok.kind
			pos = self.tok.pos
			self.next()
			is_ref_mut = op == Kind.Amp and self.accept(Kind.KwMut)
			right = self.parse_unary_expr()
			expr = ast.UnaryExpr(right, op, is_ref_mut, pos)
		else:
			expr = self.parse_primary_expr()
		return expr

	def parse_primary_expr(self):
		expr = self.empty_expr()
		if self.tok.kind in [
		    Kind.KwTrue, Kind.KwFalse, Kind.Char, Kind.Number, Kind.String,
		    Kind.KwNone, Kind.KwSelf, Kind.KwBase, Kind.KwSelfTy
		]:
			expr = self.parse_literal()
		elif self.accept(Kind.Dollar):
			expr = self.parse_ident(True)
		elif self.tok.kind == Kind.Dot and self.peek_tok.kind == Kind.Name:
			pos = self.tok.pos
			self.next()
			expr = ast.EnumValueExpr(self.parse_name(), pos)
		elif self.accept(Kind.DoubleColon):
			pos = self.prev_tok.pos
			field_pos = self.tok.pos
			field_name = self.parse_name()
			expr = ast.PathExpr(True, None, field_name, pos, field_pos)
		elif self.tok.kind in (Kind.KwContinue, Kind.KwBreak):
			op = self.tok.kind
			pos = self.tok.pos
			self.next()
			expr = ast.BranchExpr(op, pos)
		elif self.accept(Kind.KwReturn):
			pos = self.prev_tok.pos
			has_expr = self.tok.kind not in (
			    Kind.Comma, Kind.Semicolon, Kind.Rbrace
			)
			if has_expr:
				expr = self.parse_expr()
			else:
				expr = self.empty_expr()
			expr = ast.ReturnExpr(expr, has_expr, pos)
		elif self.tok.kind == Kind.KwIf:
			expr = self.parse_if_expr()
		elif self.accept(Kind.KwSwitch):
			expr = self.parse_switch_expr()
		elif self.accept(Kind.Lparen):
			pos = self.prev_tok.pos
			if self.accept(Kind.Rparen):
				expr = self.empty_expr()
			else:
				e = self.parse_expr()
				if self.accept(Kind.Comma): # tuple
					exprs = [e]
					while True:
						exprs.append(self.parse_expr())
						if not self.accept(Kind.Comma):
							break
					self.expect(Kind.Rparen)
					if len(exprs) > 8:
						report.error(
						    "tuples can have a maximum of 8 expressions", pos
						)
					expr = ast.TupleLiteral(exprs, pos)
				else:
					self.expect(Kind.Rparen)
					expr = ast.ParExpr(e, e.pos)
		elif self.tok.kind in (Kind.KwUnsafe, Kind.Lbrace):
			# block expression
			pos = self.tok.pos
			is_unsafe = self.accept(Kind.KwUnsafe)
			self.expect(Kind.Lbrace)
			old_inside_block = self.inside_block
			self.inside_block = True
			stmts = []
			has_expr = False
			self.open_scope()
			sc = self.scope
			while not self.accept(Kind.Rbrace):
				stmt = self.parse_stmt()
				has_expr = isinstance(
				    stmt, ast.ExprStmt
				) and self.prev_tok.kind != Kind.Semicolon
				stmts.append(stmt)
			self.close_scope()
			if has_expr:
				expr = ast.Block(
				    sc, is_unsafe, stmts[:-1], stmts[-1].expr, True, pos
				)
			else:
				expr = ast.Block(sc, is_unsafe, stmts, None, False, pos)
			self.inside_block = old_inside_block
		elif self.accept(Kind.KwAs):
			self.expect(Kind.Lparen)
			typ = self.parse_type()
			self.expect(Kind.Comma)
			expr = self.parse_expr()
			self.expect(Kind.Rparen)
			expr = ast.AsExpr(expr, typ, expr.pos)
		elif self.tok.kind == Kind.Lbracket:
			elems = []
			pos = self.tok.pos
			self.next()
			if self.tok.kind != Kind.Rbracket:
				while True:
					elems.append(self.parse_expr())
					if not self.accept(Kind.Comma):
						break
			self.expect(Kind.Rbracket)
			expr = ast.ArrayLiteral(elems, pos)
		elif self.tok.kind == Kind.KwPkg:
			expr = self.parse_pkg_expr()
		elif self.tok.kind == Kind.Name and self.peek_tok.kind == Kind.Char:
			if self.tok.lit == "b":
				expr = self.parse_character_literal()
			else:
				report.error(
				    "only `b` is recognized as a valid prefix for a character literal",
				    self.tok.pos,
				)
				self.next()
		elif self.tok.kind == Kind.Name and self.peek_tok.kind == Kind.String:
			if self.tok.lit in ("c", "b", "r"):
				expr = self.parse_string_literal()
			else:
				report.error(
				    "only `c`, `b` and `r` are recognized as valid prefixes for a string literal",
				    self.tok.pos,
				)
				self.next()
		elif self.tok.kind == Kind.Name and self.peek_tok.kind == Kind.Bang: # builtin call
			name = self.parse_name()
			self.expect(Kind.Bang)
			self.expect(Kind.Lparen)
			args = []
			if name in ("size_of", "align_of"):
				pos = self.tok.pos
				args.append(ast.TypeNode(self.parse_type(), pos))
			elif self.tok.kind != Kind.Rparen:
				while True:
					args.append(self.parse_expr())
					if not self.accept(Kind.Comma):
						break
			self.expect(Kind.Rparen)
			expr = ast.BuiltinCallExpr(name, args, expr.pos)
		else:
			expr = self.parse_ident()

		while True:
			if self.accept(Kind.Lparen):
				args = []
				if self.tok.kind != Kind.Rparen:
					expecting_named_arg = False
					while True:
						if (
						    self.tok.kind == Kind.Name
						    and self.peek_tok.kind == Kind.Colon
						):
							# named argument
							name_p = self.tok.pos
							name = self.parse_name()
							self.expect(Kind.Colon)
							expr2 = self.parse_expr()
							args.append(ast.CallArg(expr2, name_p, name))
							expecting_named_arg = True
						else:
							if expecting_named_arg:
								report.error(
								    "expected named argument, found single expression",
								    self.tok.pos
								)
							expr2 = self.parse_expr()
							args.append(ast.CallArg(expr2, expr2.pos))
						if not self.accept(Kind.Comma):
							break
				self.expect(Kind.Rparen)
				err_handler_pos = self.tok.pos
				is_propagate = False
				varname = ""
				varname_pos = self.tok.pos
				err_expr = None
				has_err_expr = False
				if self.tok.kind == Kind.Dot and self.peek_tok.kind == Kind.Bang:
					# check result value, if error propagate
					err_handler_pos = self.peek_tok.pos
					self.advance(2)
					is_propagate = True
				elif self.accept(Kind.KwCatch):
					if self.accept(Kind.Pipe):
						varname_pos = self.tok.pos
						varname = self.parse_name()
						self.expect(Kind.Pipe)
					err_expr = self.parse_expr()
					has_err_expr = True
				expr = ast.CallExpr(
				    expr, args,
				    ast.CallErrorHandler(
				        is_propagate, varname, err_expr, has_err_expr,
				        varname_pos, self.scope, err_handler_pos
				    ), expr.pos
				)
			elif self.accept(Kind.Lbracket):
				is_mut = self.accept(Kind.KwMut)
				index = self.empty_expr()
				if self.accept(Kind.DotDot):
					if self.tok.kind == Kind.Rbracket:
						index = ast.RangeExpr(
						    None, None, False, index.pos, False, False
						)
					else:
						index = ast.RangeExpr(
						    None, self.parse_expr(), False, index.pos, False,
						    True
						)
				else:
					index = self.parse_expr()
					if self.accept(Kind.DotDot):
						if self.tok.kind == Kind.Rbracket:
							index = ast.RangeExpr(
							    index, None, False, index.pos, True, False
							)
						else:
							index = ast.RangeExpr(
							    index, self.parse_expr(), False, index.pos,
							    True, True
							)
				self.expect(Kind.Rbracket)
				if is_mut and not isinstance(index, ast.RangeExpr):
					report.error(
					    "only slices can be marked as mutable", expr.pos
					)
				expr = ast.IndexExpr(expr, index, is_mut, expr.pos)
			elif self.accept(Kind.Dot):
				if self.accept(Kind.Mul):
					expr = ast.SelectorExpr(
					    expr, "", expr.pos, self.prev_tok.pos,
					    is_indirect = True
					)
				elif self.accept(Kind.Question):
					# check optional value, if none panic
					expr = ast.SelectorExpr(
					    expr, "", expr.pos, self.prev_tok.pos,
					    is_nonecheck = True
					)
				else:
					field_pos = self.tok.pos
					if self.tok.kind == Kind.Number:
						name = self.tok.lit
						self.next()
					elif self.tok.kind.is_overloadable_op():
						name = str(self.tok.kind)
						self.next()
					else:
						name = self.parse_name()
					expr = ast.SelectorExpr(expr, name, expr.pos, field_pos)
			elif self.tok.kind == Kind.DoubleColon:
				expr = self.parse_path_expr(expr)
			else:
				break
		return expr

	def parse_if_expr(self):
		branches = []
		has_else = False
		pos = self.tok.pos
		while self.tok.kind in (Kind.KwIf, Kind.KwElse):
			if self.accept(Kind.KwElse) and self.tok.kind != Kind.KwIf:
				branches.append(
				    ast.IfBranch(
				        self.empty_expr(), self.parse_expr(), True, Kind.KwElse
				    )
				)
				has_else = True
				break
			self.next()
			self.expect(Kind.Lparen)
			if self.tok.kind == Kind.KwLet:
				self.open_scope()
				cond = self.parse_guard_expr()
			else:
				cond = self.parse_expr()
			self.expect(Kind.Rparen)
			branches.append(
			    ast.IfBranch(cond, self.parse_expr(), False, Kind.KwIf)
			)
			if isinstance(cond, ast.GuardExpr):
				self.close_scope()
			if self.tok.kind != Kind.KwElse:
				break
		return ast.IfExpr(branches, has_else, pos)

	def parse_switch_expr(self):
		branches = []
		pos = self.prev_tok.pos
		is_typeswitch = False
		if self.accept(Kind.Lparen):
			if self.tok.kind == Kind.KwLet:
				self.open_scope()
				expr = self.parse_guard_expr()
			else:
				expr = self.parse_expr()
			self.expect(Kind.Rparen)
			is_typeswitch = self.accept(Kind.KwIs)
		else:
			expr = ast.BoolLiteral(True, pos)
		self.expect(Kind.Lbrace)
		while True:
			pats = []
			is_else = self.accept(Kind.KwElse)
			if not is_else:
				while True:
					if is_typeswitch:
						pos = self.tok.pos
						pats.append(ast.TypeNode(self.parse_type(), pos))
					else:
						pats.append(self.parse_expr())
					if not self.accept(Kind.Comma):
						break
			self.expect(Kind.Arrow)
			branches.append(ast.SwitchBranch(pats, self.parse_expr(), is_else))
			if not self.accept(Kind.Comma):
				break
		self.expect(Kind.Rbrace)
		if isinstance(expr, ast.GuardExpr): self.close_scope()
		return ast.SwitchExpr(expr, branches, is_typeswitch, self.scope, pos)

	def parse_guard_expr(self):
		self.expect(Kind.KwLet)
		pos = self.prev_tok.pos
		vars = []
		while True:
			vars.append(self.parse_name())
			if not self.accept(Kind.Comma):
				break
		self.expect(Kind.Assign)
		e = self.parse_expr()
		if self.accept(Kind.Semicolon):
			has_cond = True
			cond = self.parse_expr()
		else:
			has_cond = False
			cond = self.empty_expr()
		return ast.GuardExpr(vars, e, has_cond, cond, self.scope, pos)

	def parse_path_expr(self, left):
		self.expect(Kind.DoubleColon)
		pos = self.tok.pos
		name = self.parse_name()
		expr = ast.PathExpr(False, left, name, left.pos, pos)
		expr.is_last = self.tok.kind != Kind.DoubleColon
		return expr

	def parse_literal(self):
		if self.tok.kind in [Kind.KwTrue, Kind.KwFalse]:
			pos = self.tok.pos
			lit = self.tok.kind == Kind.KwTrue
			self.next()
			return ast.BoolLiteral(lit, pos)
		elif self.tok.kind == Kind.Char:
			return self.parse_character_literal()
		elif self.tok.kind == Kind.Number:
			return self.parse_integer_literal()
		elif self.tok.kind == Kind.String:
			return self.parse_string_literal()
		elif self.accept(Kind.KwNone):
			return ast.NoneLiteral(self.prev_tok.pos)
		elif self.accept(Kind.KwSelf):
			return ast.SelfExpr(self.scope, self.prev_tok.pos)
		elif self.accept(Kind.KwSelfTy):
			return ast.SelfTyExpr(self.scope, self.prev_tok.pos)
		elif self.accept(Kind.KwBase):
			return ast.BaseExpr(self.scope, self.prev_tok.pos)
		elif self.accept(Kind.KwBaseTy):
			return ast.BaseTyExpr(self.scope, self.prev_tok.pos)
		else:
			report.error(f"expected literal, found {self.tok}", self.tok.pos)
		return self.empty_expr()

	def parse_integer_literal(self):
		pos = self.tok.pos
		lit = self.tok.lit
		node = ast.FloatLiteral(lit, pos) if lit[:2] not in [
		    '0x', '0o', '0b'
		] and utils.index_any(lit,
		                      ".eE") >= 0 else ast.IntegerLiteral(lit, pos)
		self.next()
		return node

	def parse_character_literal(self):
		is_byte = False
		if self.tok.kind == Kind.Name:
			is_byte = self.tok.lit == "b"
			self.expect(Kind.Name)
		lit = self.tok.lit
		pos = self.tok.pos
		self.expect(Kind.Char)
		return ast.CharLiteral(lit, pos, is_byte)

	def parse_string_literal(self):
		is_raw = False
		is_bytestr = False
		is_cstr = False
		if self.tok.kind == Kind.Name:
			is_raw = self.tok.lit == "r"
			is_bytestr = self.tok.lit == "b"
			is_cstr = self.tok.lit == "c"
			self.expect(Kind.Name)
		lit = self.tok.lit
		pos = self.tok.pos
		self.expect(Kind.String)
		while self.accept(Kind.String):
			lit += self.prev_tok.lit
		return ast.StringLiteral(lit, is_raw, is_bytestr, is_cstr, pos)

	def parse_ident(self, is_comptime = False):
		pos = self.tok.pos
		name = self.parse_name()
		sc = self.scope
		if sc == None:
			sc = sym.Scope(sc)
		id = ast.Ident(name, pos, sc, is_comptime)
		return id

	def parse_pkg_expr(self):
		pos = self.tok.pos
		self.next()
		return ast.PkgExpr(pos)

	def empty_expr(self):
		return ast.EmptyExpr(self.tok.pos)

	# ---- types -------------------------------
	def parse_type(self):
		pos = self.tok.pos
		if self.accept(Kind.Question):
			# optional
			typ = self.parse_type()
			if isinstance(typ, type.Ptr):
				report.error("pointers cannot be optional", pos)
				report.note("by default pointers can contain the value `none`")
			elif isinstance(typ, type.Optional):
				report.error("optional multi-level types are not allowed", pos)
			return type.Optional(typ)
		elif self.tok.kind in (Kind.KwExtern, Kind.KwFn):
			# function types
			is_extern = self.accept(Kind.KwExtern)
			abi = self.parse_abi() if is_extern else sym.ABI.Rivet
			if is_extern and not self.inside_extern: self.inside_extern = True
			args = []
			is_variadic = False
			self.expect(Kind.KwFn)
			self.expect(Kind.Lparen)
			if self.tok.kind != Kind.Rparen:
				while True:
					if is_extern and self.accept(Kind.Ellipsis):
						is_variadic = True
						break
					pos = self.tok.pos
					is_mut = self.accept(Kind.KwMut)
					arg_typ = self.parse_type()
					args.append(
					    sym.Arg(
					        f"arg{len(args)}", is_mut, arg_typ, None, False, pos
					    )
					)
					if not self.accept(Kind.Comma):
						break
			self.expect(Kind.Rparen)
			ret_typ = self.parse_type()
			if is_extern and self.inside_extern:
				self.inside_extern = False
			return type.Fn(
			    is_extern, abi, False, args, is_variadic, ret_typ, False
			)
		elif self.accept(Kind.Amp):
			# references
			is_mut = self.accept(Kind.KwMut)
			typ = self.parse_type()
			return type.Ref(typ, is_mut)
		elif self.accept(Kind.Mul):
			# pointers
			is_mut = self.accept(Kind.KwMut)
			typ = self.parse_type()
			return type.Ptr(typ, is_mut)
		elif self.accept(Kind.Lbracket):
			# arrays or slices
			mut_pos = self.tok.pos
			is_mut = self.accept(Kind.KwMut)
			typ = self.parse_type()
			if self.accept(Kind.Semicolon):
				if is_mut:
					report.error("arrays are mutable by default", mut_pos)
				size = self.parse_expr()
				self.expect(Kind.Rbracket)
				return type.Array(typ, size)
			self.expect(Kind.Rbracket)
			return type.Slice(typ, is_mut)
		elif self.accept(Kind.Lparen):
			# tuples
			types = []
			while True:
				types.append(self.parse_type())
				if not self.accept(Kind.Comma):
					break
			if len(types) > 8:
				report.error("tuples can have a maximum of 8 types", pos)
				report.help("you can use a struct instead")
			self.expect(Kind.Rparen)
			return type.Tuple(types)
		elif self.accept(Kind.Ellipsis):
			return type.Variadic(self.parse_type())
		elif self.accept(Kind.KwSelfTy):
			return type.Type.unresolved(
			    ast.SelfTyExpr(self.scope, self.prev_tok.pos)
			)
		elif self.accept(Kind.KwNone):
			return self.comp.none_t
		elif self.tok.kind in (Kind.KwPkg, Kind.Name):
			if self.peek_tok.kind == Kind.DoubleColon:
				# normal type
				path_expr = self.parse_path_expr(
				    self.parse_pkg_expr() if self.tok.kind ==
				    Kind.KwPkg else self.parse_ident()
				)
				if self.tok.kind == Kind.DoubleColon:
					while True:
						path_expr = self.parse_path_expr(path_expr)
						if self.tok.kind != Kind.DoubleColon:
							break
				return type.Type.unresolved(path_expr)
			elif self.tok.kind == Kind.Name:
				prev_tok_kind = self.prev_tok.kind
				expr = self.parse_ident()
				lit = expr.name
				if lit == "void":
					return self.comp.void_t
				elif lit == "never":
					if prev_tok_kind != Kind.Rparen and self.tok.kind != Kind.Lbrace:
						report.error("invalid use of `never` type", pos)
					return self.comp.never_t
				elif lit == "bool":
					return self.comp.bool_t
				elif lit == "rune":
					return self.comp.rune_t
				elif lit == "i8":
					return self.comp.i8_t
				elif lit == "i16":
					return self.comp.i16_t
				elif lit == "i32":
					return self.comp.i32_t
				elif lit == "i64":
					return self.comp.i64_t
				elif lit == "isize":
					return self.comp.isize_t
				elif lit == "u8":
					return self.comp.u8_t
				elif lit == "u16":
					return self.comp.u16_t
				elif lit == "u32":
					return self.comp.u32_t
				elif lit == "u64":
					return self.comp.u64_t
				elif lit == "usize":
					return self.comp.usize_t
				elif lit == "f32":
					return self.comp.f32_t
				elif lit == "f64":
					return self.comp.f64_t
				elif lit == "string":
					return self.comp.string_t
				# only available in `core`:
				elif lit == "untyped_int":
					return self.comp.untyped_int_t
				elif lit == "untyped_float":
					return self.comp.untyped_float_t
				else:
					return type.Type.unresolved(expr)
			else:
				report.error("expected type, found keyword `pkg`", pos)
				self.next()
		else:
			report.error(f"expected type, found {self.tok}", pos)
			self.next()
		return type.Type.unresolved(self.empty_expr())
