# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .token import Kind
from .sym import TypeKind
from . import ast, sym, type, report, utils

class Checker:
	def __init__(self, comp):
		self.comp = comp
		self.source_file = None

		self.sym = None
		self.cur_fn = None

		self.expected_type = self.comp.void_t
		self.void_types = (self.comp.void_t, self.comp.never_t)
		self.unsafe_ops = 0
		self.inside_unsafe = False

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
				self.check_expr(decl.expr)
			elif isinstance(decl, ast.LetDecl):
				self.check_expr(decl.right)
			elif isinstance(decl, ast.TypeDecl):
				pass
			elif isinstance(decl, ast.EnumDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.TraitDecl):
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.ClassDecl):
				has_base = False
				for base in decl.bases:
					base_sym = base.symbol()
					if base_sym.kind == TypeKind.Class:
						if has_base:
							report.error(
							    "classes cannot have multiple base classes",
							    decl.pos
							)
						else:
							has_base = True
					elif base_sym.kind == TypeKind.Trait:
						pass
					else:
						report.error(
						    f"base type `{base}` of class `{decl.name}` is not a class",
						    decl.pos
						)
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.StructDecl):
				for base in decl.bases:
					base_sym = base.symbol()
					if base_sym.kind == TypeKind.Struct:
						pass
					elif base_sym.kind == TypeKind.Trait:
						pass
					else:
						report.error(
						    f"base type `{base}` of struct `{decl.name}` is not a struct",
						    decl.pos
						)
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.FieldDecl):
				if decl.has_def_expr:
					old_expected_type = self.expected_type
					self.check_expr(decl.def_expr)
					self.expected_type = old_expected_type
			elif isinstance(decl, ast.ExtendDecl):
				self_sym = decl.typ.symbol()
				has_base_class = False
				for base in decl.bases:
					base_sym = base.symbol()
					base_kind = str(base_sym.kind)
					if base_sym.kind == TypeKind.Class:
						if self_sym.kind != TypeKind.Class:
							report.error(
							    "only classes can inherit from other classes",
							    decl.pos
							)
							return
						if has_base_class:
							report.error(
							    "classes cannot have multiple base classes",
							    decl.pos
							)
							return
						has_base_class = True
					elif base_sym.kind == TypeKind.Struct:
						if self_sym.kind != TypeKind.Struct:
							report.error(
							    "only structs can inherit from other structs",
							    decl.pos
							)
							return
					elif base_sym.kind == TypeKind.Trait:
						pass
					else:
						report.error(
						    f"base type `{base}` of {base_kind} `{self_sym.name}` is not a {base_kind}",
						    decl.pos
						)
				self.check_decls(decl.decls)
			elif isinstance(decl, ast.FnDecl):
				old_expected_type = self.expected_type
				self.cur_fn = decl.sym
				self.expected_type = decl.ret_typ
				self.check_stmts(decl.stmts)
				self.expected_type = old_expected_type
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
			if len(stmt.lefts) == 1:
				if stmt.lefts[0].has_typ:
					self.expected_type = stmt.lefts[0].typ
				right_typ = self.check_expr(stmt.right)
				if right_typ == self.comp.void_t:
					report.error("void expression used as value", stmt.pos)
				if stmt.lefts[0].has_typ:
					try:
						self.check_types(right_typ, self.expected_type)
					except utils.CompilerError as e:
						report.error(e.args[0], stmt.pos)
					self.expected_type = self.comp.void_t
				else:
					right_typ = self.comp.untyped_to_type(right_typ)
					stmt.lefts[0].typ = right_typ
					stmt.scope.update_type(stmt.lefts[0].name, right_typ)
			else:
				right_typ = self.check_expr(stmt.right)
				symbol = right_typ.symbol()
				if symbol.kind != TypeKind.Tuple:
					report.error(
					    f"expected tuple value, found `{right_typ}`",
					    stmt.right.pos
					)
				elif len(stmt.lefts) != len(symbol.info.types):
					report.error(
					    f"expected {len(stmt.lefts)} values, found {len(symbol.info.types)}",
					    stmt.right.pos
					)
				else:
					for i, vd in enumerate(stmt.lefts):
						if vd.name == "_" and stmt.op != Kind.Assign:
							report.error(
							    "cannot modify blank identifier (`_`)", vd.pos
							)
						if not vd.has_typ:
							vtyp = self.comp.untyped_to_type(
							    symbol.info.types[i]
							)
							vd.typ = vtyp
							stmt.scope.update_type(vd.name, vtyp)
		elif isinstance(stmt, ast.ExprStmt):
			expr_typ = self.check_expr(stmt.expr)
			if not (
			    isinstance(expr_typ, type.Result) and expr_typ.typ
			    in self.void_types or isinstance(expr_typ, type.Optional) and
			    expr_typ.typ in self.void_types or expr_typ in self.void_types
			):
				report.warn("expression evaluated but not used", stmt.expr.pos)
		elif isinstance(stmt, ast.WhileStmt):
			if not stmt.is_inf and self.check_expr(
			    stmt.cond
			) != self.comp.bool_t:
				if not isinstance(stmt.cond, ast.GuardExpr):
					report.error(
					    "non-boolean expression used as `while` condition",
					    stmt.cond.pos
					)
			self.check_stmt(stmt.stmt)
		elif isinstance(stmt, ast.ForInStmt):
			iterable_t = self.check_expr(stmt.iterable)
			iterable_sym = iterable_t.symbol()
			vars_len = len(stmt.vars)
			if isinstance(stmt.iterable, ast.RangeExpr):
				if vars_len == 1:
					stmt.scope.update_type(
					    stmt.vars[0], self.comp.untyped_to_type(iterable_t)
					)
				else:
					report.error(
					    f"expected 1 variable, found {vars_len}", stmt.pos
					)
				self.check_stmt(stmt.stmt)
			elif iterable_sym.kind in (
			    TypeKind.Array, TypeKind.Slice, TypeKind.String
			):
				elem_typ = self.comp.u8_t if iterable_sym.kind == TypeKind.String else self.comp.untyped_to_type(
				    iterable_sym.info.elem_typ
				)
				if vars_len == 1:
					stmt.scope.update_type(stmt.vars[0], elem_typ)
				else:
					stmt.scope.update_type(stmt.vars[0], self.comp.usize_t)
					stmt.scope.update_type(stmt.vars[1], elem_typ)
				self.check_stmt(stmt.stmt)
			else:
				report.error(
				    f"`{iterable_t}` is not an iterable type", stmt.iterable.pos
				)
				report.note("expected array, slice or string value")

	def check_expr(self, expr):
		if isinstance(expr, ast.EmptyExpr):
			pass # error raised in `Resolver`
		elif isinstance(expr, ast.TypeNode):
			return expr.typ
		elif isinstance(expr, ast.AssignExpr):
			expr.typ = self.comp.void_t
			left_t = self.check_expr(expr.left)
			old_expected_type = self.expected_type
			self.expected_type = left_t
			right_t = self.check_expr(expr.right)
			if right_t == self.comp.void_t:
				report.error("void expression used as value", expr.right.pos)
			self.expected_type = old_expected_type
			if isinstance(expr.left, ast.Ident) and (expr.left.name == "_"):
				return expr.typ
			try:
				self.check_types(right_t, left_t)
			except utils.CompilerError as e:
				report.error(e.args[0], expr.right.pos)
			return expr.typ
		elif isinstance(expr, ast.EnumValueExpr):
			expr.typ = self.comp.void_t
			_sym = self.expected_type.symbol()
			if _sym.kind == TypeKind.Enum:
				if v := _sym.info.get_value(expr.value):
					expr.info = v
					expr.typ = type.Type(_sym)
				else:
					report.error(
					    f"enum `{_sym.name}` has no value `{expr.value}`",
					    expr.pos
					)
			else:
				report.error(f"`{_sym.name}` is not a enum", expr.pos)
			return expr.typ
		elif isinstance(expr, ast.Ident):
			if expr.name == "_":
				expr.typ = self.comp.void_t
			elif expr.is_comptime:
				expr.typ = self.comp.string_t
			elif expr.is_obj:
				expr.typ = expr.obj.typ
			elif isinstance(expr.sym, sym.Fn):
				expr.sym.uses += 1
				expr.typ = expr.sym.typ()
			elif isinstance(expr.sym, sym.Const):
				expr.typ = expr.sym.typ
			elif isinstance(expr.sym, sym.Var):
				if (
				    expr.sym.is_mut or
				    (expr.sym.is_extern and expr.sym.abi != sym.ABI.Rivet)
				) and not self.inside_unsafe_block():
					if expr.sym.is_extern:
						report.error(
						    "use of external objects is unsafe and requires `unsafe` block",
						    expr.pos
						)
					else:
						report.error(
						    "use of mutable global objects is unsafe and requires `unsafe` block",
						    expr.pos
						)
						report.note(
						    "mutable global objects can be mutated by multiple threads: "
						    "aliasing violations or data races will cause undefined behavior"
						)
				expr.sym.uses += 1
				expr.typ = expr.sym.typ
			else:
				expr.typ = self.comp.void_t
			if isinstance(expr.typ,
			              type.Ptr) and not self.inside_unsafe_block():
				report.error(
				    "pointers are usable only inside `unsafe` blocks", expr.pos
				)
			return expr.typ
		elif isinstance(expr, ast.SelfExpr):
			return expr.typ
		elif isinstance(expr, ast.SelfTyExpr):
			return type.Type(expr.sym)
		elif isinstance(expr, ast.BaseExpr):
			return expr.typ
		elif isinstance(expr, ast.BaseTyExpr):
			return type.Type(expr.sym)
		elif isinstance(expr, ast.NoneLiteral):
			return self.comp.none_t
		elif isinstance(expr, ast.BoolLiteral):
			return self.comp.bool_t
		elif isinstance(expr, ast.CharLiteral):
			if expr.is_byte:
				return self.comp.u8_t
			return self.comp.rune_t
		elif isinstance(expr, ast.IntegerLiteral):
			return self.comp.untyped_int_t
		elif isinstance(expr, ast.FloatLiteral):
			return self.comp.untyped_float_t
		elif isinstance(expr, ast.StringLiteral):
			if expr.is_bytestr:
				return type.Type(
				    self.comp.universe.add_or_get_array(
				        self.comp.u8_t,
				        utils.bytestr(expr.lit).len
				    )
				)
			elif expr.is_cstr:
				return type.Ptr(self.comp.u8_t, False)
			return self.comp.string_t
		elif isinstance(expr, ast.TupleLiteral):
			types = []
			old_expected_type = self.expected_type
			expected_type_sym = self.expected_type.symbol()
			if expected_type_sym.kind == TypeKind.Tuple:
				expected_types = expected_type_sym.info.types
				has_expected = len(expected_types) == len(expr.exprs)
			else:
				has_expected = False
				expected_types = []
			for i, e in enumerate(expr.exprs):
				if has_expected:
					self.expected_type = expected_types[i]
				tt = self.comp.untyped_to_type(self.check_expr(e))
				if tt == self.comp.void_t:
					report.error("void expression used as value", e.pos)
				if has_expected:
					self.expected_type = old_expected_type
					types.append(expected_types[i])
				else:
					types.append(tt)
			expr.typ = type.Type(self.comp.universe.add_or_get_tuple(types))
			return expr.typ
		elif isinstance(expr, ast.ArrayLiteral):
			old_exp_typ = self.expected_type
			has_exp_typ = False
			size = ""
			if not isinstance(self.expected_type, type.Fn):
				elem_sym = self.expected_type.symbol()
				if elem_sym.kind == TypeKind.Array:
					has_exp_typ = True
					elem_typ = elem_sym.info.elem_typ
					size = elem_sym.info.size.lit
					self.expected_type = elem_typ
				else:
					elem_typ = self.comp.void_t
			else:
				elem_typ = self.comp.void_t
			for i, e in enumerate(expr.elems):
				typ = self.check_expr(e)
				if typ == self.comp.void_t:
					report.error("void expression used as value", e.pos)
				if i == 0 and not has_exp_typ:
					elem_typ = typ
					self.expected_type = elem_typ
				else:
					try:
						self.check_types(typ, elem_typ)
					except utils.CompilerError as err:
						report.error(err.args[0], e.pos)
						report.note(f"in element {i + 1} of array literal")
			if len(expr.elems) > 0:
				arr_len = str(len(expr.elems))
			else:
				if not has_exp_typ:
					report.error(
					    "could not infer type and size of array", expr.pos
					)
				arr_len = size
			expr.typ = type.Type(
			    self.comp.universe.add_or_get_array(
			        self.comp.untyped_to_type(elem_typ),
			        ast.IntegerLiteral(arr_len, expr.pos)
			    )
			)
			self.expected_type = old_exp_typ
			return expr.typ
		elif isinstance(expr, ast.AsExpr):
			old_exp_typ = self.expected_type
			self.expected_type = expr.typ
			self.check_expr(expr.expr)
			self.expected_type = old_exp_typ
			return expr.typ
		elif isinstance(expr, ast.GuardExpr):
			expr_t = self.check_expr(expr.expr)
			if isinstance(expr_t, (type.Result, type.Optional)):
				expr.is_result = isinstance(expr_t, type.Result)
				expr.scope.update_type(expr.vars[0], expr_t.typ)
				expr.typ = expr_t.typ
			else:
				report.error("expected result or optional value", expr.expr.pos)
				expr.typ = self.comp.void_t
			if expr.has_cond:
				if self.check_expr(expr.cond) != self.comp.bool_t:
					report.error(
					    "guard condition must be boolean", expr.cond.pos
					)
			return expr.typ
		elif isinstance(expr, ast.UnaryExpr):
			expr.typ = self.check_expr(expr.right)
			expr.right_typ = expr.typ
			if expr.op == Kind.Bang:
				if expr.typ != self.comp.bool_t:
					report.error(
					    "operator `!` can only be used with boolean values",
					    expr.pos
					)
			elif expr.op == Kind.BitNot:
				if not self.comp.is_int(expr.typ):
					report.error(
					    "operator `~` can only be used with numeric values",
					    expr.pos
					)
			elif expr.op == Kind.Minus:
				if self.comp.is_unsigned_int(expr.typ):
					report.error(
					    f"cannot apply unary operator `-` to type `{expr.typ}`",
					    expr.pos
					)
					report.note("unsigned values cannot be negated")
				elif not self.comp.is_signed_int(expr.typ):
					report.error(
					    "operator `-` can only be used with signed values",
					    expr.pos
					)
			elif expr.op == Kind.Amp:
				right = expr.right
				if isinstance(right, ast.ParExpr):
					right = right.expr
				if isinstance(right, ast.IndexExpr):
					if isinstance(right.left_typ, type.Ptr):
						report.error(
						    "cannot reference a pointer indexing", expr.pos
						)
				elif isinstance(expr.typ, type.Ref):
					report.error(
					    "cannot take the address of other reference", expr.pos
					)
				elif expr.typ == self.comp.error_t:
					report.error(
					    "cannot take the address of a error value", expr.pos
					)
				elif expr.is_ref_mut:
					self.check_expr_is_mut(right)
				expr.typ = type.Ref(expr.typ, expr.is_ref_mut)
			return expr.typ
		elif isinstance(expr, ast.BinaryExpr):
			ltyp = self.check_expr(expr.left)
			old_expected_type = self.expected_type
			self.expected_type = ltyp
			rtyp = self.check_expr(expr.right)
			self.expected_type = old_expected_type

			if expr.op in (
			    Kind.Plus, Kind.Minus, Kind.Mul, Kind.Div, Kind.Mod, Kind.Xor,
			    Kind.Amp, Kind.Pipe
			):
				if isinstance(ltyp, type.Ptr):
					report.error("pointer arithmetic is not allowed", expr.pos)
					if expr.op == Kind.Plus:
						report.help(
						    "use the `ptr_add` builtin function instead"
						)
					elif expr.op == Kind.Minus:
						report.help(
						    "use the `ptr_diff` builtin function instead"
						)
				elif isinstance(ltyp, type.Ref):
					report.error(
					    "cannot use arithmetic operations with references",
					    expr.pos
					)

			return_type = ltyp
			if expr.op in (
			    Kind.Plus, Kind.Minus, Kind.Mul, Kind.Div, Kind.Mod, Kind.Xor,
			    Kind.Amp, Kind.Pipe
			):
				promoted_type = self.comp.void_t
				if isinstance(ltyp, type.Ptr) and isinstance(
				    rtyp, type.Ptr
				) and expr.op == Kind.Minus:
					promoted_type = self.comp.isize_t
				else:
					lsym = ltyp.symbol()
					if lsym.kind == TypeKind.Struct:
						if op_method := lsym.find(str(expr.op)):
							promoted_type = op_method.ret_typ
						else:
							report.error(
							    f"undefined operation `{ltyp}` {expr.op} `{rtyp}`",
							    expr.pos
							)
					else:
						promoted_type = self.promote(ltyp, rtyp)
						if promoted_type == self.comp.void_t:
							report.error(
							    f"mismatched types `{ltyp}` and `{rtyp}`",
							    expr.pos
							)
						elif isinstance(promoted_type, type.Optional):
							report.error(
							    f"operator `{expr.op}` cannot be used with `{promoted_type}`",
							    expr.pos
							)

				return_type = promoted_type
			elif expr.op == Kind.OrElse:
				if isinstance(ltyp, type.Optional):
					if not self.check_compatible_types(
					    rtyp, ltyp.typ
					) and rtyp != self.comp.never_t:
						report.error(
						    f"expected type `{ltyp.typ}`, found `{rtyp}`",
						    expr.right.pos
						)
						report.note("in right operand for operator `orelse`")
					expr.typ = ltyp.typ
				else:
					report.error(
					    "expected optional value in left operand for operator `orelse`",
					    expr.pos
					)
					expr.typ = ltyp
				return expr.typ
			elif expr.op in (Kind.KwIs, Kind.KwNotIs):
				lsym = ltyp.symbol()
				if lsym.kind != TypeKind.Class:
					report.error(
					    f"`{expr.op}` can only be used with classes",
					    expr.left.pos
					)
				expr.typ = self.comp.bool_t
				return expr.typ
			elif expr.op in (Kind.KwAnd, Kind.KwOr):
				if ltyp != self.comp.bool_t:
					report.error(
					    f"non-boolean expression in left operand for `{expr.op}`",
					    expr.left.pos
					)
				elif rtyp != self.comp.bool_t:
					report.error(
					    f"non-boolean expression in right operand for `{expr.op}`",
					    expr.right.pos
					)
				elif isinstance(expr.left, ast.BinaryExpr):
					if expr.left.op != expr.op and expr.left.op in (
					    Kind.KwAnd, Kind.KwOr
					):
						# use `(a and b) or c` instead of `a and b or c`
						report.error("ambiguous boolean expression", expr.pos)
						report.help(
						    f"use `({expr.left}) {expr.op} {expr.right}` instead"
						)
				expr.typ = self.comp.bool_t
				return expr.typ
			elif expr.op in (Kind.Lshift, Kind.Rshift):
				if not self.comp.is_int(ltyp):
					report.error(f"shift on type `{ltyp}`", expr.left.pos)
				elif not self.comp.is_int(rtyp):
					report.error(
					    f"cannot shift non-integer type `{rtyp}` into type `{ltyp}`",
					    expr.right.pos
					)
				elif expr.op == Kind.Lshift and self.comp.is_signed_int(
				    ltyp
				) and not self.inside_unsafe_block():
					report.warn(
					    f"shifting a value from a signed type `{ltyp}` can change the sign",
					    expr.left.pos
					)
				expr.typ = ltyp
				return expr.typ

			if ltyp == self.comp.bool_t and rtyp == self.comp.bool_t and expr.op not in (
			    Kind.Eq, Kind.Ne, Kind.KwAnd, Kind.KwOr, Kind.Pipe, Kind.Amp
			):
				report.error(
				    "boolean values only support the following operators: `==`, `!=`, `and`, `or`, `&` and `|`",
				    expr.pos
				)
			elif ltyp == self.comp.string_t and rtyp == self.comp.string_t and expr.op not in (
			    Kind.Eq, Kind.Ne, Kind.Lt, Kind.Gt, Kind.Le, Kind.Ge
			):
				report.error(
				    "string values only support the following operators: `==`, `!=`, `<`, `>`, `<=` and `>=`",
				    expr.pos
				)
			elif ltyp == self.comp.error_t and expr.op not in (
			    Kind.KwIs, Kind.KwNotIs
			):
				report.error(
				    "error values only support `is` and `!is`", expr.pos
				)

			if not self.check_compatible_types(rtyp, ltyp):
				if ltyp == self.comp.void_t or rtyp == self.comp.void_t or return_type == self.comp.void_t:
					expr.typ = return_type
					return expr.typ
				report.error(
				    f"expected type `{ltyp}`, found `{rtyp}`", expr.right.pos
				)

			if expr.op.is_relational():
				expr.typ = self.comp.bool_t
			else:
				expr.typ = return_type
			return expr.typ
		elif isinstance(expr, ast.ParExpr):
			expr.typ = self.check_expr(expr.expr)
			return expr.typ
		elif isinstance(expr, ast.IndexExpr):
			expr.left_typ = self.check_expr(expr.left)
			left_sym = expr.left_typ.symbol()
			idx_t = self.check_expr(expr.index)
			if left_sym.kind in (TypeKind.Array, TypeKind.Slice):
				if idx_t != self.comp.untyped_int_t and not self.comp.is_unsigned_int(
				    idx_t
				):
					report.error(
					    f"expected unsigned integer value, found `{idx_t}`",
					    expr.index.pos
					)
				if isinstance(expr.index, ast.RangeExpr):
					if expr.is_mut:
						if isinstance(
						    expr.left_typ, type.Slice
						) and not expr.left_typ.is_mut:
							report.error(
							    "cannot create a mutable slice from an immutable one"
							)
						else:
							self.check_expr_is_mut(expr.left)
					if left_sym.kind == TypeKind.Slice:
						expr.typ = expr.left_typ
					else:
						expr.typ = type.Slice(
						    left_sym.info.elem_typ, expr.is_mut
						)
						expr.typ.sym = self.comp.universe.add_or_get_slice(
						    left_sym.info.elem_typ, expr.is_mut
						)
				elif left_sym.kind == TypeKind.Slice:
					expr.typ = left_sym.info.elem_typ
				else:
					expr.typ = left_sym.info.elem_typ
			else:
				if not (
				    isinstance(expr.left_typ, type.Ptr)
				    or expr.left_typ == self.comp.string_t
				):
					report.error(
					    f"type `{expr.left_typ}` does not support indexing",
					    expr.pos
					)
					report.note(
					    "only pointers, arrays, slices and string supports indexing"
					)
				elif idx_t != self.comp.untyped_int_t and not self.comp.is_unsigned_int(
				    idx_t
				):
					report.error(
					    f"expected unsigned integer value, found `{idx_t}`",
					    expr.index.pos
					)
				elif isinstance(expr.left_typ, type.Ptr):
					if not self.inside_unsafe_block():
						report.error(
						    "pointer indexing is only allowed inside `unsafe` blocks",
						    expr.pos
						)
					elif isinstance(expr.index, ast.RangeExpr):
						report.error("cannot slice a pointer", expr.index.pos)

				if expr.left_typ == self.comp.string_t:
					if isinstance(expr.index, ast.RangeExpr):
						expr.typ = self.comp.string_t
					else:
						expr.typ = self.comp.u8_t
				else:
					expr.typ = expr.left_typ.typ
			return expr.typ
		elif isinstance(expr, ast.CallExpr):
			expr.typ = self.comp.void_t
			expr_left = expr.left

			inside_parens = False
			if isinstance(expr_left, ast.ParExpr
			              ) and isinstance(expr_left.expr, ast.SelectorExpr):
				expr_left = expr_left.expr
				inside_parens = True

			if isinstance(expr_left, ast.SelfTyExpr):
				expr.sym = expr_left.sym
				self.check_ctor(expr_left.sym, expr)
			elif isinstance(expr_left, ast.Ident):
				if isinstance(expr_left.sym, sym.Fn):
					expr.sym = expr_left.sym
					if expr.sym.is_main:
						report.error(
						    "cannot call to `main` function", expr_left.pos
						)
					else:
						self.check_call(expr_left.sym, expr)
				elif isinstance(expr_left.sym,
				                sym.Type) and expr_left.sym.kind in (
				                    TypeKind.Trait, TypeKind.Struct,
				                    TypeKind.Class
				                ):
					expr.sym = expr_left.sym
					self.check_ctor(expr_left.sym, expr)
				elif expr_left.is_obj:
					if isinstance(expr_left.typ, type.Fn):
						expr.sym = expr_left.typ.info()
						expr.is_closure = True
						self.check_call(expr.sym, expr)
					else:
						report.error(
						    f"expected function, found {expr_left.typ}",
						    expr_left.pos
						)
			elif isinstance(expr_left, ast.SelectorExpr):
				expr_left.left_typ = self.check_expr(expr_left.left)
				left_sym = expr_left.left_typ.symbol()
				if m := left_sym.find(expr_left.field_name):
					if isinstance(m, sym.Fn):
						if m.is_method:
							expr.sym = m
							if isinstance(expr_left.left_typ, type.Optional):
								report.error(
								    "optional value cannot be called directly",
								    expr_left.field_pos
								)
								report.help(
								    "use the none-check syntax: `foo.?.method()`"
								)
								report.help(
								    "or use `orelse`: `(foo orelse 5).method()`"
								)
							elif isinstance(expr_left.left_typ, type.Ptr):
								report.error(
								    "unexpected pointer type as receiver",
								    expr.pos
								)
								report.help(
								    "consider dereferencing this pointer"
								)
							else:
								self.check_call(m, expr)
						else:
							report.error(
							    f"`{expr_left.field_name}` is not a method",
							    expr_left.field_pos
							)
					else:
						report.error(
						    f"expected method, found {m.typeof()}",
						    expr_left.field_pos
						)
				elif f := left_sym.find_field(expr_left.field_name):
					if isinstance(f.typ, type.Fn):
						if inside_parens:
							expr.sym = f.typ.info()
							expr.is_closure = True
							expr.left.typ = f.typ
							self.check_call(expr.sym, expr)
						else:
							report.error(
							    f"type `{left_sym.name}` has no method `{expr_left.field_name}`",
							    expr_left.field_pos
							)
							report.help(
							    f"to call the function stored in `{expr_left.field_name}`, surround the field access with parentheses"
							)
					else:
						report.error(
						    f"field `{expr_left.field_name}` of type `{left_sym.name}` is not function type",
						    expr_left.field_pos
						)
				else:
					report.error(
					    f"type `{left_sym.name}` has no method `{expr_left.field_name}`",
					    expr_left.field_pos
					)
			elif isinstance(expr_left, ast.PathExpr):
				if isinstance(expr_left.field_info, sym.Fn):
					expr.sym = expr_left.field_info
					self.check_call(expr.sym, expr)
				elif isinstance(expr_left.field_info,
				                sym.Type) and expr_left.field_info.kind in (
				                    TypeKind.Trait, TypeKind.Class,
				                    TypeKind.Struct
				                ):
					self.check_ctor(expr_left.field_info, expr)
				else:
					report.error(
					    f"expected function, found {expr_left.field_info.typeof()}",
					    expr.pos
					)
			else:
				report.error(
				    "invalid expression used in call expression", expr.pos
				)

			if expr.has_err_handler():
				if isinstance(expr.typ, type.Result):
					if expr.err_handler.is_propagate:
						if not (
						    self.cur_fn.is_main
						    or isinstance(self.cur_fn.ret_typ, type.Result)
						):
							report.error(
							    f"to propagate the call, `{self.cur_fn.name}` must return an result type",
							    expr.err_handler.pos
							)
					else:
						self.check_expr(expr.err_handler.expr)
					expr.typ = expr.typ.typ
				else:
					report.error(
					    f"{expr.sym.kind()} `{expr.sym.name}` does not returns a result value",
					    expr.err_handler.pos
					)
			elif isinstance(expr.typ, type.Result):
				report.error(
				    f"{expr.sym.kind()} `{expr.sym.name}` returns a result",
				    expr.pos
				)
				report.note(
				    "should handle this with `catch` or propagate with `.!`"
				)
			return expr.typ
		elif isinstance(expr, ast.BuiltinCallExpr):
			expr.typ = self.comp.void_t
			if expr.name in ("addr_of", "addr_of_mut"):
				if not self.inside_unsafe_block():
					report.error(
					    f"`{expr.name}` should be called inside an `unsafe` block",
					    expr.pos
					)
				is_addr_of_mut = expr.name == "addr_of_mut"
				arg0 = expr.args[0]
				if is_addr_of_mut:
					self.check_expr_is_mut(arg0)
				expr.typ = type.Ptr(self.check_expr(arg0), is_addr_of_mut)
			elif expr.name in ("ptr_add", "ptr_diff"):
				if not self.inside_unsafe_block():
					report.error(
					    f"`{expr.name}` should be called inside an `unsafe` block",
					    expr.pos
					)
				elif len(expr.args) == 0:
					report.error(
					    "expected 1 or more arguments, found 0", expr.pos
					)
				else:
					ptr_t = self.check_expr(expr.args[0])
					if not isinstance(ptr_t, type.Ptr):
						report.error(
						    "a pointer was expected as the first argument",
						    expr.pos
						)
						return expr.typ
					for arg in expr.args[1:]:
						arg_t = self.check_expr(arg)
						if not self.comp.is_int(arg_t):
							report.error(
							    f"expected integer value, found `{arg_t}`",
							    expr.pos
							)
							return expr.typ
					expr.typ = self.comp.isize_t if expr.name == "ptr_diff" else ptr_t
			elif expr.name in ("size_of", "align_of"):
				expr.typ = self.comp.usize_t
			elif expr.name == "type_of":
				expr.typ = self.comp.string_t # TODO: core::TypeInfo
			elif expr.name in ("unreachable", "breakpoint"):
				expr.typ = self.comp.never_t
			elif expr.name == "assert":
				cond = expr.args[0]
				if self.check_expr(cond) != self.comp.bool_t:
					report.error(
					    "non-boolean expression used as `assert` condition",
					    cond.pos
					)
			else:
				report.error(
				    f"unknown builtin function `{expr.name}`", expr.pos
				)
			return expr.typ
		elif isinstance(expr, ast.RangeExpr):
			if expr.has_start:
				expr.typ = self.check_expr(expr.start)
			else:
				expr.typ = self.comp.usize_t
			if expr.has_end:
				end_t = self.check_expr(expr.end)
			else:
				end_t = self.comp.usize_t
			if expr.typ in (self.comp.untyped_int_t, self.comp.untyped_float_t):
				expr.typ = end_t
			return expr.typ
		elif isinstance(expr, ast.SelectorExpr):
			expr.typ = self.comp.void_t
			left_typ = self.check_expr(expr.left)
			expr.left_typ = left_typ
			if expr.is_nonecheck:
				if not isinstance(left_typ, type.Optional):
					report.error(
					    "cannot check a non-optional value", expr.field_pos
					)
				else:
					expr.typ = left_typ.typ
			elif expr.is_indirect:
				if not (
				    isinstance(left_typ, type.Ptr)
				    or isinstance(left_typ, type.Ref)
				):
					report.error(
					    f"invalid indirect for `{left_typ}`", expr.field_pos
					)
				elif isinstance(left_typ,
				                type.Ptr) and not self.inside_unsafe_block():
					report.error(
					    "dereference of pointer is unsafe and requires `unsafe` block",
					    expr.pos
					)
				elif left_typ.typ == self.comp.void_t:
					report.error("invalid indirect for `*void`", expr.field_pos)
					report.help(
					    "consider casting this to another pointer type, e.g. `*u8`"
					)
				else:
					expr.field_is_mut = left_typ.is_mut
					expr.typ = left_typ.typ
			else:
				left_sym = left_typ.symbol()
				if left_sym.kind == TypeKind.Slice:
					left_sym = self.comp.slice_sym
				if isinstance(left_typ, type.Optional):
					report.error(
					    "fields of an optional value cannot be accessed directly",
					    expr.pos
					)
					report.help("handle it with `.?` or `orelse`")
				elif left_sym.kind == TypeKind.Array and expr.field_name == "len":
					expr.typ = self.comp.usize_t
				elif left_sym.kind == TypeKind.Tuple and expr.field_name.isdigit(
				):
					idx = int(expr.field_name)
					if idx < len(left_sym.info.types):
						expr.typ = left_sym.info.types[idx]
					else:
						report.error(
						    f"type `{left_sym.name}` has no field `{expr.field_name}`",
						    expr.pos
						)
				elif field := left_sym.find_field(expr.field_name):
					if (not field.vis.is_pub()
					    ) and not self.sym.has_access_to(left_sym):
						report.error(
						    f"field `{expr.field_name}` of type `{left_sym.name}` is private",
						    expr.field_pos
						)
					expr.typ = field.typ
					expr.field_is_mut = field.is_mut
				elif decl := left_sym.find(expr.field_name):
					if isinstance(decl, sym.Fn):
						if decl.is_method:
							report.error(
							    f"cannot take value of method `{expr.field_name}`",
							    expr.field_pos
							)
							report.help(
							    f"use parentheses to call the method: `{expr}()`"
							)
						else:
							report.error(
							    f"cannot take value of associated function `{expr.field_name}` from value",
							    expr.field_pos
							)
							report.help(
							    f"use `{left_sym.name}::{expr.field_name}` instead"
							)
							expr.typ = decl.typ()
					else:
						report.error(
						    f"cannot take value of {decl.typeof()} `{left_sym.name}::{expr.field_name}`",
						    expr.field_pos
						)
				else:
					report.error(
					    f"type `{left_sym.name}` has no field `{expr.field_name}`",
					    expr.field_pos
					)
					if expr.field_name.isdigit():
						if left_sym.kind in (TypeKind.Array, TypeKind.Slice):
							report.note(
							    f"instead of using tuple indexing, use array indexing: `expr[{expr.field_name}]`"
							)
			expr.left_typ = left_typ
			if isinstance(expr.typ,
			              type.Ptr) and not self.inside_unsafe_block():
				report.error(
				    "pointers are usable only inside `unsafe` blocks", expr.pos
				)
			return expr.typ
		elif isinstance(expr, ast.PathExpr):
			expr.typ = self.comp.void_t
			if isinstance(expr.field_info, sym.Fn):
				if expr.field_info.is_method:
					report.error(
					    f"cannot take value of method `{expr.field_name}`",
					    expr.field_pos
					)
				expr.typ = expr.field_info.typ()
			elif isinstance(expr.left_info, sym.Type):
				expr.typ = type.Type(expr.left_info)
			elif isinstance(expr.field_info, sym.Type):
				expr.typ = type.Type(expr.field_info)
			elif isinstance(expr.field_info, sym.Const):
				expr.typ = expr.field_info.typ
			elif isinstance(expr.field_info, sym.Var):
				if (
				    expr.field_info.is_mut or (
				        expr.field_info.is_extern
				        and expr.field_info.abi != sym.ABI.Rivet
				    )
				) and not self.inside_unsafe_block():
					if expr.field_info.is_extern:
						report.error(
						    "use of external objects is unsafe and requires `unsafe` block",
						    expr.pos
						)
					else:
						report.error(
						    "use of mutable global objects is unsafe and requires `unsafe` block",
						    expr.pos
						)
						report.note(
						    "mutable global objects can be mutated by multiple threads: "
						    "aliasing violations or data races will cause undefined behavior"
						)
				expr.typ = expr.field_info.typ
			else:
				report.error(
				    "unexpected bug for path expression", expr.field_pos
				)
				report.note("please report this bug, thanks =D")
			return expr.typ
		elif isinstance(expr, ast.ReturnExpr):
			if expr.has_expr:
				if self.cur_fn.ret_typ == self.comp.void_t:
					report.error(
					    f"void {self.cur_fn.typeof()} `{self.cur_fn.name}` should not return a value",
					    expr.expr.pos
					)
				else:
					old_expected_type = self.expected_type
					self.expected_type = self.cur_fn.ret_typ.typ if isinstance(
					    self.cur_fn.ret_typ, type.Result
					) else self.cur_fn.ret_typ
					ret_typ = self.check_expr(expr.expr)
					self.expected_type = old_expected_type
					try:
						self.check_types(ret_typ, self.cur_fn.ret_typ)
					except utils.CompilerError as e:
						if not (
						    isinstance(self.cur_fn.ret_typ, type.Result)
						    and ret_typ.symbol().is_subtype_of(
						        self.comp.error_t.sym
						    )
						):
							report.error(e.args[0], expr.expr.pos)
							report.note(
							    f"in return argument of {self.cur_fn.typeof()} `{self.cur_fn.name}`"
							)
			elif self.cur_fn and not (
			    (self.cur_fn.ret_typ == self.comp.void_t) or (
			        isinstance(self.cur_fn.ret_typ, type.Result)
			        and self.cur_fn.ret_typ.typ == self.comp.void_t
			    )
			):
				report.error(
				    f"expected `{self.cur_fn.ret_typ}` argument", expr.pos
				)
				report.note(
				    f"in return argument of {self.cur_fn.typeof()} `{self.cur_fn.name}`"
				)
			expr.typ = self.comp.never_t
			return expr.typ
		elif isinstance(expr, ast.Block):
			expr.typ = self.comp.void_t
			if expr.is_unsafe:
				if self.inside_unsafe:
					report.warn("unnecessary `unsafe` block", expr.pos)
				self.inside_unsafe = True
			for stmt in expr.stmts:
				self.check_stmt(stmt)
			if expr.is_expr:
				expr.typ = self.check_expr(expr.expr)
			if expr.is_unsafe:
				self.inside_unsafe = False
				if self.unsafe_ops == 0:
					#report.warn("unnecessary `unsafe` block", expr.pos)
					pass
				else:
					self.unsafe_ops = 0
			return expr.typ
		elif isinstance(expr, ast.IfExpr):
			for i, b in enumerate(expr.branches):
				if not b.is_else:
					bcond_t = self.check_expr(b.cond)
					if not isinstance(
					    b.cond, ast.GuardExpr
					) and bcond_t != self.comp.bool_t:
						report.error(
						    "non-boolean expression used as `if` condition",
						    b.cond.pos
						)
				expr_typ = self.check_expr(b.expr)
				if i == 0:
					expr.typ = expr_typ
			return expr.typ
		elif isinstance(expr, ast.SwitchExpr):
			old_expected_type = self.expected_type
			expr_typ = self.check_expr(expr.expr)
			self.expected_type = expr_typ
			expr_sym = expr_typ.symbol()
			expected_branch_typ = self.comp.void_t
			if expr.is_typeswitch:
				if expr_sym.kind != TypeKind.Class:
					report.error("invalid value for typeswitch", expr.expr.pos)
					report.note(f"expected class value, found `{expr_typ}`")
			for i, b in enumerate(expr.branches):
				for p in b.pats:
					pat_t = self.check_expr(p)
					if expr.is_typeswitch:
						pat_t = self.comp.untyped_to_type(pat_t)
						pat_t_sym = pat_t.symbol()
					else:
						try:
							self.check_types(pat_t, expr_typ)
						except utils.CompilerError as e:
							report.error(e.args[0], p.pos)
				branch_t = self.check_expr(b.expr)
				if i == 0:
					expected_branch_typ = branch_t
				else:
					try:
						self.check_types(branch_t, expected_branch_typ)
					except utils.CompilerError as e:
						report.error(e.args[0], b.expr.pos)
			expr.typ = expected_branch_typ
			self.expected_type = old_expected_type
			return expr.typ
		return self.comp.void_t

	def check_ctor(self, info, expr):
		info.uses += 1
		expr.is_ctor = True
		expr.typ = type.Type(info)
		if info.kind == TypeKind.Trait:
			if len(expr.args) == 1:
				value_t = self.comp.untyped_to_type(
				    self.check_expr(expr.args[0].expr)
				)
				if value_t.symbol() in info.info.implements:
					info.info.has_objects = True
				else:
					report.error(
					    f"type `{value_t}` does not implement trait `{info.name}`",
					    expr.args[0].pos
					)
			else:
				report.error(
				    f"expected 1 argument, found {len(expr.args)}", expr.pos
				)

	def check_call(self, info, expr):
		info.uses += 1
		kind = info.kind()
		expr.typ = info.ret_typ

		if info.is_unsafe and not self.inside_unsafe_block():
			report.warn(
			    f"{kind} `{info.name}` should be called inside `unsafe` block",
			    expr.pos
			)

		func_args_len = len(info.args)
		if info.is_variadic and not info.is_extern:
			func_args_len -= 1
		if func_args_len < 0:
			func_args_len = 0

		# name arguments
		err = False
		for arg in expr.args:
			if arg.is_named:
				found = False
				for arg_fn in info.args:
					if arg_fn.name == arg.name:
						found = True
						if not arg_fn.has_def_expr:
							report.error(
							    f"argument `{arg.name}` is not optional",
							    arg.pos
							)
				if not found:
					err = True
					report.error(
					    f"{kind} `{info.name}` does not have an argument called `{arg.name}`",
					    arg.pos
					)
		if err:
			return expr.typ

		# default exprs
		if info.has_named_args:
			args_len = expr.pure_args_count()
			args = expr.args[:args_len]
			for i in range(args_len, func_args_len):
				arg = info.args[i]
				if arg.has_def_expr:
					if carg := expr.get_named_arg(arg.name):
						args.append(ast.CallArg(carg.expr, carg.expr.pos))
					else:
						args.append(ast.CallArg(arg.def_expr, arg.def_expr.pos))
			expr.args = args

		expr_args_len = expr.pure_args_count()
		expr_msg = f"expected {func_args_len} argument(s), found {expr_args_len}"
		if expr_args_len < func_args_len:
			report.error(f"too few arguments to {kind} `{info.name}`", expr.pos)
			report.note(expr_msg)
			return expr.typ
		elif not info.is_variadic and expr_args_len > func_args_len:
			report.error(
			    f"too many arguments to {kind} `{info.name}`", expr.pos
			)
			report.note(expr_msg)
			return expr.typ

		oet = self.expected_type
		for i, arg in enumerate(expr.args):
			if info.is_variadic and i >= len(info.args) - 1:
				arg_fn = info.args[len(info.args) - 1]
			else:
				arg_fn = info.args[i]

			self.expected_type = arg_fn.typ
			arg.typ = self.check_expr(arg.expr)
			self.expected_type = oet

			if not (
			    info.is_variadic and info.is_extern and i >= len(info.args) - 1
			):
				self.check_argument_type(
				    arg.typ, arg_fn.typ, arg.pos, arg_fn.name, kind, info.name
				)
		return expr.typ

	def check_argument_type(
	    self, got, expected, pos, arg_name, func_kind, func_name
	):
		expected_sym = expected.symbol()
		pos_msg = f"in argument `{arg_name}` of {func_kind} `{func_name}`"
		if expected_sym.kind == TypeKind.Trait:
			if expected != got:
				got_t = self.comp.untyped_to_type(got)
				if got_t.symbol() not in expected_sym.info.implements:
					report.error(
					    f"type `{got_t}` does not implement trait `{expected_sym.name}`",
					    pos
					)
					report.note(pos_msg)
		else:
			try:
				self.check_types(got, expected)
			except utils.CompilerError as e:
				report.error(e.args[0], pos)
				report.note(pos_msg)

	def check_types(self, got, expected):
		if not self.check_compatible_types(got, expected):
			if got == self.comp.none_t:
				if isinstance(expected, type.Optional):
					got_str = str(expected)
				else:
					got_str = f"?{expected}"
			else:
				got_str = str(got)
			if got != self.comp.void_t and expected != self.comp.void_t:
				raise utils.CompilerError(
				    f"expected type `{expected}`, found `{got_str}`"
				)

	def check_compatible_types(self, got, expected):
		if expected == got:
			return True

		if got == self.comp.never_t:
			return True

		if isinstance(expected, type.Result):
			return self.check_compatible_types(got, expected.typ)
		elif isinstance(expected,
		                type.Optional) and isinstance(got, type.Optional):
			return expected.typ == got.typ
		elif isinstance(expected,
		                type.Optional) and not isinstance(got, type.Optional):
			if got == self.comp.none_t:
				return True
			return self.check_compatible_types(got, expected.typ)
		elif isinstance(expected, type.Ptr) and isinstance(got, type.Ref):
			# allow &[mut] T == *[const|mut] T and &T == *const T inside
			# `unsafe` block
			if not self.inside_unsafe:
				return False
			if expected.is_mut and not got.is_mut:
				return False
			if expected.typ == self.comp.void_t:
				return True
			return expected.typ == got.typ
		elif isinstance(expected, type.Ptr) and got == self.comp.none_t:
			return True # allow *[const|mut] T == none
		elif expected == self.comp.none_t and (
		    isinstance(got, type.Optional) or isinstance(got, type.Ptr)
		):
			return True

		if (isinstance(expected, type.Ref)
		    and not isinstance(got, type.Ref)) or (
		        not isinstance(expected, type.Ref)
		        and isinstance(got, type.Ref)
		    ):
			return False
		elif (isinstance(expected, type.Ptr)
		      and not isinstance(got, type.Ptr)) or (
		          not isinstance(expected, type.Ptr)
		          and isinstance(got, type.Ptr)
		      ):
			return False

		exp_sym = expected.symbol()
		got_sym = got.symbol()

		if isinstance(expected, type.Variadic):
			if isinstance(got, type.Variadic):
				return exp_sym.info.elem_typ == got.typ
			return self.check_compatible_types(got, exp_sym.info.elem_typ)

		if isinstance(expected, type.Fn) and isinstance(got, type.Fn):
			return expected == got
		elif isinstance(expected, type.Slice) and isinstance(got, type.Slice):
			if expected.is_mut != got.is_mut:
				return False
			return expected.elem_typ == got.elem_typ

		if isinstance(expected, type.Ref) and isinstance(got, type.Ref):
			if expected.is_mut and not got.is_mut:
				return False
			return expected.typ == got.typ
		elif isinstance(expected, type.Ptr) and isinstance(got, type.Ptr):
			if expected.is_mut and not got.is_mut:
				return False
			if expected.typ == self.comp.void_t:
				# *void == *T, is valid
				return True
			return expected.typ == got.typ

		if self.comp.is_number(expected) and self.comp.is_number(got):
			return self.promote_number(expected, got) == expected
		elif exp_sym.kind == TypeKind.Trait:
			if self.comp.untyped_to_type(got
			                             ).symbol() in exp_sym.info.implements:
				exp_sym.info.has_objects = True
				return True
		elif exp_sym.kind == TypeKind.Array and got_sym.kind == TypeKind.Array:
			return exp_sym.info.elem_typ == got_sym.info.elem_typ and exp_sym.info.size == got_sym.info.size
		elif exp_sym.kind == TypeKind.Tuple and got_sym.kind == TypeKind.Tuple:
			if len(exp_sym.info.types) != len(got_sym.info.types):
				return False
			for i, t in enumerate(exp_sym.info.types):
				if t != got_sym.info.types[i]:
					return False
			return True

		if self.sym.is_core_pkg():
			if exp_sym.kind == TypeKind.Slice and got_sym == self.comp.slice_sym:
				return True

		return False

	def promote(self, left_typ, right_typ):
		if left_typ == right_typ:
			return left_typ
		elif self.comp.is_number(left_typ) and self.comp.is_number(right_typ):
			return self.promote_number(left_typ, right_typ)
		return left_typ

	def promote_number(self, expected, got):
		type_hi = expected
		type_lo = got
		bits_hi = self.comp.num_bits(type_hi)
		bits_lo = self.comp.num_bits(type_lo)
		if bits_hi < bits_lo:
			old_hi = type_hi
			type_hi = type_lo
			type_lo = old_hi

			old_bhi = bits_hi
			bits_hi = bits_lo
			bits_lo = old_bhi

		if type_hi == self.comp.untyped_int_t:
			return type_lo
		elif self.comp.is_float(type_hi):
			if self.comp.is_float(type_lo):
				# float -> float (good)
				return type_hi
			# float -> int (bad)
			return self.comp.void_t

		is_signed_lo = self.comp.is_signed_int(type_lo)
		is_unsigned_lo = not is_signed_lo
		is_signed_hi = self.comp.is_signed_int(type_hi)
		is_unsigned_hi = not is_signed_hi

		if is_unsigned_lo and is_unsigned_hi:
			# unsigned number -> unsigned number (good)
			return type_hi
		elif is_signed_lo and is_signed_hi:
			# signed number -> signed number (good)
			return type_lo if (bits_lo == 64 and is_signed_lo) else type_hi
		elif is_unsigned_lo and is_signed_hi and (bits_lo < bits_hi):
			# unsigned number -> signed number (good, if signed type is larger)
			return type_lo
		else:
			# signed number -> unsigned number (bad)
			return self.comp.void_t

	def inside_unsafe_block(self):
		self.unsafe_ops += 1
		return self.inside_unsafe

	def check_expr_is_mut(self, expr):
		if isinstance(expr, ast.ParExpr):
			self.check_expr_is_mut(expr.expr)
		elif isinstance(expr, ast.Ident):
			if expr.is_comptime:
				report.error(
				    f"cannot use constant `${expr.name}` as mutable value",
				    expr.pos
				)
			elif expr.is_obj and not expr.obj.is_mut:
				kind = "argument" if expr.obj.level == sym.ObjLevel.Arg else "object"
				report.error(
				    f"cannot use `{expr.obj.name}` as mutable {kind}", expr.pos
				)
				if expr.obj.level != sym.ObjLevel.Arg:
					report.help(
					    f"consider making this {kind} mutable: `mut {expr.name}`"
					)
			elif expr.sym:
				self.check_sym_is_mut(expr.sym, expr.pos)
		elif isinstance(expr, ast.SelfExpr):
			if not expr.is_mut:
				report.error("cannot use `self` as mutable value", expr.pos)
				report.help("consider making `self` as mutable: `mut self`")
		elif isinstance(expr, ast.SelectorExpr):
			if expr.is_indirect and isinstance(
			    expr.left_typ, (type.Ptr, type.Ref)
			):
				if not expr.left_typ.is_mut:
					kind = "pointer" if isinstance(
					    expr.left_typ, type.Ptr
					) else "reference"
					report.error(
					    f"cannot use a immutable {kind} as mutable value",
					    expr.pos
					)
			elif not expr.field_is_mut:
				report.error(
				    f"field `{expr.field_name}` of type `{expr.left_typ.symbol().name}` is immutable",
				    expr.pos
				)
		elif isinstance(expr, ast.PathExpr):
			if expr.field_info:
				self.check_sym_is_mut(expr.field_info)
		elif isinstance(expr, ast.NoneLiteral):
			report.error("`none` cannot be modified", expr.pos)
		elif isinstance(expr, ast.StringLiteral):
			report.error("string literals cannot be modified", expr.pos)
		elif isinstance(expr, ast.ArrayLiteral):
			report.error("array literals cannot be modified", expr.pos)
		elif isinstance(expr, ast.TupleLiteral):
			report.error("tuple literals cannot be modified", expr.pos)
		elif isinstance(expr, ast.AsExpr):
			self.check_expr_is_mut(expr.expr)
		elif isinstance(expr, ast.Block) and expr.is_expr:
			self.check_expr_is_mut(expr.expr)
		elif isinstance(expr, ast.IndexExpr):
			self.check_expr_is_mut(expr.left)
		elif isinstance(expr, ast.UnaryExpr):
			self.check_expr_is_mut(expr.right)
		elif isinstance(expr, ast.BinaryExpr):
			self.check_expr_is_mut(expr.left)
			self.check_expr_is_mut(expr.right)

	def check_sym_is_mut(self, sy, pos):
		if isinstance(sy, sym.Const):
			report.error(
			    f"cannot use constant `{sy.name}` as mutable value", pos
			)
		elif isinstance(sy, sym.Var) and not sy.is_mut:
			report.error(f"cannot use object `{sy.name}` as mutable value", pos)
