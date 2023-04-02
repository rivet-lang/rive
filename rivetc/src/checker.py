# Copyright (C) 2023 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

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

        self.inside_unsafe = False
        self.inside_test = False
        self.inside_guard_expr = False
        self.inside_var_decl = False

        self.defer_stmts = []

    def check_global_vars(self, decls):
        for decl in decls:
            old_sym = self.sym
            if isinstance(decl, (ast.ConstDecl, ast.VarDecl)):
                self.check_decl(decl)
            elif hasattr(decl, "decls"):
                self.check_global_vars(decl.decls)
            self.sym = old_sym

    def check_files(self, source_files):
        # check global vars
        for sf in source_files:
            self.sym = sf.sym
            self.source_file = sf
            self.expected_type = self.comp.void_t
            self.check_global_vars(self.source_file.decls)

        for sf in source_files:
            self.sym = sf.sym
            self.source_file = sf
            self.expected_type = self.comp.void_t
            self.check_decls(self.source_file.decls)

        for m in self.comp.universe:
            if isinstance(m, sym.Mod):
                for mod_var in m.syms:
                    if isinstance(mod_var, sym.Var):
                        if not mod_var.is_public and mod_var.is_mut and not mod_var.is_changed:
                            report.warn(
                                "variable does not need to be mutable",
                                mod_var.pos
                            )

    def check_decls(self, decls):
        for decl in decls:
            if not isinstance(decl, (ast.ConstDecl, ast.VarDecl)):
                self.check_decl(decl)

    def check_decl(self, decl):
        old_sym = self.sym
        if isinstance(decl, ast.ExternDecl):
            self.check_decls(decl.decls)
        elif isinstance(decl, ast.ConstDecl):
            if decl.has_typ:
                old_expected_type = self.expected_type
                self.expected_typ = decl.typ
                field_typ = self.check_expr(decl.expr)
                self.expected_type = old_expected_type
                try:
                    self.check_compatible_types(field_typ, decl.typ)
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.pos)
            else:
                decl.typ = self.check_expr(decl.expr)
                decl.sym.typ = decl.typ
        elif isinstance(decl, ast.VarDecl):
            self.inside_var_decl = True
            left0 = decl.lefts[0]
            if left0.has_typ:
                old_expected_type = self.expected_type
                expr_t = self.check_expr(decl.right)
                self.expected_type = old_expected_type
                try:
                    self.check_compatible_types(left0.typ, expr_t)
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.pos)
            else:
                left0.typ = self.check_expr(decl.right)
                left0.sym.typ = left0.typ
            self.inside_var_decl = False
        elif isinstance(decl, ast.EnumDecl):
            for base in decl.bases:
                base_sym = base.symbol()
                if base_sym.kind != TypeKind.Trait:
                    report.error(
                        f"base type `{base}` of enum `{decl.name}` is not a trait",
                        decl.pos
                    )
            for v in decl.variants:
                self.check_decls(v.decls)
            self.check_decls(decl.decls)
        elif isinstance(decl, ast.TraitDecl):
            for base in decl.bases:
                base_sym = base.symbol()
                if base_sym.kind != TypeKind.Trait:
                    report.error(f"traits can only inherit traits", decl.pos)
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
                        f"structs can only inherit traits and embed other structs",
                        decl.pos
                    )
            self.check_decls(decl.decls)
        elif isinstance(decl, ast.FieldDecl):
            if decl.has_def_expr:
                old_expected_type = self.expected_type
                self.expected_type = decl.typ
                field_typ = self.check_expr(decl.def_expr)
                self.expected_type = old_expected_type
                try:
                    self.check_compatible_types(field_typ, decl.typ)
                except utils.CompilerError as e:
                    report.error(e.args[0], decl.pos)
        elif isinstance(decl, ast.ExtendDecl):
            self_sym = decl.typ.symbol()
            for base in decl.bases:
                base_sym = base.symbol()
                base_kind = str(base_sym.kind)
                if base_sym.kind == TypeKind.Struct:
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
            for arg in decl.args:
                if arg.has_def_expr:
                    old_expected_type = self.expected_type
                    self.expected_type = arg.typ
                    def_expr_t = self.check_expr(arg.def_expr)
                    self.expected_type = old_expected_type
                    try:
                        self.check_compatible_types(def_expr_t, arg.typ)
                    except utils.CompilerError as e:
                        report.error(e.args[0], arg.pos)
            if isinstance(decl.ret_typ, type.Ptr) and decl.abi != sym.ABI.Rivet:
                report.error(
                    f"`{decl.name}` should return an optional pointer",
                    decl.name_pos
                )
                report.note(
                    "this is because Rivet cannot ensure that the function does not always return `none`"
                )
            self.cur_fn = decl.sym
            self.check_stmts(decl.stmts)
            decl.defer_stmts = self.defer_stmts
            self.defer_stmts = []
            self.check_mut_vars(decl.scope)
        elif isinstance(decl, ast.DestructorDecl):
            self.check_stmts(decl.stmts)
            decl.defer_stmts = self.defer_stmts
            self.defer_stmts = []
            self.check_mut_vars(decl.scope)
        elif isinstance(decl, ast.TestDecl):
            old_cur_fn = self.cur_fn
            self.cur_fn = None
            self.inside_test = True
            self.check_stmts(decl.stmts)
            self.inside_test = False
            self.cur_fn = old_cur_fn
            self.check_mut_vars(decl.scope)
        self.sym = old_sym

    def check_stmts(self, stmts):
        for stmt in stmts:
            self.check_stmt(stmt)

    def check_stmt(self, stmt):
        if isinstance(stmt, ast.VarDeclStmt):
            old_expected_type = self.expected_type
            if len(stmt.lefts) == 1:
                if stmt.lefts[0].has_typ:
                    self.expected_type = stmt.lefts[0].typ
                right_typ = self.check_expr(stmt.right)
                if stmt.lefts[0].has_typ:
                    try:
                        self.check_types(right_typ, self.expected_type)
                    except utils.CompilerError as e:
                        report.error(e.args[0], stmt.pos)
                    self.expected_type = self.comp.void_t
                else:
                    right_typ = self.comp.comptime_number_to_type(right_typ)
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
                            vtyp = self.comp.comptime_number_to_type(
                                symbol.info.types[i]
                            )
                            vd.typ = vtyp
                            stmt.scope.update_type(vd.name, vtyp)
            self.expected_type = old_expected_type
        elif isinstance(stmt, ast.ExprStmt):
            expr_typ = self.check_expr(stmt.expr)
            if not ((
                isinstance(expr_typ, type.Result)
                and expr_typ.typ in self.void_types
            ) or (
                isinstance(expr_typ, type.Option)
                and expr_typ.typ in self.void_types
            ) or expr_typ in self.void_types):
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
            if stmt.has_continue_expr:
                self.check_expr(stmt.continue_expr)
            self.check_stmt(stmt.stmt)
            if stmt.has_else_stmt:
                self.check_stmt(stmt.else_stmt)
        elif isinstance(stmt, ast.ForStmt):
            iterable_t = self.check_expr(stmt.iterable)
            iterable_sym = iterable_t.symbol()
            if iterable_sym.kind in (TypeKind.Array, TypeKind.Vec):
                elem_typ = self.comp.comptime_number_to_type(
                    iterable_sym.info.elem_typ
                )
                if stmt.value.is_mut and not iterable_sym.info.is_mut:
                    report.error(
                        f"cannot modify immutable {iterable_sym.kind}",
                        stmt.iterable.pos
                    )
                elif stmt.value.is_ref:
                    elem_typ = type.Ref(elem_typ)
                if stmt.index != None:
                    stmt.scope.update_type(stmt.index.name, self.comp.usize_t)
                stmt.scope.update_type(stmt.value.name, elem_typ)
                stmt.scope.update_is_hidden_ref(
                    stmt.value.name, stmt.value.is_mut
                )
                self.check_stmt(stmt.stmt)
            else:
                report.error(
                    f"`{iterable_t}` is not an iterable type", stmt.iterable.pos
                )
                report.note("expected array or slice value")
        elif isinstance(stmt, ast.DeferStmt):
            self.check_expr(stmt.expr)
            self.defer_stmts.append(stmt)

    def check_expr(self, expr):
        if isinstance(expr, ast.EmptyExpr):
            pass # error raised in `Resolver`
        elif isinstance(expr, ast.TypeNode):
            return expr.typ
        elif isinstance(expr, ast.AssignExpr):
            expr.typ = self.comp.void_t
            left_t = self.check_expr(expr.left)
            self.check_expr_is_mut(expr.left, True)
            old_expected_type = self.expected_type
            self.expected_type = left_t
            right_t = self.check_expr(expr.right)
            self.expected_type = old_expected_type
            if isinstance(expr.left, ast.Ident) and (expr.left.name == "_"):
                return expr.typ
            try:
                self.check_types(right_t, left_t)
            except utils.CompilerError as e:
                report.error(e.args[0], expr.right.pos)
            return expr.typ
        elif isinstance(expr, ast.EnumLiteral):
            expr.typ = self.comp.void_t
            _sym = self.expected_type.symbol()
            if _sym.kind == TypeKind.Enum:
                expr.sym = _sym
                if v := _sym.info.get_variant(expr.value):
                    expr.variant_info = v
                    expr.typ = type.Type(_sym)
                    if _sym.info.is_boxed_enum and not expr.from_is_cmp and not expr.is_instance:
                        report.error(
                            f"cannot use variant `{expr}` as a simple value",
                            expr.pos
                        )
                        report.note(f"make an instance instead: `{expr}()`")
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
                if expr.sym.abi != sym.ABI.Rivet:
                    report.error(
                        "cannot use an extern function as value", expr.pos
                    )
                    report.help(
                        "you can wrap the extern function with a function"
                    )
                expr.typ = expr.sym.typ()
            elif isinstance(expr.sym, sym.Const):
                expr.typ = expr.sym.typ
            elif isinstance(expr.sym, sym.Var):
                expr.typ = expr.sym.typ
            else:
                expr.typ = self.comp.void_t
            return expr.typ
        elif isinstance(expr, ast.SelfExpr):
            return expr.typ
        elif isinstance(expr, ast.SelfTyExpr):
            expr.typ = type.Type(expr.sym)
            return expr.typ
        elif isinstance(expr, ast.NoneLiteral):
            expr.typ = self.comp.none_t
            return expr.typ
        elif isinstance(expr, ast.BoolLiteral):
            expr.typ = self.comp.bool_t
            return expr.typ
        elif isinstance(expr, ast.CharLiteral):
            if expr.is_byte:
                expr.typ = self.comp.uint8_t
            else:
                expr.typ = self.comp.rune_t
            return expr.typ
        elif isinstance(expr, ast.IntegerLiteral):
            expr.typ = self.comp.comptime_int_t
            return expr.typ
        elif isinstance(expr, ast.FloatLiteral):
            expr.typ = self.comp.comptime_float_t
            return expr.typ
        elif isinstance(expr, ast.StringLiteral):
            if expr.is_bytestr:
                expr.typ = type.Type(
                    self.comp.universe.add_or_get_array(
                        self.comp.uint8_t,
                        ast.IntegerLiteral(
                            str(utils.bytestr(expr.lit).len), expr.pos
                        )
                    )
                )
            elif expr.is_cstr:
                expr.typ = type.Ptr(self.comp.uint8_t, False, True)
            else:
                expr.typ = self.comp.string_t
            return expr.typ
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
                tt = self.comp.comptime_number_to_type(self.check_expr(e))
                if has_expected:
                    self.expected_type = old_expected_type
                    types.append(expected_types[i])
                else:
                    types.append(tt)
            expr.typ = type.Type(self.comp.universe.add_or_get_tuple(types))
            return expr.typ
        elif isinstance(expr, ast.VectorLiteral):
            old_expected_type = self.expected_type
            size = ""
            is_mut = False
            has_exp_typ = False
            if not isinstance(self.expected_type, type.Fn):
                elem_sym = self.expected_type.symbol()
                if elem_sym.kind in (TypeKind.Array, TypeKind.Vec):
                    has_exp_typ = True
                    elem_typ = elem_sym.info.elem_typ
                    self.expected_type = elem_typ
                    if elem_sym.kind == TypeKind.Array:
                        size = elem_sym.info.size.lit
                    is_mut = elem_sym.info.is_mut
                else:
                    elem_typ = self.comp.void_t
            else:
                elem_typ = self.comp.void_t
            for i, e in enumerate(expr.elems):
                typ = self.check_expr(e)
                if i == 0 and not has_exp_typ:
                    elem_typ = typ
                    self.expected_type = elem_typ
                else:
                    try:
                        self.check_types(typ, elem_typ)
                    except utils.CompilerError as err:
                        report.error(err.args[0], e.pos)
                        if expr.is_arr:
                            report.note(f"in element {i + 1} of array literal")
                        else:
                            report.note(f"in element {i + 1} of vector literal")
            if expr.is_arr:
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
                        self.comp.comptime_number_to_type(elem_typ),
                        ast.IntegerLiteral(arr_len, expr.pos), is_mut
                    )
                )
            else:
                expr.typ = type.Type(
                    self.comp.universe.add_or_get_vec(
                        self.comp.comptime_number_to_type(elem_typ), is_mut
                    )
                )
            self.expected_type = old_expected_type
            return expr.typ
        elif isinstance(expr, ast.GuardExpr):
            old_inside_guard_expr = self.inside_guard_expr
            self.inside_guard_expr = True
            expr_t = self.check_expr(expr.expr)
            if isinstance(expr_t, (type.Result, type.Option)):
                expr.is_result = isinstance(expr_t, type.Result)
                var0 = expr.vars[0]
                expr.scope.update_type(var0.name, expr_t.typ)
                if var0.is_mut:
                    self.check_expr_is_mut(expr.expr)
                expr.typ = expr_t.typ
            else:
                report.error("expected result or option value", expr.expr.pos)
                expr.typ = self.comp.void_t
            if expr.has_cond:
                if self.check_expr(expr.cond) != self.comp.bool_t:
                    report.error(
                        "non-boolean expression used as guard condition",
                        expr.cond.pos
                    )
            self.inside_guard_expr = old_inside_guard_expr
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
                elif not (
                    self.comp.is_signed_int(expr.typ)
                    or self.comp.is_float(expr.typ)
                ):
                    report.error(
                        "operator `-` can only be used with signed values",
                        expr.pos
                    )
            elif expr.op == Kind.Amp:
                if isinstance(self.expected_type, type.Ptr):
                    expected_pointer = True
                    indexable_pointer = self.expected_type.is_indexable
                elif isinstance(self.expected_type, type.Option) and isinstance(
                    self.expected_type.typ, type.Ptr
                ):
                    expected_pointer = True
                    indexable_pointer = self.expected_type.typ.is_indexable
                else:
                    expected_pointer = False
                    indexable_pointer = False
                right = expr.right
                if isinstance(right, ast.ParExpr):
                    right = right.expr
                if isinstance(right, ast.IndexExpr):
                    if isinstance(
                        right.left_typ, type.Ptr
                    ) and not expected_pointer:
                        report.error(
                            "cannot reference a pointer indexing", expr.pos
                        )
                    right.is_ref = True
                elif isinstance(expr.typ, type.Ref):
                    report.error(
                        "cannot take the address of other reference", expr.pos
                    )
                elif expr.is_ref_mut:
                    self.check_expr_is_mut(right)
                if expected_pointer:
                    if not self.inside_unsafe:
                        report.error(
                            "cannot take the address of a value outside of an `unsafe` block",
                            expr.pos
                        )
                    expr.typ = type.Ptr(
                        expr.typ, expr.is_ref_mut, indexable_pointer
                    )
                else:
                    expr.typ = type.Ref(expr.typ, expr.is_ref_mut)
            return expr.typ
        elif isinstance(expr, ast.BinaryExpr):
            ltyp = self.check_expr(expr.left)
            old_expected_type = self.expected_type
            self.expected_type = ltyp
            rtyp = self.check_expr(expr.right)
            self.expected_type = old_expected_type

            lsym = ltyp.symbol()
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
            elif isinstance(
                ltyp, type.Option
            ) and expr.op not in (Kind.KwIs, Kind.KwNotIs, Kind.OrElse):
                report.error(
                    "option values only support `??`, `is` and `!is`", expr.pos
                )
            elif ltyp == self.comp.bool_t and rtyp == self.comp.bool_t and expr.op not in (
                Kind.Eq, Kind.Ne, Kind.KwAnd, Kind.KwOr, Kind.Pipe, Kind.Amp
            ):
                report.error(
                    "boolean values only support the following operators: `==`, `!=`, `and`, `or`, `&` and `|`",
                    expr.pos
                )
            elif ltyp == self.comp.string_t and rtyp == self.comp.string_t and expr.op not in (
                Kind.Eq, Kind.Ne, Kind.Lt, Kind.Gt, Kind.Le, Kind.Ge, Kind.KwIn,
                Kind.KwNotIn
            ):
                report.error(
                    "string values only support the following operators: `==`, `!=`, `<`, `>`, `<=` and `>=`",
                    expr.pos
                )

            return_type = ltyp
            if expr.op in (
                Kind.Plus, Kind.Minus, Kind.Mul, Kind.Div, Kind.Mod, Kind.Xor,
                Kind.Amp, Kind.Pipe
            ):
                promoted_type = self.comp.void_t
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
                            f"mismatched types `{ltyp}` and `{rtyp}`", expr.pos
                        )
                    elif isinstance(promoted_type, type.Option):
                        report.error(
                            f"operator `{expr.op}` cannot be used with `{promoted_type}`",
                            expr.pos
                        )
                return_type = promoted_type
            elif expr.op == Kind.OrElse:
                if isinstance(ltyp, type.Option):
                    if not self.check_compatible_types(
                        rtyp, ltyp.typ
                    ) and rtyp != self.comp.never_t:
                        report.error(
                            f"expected type `{ltyp.typ}`, found `{rtyp}`",
                            expr.right.pos
                        )
                        report.note("in right operand for operator `??`")
                    expr.typ = ltyp.typ
                else:
                    report.error(
                        "expected option value in left operand for operator `??`",
                        expr.pos
                    )
                    expr.typ = ltyp
                return expr.typ
            elif expr.op in (Kind.KwIn, Kind.KwNotIn):
                expr.typ = self.comp.bool_t
                lsym = ltyp.symbol()
                rsym = rtyp.symbol()
                assert rsym != None, (expr.pos)
                if rsym.kind not in (TypeKind.Vec, TypeKind.Array):
                    report.error(
                        f"operator `{expr.op}` can only be used with arrays and vectors",
                        expr.pos
                    )
                    return expr.typ
                elem_typ = rsym.info.elem_typ
                op_m = "==" if expr.op == Kind.KwIn else "!="
                try:
                    self.check_types(ltyp, elem_typ)
                    if not lsym.kind.is_primitive(
                    ) and lsym.kind != TypeKind.Enum and not lsym.exists(op_m):
                        report.error(
                            f"cannot use operator `{expr.op}` with type `{lsym.name}`",
                            expr.pos
                        )
                        report.help(
                            f"the type should define the operator `{op_m}`"
                        )
                except utils.CompilerError as e:
                    report.error(e.args[0], expr.pos)
                return expr.typ
            elif expr.op in (Kind.KwIs, Kind.KwNotIs):
                lsym = ltyp.symbol()
                if not (
                    lsym.kind in (TypeKind.Trait, TypeKind.Enum)
                    or isinstance(ltyp, type.Option)
                ):
                    report.error(
                        f"`{expr.op}` can only be used with traits, enums and options",
                        expr.left.pos
                    )
                if expr.has_var:
                    if lsym.kind == TypeKind.Enum and lsym.info.is_boxed_enum:
                        if expr.right.variant_info.has_typ:
                            expr.scope.update_type(
                                expr.var.name, expr.right.variant_info.typ
                            )
                            expr.var.typ = expr.right.variant_info.typ
                        else:
                            report.error(
                                "variant `{expr.right}` has no value",
                                expr.right.pos
                            )
                    else:
                        expr.scope.update_type(expr.var.name, rtyp)
                        expr.var.typ = rtyp
                    if expr.var.is_mut:
                        expr.scope.update_is_hidden_ref(expr.var.name, True)
                if lsym.kind == TypeKind.Enum:
                    if lsym.info.is_boxed_enum and expr.op not in (
                        Kind.KwIs, Kind.KwNotIs
                    ):
                        report.error(
                            "boxed enum values only support `is` and `!is`",
                            expr.pos
                        )
                    elif not lsym.info.is_boxed_enum and expr.op not in (
                        Kind.Eq, Kind.Ne
                    ):
                        report.error(
                            "enum values only support `==` and `!=`", expr.pos
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
                ) and not self.inside_unsafe:
                    report.warn(
                        f"shifting a value from a signed type `{ltyp}` can change the sign",
                        expr.left.pos
                    )
                expr.typ = ltyp
                return expr.typ

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
            if left_sym.kind in (TypeKind.Array, TypeKind.Vec):
                if idx_t != self.comp.comptime_int_t and not self.comp.is_unsigned_int(
                    idx_t
                ):
                    report.error(
                        f"expected unsigned integer value, found `{idx_t}`",
                        expr.index.pos
                    )
                if isinstance(expr.index, ast.RangeExpr):
                    if left_sym.kind == TypeKind.Vec:
                        expr.typ = expr.left_typ
                    else:
                        expr.typ = type.Vec(
                            left_sym.info.elem_typ, left_sym.info.is_mut
                        )
                        expr.typ.sym = self.comp.universe.add_or_get_vec(
                            left_sym.info.elem_typ, left_sym.info.is_mut
                        )
                elif left_sym.kind == TypeKind.Vec:
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
                elif idx_t != self.comp.comptime_int_t and not self.comp.is_unsigned_int(
                    idx_t
                ):
                    report.error(
                        f"expected unsigned integer value, found `{idx_t}`",
                        expr.index.pos
                    )
                elif isinstance(expr.left_typ, type.Ptr):
                    if not self.inside_unsafe:
                        report.error(
                            "pointer indexing is only allowed inside `unsafe` blocks",
                            expr.pos
                        )
                    elif isinstance(expr.index, ast.RangeExpr):
                        report.error("cannot slice a pointer", expr.index.pos)
                    elif not expr.left_typ.is_indexable:
                        report.error(
                            "cannot index a non-indexable pointer", expr.pos
                        )

                if expr.left_typ == self.comp.string_t:
                    if isinstance(expr.index, ast.RangeExpr):
                        expr.typ = self.comp.string_t
                    else:
                        expr.typ = self.comp.uint8_t
                elif hasattr(expr.left_typ, "typ"):
                    expr.typ = expr.left_typ.typ
                else:
                    expr.typ = self.comp.void_t
            return expr.typ
        elif isinstance(expr, ast.CallExpr):
            inside_parens = False
            expr.typ = self.comp.void_t

            expr_left = expr.left
            if isinstance(expr_left, ast.ParExpr) and isinstance(
                expr_left.expr, ast.SelectorExpr
            ) and not expr_left.expr.is_path:
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
                                    TypeKind.String, TypeKind.Enum
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
                if expr_left.is_path:
                    if isinstance(expr_left.field_sym,
                                  sym.Type) and expr_left.field_sym.kind in (
                                      TypeKind.Trait, TypeKind.Struct,
                                      TypeKind.Enum
                                  ):
                        self.check_ctor(expr_left.field_sym, expr)
                    elif isinstance(expr_left.field_sym, sym.Fn):
                        expr.sym = expr_left.field_sym
                        self.check_call(expr.sym, expr)
                    else:
                        report.error(
                            f"expected function, found {expr_left.field_sym.typeof()}",
                            expr.pos
                        )
                else:
                    expr_left.left_typ = self.check_expr(expr_left.left)
                    left_sym = expr_left.left_typ.symbol()
                    if m := left_sym.find(expr_left.field_name):
                        if isinstance(m, sym.Fn):
                            if m.is_method:
                                expr.sym = m
                                if isinstance(expr_left.left_typ, type.Option):
                                    report.error(
                                        "option value cannot be called directly",
                                        expr_left.field_pos
                                    )
                                    report.help(
                                        "use the option-check syntax: `foo?.method()`"
                                    )
                                    report.help(
                                        "or use `??`: `(foo ?? 5).method()`"
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
                                expr_left.typ = f.typ
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
            elif isinstance(expr_left, ast.EnumLiteral):
                expr_left.is_instance = True
                self.check_expr(expr_left)
                self.check_ctor(expr_left.sym, expr)
            else:
                report.error(
                    "invalid expression used in call expression", expr.pos
                )

            if expr.has_err_handler():
                if isinstance(expr.typ, type.Result):
                    if expr.err_handler.is_propagate:
                        if self.cur_fn and not (
                            self.cur_fn.is_main or self.inside_test
                            or self.inside_var_decl
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
            elif isinstance(
                expr.typ, type.Result
            ) and not self.inside_guard_expr:
                report.error(
                    f"{expr.sym.kind()} `{expr.sym.name}` returns a result",
                    expr.pos
                )
                report.note(
                    "should handle this with `catch` or propagate with `!`"
                )
            return expr.typ
        elif isinstance(expr, ast.BuiltinCallExpr):
            expr.typ = self.comp.void_t
            if expr.name == "ignore_not_mutated_warn":
                self.check_expr_is_mut(expr.args[0])
            elif expr.name == "vec":
                if len(expr.args) in (1, 2):
                    elem_t = expr.args[0].typ
                    expr.typ = type.Type(
                        self.comp.universe.add_or_get_vec(
                            elem_t, expr.vec_is_mut
                        )
                    )
                    if len(expr.args) == 2:
                        arg1_t = self.check_expr(expr.args[1])
                        try:
                            self.check_types(arg1_t, self.comp.usize_t)
                        except utils.CompilerError as e:
                            report.error(e.args[0], expr.args[1].pos)
                            report.note(
                                "in second argument of builtin function `vec`"
                            )
                else:
                    report.error(
                        f"expected 1 or 2 arguments, found {len(expr.args)}",
                        expr.pos
                    )
            elif expr.name == "cast":
                old_expected_type = self.expected_type
                self.expected_type = expr.typ
                expr_t = self.check_expr(expr.args[1])
                self.expected_type = old_expected_type
                expr.typ = expr.args[0].typ
                if expr.typ == expr_t:
                    report.warn(
                        f"attempt to cast an expression that is already of type `{expr.typ}`",
                        expr.pos
                    )
            elif expr.name in ("ptr_add", "ptr_diff"):
                if not self.inside_unsafe:
                    report.error(
                        f"`{expr.name}` should be called inside an `unsafe` block",
                        expr.pos
                    )
                elif len(expr.args) < 2:
                    report.error(
                        f"expected 2 or more arguments, found {len(expr.args)}",
                        expr.pos
                    )
                else:
                    ptr_t = self.check_expr(expr.args[0])
                    if not isinstance(ptr_t, type.Ptr):
                        report.error(
                            "a pointer was expected as the first argument",
                            expr.pos
                        )
                        return expr.typ
                    elif not ptr_t.is_indexable:
                        report.error(
                            f"`{expr.name}` requires indexable pointers",
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
            elif expr.name == "type_name":
                expr.typ = self.comp.string_t
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
            if expr.typ in (
                self.comp.comptime_int_t, self.comp.comptime_float_t
            ):
                expr.typ = end_t
            return expr.typ
        elif isinstance(expr, ast.SelectorExpr):
            expr.typ = self.comp.void_t
            if expr.is_path:
                if isinstance(expr.field_sym, sym.Fn):
                    if expr.field_sym.is_method:
                        report.error(
                            f"cannot take value of method `{expr.field_name}`",
                            expr.field_pos
                        )
                    expr.typ = expr.field_sym.typ()
                elif isinstance(expr.left_sym, sym.Type):
                    expr.typ = type.Type(expr.left_sym)
                elif isinstance(expr.field_sym, sym.Type):
                    expr.typ = type.Type(expr.field_sym)
                elif isinstance(expr.field_sym, sym.Const):
                    expr.typ = expr.field_sym.typ
                elif isinstance(expr.field_sym, sym.Var):
                    expr.typ = expr.field_sym.typ
                else:
                    report.error(
                        "unexpected bug for selector expression", expr.field_pos
                    )
                    report.note("please report this bug, thanks =D")
            else:
                left_typ = self.check_expr(expr.left)
                expr.left_typ = left_typ
                if expr.is_option_check:
                    if not isinstance(left_typ, type.Option):
                        report.error(
                            "cannot check a non-option value", expr.field_pos
                        )
                    else:
                        expr.typ = left_typ.typ
                elif expr.is_indirect:
                    if not (
                        isinstance(left_typ, type.Ptr)
                        or isinstance(left_typ, type.Ref)
                    ) or (
                        isinstance(left_typ, type.Ptr) and left_typ.is_indexable
                    ):
                        report.error(
                            f"invalid indirect for `{left_typ}`", expr.field_pos
                        )
                    elif left_typ.typ == self.comp.void_t:
                        report.error(
                            "invalid indirect for `anyptr`", expr.field_pos
                        )
                        report.help(
                            "consider casting this to another pointer type, e.g. `*uint8`"
                        )
                    else:
                        expr.field_is_mut = left_typ.is_mut
                        expr.typ = left_typ.typ
                else:
                    left_sym = left_typ.symbol()
                    if left_sym.kind == TypeKind.Array and expr.field_name == "len":
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
                        if (not field.is_public
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
                                    f"use `{left_sym.name}.{expr.field_name}` instead"
                                )
                                expr.typ = decl.typ()
                        else:
                            report.error(
                                f"cannot take value of {decl.typeof()} `{left_sym.name}.{expr.field_name}`",
                                expr.field_pos
                            )
                    else:
                        report.error(
                            f"type `{left_sym.name}` has no field `{expr.field_name}`",
                            expr.field_pos
                        )
                        if expr.field_name.isdigit():
                            if left_sym.kind in (TypeKind.Array, TypeKind.Vec):
                                report.note(
                                    f"instead of using tuple indexing, use array indexing: `expr[{expr.field_name}]`"
                                )
                expr.left_typ = left_typ
            if isinstance(
                expr.left_typ, type.Ptr
            ) and expr.left_typ.nr_level() > 1 and not expr.is_indirect:
                report.error(
                    "fields of an multi-level pointer cannot be accessed directly",
                    expr.pos
                )
                report.help(f"use `{expr.left}.*.{expr.field_name}` instead")
            elif isinstance(
                expr.left_typ, type.Option
            ) and not expr.is_option_check:
                report.error(
                    "fields of an option value cannot be accessed directly",
                    expr.pos
                )
                report.help("handle it with `?` or `??`")
            return expr.typ
        elif isinstance(expr, ast.ReturnExpr):
            if self.inside_test and expr.has_expr:
                report.error(
                    "cannot return values inside `test` declaration", expr.pos
                )
            elif expr.has_expr:
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
                    expr_typ = self.check_expr(expr.expr)
                    self.expected_type = old_expected_type
                    try:
                        self.check_types(expr_typ, self.cur_fn.ret_typ)
                    except utils.CompilerError as e:
                        if not (
                            isinstance(self.cur_fn.ret_typ, type.Result)
                            and expr_typ.symbol().implement_trait(
                                self.comp.error_sym
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
            if expr.is_unsafe:
                if self.inside_unsafe:
                    report.warn("unnecessary `unsafe` block", expr.pos)
                self.inside_unsafe = True
            old_expected_type = self.expected_type
            self.expected_type = self.comp.void_t
            for stmt in expr.stmts:
                self.check_stmt(stmt)
            self.expected_type = old_expected_type
            if expr.is_expr:
                expr.typ = self.check_expr(expr.expr)
            else:
                expr.typ = self.comp.void_t
            if expr.is_unsafe:
                self.inside_unsafe = False
            return expr.typ
        elif isinstance(expr, ast.IfExpr):
            expr.expected_typ = self.expected_type
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
                branch_t = self.comp.void_t
                if i == 0:
                    branch_t = self.check_expr(b.expr)
                    if expr.expected_typ == self.comp.void_t:
                        expr.expected_typ = branch_t
                    expr.typ = branch_t
                else:
                    old_expected_typ = self.expected_type
                    self.expected_type = expr.expected_typ
                    branch_t = self.check_expr(b.expr)
                    self.expected_type = old_expected_typ
                    try:
                        self.check_types(branch_t, expr.expected_typ)
                    except utils.CompilerError as e:
                        report.error(e.args[0], b.expr.pos)
                b.typ = branch_t
            return expr.typ
        elif isinstance(expr, ast.SwitchExpr):
            expr.typ = self.comp.void_t
            expr_typ = self.check_expr(expr.expr)
            expr_sym = expr_typ.symbol()
            if expr.is_typeswitch and expr_sym.kind not in (
                TypeKind.Enum, TypeKind.Trait
            ):
                report.error("invalid value for typeswitch", expr.expr.pos)
                report.note(f"expected enum or trait value, found `{expr_typ}`")
            elif expr_sym.kind == TypeKind.Enum:
                if expr_sym.info.is_boxed_enum and not expr.is_typeswitch:
                    report.error(
                        "cannot use `switch` with a boxed enum value", expr.pos
                    )
                    report.note(f"use a typeswitch instead")
                elif not expr_sym.info.is_boxed_enum and expr.is_typeswitch:
                    report.error(
                        "cannot use typeswitch with a enum value", expr.pos
                    )
                    report.note(f"use a simple `switch` instead")
            expr.expected_typ = self.expected_type
            for i, b in enumerate(expr.branches):
                if not b.is_else:
                    old_expected_type = self.expected_type
                    self.expected_type = expr_typ
                    for p in b.pats:
                        pat_t = self.check_expr(p)
                        if expr.is_typeswitch:
                            pat_t = self.comp.comptime_number_to_type(pat_t)
                        try:
                            self.check_types(pat_t, expr_typ)
                        except utils.CompilerError as e:
                            report.error(e.args[0], p.pos)
                    if b.has_var:
                        if b.var_is_mut:
                            self.check_expr_is_mut(expr.expr)
                        if len(b.pats) != 1:
                            report.error(
                                "multiple patterns cannot have variable",
                                b.var_pos
                            )
                        elif expr_sym.is_boxed():
                            var_t = self.comp.void_t
                            if expr_sym.kind == TypeKind.Enum:
                                pat0 = b.pats[0].variant_info
                                if pat0.has_typ:
                                    var_t = pat0.typ
                                else:
                                    report.error(
                                        "cannot use void expression",
                                        b.pats[0].pos
                                    )
                                expr.scope.update_is_hidden_ref(
                                    b.var_name, b.var_is_mut
                                )
                            else:
                                var_t = b.pats[0].typ
                            b.var_typ = var_t
                            expr.scope.update_type(b.var_name, var_t)
                        else:
                            report.error(
                                "only boxed types can have vars", b.var_pos
                            )
                    if b.has_cond:
                        cond_t = self.check_expr(b.cond)
                        if cond_t != self.comp.bool_t:
                            report.error(
                                "non-boolean expression use as `switch` branch condition",
                                b.cond.pos
                            )
                    self.expected_type = old_expected_type
                branch_t = self.comp.void_t
                if i == 0:
                    branch_t = self.check_expr(b.expr)
                    if expr.expected_typ == self.comp.void_t:
                        expr.expected_typ = branch_t
                    expr.typ = branch_t
                else:
                    old_expected_type = self.expected_type
                    self.expected_type = expr.expected_typ
                    branch_t = self.check_expr(b.expr)
                    self.expected_type = old_expected_type
                    try:
                        self.check_types(branch_t, expr.expected_typ)
                    except utils.CompilerError as e:
                        report.error(e.args[0], b.expr.pos)
                b.typ = branch_t
            return expr.typ
        elif isinstance(expr, ast.BranchExpr):
            expr.typ = self.comp.never_t
            return expr.typ
        elif isinstance(expr, ast.EmptyExpr):
            report.error("unexpected empty expression", expr.pos)
            report.note("bug detected on parser")
        return self.comp.void_t

    def check_ctor(self, info, expr):
        expr.is_ctor = True
        if info.kind == TypeKind.Struct and info.info.is_enum_variant:
            expr.is_enum_variant = True
            expr.enum_variant_sym = info
            expr.typ = type.Type(info.parent)
        else:
            expr.typ = type.Type(info)
        if info.kind == TypeKind.Enum:
            if isinstance(expr.left, ast.SelectorExpr):
                if v_ := expr.left.left_sym.info.get_variant(
                    expr.left.field_name
                ):
                    v = v_
                else:
                    assert False
            elif v_ := info.info.get_variant(expr.left.value):
                v = v_
            else:
                assert False
            has_args = len(expr.args) > 0
            if not info.info.is_boxed_enum:
                report.error(f"`{expr.left}` not expects value", expr.left.pos)
            elif has_args:
                if v.has_fields:
                    self.check_ctor(v.typ.symbol(), expr)
                elif v.has_typ:
                    try:
                        old_expected_type = self.expected_type
                        self.expected_type = v.typ
                        self.check_types(
                            self.check_expr(expr.args[0].expr), v.typ
                        )
                        self.expected_type = old_expected_type
                    except utils.CompilerError as e:
                        report.error(e.args[0], expr.args[0].expr.pos)
                else:
                    report.error(f"`{expr.left}` not expects a value", expr.pos)
            elif v.has_typ and not (v.has_fields or expr.left.from_is_cmp):
                report.error(f"`{expr.left}` expects a value", expr.pos)
        elif info.kind == TypeKind.Trait:
            if expr.has_spread_expr:
                report.error(
                    "cannot use spread expression with trait constructor",
                    expr.pos
                )
            elif len(expr.args) == 1:
                value_t = self.comp.comptime_number_to_type(
                    self.check_expr(expr.args[0].expr)
                )
                if value_t.symbol() in info.info.implements:
                    info.info.mark_has_objects()
                else:
                    report.error(
                        f"type `{value_t}` does not implement trait `{info.name}`",
                        expr.args[0].pos
                    )
            else:
                report.error(
                    f"expected 1 argument, found {len(expr.args)}", expr.pos
                )
        else:
            type_fields = info.full_fields()
            if not expr.has_named_args():
                expr_args_len = len(expr.args)
                if expr_args_len > len(type_fields):
                    report.error(
                        f"too many arguments to {info.kind} `{info.name}`",
                        expr.pos
                    )
                    report.note(
                        f"expected {len(type_fields)} argument(s), found {expr_args_len}"
                    )
                    return

            for i, arg in enumerate(expr.args):
                field_typ = self.comp.void_t
                if arg.is_named:
                    found = False
                    for field in type_fields:
                        if field.name == arg.name:
                            field_typ = field.typ
                            found = True
                            break
                    if not found:
                        report.error(
                            f"type `{info.name}` has no field `{arg.name}`",
                            arg.pos
                        )
                        continue
                else:
                    field = type_fields[i]
                    field_typ = field.typ
                arg.typ = field_typ
                old_expected_type = self.expected_type
                self.expected_type = field_typ
                arg_t = self.check_expr(arg.expr)
                self.expected_type = old_expected_type
                try:
                    self.check_types(arg_t, field_typ)
                except utils.CompilerError as e:
                    report.error(e.args[0], arg.pos)

            if expr.has_spread_expr:
                spread_expr_t = self.check_expr(expr.spread_expr)
                if spread_expr_t != expr.typ:
                    report.error(
                        f"expected type `{expr.typ}`, found `{spread_expr_t}`",
                        expr.pos
                    )
                    report.note("in spread expression of constructor")

    def check_call(self, info, expr):
        kind = info.kind()
        expr.typ = info.ret_typ

        if info.is_unsafe and not self.inside_unsafe:
            report.warn(
                f"{kind} `{info.name}` should be called inside `unsafe` block",
                expr.pos
            )
        elif info.is_method and info.self_is_mut:
            self.check_expr_is_mut(expr.left.left)

        func_args_len = len(info.args)
        if info.is_variadic and not info.is_extern:
            func_args_len -= 1
        if func_args_len < 0:
            func_args_len = 0

        # named arguments
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

            if isinstance(arg_fn.typ, type.Variadic):
                self.expected_type = arg_fn.typ.typ
            else:
                self.expected_type = arg_fn.typ
            arg.typ = self.check_expr(arg.expr)
            self.expected_type = oet

            if arg_fn.is_mut and not isinstance(
                arg_fn.typ, (type.Ptr, type.Ref)
            ) and not arg_fn.typ.symbol().is_primitive():
                self.check_expr_is_mut(arg.expr)

            if not (
                info.is_variadic and info.is_extern and i >= len(info.args) - 1
            ):
                self.check_argument_type(
                    arg.typ, arg_fn.typ, arg.pos, arg_fn.name, kind, info.name
                )

        if expr.has_spread_expr:
            spread_expr_t = self.check_expr(expr.spread_expr)
            spread_expr_sym = spread_expr_t.symbol()
            if spread_expr_sym.kind != TypeKind.Vec:
                report.error(
                    "spread operator can only be used with vectors",
                    expr.spread_expr.pos
                )
            elif not isinstance(info.args[-1].typ, type.Variadic):
                report.error(
                    "unexpected spread expression", expr.spread_expr.pos
                )
            else:
                last_arg_typ = info.args[-1].typ
                vec_t = type.Vec(last_arg_typ, False)
                vec_t.sym = last_arg_typ.sym
                try:
                    self.check_types(spread_expr_t, vec_t)
                except utils.CompilerError as e:
                    report.error(e.args[0], expr.spread_expr.pos)
        return expr.typ

    def check_argument_type(
        self, got, expected, pos, arg_name, func_kind, func_name
    ):
        expected_sym = expected.symbol()
        pos_msg = f"in argument `{arg_name}` of {func_kind} `{func_name}`"
        if expected_sym.kind == TypeKind.Trait:
            if expected != got:
                got_t = self.comp.comptime_number_to_type(got)
                if got_t.symbol() in expected_sym.info.implements:
                    expected_sym.info.mark_has_objects()
                else:
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
                if isinstance(expected, type.Option):
                    got_str = f"`{str(expected)}`"
                elif expected == self.comp.void_t:
                    got_str = f"option"
                else:
                    got_str = f"`?{expected}`"
            else:
                got_str = f"`{str(got)}`"
            if expected == self.comp.void_t:
                raise utils.CompilerError(
                    f"no value expected, found {got_str} value instead"
                )
            elif got == self.comp.void_t:
                raise utils.CompilerError("void expression used as value")
            else:
                raise utils.CompilerError(
                    f"expected type `{expected}`, found {got_str}"
                )

    def check_compatible_types(self, got, expected):
        if expected == got:
            return True

        if got == self.comp.never_t:
            return True
        elif expected == self.comp.never_t and got == self.comp.void_t:
            return True
        elif expected == self.comp.void_t and got == self.comp.never_t:
            return True

        if isinstance(expected, type.Result):
            return self.check_compatible_types(got, expected.typ)
        elif isinstance(got, type.Option) and isinstance(expected, type.Ptr):
            return self.check_pointer(expected, got.typ)
        elif isinstance(got,
                        type.Option) and not isinstance(expected, type.Option):
            return False
        elif isinstance(expected, type.Option) and isinstance(got, type.Option):
            if isinstance(expected.typ,
                          type.Ptr) and isinstance(got.typ, type.Ptr):
                return self.check_pointer(expected.typ, got.typ)
            return expected.typ == got.typ.typ
        elif isinstance(expected,
                        type.Option) and not isinstance(got, type.Option):
            if got == self.comp.none_t:
                return True
            return self.check_compatible_types(got, expected.typ)
        elif expected == self.comp.none_t and isinstance(got, type.Option):
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
            elem_sym = exp_sym.info.elem_typ.symbol()
            if got_sym.kind == TypeKind.Trait and elem_sym in got_sym.info.bases:
                return True
            return self.check_compatible_types(got, exp_sym.info.elem_typ)

        if isinstance(expected, type.Fn) and isinstance(got, type.Fn):
            return expected == got
        elif isinstance(expected, type.Vec) and isinstance(got, type.Vec):
            return expected.typ == got.typ

        if isinstance(expected, type.Ref) and isinstance(got, type.Ref):
            if expected.is_mut and not got.is_mut:
                return False
            return expected.typ == got.typ
        elif isinstance(expected, type.Ptr) and isinstance(got, type.Ptr):
            return self.check_pointer(expected, got)

        if expected == self.comp.rune_t and got == self.comp.comptime_int_t:
            return True
        elif expected == self.comp.comptime_int_t and got == self.comp.rune_t:
            return True
        elif self.comp.is_number(expected) and self.comp.is_number(got):
            if self.comp.is_comptime_number(
                expected
            ) or self.comp.is_comptime_number(got):
                return True
            return self.promote_number(expected, got) == expected
        elif exp_sym.kind == TypeKind.Trait:
            if self.comp.comptime_number_to_type(got).symbol(
            ) in exp_sym.info.implements:
                exp_sym.info.mark_has_objects()
                return True
        elif exp_sym.kind == TypeKind.Array and got_sym.kind == TypeKind.Array:
            if exp_sym.info.is_mut and not got_sym.info.is_mut:
                return False
            return exp_sym.info.elem_typ == got_sym.info.elem_typ and exp_sym.info.size == got_sym.info.size
        elif exp_sym.kind == TypeKind.Vec and got_sym.kind == TypeKind.Vec:
            if exp_sym.info.is_mut and not got_sym.info.is_mut:
                return False
            return exp_sym.info.elem_typ == got_sym.info.elem_typ
        elif exp_sym.kind == TypeKind.Tuple and got_sym.kind == TypeKind.Tuple:
            if len(exp_sym.info.types) != len(got_sym.info.types):
                return False
            for i, t in enumerate(exp_sym.info.types):
                if t != got_sym.info.types[i]:
                    return False
            return True

        if self.sym.is_core_mod():
            if exp_sym.kind == TypeKind.Vec and got_sym == self.comp.vec_sym:
                return True

        return False

    def check_pointer(self, expected, got):
        if expected.is_mut and not got.is_mut:
            return False
        elif expected.typ == self.comp.void_t:
            # anyptr == *T, is valid
            return True
        elif expected.is_indexable and not got.is_indexable:
            return False
        return expected.typ == got.typ

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

        if self.comp.is_float(type_hi):
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

    def check_expr_is_mut(self, expr, from_assign = False):
        if isinstance(expr, ast.ParExpr):
            self.check_expr_is_mut(expr.expr)
        elif isinstance(expr, ast.Ident):
            if expr.is_comptime:
                report.error(
                    f"cannot use constant `${expr.name}` as mutable value",
                    expr.pos
                )
            elif expr.name == "_":
                return
            elif expr.is_obj:
                if not expr.obj.is_mut:
                    kind = "argument" if expr.obj.level == sym.ObjLevel.Arg else "object"
                    report.error(
                        f"cannot use `{expr.obj.name}` as mutable {kind}",
                        expr.pos
                    )
                    if expr.obj.level != sym.ObjLevel.Arg:
                        report.help(
                            f"consider making this {kind} mutable: `mut {expr.name}`"
                        )
                expr.obj.is_changed = True
            elif expr.sym:
                self.check_sym_is_mut(expr.sym, expr.pos)
        elif isinstance(expr, ast.SelfExpr):
            if not expr.obj.is_mut:
                report.error("cannot use `self` as mutable value", expr.pos)
                report.help("consider making `self` as mutable: `mut self`")
            expr.obj.is_changed = True
        elif isinstance(expr, ast.SelectorExpr):
            if expr.is_path:
                self.check_sym_is_mut(expr.field_sym, expr.pos)
                return
            elif isinstance(expr.left, ast.SelfExpr):
                if not expr.left.obj.is_mut:
                    report.error(
                        "cannot use `self` as mutable receiver", expr.pos
                    )
                    report.help("consider making `self` as mutable: `mut self`")
                expr.left.obj.is_changed = True
            elif isinstance(expr.left, ast.Ident):
                if expr.left.sym:
                    self.check_sym_is_mut(expr.left.sym, expr.pos)
                elif expr.left.obj.level == sym.ObjLevel.Arg:
                    if not expr.left.obj.is_mut:
                        report.error(
                            f"cannot use `{expr.left.name}` as mutable argument",
                            expr.pos
                        )
                        report.help(
                            f"consider making this argument mutable: `mut {expr.left.name}`"
                        )
                    expr.left.obj.is_changed = True
            elif isinstance(expr.left, ast.SelectorExpr):
                self.check_expr_is_mut(expr.left)
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
            elif not expr.field_is_mut and not expr.is_option_check:
                report.error(
                    f"field `{expr.field_name}` of type `{expr.left.typ.symbol().name}` is immutable",
                    expr.pos
                )
        elif isinstance(expr, ast.NoneLiteral):
            report.error("`none` cannot be modified", expr.pos)
        elif isinstance(expr, ast.StringLiteral):
            report.error("string literals cannot be modified", expr.pos)
        elif isinstance(expr, ast.TupleLiteral):
            if from_assign:
                for e in expr.exprs:
                    self.check_expr_is_mut(e)
            else:
                report.error("tuple literals cannot be modified", expr.pos)
        elif isinstance(expr, ast.EnumLiteral) and not expr.is_instance:
            report.error("enum literals cannot be modified", expr.pos)
        elif isinstance(expr, ast.BuiltinCallExpr):
            for arg in expr.args:
                self.check_expr_is_mut(arg)
        elif isinstance(expr, ast.Block) and expr.is_expr:
            self.check_expr_is_mut(expr.expr)
        elif isinstance(expr, ast.IndexExpr):
            if isinstance(expr.left.typ, type.Ptr):
                if not expr.left.typ.is_mut:
                    report.error(
                        "cannot modify elements of an immutable pointer",
                        expr.pos
                    )
                return
            expr_sym = expr.left.typ.symbol()
            if not expr_sym.info.is_mut:
                report.error(
                    f"cannot modify elements of an immutable {expr_sym.kind}",
                    expr.pos
                )
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
        elif isinstance(sy, sym.Var):
            if not sy.is_mut:
                report.error(
                    f"cannot use object `{sy.name}` as mutable value", pos
                )
            sy.is_changed = True

    def check_mut_vars(self, sc):
        for obj in sc.objects:
            if obj.is_mut and not obj.is_changed:
                report.warn("variable does not need to be mutable", obj.pos)
        for ch in sc.childrens:
            self.check_mut_vars(ch)
