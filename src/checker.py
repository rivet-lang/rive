# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .token import Kind
from .ast import sym, type
from .ast.sym import TypeKind
from . import ast, colors, report, utils

class Checker:
    def __init__(self, comp):
        self.comp = comp

        self.cur_fn = None
        self.cur_sym = self.comp.pkg_sym
        self.expected_type = self.comp.void_t

        self.unsafe_operations = 0
        self.inside_unsafe = False

        self.trait_not_satisfied = False

        self.valid_types = (
            self.comp.void_t, self.comp.c_void_t, self.comp.no_return_t
        )

    def check_files(self, source_files):
        for sf in source_files:
            self.unsafe_operations = 0
            self.check_decls(sf.decls)

    def check_decls(self, decls):
        for decl in decls:
            self.check_decl(decl)

    def check_decl(self, decl):
        should_check = True
        if not decl.__class__ in (
            ast.TestDecl, ast.ExternPkg, ast.DestructorDecl
        ):
            should_check = decl.attrs.if_check
        if isinstance(decl, ast.ExternDecl):
            if should_check:
                self.check_decls(decl.protos)
        elif isinstance(decl, ast.ConstDecl):
            if should_check:
                self.check_expr(decl.expr)
        elif isinstance(decl, ast.StaticDecl):
            if should_check:
                self.check_expr(decl.expr)
        elif isinstance(decl, ast.ModDecl):
            if should_check:
                old_sym = self.cur_sym
                self.cur_sym = decl.sym
                self.check_decls(decl.decls)
                self.cur_sym = old_sym
        elif isinstance(decl, ast.TypeDecl):
            if should_check:
                pass
        elif isinstance(decl, ast.TraitDecl):
            if should_check:
                self.check_decls(decl.decls)
        elif isinstance(decl, ast.UnionDecl):
            if should_check:
                self.check_decls(decl.decls)
        elif isinstance(decl, ast.EnumDecl):
            if should_check:
                self.check_decls(decl.decls)
        elif isinstance(decl, ast.StructDecl):
            if should_check:
                self.check_decls(decl.decls)
        elif isinstance(decl, ast.StructField):
            if should_check:
                if decl.has_def_expr:
                    old_expected_type = self.expected_type
                    self.expected_type = decl.typ
                    self.check_expr(decl.def_expr)
                    self.expected_type = old_expected_type
        elif isinstance(decl, ast.ExtendDecl):
            if should_check:
                self.check_decls(decl.decls)
                if decl.is_for_trait:
                    typ_sym = decl.typ.get_sym()
                    trait_sym = decl.for_trait.get_sym()
                    if trait_sym.kind == TypeKind.Trait:
                        not_implemented = []
                        for proto in trait_sym.syms:
                            if d := typ_sym.lookup(proto.name):
                                # check signature
                                ptyp = proto.typ()
                                dtyp = d.typ()
                                if ptyp != dtyp:
                                    report.error(
                                        f"type `{typ_sym.name}` incorrectly implements {d.kind()} `{d.name}` of trait `{trait_sym.name}`",
                                        d.name_pos
                                    )
                                    report.note(f"expected `{ptyp}`")
                                    report.note(f"found `{dtyp}`")
                            elif proto.has_body:
                                pass # trait implementation
                            else:
                                not_implemented.append(proto.name)
                        if len(not_implemented) > 0:
                            word = "method" if len(
                                not_implemented
                            ) == 1 else "methods"
                            report.error(
                                f"type `{typ_sym.name}` does not implement trait `{trait_sym.name}`",
                                decl.pos
                            )
                            report.note(
                                f"missing {word}: `{'`, `'.join(not_implemented)}`"
                            )
                        else:
                            typ_sym.implements.append(trait_sym.qualname())
                    else:
                        report.error(
                            f"`{trait_sym.name}` is not a trait", decl.pos
                        )
        elif isinstance(decl, ast.TestDecl):
            self.check_stmts(decl.stmts)
        elif isinstance(decl, ast.FnDecl):
            if should_check:
                self.cur_fn = decl.sym
                for arg in decl.args:
                    if arg.is_mut and not (
                        isinstance(arg.typ, type.Ref)
                        or isinstance(arg.typ, type.Ptr)
                    ):
                        report.error(
                            "arguments passed by value cannot be mutated",
                            arg.pos
                        )
                        report.note(
                            "only arguments passed by reference can be mutated"
                        )
                    elif arg.has_def_expr:
                        self.check_expr(arg.def_expr)
                self.check_stmts(decl.stmts)
                self.cur_fn = None
        elif isinstance(decl, ast.DestructorDecl):
            self.check_stmts(decl.stmts)

    def check_stmts(self, stmts):
        for stmt in stmts:
            self.check_stmt(stmt)

    def check_stmt(self, stmt):
        if isinstance(stmt, ast.LetStmt):
            if len(stmt.lefts) == 1:
                if stmt.lefts[0].has_typ:
                    self.expected_type = stmt.lefts[0].typ
                right_typ = self.check_expr(stmt.right)
                if stmt.lefts[0].has_typ:
                    self.expected_type = self.comp.void_t
                else:
                    stmt.lefts[0].typ = right_typ
                    stmt.scope.update_typ(stmt.lefts[0].name, right_typ)
            else:
                right_typ = self.check_expr(stmt.right)
                symbol = right_typ.get_sym()
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
                        if not vd.has_typ:
                            vtyp = symbol.info.types[i]
                            vd.typ = vtyp
                            stmt.scope.update_typ(vd.name, vtyp)
        elif isinstance(stmt, ast.AssignStmt):
            self.check_expr(stmt.left)
            self.check_expr(stmt.right)
        elif isinstance(stmt, ast.ExprStmt):
            expr_typ = self.check_expr(stmt.expr)
            if not (
                isinstance(expr_typ, type.Result) and expr_typ.typ
                in self.valid_types or isinstance(expr_typ, type.Optional) and
                expr_typ.typ in self.valid_types or expr_typ in self.valid_types
            ):
                report.warn("expression evaluated but not used", stmt.expr.pos)
        elif isinstance(stmt, ast.WhileStmt):
            if not stmt.is_inf and self.check_expr(
                stmt.cond
            ) != self.comp.bool_t:
                report.error(
                    "non-boolean expression used as `while` condition",
                    stmt.cond.pos
                )
            self.check_stmt(stmt.stmt)
        elif isinstance(stmt, ast.ForInStmt):
            # TODO: check variables
            self.check_expr(stmt.iterable)
            self.check_stmt(stmt.stmt)
        elif isinstance(stmt, ast.GotoStmt):
            pass # TODO

    def check_expr(self, expr):
        if isinstance(expr, ast.ParExpr):
            if isinstance(expr.expr, ast.ParExpr):
                report.warn("redundant parentheses are used", expr.pos)
            expr.typ = self.check_expr(expr.expr)
            return expr.typ
        elif isinstance(expr, ast.TypeNode):
            return expr.typ
        elif isinstance(expr, ast.Ident):
            if expr.is_comptime:
                expr.typ = self.comp.str_t
            elif expr.is_obj:
                expr.typ = expr.obj.typ
            elif isinstance(expr.sym, sym.Fn):
                expr.typ = expr.sym.typ()
            elif isinstance(expr.sym, sym.Const):
                expr.typ = expr.sym.typ
            elif isinstance(expr.sym, sym.Static):
                if expr.sym.is_mut and not self.inside_unsafe_block():
                    report.error(
                        "use of mutable static is unsafe and requires `unsafe` block",
                        expr.pos
                    )
                    report.note(
                        "mutable statics can be mutated by multiple threads: "
                        "aliasing violations or data races will cause undefined behavior"
                    )
                expr.typ = expr.sym.typ
            else:
                #report.error(f"expected value, found {expr.sym}", expr.pos)
                expr.typ = self.comp.void_t
            return expr.typ
        elif isinstance(expr, ast.EnumVariantExpr):
            expr.typ = self.comp.void_t
            _sym = self.expected_type.get_sym()
            if _sym.kind == TypeKind.Enum:
                if expr.variant not in _sym.info.variants:
                    report.error(
                        f"enum `{_sym.name}` has no variant `{expr.variant}`",
                        expr.pos
                    )
                else:
                    expr.typ = type.Type(_sym)
            else:
                report.error(f"`{_sym.name}` is not a enum", expr.pos)
            return expr.typ
        elif isinstance(expr, ast.SelfExpr):
            return expr.typ
        elif isinstance(expr, ast.SelfTyExpr):
            return self.comp.void_t #TODO
        elif isinstance(expr, ast.VoidLiteral):
            return self.comp.void_t
        elif isinstance(expr, ast.NoneLiteral):
            return self.comp.none_t
        elif isinstance(expr, ast.BoolLiteral):
            return self.comp.bool_t
        elif isinstance(expr, ast.CharLiteral):
            if expr.is_byte:
                expr.typ = self.comp.uint8_t
            else:
                expr.typ = self.comp.rune_t
            return expr.typ
        elif isinstance(expr, ast.IntegerLiteral):
            return self.comp.int32_t # TODO
        elif isinstance(expr, ast.FloatLiteral):
            return self.comp.float64_t # TODO
        elif isinstance(expr, ast.StringLiteral):
            if expr.is_bytestr:
                _, size = utils.bytestr(expr.lit)
                expr.typ = type.Type(
                    self.comp.universe.add_or_get_array(
                        self.comp.uint8_t,
                        ast.IntegerLiteral(str(size), expr.pos)
                    )
                )
            else:
                expr.typ = self.comp.str_t
            return expr.typ
        elif isinstance(expr, ast.TupleLiteral):
            types = []
            for e in expr.exprs:
                types.append(self.check_expr(e))
            expr.typ = type.Type(self.comp.universe.add_or_get_tuple(types))
            return expr.typ
        elif isinstance(expr, ast.ArrayLiteral):
            old_exp_typ = self.expected_type
            has_exp_typ = False
            if not isinstance(self.expected_type, type.Fn):
                elem_sym = self.expected_type.get_sym()
                if elem_sym.kind == TypeKind.Array:
                    has_exp_typ = True
                    elem_typ = elem_sym.info.elem_typ
                    self.expected_type = elem_typ
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
                        report.note(f"in element {i + 1} of array literal")
            expr.typ = type.Type(
                self.comp.universe.add_or_get_array(
                    elem_typ,
                    ast.IntegerLiteral(str(len(expr.elems)), expr.pos)
                )
            )
            self.expected_type = old_exp_typ
            return expr.typ
        elif isinstance(expr, ast.StructLiteral):
            if isinstance(expr.expr, (ast.PathExpr, ast.Ident, ast.SelfTyExpr)):
                expr_sym = expr.expr.typ.get_sym() if isinstance(
                    expr.expr, ast.SelfTyExpr
                ) else expr.expr.sym if isinstance(
                    expr.expr, ast.Ident
                ) else expr.expr.left_info
                expr.typ = type.Type(expr_sym)
                if expr_sym.kind == TypeKind.Struct:
                    for f in expr.fields:
                        if field := expr_sym.lookup_field(f.name):
                            oet = self.expected_type
                            self.expected_type = field.typ
                            fexpr_t = self.check_expr(f.expr)
                            self.expected_type = oet
                            try:
                                self.check_types(fexpr_t, field.typ)
                            except utils.CompilerError as e:
                                report.error(e.args[0], f.expr.pos)
                                report.note(
                                    f"in field `{field.name}` of struct `{expr_sym.name}`"
                                )
                        else:
                            report.error(
                                f"struct `{expr_sym.name}` has no field `{f.name}`",
                                f.pos
                            )
                else:
                    report.error(
                        f"expected struct, found {expr_sym.kind}", expr.expr.pos
                    )
                    if expr_sym.kind in (
                        TypeKind.ErrType, TypeKind.Union, TypeKind.Trait
                    ):
                        report.help(f"use `{expr_sym.name}(value)` instead")
                return expr.typ
            report.error(
                "expected identifier or path expression", expr.expr.pos
            )
            return self.comp.void_t
        elif isinstance(expr, ast.UnaryExpr):
            expr.typ = self.check_expr(expr.right)
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
            elif expr.op in (Kind.Inc, Kind.Dec):
                if not self.comp.is_int(expr.typ):
                    report.error(
                        f"operator `{expr.op}` can only be used with numeric values",
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

                if isinstance(self.expected_type, type.Ptr):
                    expr.typ = type.Ptr(expr.typ)
                else:
                    expr.typ = type.Ref(expr.typ)
            return expr.typ
        elif isinstance(expr, ast.BinaryExpr):
            ltyp = self.check_expr(expr.left)
            rtyp = self.check_expr(expr.right)

            if expr.op in (
                Kind.Plus, Kind.Minus, Kind.Mult, Kind.Div, Kind.Mod, Kind.Xor,
                Kind.Amp, Kind.Pipe
            ):
                if isinstance(ltyp, type.Ptr):
                    if (isinstance(rtyp, type.Ptr)
                        and expr.op != Kind.Minus) or (
                            not isinstance(rtyp, type.Ptr)
                            and expr.op not in (Kind.Plus, Kind.Minus)
                        ):
                        report.error(
                            f"invalid operator `{expr.op}` to `{ltyp}` and `{rtyp}`",
                            expr.pos
                        )
                    elif expr.op in (Kind.Plus, Kind.Minus
                                     ) and not self.inside_unsafe_block():
                        report.error(
                            "pointer arithmetic is only allowed inside `unsafe` block",
                            expr.pos
                        )
                elif isinstance(ltyp, type.Ref):
                    report.error(
                        "cannot use arithmetic operations with references",
                        expr.pos
                    )

            return_type = ltyp
            if expr.op in (
                Kind.Plus, Kind.Minus, Kind.Mult, Kind.Div, Kind.Mod, Kind.Xor,
                Kind.Amp, Kind.Pipe
            ):
                if isinstance(ltyp, type.Ptr) and isinstance(
                    rtyp, type.Ptr
                ) and expr.op == Kind.Minus:
                    promoted_type = self.comp.isize_t
                else:
                    promoted_type = self.promote(ltyp, rtyp)

                if promoted_type == self.comp.void_t:
                    report.error(
                        f"mismatched types `{ltyp}` and `{rtyp}`", expr.pos
                    )
                elif isinstance(promoted_type, type.Optional):
                    report.error(
                        f"operator `{expr.op}` cannot be used with `{promoted_type}`",
                        expr.pos
                    )

                return_type = promoted_type
            elif expr.op == Kind.KeyOrElse:
                if isinstance(ltyp, type.Optional):
                    if ltyp.typ != rtyp and rtyp != self.comp.no_return_t:
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
            elif expr.op in (Kind.KeyIs, Kind.KeyNotIs):
                lsym = ltyp.get_sym()
                if ltyp == self.comp.error_t:
                    rsym = rtyp.get_sym()
                    if rsym.kind != TypeKind.ErrType:
                        report.error(
                            f"expected errtype value, found `{rtyp}`",
                            expr.right.pos
                        )
                        report.note(
                            f"in right operand for operator `{expr.op}`"
                        )
                elif lsym.kind == TypeKind.Union:
                    if lsym.info.no_tag:
                        report.error(
                            f"union `{lsym.name}` does not support operator `{expr.op}`",
                            expr.left.pos
                        )
                        report.note(f"`{lsym.name}` is marked with `#[no_tag]`")
                    elif rtyp not in lsym.info.variants:
                        report.error(
                            f"union `{lsym.name}` has no variant `{rtyp}`",
                            expr.right.pos
                        )
                        report.note(
                            f"in right operand for operator `{expr.op}`"
                        )
                else:
                    report.error(
                        f"`{expr.op}` can only be used with traits, tagged unions and error values",
                        expr.left.pos
                    )
                expr.typ = self.comp.bool_t
                return expr.typ
            elif expr.op in (Kind.KeyAnd, Kind.KeyOr):
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
                        Kind.KeyAnd, Kind.KeyOr
                    ):
                        # use `(a and b) or c` instead of `a and b or c`
                        report.error("ambiguous boolean expression", expr.pos)
                        report.help(
                            f"use `({expr.left}) {expr.op} {expr.right}` instead"
                        )
                expr.typ = self.comp.bool_t
                return expr.typ

            if ltyp == self.comp.bool_t and rtyp == self.comp.bool_t and expr.op not in (
                Kind.Eq, Kind.Ne, Kind.KeyAnd, Kind.KeyOr, Kind.Pipe, Kind.Amp
            ):
                report.error(
                    "boolean values only support the following operators: `==`, `!=`, `and`, `or`, `&` and `|`",
                    expr.pos
                )
            elif ltyp == self.comp.str_t and rtyp == self.comp.str_t and expr.op not in (
                Kind.Eq, Kind.Ne, Kind.Lt, Kind.Gt, Kind.Le, Kind.Ge
            ):
                report.error(
                    "string values only support the following operators: `==`, `!=`, `<`, `>`, `<=` and `>=`",
                    expr.pos
                )
            elif ltyp == self.comp.error_t and expr.op not in (
                Kind.KeyIs, Kind.KeyNotIs
            ):
                report.error(
                    "error values only support `is` and `!is`", expr.pos
                )

            if not (
                self.check_compatible_types(ltyp, rtyp)
                and self.check_compatible_types(rtyp, ltyp)
            ):
                if ltyp == self.comp.void_t or rtyp == self.comp.void_t or return_type == self.comp.void_t:
                    expr.typ = return_type
                    return expr.typ
                report.error(
                    f"expected type {ltyp}, found {rtyp}", expr.right.pos
                )

            if expr.op.is_relational():
                expr.typ = self.comp.bool_t
            else:
                expr.typ = return_type
            return expr.typ
        elif isinstance(expr, ast.PostfixExpr):
            expr.typ = self.check_expr(expr.left)
            if expr.op in (Kind.Inc, Kind.Dec):
                report.error(
                    f"operator `{expr.op}` can only be used with numeric values",
                    expr.pos
                )
            return expr.typ
        elif isinstance(expr, ast.CastExpr):
            old_exp_typ = self.expected_type
            self.expected_type = expr.typ
            self.check_expr(expr.expr)
            self.expected_type = old_exp_typ
            return expr.typ
        elif isinstance(expr, ast.IndexExpr):
            expr.left_typ = self.check_expr(expr.left)
            left_sym = expr.left_typ.get_sym()
            idx_t = self.check_expr(expr.index)
            if left_sym.kind in (TypeKind.Array, TypeKind.Slice):
                if not self.comp.is_unsigned_int(idx_t):
                    report.error(
                        f"expected unsigned integer type, found {idx_t}",
                        expr.index.pos
                    )

                if isinstance(expr.index, ast.RangeExpr):
                    if left_sym.kind == TypeKind.Slice:
                        expr.typ = expr.left_typ
                    else:
                        expr.typ = type.Type(
                            self.comp.universe.add_or_get_slice(
                                left_sym.info.elem_typ
                            )
                        )
                elif left_sym.kind == TypeKind.Slice:
                    expr.typ = left_sym.info.elem_typ
                else:
                    expr.typ = left_sym.info.elem_typ
            else:
                if not (
                    isinstance(expr.left_typ, type.Ptr)
                    or expr.left_typ == self.comp.str_t
                ):
                    report.error(
                        f"type `{expr.left_typ}` does not support indexing",
                        expr.pos
                    )
                    report.note(
                        "only `str`, pointers, arrays and slices supports indexing"
                    )
                elif not self.comp.is_unsigned_int(idx_t):
                    report.error(
                        f"expected unsigned integer type, found {idx_t}",
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

                if expr.left_typ == self.comp.str_t:
                    if isinstance(expr.index, ast.RangeExpr):
                        expr.typ = self.comp.str_t
                    else:
                        expr.typ = self.comp.u8_t
                else:
                    expr.typ = expr.left_typ.typ
            return expr.typ
        elif isinstance(expr, ast.RangeExpr):
            if expr.has_start:
                self.check_expr(expr.start)
            if expr.has_end:
                self.check_expr(expr.end)
        elif isinstance(expr, ast.SelectorExpr):
            expr.typ = self.comp.void_t
            left_typ = self.check_expr(expr.left)
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
                elif left_typ.typ == self.comp.c_void_t:
                    report.error(
                        f"invalid indirect for `*c_void`", expr.field_pos
                    )
                    report.help(
                        "consider casting this to another pointer type, e.g. `*u8`"
                    )
                else:
                    expr.typ = left_typ.typ
            else:
                left_sym = left_typ.get_sym()
                if isinstance(left_typ, type.Optional):
                    report.error(
                        "fields of an optional value cannot be accessed directly",
                        expr.pos
                    )
                    report.help("handle it with `.?` or `orelse`")
                elif isinstance(left_typ, type.Ptr):
                    report.error(
                        "fields of a pointer value cannot be accessed directly",
                        expr.pos
                    )
                    report.help(
                        "use the dereference operator instead: `ptr_value.*.field_name`"
                    )
                elif left_sym.kind in (
                    TypeKind.Array, TypeKind.Slice
                ) and expr.field_name == "len":
                    expr.typ = self.comp.usize_t
                    return expr.typ
                elif field := left_sym.lookup_field(expr.field_name):
                    if not field.is_pub and self.cur_sym != left_sym.parent:
                        report.error(
                            f"field `{expr.field_name}` of type `{left_sym.name}` is private",
                            expr.field_pos
                        )
                    expr.typ = field.typ
                elif decl := left_sym.lookup(expr.field_name):
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
                            f"cannot take value of {decl.sym_kind()} `{left_sym.name}::{expr.field_name}`",
                            expr.field_pos
                        )
                else:
                    report.error(
                        f"type `{left_sym.name}` has no field `{expr.field_name}`",
                        expr.pos
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
                if expr.left_info.kind == TypeKind.Enum:
                    expr.typ = type.Type(expr.left_info)
            elif isinstance(expr.left_info, sym.Const):
                expr.typ = expr.left_info.typ
            elif isinstance(expr.left_info, sym.Static):
                if expr.left_info.is_mut and not self.inside_unsafe_block():
                    report.error(
                        "use of mutable static is unsafe and requires `unsafe` block",
                        expr.pos
                    )
                    report.note(
                        "mutable statics can be mutated by multiple threads: "
                        "aliasing violations or data races will cause undefined behavior"
                    )
                expr.typ = expr.left_info.typ
            elif isinstance(expr.field_info, sym.Type):
                expr.typ = type.Type(expr.field_info)
            else:
                report.error(
                    "unexpected bug for path expression", expr.field_pos
                )
            return expr.typ
        elif isinstance(expr, ast.BuiltinCallExpr):
            return self.check_builtin_call(expr)
        elif isinstance(expr, ast.CallExpr):
            expr.typ = self.comp.void_t
            expr_left = expr.left
            inside_parens = False
            if isinstance(expr_left, ast.ParExpr
                          ) and isinstance(expr_left.expr, ast.SelectorExpr):
                expr_left = expr_left.expr
                inside_parens = True

            if isinstance(expr_left, ast.Ident):
                if isinstance(expr_left.sym, sym.Fn):
                    expr.info = expr_left.sym
                    self.check_call(expr_left.sym, expr)
                elif isinstance(expr_left.sym,
                                sym.Type) and expr_left.sym.kind in (
                                    TypeKind.ErrType, TypeKind.Union,
                                    TypeKind.Trait
                                ):
                    self.check_special_ctor(expr_left.sym, expr)
                elif expr_left.is_obj:
                    if isinstance(expr_left.typ, type.Fn):
                        self.check_call(expr_left.typ.info(), expr)
                    else:
                        report.error(
                            f"expected function, found {expr_left.typ}",
                            expr_left.pos
                        )
            elif isinstance(expr_left, ast.SelectorExpr):
                left_typ = self.check_expr(expr_left.left)
                left_sym = left_typ.get_sym()
                if m := left_sym.lookup(expr_left.field_name):
                    if isinstance(m, sym.Fn):
                        if m.is_method:
                            if isinstance(left_typ, type.Optional):
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
                            elif isinstance(left_typ, type.Ptr):
                                if m.self_is_ref:
                                    report.error(
                                        "cannot use pointers as references",
                                        expr.pos
                                    )
                                    report.help(
                                        "consider casting this pointer to a reference"
                                    )
                                else:
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
                            f"expected method, found {m.sym_kind()}",
                            expr_left.field_pos
                        )
                elif f := left_sym.lookup_field(expr_left.field_name):
                    if isinstance(f.typ, type.Fn):
                        if inside_parens:
                            self.check_call(f.typ.info(), expr)
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
                    expr.info = expr_left.field_info
                    self.check_call(expr.info, expr)
                elif isinstance(expr_left.field_info,
                                sym.Type) and expr_left.field_info.kind in (
                                    TypeKind.ErrType, TypeKind.Union,
                                    TypeKind.Trait
                                ):
                    self.check_special_ctor(expr_left.sym, expr)
                else:
                    report.error(
                        f"expected function, found {expr_left.field_info.sym_kind()}",
                        expr.pos
                    )
            else:
                report.error(
                    "invalid expression used in call expression", expr.pos
                )

            if expr.has_err_handler():
                if isinstance(expr.typ, type.Result):
                    if expr.err_handler.is_propagate:
                        pass
                    else:
                        self.check_expr(expr.err_handler.expr)
                else:
                    report.error(
                        f"{expr.info.kind()} `{expr.info.name}` does not returns a result value",
                        expr.err_handler.pos
                    )
            elif isinstance(expr.typ, type.Result):
                report.error(
                    f"{expr.info.kind()} `{expr}` returns a result", expr.pos
                )
                report.note(
                    "should handle this with `catch` or propagate with `.!`"
                )

            return expr.typ
        elif isinstance(expr, ast.ReturnExpr):
            if expr.has_expr:
                ret_typ = self.check_expr(expr.expr)
                try:
                    self.check_types(ret_typ, self.cur_fn.ret_typ)
                except utils.CompilerError as e:
                    report.error(e.args[0], expr.expr.pos)
                    report.note(
                        f"in return argument for {self.cur_fn.sym_kind()} `{self.cur_fn.name}`"
                    )
            elif self.cur_fn != None and self.cur_fn.ret_typ != self.comp.void_t:
                report.error(
                    f"expected `{self.cur_fn.ret_typ}` argument", expr.pos
                )
                report.note(
                    f"in return argument for {self.cur_fn.sym_kind()} `{self.cur_fn.name}`"
                )
            return self.comp.no_return_t
        elif isinstance(expr, ast.RaiseExpr):
            if self.cur_fn != None and not isinstance(
                self.cur_fn.ret_typ, type.Result
            ):
                report.error(
                    f"current {self.cur_fn.sym_kind()} does not returns a result value",
                    expr.pos
                )
            expr_typ = self.check_expr(expr.expr)
            if expr_typ.get_sym().kind != TypeKind.ErrType:
                report.error("expected a errtype value", expr.expr.pos)
            return self.comp.no_return_t
        elif isinstance(expr, ast.Block):
            if self.inside_unsafe and expr.is_unsafe:
                report.warn("unnecesary `unsafe` block", expr.pos)
            self.inside_unsafe = expr.is_unsafe
            for stmt in expr.stmts:
                self.check_stmt(stmt)
            if expr.is_expr:
                expr.typ = self.check_expr(expr.expr)
            else:
                expr.typ = self.comp.void_t
            self.inside_unsafe = False
            if expr.is_unsafe and self.unsafe_operations == 0:
                report.warn("unnecesary `unsafe` block", expr.pos)
            self.unsafe_operations = 0
            return expr.typ
        elif isinstance(expr, ast.IfExpr):
            if expr.is_comptime:
                expr.typ = self.comp.void_t
                if expr.branch_idx > -1:
                    expr.typ = self.check_expr(
                        expr.branches[expr.branch_idx].expr
                    )
            else:
                for i, b in enumerate(expr.branches):
                    if not b.is_else:
                        if self.check_expr(b.cond) != self.comp.bool_t:
                            report.error(
                                "non-boolean expression used as `if` condition",
                                b.cond.pos
                            )
                    if i == 0: expr.typ = self.check_expr(b.expr)
            return expr.typ
        elif isinstance(expr, ast.MatchExpr):
            expr_typ = self.check_expr(expr.expr)
            expr_sym = expr_typ.get_sym()
            expected_branch_typ = self.comp.void_t
            if expr.is_typematch and not (
                expr_typ == self.comp.error_t
                or expr_sym.kind in (TypeKind.Union, TypeKind.Trait)
            ):
                report.error(
                    f"expected union, trait or error value, found `{expr_typ}`, for typematch",
                    expr.expr.pos
                )
            for i, b in enumerate(expr.branches):
                for p in b.pats:
                    pat_t = self.check_expr(p)
                    if expr.is_typematch:
                        if expr_sym.kind == TypeKind.Union:
                            if pat_t not in expr_sym.info.variants:
                                report.error(
                                    f"union `{expr_sym.name}` has no variant `{pat_t}`",
                                    p.pos
                                )
                        elif expr_sym.kind == TypeKind.Trait:
                            if not pat_t.get_sym().implement_trait(
                                expr_sym.qualname()
                            ):
                                report.error(
                                    f"type `{pat_t}` does not implement trait `{expr_sym.name}`",
                                    p.pos
                                )
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
            return expr.typ
        else:
            print(expr.__class__, expr)

    def inside_unsafe_block(self):
        self.unsafe_operations += 1
        return self.inside_unsafe

    def check_special_ctor(self, info, expr):
        expr.typ = type.Type(info)
        if info.kind == TypeKind.ErrType:
            if len(expr.args) == 1:
                msg_t = self.check_expr(expr.args[0].expr)
                if msg_t != self.comp.str_t:
                    report.error(
                        f"expected string value, found `{msg_t}`",
                        expr.args[0].pos
                    )
            elif len(expr.args) != 0:
                report.error(
                    f"expected 1 argument, found {len(expr.args)}", expr.pos
                )
        elif info.kind == TypeKind.Union:
            if len(expr.args) == 1:
                value_t = self.check_expr(expr.args[0].expr)
                if value_t not in info.info.variants:
                    report.error(
                        f"union `{info.name}` has no variant `{value_t}`",
                        expr.args[0].pos
                    )
            else:
                report.error(
                    f"expected 1 argument, found {len(expr.args)}", expr.pos
                )
        else: # trait
            if len(expr.args) == 1:
                value_t = self.check_expr(expr.args[0].expr)
                if not value_t.get_sym().implement_trait(info.qualname()):
                    report.error(
                        f"type `{value_t}` does not implement trait `{info.name}`",
                        expr.args[0].pos
                    )
            else:
                report.error(
                    f"expected 1 argument, found {len(expr.args)}", expr.pos
                )

    def check_call(self, info, expr):
        kind = info.kind()
        expr.typ = info.ret_typ

        if info.is_unsafe and not self.inside_unsafe_block():
            report.warn(
                f"{kind} `{info.name}` should be called inside `unsafe` block",
                expr.pos
            )

        fn_args_len = len(info.args)
        if (fn_args_len < 0):
            fn_args_len = 0

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
            for i in range(args_len, fn_args_len):
                arg = info.args[i]
                if arg.has_def_expr:
                    if carg := expr.get_named_arg(arg.name):
                        args.append(ast.CallArg(carg.expr, carg.expr.pos))
                    else:
                        args.append(ast.CallArg(arg.def_expr, arg.def_expr.pos))
            expr.args = args

        expr_args_len = expr.pure_args_count()
        expr_msg = f"expected {fn_args_len} argument(s), found {expr_args_len}"
        if expr_args_len < fn_args_len:
            report.error(f"too few arguments to {kind} `{info.name}`", expr.pos)
            report.note(expr_msg)
            return expr.typ
        elif expr_args_len > fn_args_len:
            report.error(
                f"too many arguments to {kind} `{info.name}`", expr.pos
            )
            report.note(expr_msg)
            return expr.typ

        oet = self.expected_type
        for i, arg in enumerate(expr.args):
            arg_fn = info.args[i]

            self.expected_type = arg_fn.typ
            call_arg_t = self.check_expr(arg.expr)
            self.expected_type = oet

            if not info.is_extern and i >= len(info.args) - 1:
                try:
                    self.check_types(call_arg_t, arg_fn.typ)
                except utils.CompilerError as e:
                    report.error(e.args[0], arg.pos)
                    report.note(
                        f"in argument `{arg_fn.name}` of {info.kind()} `{info.name}`"
                    )
        return expr.typ

    def check_builtin_call(self, builtin_call):
        ret_typ = self.comp.void_t
        if builtin_call.name == "trace":
            # TODO: runtime, not comptime
            # pos = colors.bold(f"{builtin_call.pos.file}:{builtin_call.pos.line}:")
            # utils.eprint(f"{pos} {builtin_call.args[0].lit}")
            pass
        elif builtin_call.name in ("sizeof", "alignof"):
            size, align = self.comp.type_size(builtin_call.args[0].typ)
            if builtin_call.name == "sizeof":
                pass #print(size)
            else:
                pass #print(align)
            ret_typ = self.comp.usize_t
        elif builtin_call.name in ("compile_warn", "compile_error"):
            msg = builtin_call.args[0].lit
            if builtin_call.name == "compile_warn":
                report.warn(msg, builtin_call.pos)
            elif builtin_call.name == "compile_error":
                report.error(msg, builtin_call.pos)
        elif builtin_call.name == "assert":
            cond = builtin_call.args[0]
            if self.check_expr(cond) != self.comp.bool_t:
                report.error(
                    "non-boolean expression used as `assert` condition",
                    cond.pos
                )
        else:
            report.error(
                f"unknown builtin function `{builtin_call.name}`",
                builtin_call.pos
            )
        return ret_typ

    def check_types(self, got, expected):
        if not self.check_compatible_types(got, expected):
            if got == self.comp.none_t:
                if isinstance(expected, type.Optional):
                    got_str = str(expected)
                else:
                    got_str = f"?{expected}"
            else:
                got_str = str(got)
            if self.trait_not_satisfied:
                self.trait_not_satisfied = False
                raise utils.CompilerError(
                    f"type `{got_str}` does not implement trait `{expected}`"
                )
            raise utils.CompilerError(
                f"expected type `{expected}`, found `{got_str}`"
            )

    def check_compatible_types(self, got, expected):
        if isinstance(expected, type.Ptr) and got == self.comp.none_t:
            return True # allow *T == none
        elif (isinstance(expected, type.Ref)
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

        if isinstance(expected, type.Fn) and isinstance(got, type.Fn):
            return expected == got

        if isinstance(expected, type.Ref) and isinstance(got, type.Ref):
            return expected.typ == got.typ
        elif isinstance(expected, type.Ptr) and isinstance(got, type.Ptr):
            if expected.typ == self.comp.c_void_t:
                # *c_void == *T, is valid
                return True
            return expected.typ == got.typ

        if isinstance(expected,
                      type.Optional) and isinstance(got, type.Optional):
            return expected.typ == got.typ
        elif isinstance(expected,
                        type.Optional) and not isinstance(got, type.Optional):
            if got == self.comp.none_t:
                return True
            return expected.typ == got
        elif expected == self.comp.none_t and (
            isinstance(got, type.Optional) or isinstance(got, type.Ptr)
        ):
            return True

        got_sym = got.get_sym()
        exp_sym = expected.get_sym()

        if self.comp.is_number(expected) and self.comp.is_number(got):
            return self.promote_number(expected, got) == expected
        elif expected == self.comp.error_t and got_sym.kind == TypeKind.ErrType:
            return True # valid
        elif exp_sym.kind == TypeKind.Array and got_sym.kind == TypeKind.Array:
            return exp_sym.info.elem_typ == got_sym.info.elem_typ and exp_sym.info.size == got_sym.info.size
        elif exp_sym.kind == TypeKind.Slice and got_sym.kind == TypeKind.Slice:
            return exp_sym.info.elem_typ == got_sym.info.elem_typ
        elif exp_sym.kind == TypeKind.Tuple and got_sym.kind == TypeKind.Tuple:
            if len(exp_sym.info.types) != len(got_sym.info.types):
                return False
            for i, t in enumerate(exp_sym.info.types):
                if t != got_sym.info.types[i]:
                    return False
            return True
        elif exp_sym.kind == TypeKind.Union:
            return got in exp_sym.info.variants
        elif exp_sym.kind == TypeKind.Trait and got_sym.kind != TypeKind.Trait:
            if got_sym.implement_trait(exp_sym.qualname()):
                return True
            self.trait_not_satisfied = True
            return False

        return exp_sym == got_sym

    def promote(self, left_typ, right_typ):
        if isinstance(left_typ, type.Ptr):
            if self.comp.is_int(right_typ):
                return left_typ
            return self.comp.void_t
        elif isinstance(right_typ, type.Ptr):
            if self.comp.is_int(left_typ):
                return right_typ
            return self.comp.void_t
        elif left_typ == right_typ:
            return left_typ
        elif self.comp.is_number(right_typ) and self.comp.is_number(left_typ):
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
            # float -> float (good)
            if bits_hi == 32:
                if type_lo.get_sym().kind in (TypeKind.Int64, TypeKind.Uint64):
                    return self.comp.void_t
                return type_hi
            else: # f64
                return type_hi

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
