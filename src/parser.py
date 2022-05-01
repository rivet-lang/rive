# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .ast import sym, type
from .lexer import Lexer
from .tokens import Kind
from . import report, tokens, ast

class Parser:
    def __init__(self, comp):
        self.comp = comp
        self.lexer = None

        self.prev_tok = None
        self.tok = None
        self.peek_tok = None

        self.scope = None

        # This field is `true` when we are in a root module, that is,
        # a package.
        self.is_pkg_level = False
        self.inside_unsafe = False

    def parse_pkg(self):
        self.is_pkg_level = True
        return self.parse_module_files()

    def parse_module_files(self):
        source_files = []
        for input in self.comp.prefs.inputs:
            source_files.append(self.parse_file(input))
        return source_files

    def parse_file(self, file):
        self.lexer = Lexer.from_file(file)
        self.advance(2)
        decls = self.parse_decls()
        return ast.SourceFile(file, decls)

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
        kstr = str(kind)
        if tokens.is_key(kstr) or (len(kstr) > 0 and not kstr[0].isalpha()):
            kstr = f"`{kstr}`"
        report.error(f"expected {kstr}, found {self.tok} ", self.tok.pos)
        self.next()

    # ---- utilities ------------------
    def parse_name(self):
        lit = self.tok.lit
        self.expect(Kind.Name)
        return lit

    # ---- declarations --------------
    def parse_decls(self):
        decls = []
        while self.tok.kind != Kind.EOF:
            decls.append(self.parse_decl())
        return decls

    def parse_attrs(self):
        attrs = ast.Attrs()
        while self.accept(Kind.Lbracket):
            while True:
                pos = self.tok.pos
                if self.accept(Kind.KeyUnsafe):
                    name = "unsafe"
                else:
                    name = self.parse_name()
                attrs.add(ast.Attr(name, pos))
                if not self.accept(Kind.Semicolon):
                    break
            self.expect(Kind.Rbracket)
        return attrs

    def parse_decl(self):
        attrs = self.parse_attrs()
        is_pub = self.accept(Kind.KeyPub)
        pos = self.tok.pos
        if self.accept(Kind.KeyExtern):
            if is_pub:
                report.error("`extern` declarations cannot be public", pos)
            if self.accept(Kind.KeyPkg):
                # extern package
                if not self.is_pkg_level:
                    report.error(
                        "extern packages can only be declared at the package level",
                        pos,
                    )
                extern_pkg = self.parse_name()
                self.expect(Kind.Semicolon)
                return ast.ExternPkg(extern_pkg, pos)
            else:
                # extern functions
                report.error("extern functions are not yet supported", pos)
                self.next()
        elif self.accept(Kind.KeyMod):
            old_is_pkg_level = self.is_pkg_level
            self.is_pkg_level = False

            pos = self.tok.pos
            name = self.parse_name()
            self.expect(Kind.Lbrace)
            decls = []
            while not self.accept(Kind.Rbrace):
                decls.append(self.parse_decl())

            self.is_pkg_level = old_is_pkg_level
            return ast.ModDecl(name, is_pub, decls, pos)
        else:
            report.error(f"expected declaration, found {self.tok}", pos)
            self.next()
        return ast.EmptyDecl()

    # ---- expressions -------------------------
    def parse_expr(self):
        return self.parse_or_expr()

    def parse_or_expr(self):
        left = self.parse_and_expr()
        while self.accept(Kind.KeyOr):
            right = self.parse_and_expr()
            left = ast.BinaryExpr(left, Kind.KeyOr, right, right.pos)
        return left

    def parse_and_expr(self):
        left = self.parse_equality_expr()
        while self.accept(Kind.KeyAnd):
            right = self.parse_equality_expr()
            left = ast.BinaryExpr(left, Kind.KeyAnd, right, right.pos)
        return left

    def parse_equality_expr(self):
        left = self.parse_relational_expr()
        if self.tok.kind in [Kind.Eq, Kind.Ne]:
            op = self.tok.kind
            self.next()
            right = self.parse_relational_expr()
            left = ast.BinaryExpr(left, op, right, right.pos)
        return left

    def parse_relational_expr(self):
        left = self.parse_shift_expr()
        if self.tok.kind in [
            Kind.Gt,
            Kind.Lt,
            Kind.Ge,
            Kind.Le,
            Kind.KeyIn,
            Kind.KeyNotIn,
        ]:
            op = self.tok.kind
            self.next()
            right = self.parse_shift_expr()
            left = ast.BinaryExpr(left, op, right, right.pos)
        elif self.tok.kind in [Kind.KeyIs, Kind.KeyNotIs]:
            op = self.tok.kind
            self.next()
            pos = self.tok.pos
            right = TypeNode(self.parse_type(), pos)
            left = ast.BinaryExpr(left, op, right, pos)
        return left

    def parse_shift_expr(self):
        left = self.parse_additive_expr()
        if self.tok.kind in [Kind.Lt, Kind.Gt]:
            op = Kind.Lshift if self.tok.kind == Kind.Lt else Kind.Rshift
            if self.tok.pos.pos + 1 == self.peek_tok.pos.pos:
                self.next()
                self.next()
                right = self.parse_additive_expr()
                left = ast.BinaryExpr(left, op, right, right.pos)
        elif self.tok.kind in [Kind.Amp, Kind.Pipe, Kind.Xor]:
            op = self.tok.kind
            self.next()
            right = self.parse_additive_expr()
            left = ast.BinaryExpr(left, op, right, right.pos)
        return left

    def parse_additive_expr(self):
        left = self.parse_multiplicative_expr()
        if self.tok.kind in [Kind.Plus, Kind.Minus]:
            op = self.tok.kind
            self.next()
            right = self.parse_multiplicative_expr()
            left = ast.BinaryExpr(left, op, right, right.pos)
        return left

    def parse_multiplicative_expr(self):
        left = self.parse_unary_expr()
        if self.tok.kind in [Kind.Mult, Kind.Div, Kind.Mod]:
            op = self.tok.kind
            self.next()
            right = self.parse_unary_expr()
            left = ast.BinaryExpr(left, op, right, right.pos)
        return left

    def parse_unary_expr(self):
        expr = self.empty_expr()
        if (
            self.tok.kind in [
                Kind.Amp, Kind.Bang, Kind.BitNot, Kind.Inc, Kind.Dec, Kind.Minus
            ]
        ):
            op = self.tok.kind
            pos = self.tok.pos
            self.next()
            right = self.parse_unary_expr()
            expr = ast.UnaryExpr(right, op, right.pos)
        else:
            expr = self.parse_primary_expr()
        return expr

    def parse_primary_expr(self):
        expr = self.empty_expr()
        if self.tok.kind in [
            Kind.KeyTrue,
            Kind.KeyFalse,
            Kind.Char,
            Kind.Number,
            Kind.String,
            Kind.KeyNone,
        ]:
            expr = self.parse_literal()
        elif self.tok.kind == Kind.KeySelf:
            pos = self.tok.pos
            self.next()
            expr = ast.SelfExpr(None, self.scope, pos)
        elif self.tok.kind == Kind.Lparen:
            self.expect(Kind.Lparen)
            e = self.parse_expr()
            if self.accept(Kind.Comma): # tuple
                exprs = [e]
                while True:
                    exprs.append(self.parse_expr())
                    if not self.accept(Kind.Comma):
                        break
                self.expect(Kind.Rparen)
                expr = ast.TupleLiteral(exprs, e.pos)
            else:
                self.expect(Kind.Rparen)
                expr = ast.ParExpr(e, e.pos)
        elif self.tok.kind == Kind.KeyUnsafe:
            if self.inside_unsafe:
                report.warn("`unsafe` is unnecessary", self.tok.pos)
            old_iu = self.inside_unsafe
            self.expect(Kind.KeyUnsafe)
            self.expect(Kind.Lbrace)
            self.inside_unsafe = True
            expr = self.parse_expr()
            self.inside_unsafe = old_iu
            self.expect(Kind.Rbrace)
            expr = ast.UnsafeExpr(expr, expr.pos)
        elif self.accept(Kind.KeyCast):
            self.expect(Kind.Lparen)
            expr = self.parse_expr()
            self.expect(Kind.Comma)
            ty = self.parse_type()
            self.expect(Kind.Rparen)
            expr = ast.CastExpr(expr, expr.pos, ty)
        elif self.accept(Kind.KeyTry):
            pos = self.prev_tok.pos
            expr = ast.TryExpr(self.parse_expr(), pos)
        elif self.tok.kind == Kind.Lbracket:
            elems = []
            pos = self.tok.pos
            self.next()
            elem_ty = self.parse_type()
            self.expect(Kind.Semicolon)
            has_unknown_size = False
            if self.tok.kind == Kind.Name and self.tok.lit == "_":
                size = None
                has_unknown_size = True
                self.next()
            else:
                size = self.parse_expr()
            self.expect(Kind.Rbracket)
            self.expect(Kind.Lbrace)
            if self.tok.kind != Kind.Rbrace:
                while True:
                    elems.append(self.parse_expr())
                    if not self.accept(Kind.Comma):
                        break
            self.expect(Kind.Rbrace)
            if has_unknown_size:
                size = ast.IntegerLiteral(str(len(elems)), pos)
            expr = ast.ArrayLiteral(elem_ty, elems, size, pos)
        elif self.tok.kind == Kind.KeyPkg:
            expr = self.parse_pkg_expr()
        else:
            if self.tok.kind == Kind.Name and self.peek_tok.kind == Kind.Char:
                if self.tok.lit != "b":
                    report.error(
                        "only `b` is recognized as a valid prefix for a character literal",
                        self.tok.pos,
                    )
                else:
                    expr = self.parse_character_literal()
            elif self.tok.kind == Kind.Name and self.peek_tok.kind == Kind.String:
                if self.tok.lit not in ("b", "r"):
                    report.error(
                        "only `b` and `r` are recognized as valid prefixes for a string literal",
                        self.tok.pos,
                    )
                else:
                    expr = self.parse_string_literal()
            elif self.tok.kind == Kind.Name and self.peek_tok.kind == Kind.Bang: # builtin call
                name = self.parse_name()
                self.expect(Kind.Bang)
                self.expect(Kind.Lparen)
                args = []
                if name in ("sizeof", "default"):
                    pos = self.tok.pos
                    args.append(TypeNode(self.parse_type(), pos))
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
            if self.accept(Kind.Lbrace):
                fields = {}
                if self.tok.kind != Kind.Rbrace:
                    while True:
                        key = self.parse_ident()
                        self.expect(Kind.Colon)
                        value = self.parse_expr()
                        fields[key] = value
                        if not self.accept(Kind.Comma):
                            break
                self.expect(Kind.Rbrace)
                expr = ast.StructLiteral(expr, fields, expr.pos)
            elif self.tok.kind in [Kind.Inc, Kind.Dec]:
                op = self.tok.kind
                self.next()
                expr = ast.PostfixExpr(expr, op, expr.pos)
            elif self.accept(Kind.Lparen):
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
                                    "expected named argument, found expression",
                                    self.tok.pos
                                )
                            expr2 = self.parse_expr()
                            args.append(ast.CallArg(expr2, expr2.pos))
                        if not self.accept(Kind.Comma):
                            break
                self.expect(Kind.Rparen)
                expr = ast.CallExpr(expr, args, expr.pos)
            elif self.accept(Kind.Dot):
                if self.accept(Kind.Mult):
                    expr = ast.IndirectExpr(expr, expr.pos)
                elif self.accept(Kind.Question):
                    expr = ast.NoneCheckExpr(expr, expr.pos)
                else:
                    name = self.parse_name()
                    expr = ast.SelectorExpr(expr, name, expr.pos)
            elif self.tok.kind == Kind.DoubleColon:
                expr = self.parse_path_expr(expr)
            elif self.accept(Kind.Lbracket):
                index = self.empty_expr()
                if self.accept(Kind.DotDot):
                    index = self.parse_expr()
                    index = ast.RangeExpr(
                        None, index, False, index.pos, False, True
                    )
                else:
                    index = self.parse_expr()
                    if self.accept(Kind.DotDot):
                        if self.tok.kind != Kind.Rbracket:
                            index2 = self.parse_expr()
                            index = ast.RangeExpr(
                                index, index2, False, index.pos, True, True
                            )
                        else:
                            index = ast.RangeExpr(
                                index, None, False, index.pos, True, False
                            )
                self.expect(Kind.Rbracket)
                expr = ast.IndexExpr(expr, index, expr.pos)
            elif self.tok.kind == Kind.DotDot:
                self.next()
                is_inclusive = self.accept(Kind.Assign)
                end = self.parse_expr()
                expr = ast.RangeExpr(expr, end, is_inclusive, expr.pos)
            else:
                break
        return expr

    def parse_path_expr(self, left):
        self.expect(Kind.DoubleColon)
        pos = self.tok.pos
        name = self.parse_name()
        expr = ast.PathExpr(left, name, pos)
        expr.is_last = self.tok.kind != Kind.DoubleColon
        return expr

    def parse_literal(self):
        if self.tok.kind in [Kind.KeyTrue, Kind.KeyFalse]:
            pos = self.tok.pos
            lit = self.tok.kind == Kind.KeyTrue
            self.next()
            return ast.BoolLiteral(lit, pos)
        elif self.tok.kind == Kind.Char:
            return self.parse_character_literal()
        elif self.tok.kind == Kind.Number:
            return self.parse_integer_literal()
        elif self.tok.kind == Kind.String:
            return self.parse_string_literal()
        elif self.tok.kind == Kind.KeyNone:
            pos = self.tok.pos
            self.next()
            return ast.NoneLiteral(pos)
        else:
            report.error(
                f"expected literal, found {self.tok.str()}", self.tok.pos
            )
        return self.empty_expr()

    def parse_integer_literal(self):
        pos = self.tok.pos
        lit = self.tok.lit
        is_float = "." in lit or "e" in lit or "E" in lit
        self.next()
        return (
            ast.FloatLiteral(lit, pos)
            if is_float else ast.IntegerLiteral(lit, pos)
        )

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
        is_bytestr = False
        is_raw = False
        if self.tok.kind == Kind.Name:
            is_raw = self.tok.lit == "r"
            is_bytestr = self.tok.lit == "b"
            self.expect(Kind.Name)
        lit = self.tok.lit
        pos = self.tok.pos
        self.expect(Kind.String)
        while self.accept(Kind.String):
            lit += self.prev_tok.lit
        return ast.StringLiteral(lit, is_raw, is_bytestr, pos)

    def parse_ident(self):
        pos = self.tok.pos
        name = self.parse_name()
        return ast.Ident(name, pos, scope=self.scope)

    def parse_pkg_expr(self):
        pos = self.tok.pos
        self.next()
        return ast.PkgExpr(pos)

    def empty_expr(self):
        return ast.EmptyExpr(self.tok.pos)

    # ---- types -------------------------------
    def parse_type(self):
        pos = self.tok.pos
        if self.tok.kind == Kind.Lparen and self.peek_tok.kind == Kind.Rparen:
            self.advance(2)
            return type.unit_t
        elif self.tok.kind == Kind.Name:
            lit = self.parse_name()
            if lit == "unit":
                report.error("use `()` instead of `unit`", pos)
            elif lit == "rawptr":
                return type.rawptr_t
            elif lit == "bool":
                return type.bool_t
            elif lit == "rune":
                return type.rune_t
            elif lit == "i8":
                return type.int8_t
            elif lit == "i16":
                return type.int16_t
            elif lit == "i32":
                return type.int32_t
            elif lit == "i64":
                return type.int64_t
            elif lit == "isize":
                return type.isize_t
            elif lit == "u8":
                return type.uint8_t
            elif lit == "u16":
                return type.uint16_t
            elif lit == "u32":
                return type.uint32_t
            elif lit == "u64":
                return type.uint64_t
            elif lit == "usize":
                return type.usize_t
            elif lit == "f32":
                return type.float32_t
            elif lit == "f64":
                return type.float64_t
            elif lit == "str":
                return type.str_t
        elif self.accept(Kind.Amp):
            ty = self.parse_type()
            return type.Ref(ty)
        elif self.accept(Kind.Mult):
            ty = self.parse_type()
            return type.Ptr(ty)
        elif self.accept(Kind.Question):
            ty = self.parse_type()
            return type.Optional(ty)
        else:
            report.error(f"expected type, found {self.tok}", pos)
        return type.UnknownType(self.tok)
