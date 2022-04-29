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

        # This field is `true` when we are in a root module, that is,
        # a package.
        self.is_pkg_level = False

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
                if self.tok.kind == Kind.KeyUnsafe:
                    name = "unsafe"
                    self.next()
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
                report.error(f"extern functions are not yet supported", pos)
                self.next()
        elif self.accept(Kind.KeyMod):
            old_is_pkg_level = self.is_pkg_level
            self.is_pkg_level = False

            name = self.parse_name()
            self.expect(Kind.Lbrace)
            decls = []
            while not self.accept(Kind.Rbrace):
                decls.append(self.parse_decl())

            self.is_pkg_level = old_is_pkg_level
            return ast.Mod(name, is_pub, decls)
        else:
            report.error(f"expected declaration, found {self.tok}", pos)
            self.next()
        return ast.EmptyDecl()

    # ---- types -------------------------------
    def parse_type(self):
        pos = self.tok.pos
        if self.tok.kind == Kind.Lparen and self.peek_tok.kind == Kind.Rparen:
            self.advance(2)
            return type.Type(self.comp.unit)
        elif self.tok.kind == Kind.Name:
            lit = self.parse_name()
            if lit == "unit":
                report.error("use `()` instead of `unit`", pos)
            elif lit == "rawptr":
                return type.Type(self.comp.rawptr)
            elif lit == "bool":
                return type.Type(self.comp.bool)
            elif lit == "rune":
                return type.Type(self.comp.rune)
            elif lit == "i8":
                return type.Type(self.comp.int8)
            elif lit == "i16":
                return type.Type(self.comp.int16)
            elif lit == "i32":
                return type.Type(self.comp.int32)
            elif lit == "i64":
                return type.Type(self.comp.int64)
            elif lit == "isize":
                return type.Type(self.comp.isize)
            elif lit == "u8":
                return type.Type(self.comp.uint8)
            elif lit == "u16":
                return type.Type(self.comp.uint16)
            elif lit == "u32":
                return type.Type(self.comp.uint32)
            elif lit == "u64":
                return type.Type(self.comp.uint64)
            elif lit == "usize":
                return type.Type(self.comp.usize)
            elif lit == "str":
                return type.Type(self.comp.str)
        else:
            report.error(f"expected type, found {self.tok}", pos)
        return type.UnknownType(self.tok)
