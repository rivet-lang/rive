# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import report, tokens, ast
from .tokens import Kind
from .lexer import Lexer


class Parser:
    def __init__(self, prefs):
        self.prefs = prefs
        self.lexer = None

        self.prev_tok = None
        self.tok = None
        self.peek_tok = None

    def parse_files(self):
        source_files = []
        for input in self.prefs.inputs:
            source_files.append(self.parse_file(input))
        return source_files

    def parse_file(self, file):
        self.lexer = Lexer.from_file(file)
        for _ in range(2):
            self.next()
        self.parse_decls()
        return ast.SourceFile(file, [])

    def next(self):
        self.prev_tok = self.tok
        self.tok = self.peek_tok
        self.peek_tok = self.lexer.next()

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
        while self.tok.kind != Kind.EOF:
            return self.parse_decl()

    def parse_decl(self):
        pos = self.tok.pos
        if self.accept(Kind.KeyExtern):
            if self.accept(Kind.KeyPkg):
                # extern package
                extern_pkg = self.parse_name()
                self.expect(Kind.Semicolon)
                return ast.ExternPkg(extern_pkg, pos)
            else:
                # extern functions
                report.error(f"extern functions are not yet supported", pos)
                self.next()
        else:
            report.error(f"expected declaration, found {self.tok}", pos)
            report.note("xxx")
            self.next()
        return ast.EmptyDecl()
