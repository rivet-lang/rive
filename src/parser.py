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

        # This field is `true` when we are in a root module, that
        # is, a package.
        self.is_root_mod = False

    def parse_pkg(self):
        self.is_root_mod = True
        return self.parse_module_files()

    def parse_module_files(self):
        source_files = []
        for input in self.prefs.inputs:
            source_files.append(self.parse_file(input))
        return source_files

    def parse_file(self, file):
        self.lexer = Lexer.from_file(file)
        for _ in range(2):
            self.next()
        decls = self.parse_decls()
        return ast.SourceFile(file, decls)

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
        decls = []
        while self.tok.kind != Kind.EOF:
            decls.append(self.parse_decl())
        return decls

    def parse_decl(self, is_pub=False):
        pos = self.tok.pos
        if self.accept(Kind.KeyExtern):
            if is_pub:
                report.error("`extern` declarations cannot be public", pos)
            if self.accept(Kind.KeyPkg):
                # extern package
                if not self.is_root_mod:
                    # Only packages can import other packages.
                    report.error(
                        "extern package declarations are valid only inside a package",
                        pos,
                    )
                extern_pkg = self.parse_name()
                self.expect(Kind.Semicolon)
                return ast.ExternPkg(extern_pkg, pos)
            else:
                # extern functions
                report.error(f"extern functions are not yet supported", pos)
                self.next()
        elif self.accept(Kind.KeyPub):
            return self.parse_decl(True)
        elif self.accept(Kind.KeyMod):
            name = self.parse_name()
            self.expect(Kind.Lbrace)
            decls = []
            while not self.accept(Kind.Rbrace):
                decls.append(self.parse_decl())
            return ast.Mod(name, is_pub, decls)
        else:
            report.error(f"expected declaration, found {self.tok}", pos)
            self.next()
        return ast.EmptyDecl()
