# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import report, tokens
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
        for input in self.prefs.inputs:
            self.parse_file(input)

    def parse_file(self, file):
        self.lexer = Lexer.from_file(file)
        for _ in range(2):
            self.next()

    def next(self):
        self.prev_tok = self.tok
        self.tok = self.peek_tok
        self.peek_tok = self.lexer.next()

    def accept(self, kind):
        if self.tok.kind == kind:
            self.next()
            return True
        return False

    def check(self, kind):
        if self.accept(kind):
            return
        kstr = str(kind)
        if tokens.is_key(kstr) or (len(kstr) > 0 and not kstr[0].isalpha()):
            kstr = f"`{kstr}`"
        report.error(f"expected {kstr}, found {self.tok} ", self.tok.pos)
        self.next()
