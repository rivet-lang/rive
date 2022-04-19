# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

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
        self.read_first_tokens()

    def read_first_tokens(self):
        for _ in range(3):
            self.next()

    def next(self):
        self.prev_tok = self.tok
        self.tok = self.peek_tok
        self.peek_tok = self.lexer.next()
