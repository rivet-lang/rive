# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .lexer import Lexer
from .sly import Parser as SlyParser


class Parser(SlyParser):
    tokens = Lexer.tokens

    def __init__(self):
        self.functions = {}

    @_("functions")
    def functions(self, p):
        pass


def parse(filename: str, prefs, is_root=False):
    lexer = Lexer()
    parser = Parser()
    parser.parse(lexer.tokenize(open(filename).read()))
