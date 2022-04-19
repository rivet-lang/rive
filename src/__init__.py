# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import prefs, lexer, tokens


VERSION = "0.1.0b"


class CompilerError(Exception):
    pass


class Compiler:
    def __init__(self, args: [str]):
        self.prefs = prefs.Prefs(args)
        self.source_files = []

    def parse(self, file):
        # self.source_files.append(parser.parse(file, self.prefs))
        pass


def compile(args):
    compiler = Compiler(args)
    lex = lexer.Lexer.from_file(compiler.prefs.input)

    tok = lex.next()
    while tok.kind != tokens.Kind.EOF:
        print(tok)
        tok = lex.next()
