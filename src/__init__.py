# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import prefs, parser, report


VERSION = "0.1.0b"


class CompilerError(Exception):
    pass


class Compiler:
    def __init__(self, args: [str]):
        self.prefs = prefs.Prefs(args)
        self.source_files = []

    def parse_files(self):
        parser.Parser(self.prefs).parse_files()
        if report.ERRORS > 0:
            exit(1)


def main(args):
    compiler = Compiler(args)
    compiler.parse_files()
