# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from llvmlite import binding as llvm

from . import prefs, parser, sly


class Compiler:
    def __init__(self, args: [str]):
        self.prefs = prefs.Prefs(args)
        self.source_files = []

    def parse_input(self):
        self.source_files.append(parser.parse(self.prefs.input, self.prefs, True))

    def parse(self, file):
        self.source_files.append(parser.parse(file, self.prefs))


def compile(args):
    # llvm.initialize()
    # llvm.initialize_all_asmprinters()
    # llvm.initialize_all_targets()

    compiler = Compiler(args)
    compiler.parse_input()

    # llvm.shutdown()
