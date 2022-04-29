# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .ast import sym
from . import prefs, parser, report, utils

class Compiler:
    def __init__(self, args: [str]):
        # `universe` is the mega-package where all the packages being
        # compiled reside.
        self.universe = sym.Sym("universe")

        self.prefs = prefs.Prefs(args)
        self.source_files = []

    def parse_files(self):
        self.source_files = parser.Parser(self).parse_pkg()
        if report.ERRORS > 0:
            self.abort()

    def abort(self):
        if report.ERRORS == 1:
            msg = f"could not compile package `{self.prefs.pkg_name}`, aborting due to previous error."
        else:
            msg = f"could not compile package `{self.prefs.pkg_name}`, aborting due to {report.ERRORS} previous errors."
        utils.error(msg)
        exit(1)

def main(args):
    comp = Compiler(args)
    comp.parse_files()
