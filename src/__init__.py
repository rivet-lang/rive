# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .ast import sym, type
from . import prefs, parser, report, utils

class Compiler:
    def __init__(self, args: [str]):
        # `universe` is the mega-package where all the packages being
        # compiled reside.
        self.universe = sym.universe()

        # Primitive types.
        # NOTE: the difference between `c_void` and `void` is that
        # the former corresponds to C's `void`, while the latter,
        # behind the scenes, is simply an alias to `u8`.
        self.c_void_t = type.Type(self.universe[0])
        self.void_t = type.Type(self.universe[1])
        self.rawptr_t = type.Type(self.universe[2])
        self.bool_t = type.Type(self.universe[3])
        self.rune_t = type.Type(self.universe[4])
        self.int8_t = type.Type(self.universe[5])
        self.int16_t = type.Type(self.universe[6])
        self.int32_t = type.Type(self.universe[7])
        self.int64_t = type.Type(self.universe[8])
        self.isize_t = type.Type(self.universe[9])
        self.uint8_t = type.Type(self.universe[10])
        self.uint16_t = type.Type(self.universe[11])
        self.uint32_t = type.Type(self.universe[12])
        self.uint64_t = type.Type(self.universe[13])
        self.usize_t = type.Type(self.universe[14])
        self.float32_t = type.Type(self.universe[15])
        self.float64_t = type.Type(self.universe[16])
        self.str_t = type.Type(self.universe[17])

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
