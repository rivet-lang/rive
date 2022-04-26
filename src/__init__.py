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

        # Primitive types.
        self.unit = sym.Type("unit", sym.SymKind.Unit)
        self.rawptr = sym.Type("rawptr", sym.SymKind.Rawptr)
        self.bool = sym.Type("bool", sym.SymKind.Bool)
        self.char = sym.Type("char", sym.SymKind.Char)
        self.int8 = sym.Type("i8", sym.SymKind.Int8)
        self.int16 = sym.Type("i16", sym.SymKind.Int16)
        self.int32 = sym.Type("i32", sym.SymKind.Int32)
        self.int64 = sym.Type("i64", sym.SymKind.Int64)
        self.isize = sym.Type("isize", sym.SymKind.Isize)
        self.uint8 = sym.Type("u8", sym.SymKind.Uint8)
        self.uint16 = sym.Type("u16", sym.SymKind.Uint16)
        self.uint32 = sym.Type("u32", sym.SymKind.Uint32)
        self.uint64 = sym.Type("u64", sym.SymKind.Uint64)
        self.usize = sym.Type("usize", sym.SymKind.Usize)
        self.float32 = sym.Type("f32", sym.SymKind.Float32)
        self.float64 = sym.Type("f64", sym.SymKind.Float64)
        self.str = sym.Type(
            "str", sym.SymKind.Str, [sym.Field("len", is_pub=True)]
        )

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
