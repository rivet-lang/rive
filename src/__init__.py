# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .ast import sym, type
from . import (
    ast,
    token,
    prefs,
    report,
    utils,

    # stages
    parser,
    resolver,
    checker
)

class Compiler:
    def __init__(self, args: [str]):
        # `universe` is the mega-package where all the packages being
        # compiled reside.
        self.universe = sym.universe()

        # Primitive types.
        # NOTE: the difference between `c_void` and `void` is that the former
        # corresponds to C's `void`, while the latter, behind the scenes, is
        # simply an alias to `u8`.
        self.c_void_t = type.Type(self.universe[0])
        self.none_t = type.Type(self.universe[1])
        self.void_t = type.Type(self.universe[2])
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
        self.error_t = type.Type(self.universe[18])
        self.no_return_t = type.Type(self.universe[19])

        self.universe[17].fields[0].typ = self.usize_t # str.len: usize
        self.universe[18].fields[0].typ = self.str_t # error.msg: str

        self.pkg_sym = None

        self.source_files = []
        self.prefs = prefs.Prefs(args)

        self.pointer_size = 8 if self.prefs.target_bits == prefs.Bits.X64 else 4

        self.resolver = resolver.Resolver(self)
        self.checker = checker.Checker(self)

    def build_package(self):
        self.parse_files()
        if not self.prefs.check_syntax:
            self.resolver.resolve_files(self.source_files)
            if report.ERRORS > 0:
                self.abort()
            self.checker.check_files(self.source_files)
            if report.ERRORS > 0:
                self.abort()

    def parse_files(self):
        self.source_files = parser.Parser(self).parse_pkg()
        if report.ERRORS > 0:
            self.abort()

    # ======== TODO(StunxFS): move code to Table =============
    def is_int(self, typ):
        return self.is_signed_int(typ) or self.is_unsigned_int(typ)

    def is_float(self, typ):
        return typ in (self.float32_t, self.float64_t)

    def is_signed_int(self, typ):
        return typ in (
            self.int8_t, self.int16_t, self.int32_t, self.int64_t, self.isize_t
        )

    def is_unsigned_int(self, typ):
        return typ in (
            self.uint8_t, self.uint16_t, self.uint32_t, self.uint64_t,
            self.usize_t
        )

    # Returns the size and alignment (in bytes) of `typ`, similarly to
    # C's `sizeof(T)` and `alignof(T)`.
    def type_size(self, typ):
        if isinstance(typ, type.Optional):
            return type.type_size(typ.typ)
        elif isinstance(typ, (type.Ptr, type.Ref)):
            return self.pointer_size, self.pointer_size
        elif isinstance(typ, type.Fn):
            return self.pointer_size, 0
        sy = typ.get_sym()
        if sy.size != -1:
            return sy.size, sy.align
        size, align = 0, 0
        if sy.kind in (
            sym.TypeKind.Placeholder, sym.TypeKind.CVoid, sym.TypeKind.None_
        ):
            pass
        elif sy.kind == sym.TypeKind.Alias:
            size, align = self.type_size(sym.info.parent)
        elif sy.kind in (sym.TypeKind.Usize, sym.TypeKind.Isize):
            size = self.pointer_size
        elif sy.kind in (
            sym.TypeKind.Int8, sym.TypeKind.Uint8, sym.TypeKind.Bool
        ):
            size, align = 1, 1
        elif sy.kind in (sym.TypeKind.Int16, sym.TypeKind.Uint16):
            size, align = 2, 2
        elif sy.kind in (
            sym.TypeKind.Int32, sym.TypeKind.Uint32, sym.TypeKind.Rune,
            sym.TypeKind.Float32
        ):
            size, align = 4, 4
        elif sy.kind in (
            sym.TypeKind.Int64, sym.TypeKind.Uint64, sym.TypeKind.Float64
        ):
            size, align = 8, 8
        elif sy.kind == sym.TypeKind.ErrType:
            size, align = 1, 1
        elif sy.kind == sym.TypeKind.Enum:
            size, align = 4, 4 # == i32
        elif sy.kind == sym.TypeKind.Array:
            elem_size, elem_align = self.type_size(sy.info.elem_typ)
            size, align = int(sy.info.size.lit) * elem_size, elem_align
        elif sy.kind in (sym.TypeKind.Str, sym.TypeKind.Slice):
            size = self.pointer_size * 2
        elif sy.kind in (sym.TypeKind.Struct, sym.TypeKind.Tuple):
            total_size = 0
            max_alignment = 0
            types = list(map(lambda it: it.typ, sy.fields)) if sy.kind in (
                sym.TypeKind.Struct, sym.TypeKind.Str
            ) else sy.info.types
            for ftyp in types:
                field_size, alignment = self.type_size(ftyp)
                if alignment > max_alignment:
                    max_alignment = alignment
                total_size = self.round_up(total_size, alignment) + field_size
            size = self.round_up(total_size, max_alignment)
            align = max_alignment
        sy.size = size
        sy.align = align
        return size, align

    # Rounds the number `n` up to the next multiple `multiple`.
    # NOTE: `multiple` must be a power of 2.
    def round_up(self, n, multiple):
        return (n + multiple - 1) & (-multiple)

    def evalue_comptime_condition(self, cond):
        if isinstance(cond, ast.BoolLiteral):
            return cond.lit
        elif isinstance(cond, ast.Ident):
            if cond.is_comptime:
                report.error("invalid comptime condition", cond.pos)
            # operating systems
            elif cond.name == "_LINUX_":
                return self.prefs.target_os.equals_to_string(cond.name)
            # architectures
            elif cond.name in ("_AMD64_", "_i386_"):
                return self.prefs.target_arch.equals_to_string(cond.name)
            # bits
            elif cond.name in ("_x32_", "_x64_"):
                if cond.name == "_x32_":
                    return self.prefs.target_bits == Bits.X32
                else:
                    return self.prefs.target_bits == Bits.X64
            # endian
            elif cond.name in ("_LITTLE_ENDIAN_", "_BIG_ENDIAN_"):
                if cond.name == "_LITTLE_ENDIAN_":
                    return self.prefs.target_endian == Endian.Little
                else:
                    return self.prefs.target_endian == Endian.Big
            else:
                if cond.name.startswith("_") and cond.name.endswith("_"):
                    report.error(f"unknown builtin flag: `{cond}`", cond.pos)
                    return False
                return cond.name in self.prefs.flags
        elif isinstance(cond, ast.UnaryExpr):
            if cond.op == token.Kind.Bang:
                return not self.evalue_comptime_condition(cond.right)
            else:
                report.error(f"expected `!`, found token `{cond.op}`", cond.pos)
        elif isinstance(cond, ast.BinaryExpr):
            if cond.op in (token.Kind.KeyAnd, token.Kind.KeyOr):
                if cond.op == token.Kind.KeyAnd:
                    return self.evalue_comptime_condition(
                        cond.left
                    ) and self.evalue_comptime_condition(cond.right)
                else:
                    return self.evalue_comptime_condition(
                        cond.left
                    ) or self.evalue_comptime_condition(cond.right)
            else:
                report.error("invalid comptime condition", cond.pos)
        elif isinstance(cond, ast.ParExpr):
            return self.evalue_comptime_condition(cond.expr)
        else:
            report.error("invalid comptime condition", cond.pos)
        return False

    # ========================================================

    def abort(self):
        if report.ERRORS == 1:
            msg = f"could not compile package `{self.prefs.pkg_name}`, aborting due to previous error."
        else:
            msg = f"could not compile package `{self.prefs.pkg_name}`, aborting due to {report.ERRORS} previous errors."
        utils.error(msg)
        exit(1)

def main(args):
    comp = Compiler(args)
    comp.build_package()
