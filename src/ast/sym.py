# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

from ..utils import CompilerError

class Sym:
    def __init__(self, name):
        self.name = name
        self.syms = []

    def add(self, sym):
        if self.exists(name):
            raise CompilerError(
                f"another symbol with this name already exists: `{name}`"
            )
        self.syms.append(sym)

    def lookup(self, name):
        for sym in self.syms:
            if name == name:
                return sym
        return None

    def exists(self, name):
        if _ := self.lookup(name):
            return True
        return False

class Pkg(Sym):
    pass

class Mod(Sym):
    pass

class Field:
    def __init__(self, name, is_mut=False, is_pub=False):
        self.name = name
        self.is_mut = is_mut
        self.is_pub = is_pub

class TypeKind(Enum):
    Unit = auto_enum()
    Rawptr = auto_enum()
    Bool = auto_enum()
    Rune = auto_enum()
    Int8 = auto_enum()
    Int16 = auto_enum()
    Int32 = auto_enum()
    Int64 = auto_enum()
    Isize = auto_enum()
    Uint8 = auto_enum()
    Uint16 = auto_enum()
    Uint32 = auto_enum()
    Uint64 = auto_enum()
    Usize = auto_enum()
    Float32 = auto_enum()
    Float64 = auto_enum()
    Str = auto_enum()
    Array = auto_enum()
    Slice = auto_enum()
    Tuple = auto_enum()
    Struct = auto_enum()
    Union = auto_enum()
    Trait = auto_enum()

class Type(Sym):
    def __init__(self, name, kind, fields=[]):
        Sym.__init__(self, name)
        self.kind = kind
        self.fields = fields

    def lookup_field(self, name):
        for f in self.fields:
            if f.name == name:
                return f
        return None

    def has_field(self, name):
        if _ := self.lookup_field(name):
            return True
        return False

class Fn(Sym):
    def __init__(self, name, rec_ty, args, ret_ty):
        Sym.__init__(self, name)
        self.rec_ty = rec_ty
        self.args = args
        self.ret_ty = ret_ty

# Primitives.
unit = Type("unit", TypeKind.Unit)
rawptr = Type("rawptr", TypeKind.Rawptr)
bool = Type("bool", TypeKind.Bool)
rune = Type("rune", TypeKind.Rune)
int8 = Type("i8", TypeKind.Int8)
int16 = Type("i16", TypeKind.Int16)
int32 = Type("i32", TypeKind.Int32)
int64 = Type("i64", TypeKind.Int64)
isize = Type("isize", TypeKind.Isize)
uint8 = Type("u8", TypeKind.Uint8)
uint16 = Type("u16", TypeKind.Uint16)
uint32 = Type("u32", TypeKind.Uint32)
uint64 = Type("u64", TypeKind.Uint64)
usize = Type("usize", TypeKind.Usize)
float32 = Type("f32", TypeKind.Float32)
float64 = Type("f64", TypeKind.Float64)
str = Type("str", TypeKind.Str, [Field("len", is_pub=True)])
