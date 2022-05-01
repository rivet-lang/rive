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
        if self.exists(sym.name):
            raise CompilerError(
                f"another symbol with this name already exists: `{sym.name}`"
            )
        self.syms.append(sym)

    def lookup(self, name):
        for sym in self.syms:
            if sym.name == name:
                return sym
        return None

    def exists(self, name):
        if _ := self.lookup(name):
            return True
        return False

    def __getitem__(self, idx):
        if isinstance(idx, str):
            return self.lookup(idx)
        return self.syms[idx]

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
    CVoid = auto_enum()
    Void = auto_enum()
    Ptr = auto_enum()
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
    def __init__(self, name, rec_typ, args, ret_typ):
        Sym.__init__(self, name)
        self.rec_typ = rec_typ
        self.args = args
        self.ret_typ = ret_typ

def universe():
    uni = Sym("universe")
    uni.add(Type("c_void", TypeKind.CVoid))
    uni.add(Type("void", TypeKind.Void))
    uni.add(Type("ptr", TypeKind.Ptr))
    uni.add(Type("bool", TypeKind.Bool))
    uni.add(Type("rune", TypeKind.Rune))
    uni.add(Type("i8", TypeKind.Int8))
    uni.add(Type("i16", TypeKind.Int16))
    uni.add(Type("i32", TypeKind.Int32))
    uni.add(Type("i64", TypeKind.Int64))
    uni.add(Type("isize", TypeKind.Isize))
    uni.add(Type("u8", TypeKind.Uint8))
    uni.add(Type("u16", TypeKind.Uint16))
    uni.add(Type("u32", TypeKind.Uint32))
    uni.add(Type("u64", TypeKind.Uint64))
    uni.add(Type("usize", TypeKind.Usize))
    uni.add(Type("f32", TypeKind.Float32))
    uni.add(Type("f64", TypeKind.Float64))
    uni.add(Type("str", TypeKind.Str, [Field("len", is_pub=True)]))
    return uni
