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

class Pkg(Sym):
    pass

class Mod(Sym):
    pass

class Field:
    def __init__(self, name, is_mut=False, is_pub=False):
        self.name = name
        self.is_mut = is_mut
        self.is_pub = is_pub

class SymKind(Enum):
    Unknown = auto_enum()
    Unit = auto_enum()
    Rawptr = auto_enum()
    Bool = auto_enum()
    Char = auto_enum()
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

class Type(Sym):
    def __init__(self, name, kind=SymKind.Unknown, fields=[]):
        Sym.__init__(self, name)
        self.kind = kind
        self.fields = fields

    def lookup_field(self, name):
        for f in self.fields:
            if f.name == name:
                return f
        return None

    def has_field(self, name):
        if f := self.lookup_field(name):
            return True
        return False
