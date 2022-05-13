# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

from ..ast import Visibility
from ..utils import CompilerError

class Object:
    def __init__(self, is_mut, name, typ, is_arg):
        self.name = name
        self.typ = typ
        self.is_mut = is_mut
        self.is_changed = False
        self.is_used = False
        self.is_arg = is_arg

class Label:
    def __init__(self, name):
        self.name = name

class Scope:
    def __init__(self, start, parent=None):
        self.parent = parent
        self.detached_from_parent = False
        self.objects = []
        self.start = start
        self.end = -1

    def add(self, obj):
        if obj.name == "_":
            return # ignore special var
        if _ := self.lookup(obj.name):
            raise CompilerError(f"duplicate object `{obj.name}`")
        self.objects.append(obj)

    def dont_lookup_parent(self):
        return self.detached_from_parent or self.parent == None

    def lookup(self, name):
        sc = self
        while True:
            for obj in sc.objects:
                if obj.name == name:
                    return obj
            if sc.dont_lookup_parent():
                break
            sc = sc.parent
        return None

class ABI(Enum):
    Rivet = auto_enum()
    C = auto_enum()

    def from_string(abi):
        if abi == "C":
            return ABI.C
        elif abi == "Rivet":
            return ABI.Rivet
        return None

    def __repr__(self):
        if self == ABI.Rivet:
            return "Rivet"
        return "C"

    def __str__(self):
        return self.__repr__()

class Sym:
    def __init__(self, vis, name):
        self.vis = vis
        self.name = name
        self.syms = []
        self.parent = None

    def add(self, sym):
        if asym := self.lookup(sym.name):
            if isinstance(asym, Type) and asym.kind == TypeKind.Placeholder:
                # update placeholder
                asym.vis = sym.vis
                asym.kind = sym.kind
                asym.fields = sym.fields
                for ss in sym.syms:
                    if asym.exists(ss.name):
                        raise CompilerError(
                            f"type `{asym.name}` has duplicate symbol: `{ss.name}`"
                        )
                    asym.syms.append(ss)
                asym.info = sym.info
                return
            else:
                raise CompilerError(
                    f"another symbol with this name already exists: `{sym.name}`"
                )
        sym.parent = self
        self.syms.append(sym)

    def add_or_extend_mod(self, sym):
        if m := self.lookup(sym.name):
            return m
        idx = len(self.syms)
        self.syms.append(sym)
        return self.syms[idx]

    def lookup(self, name):
        for sym in self.syms:
            if sym.name == name:
                return sym
        return None

    def exists(self, name):
        if _ := self.lookup(name):
            return True
        return False

    def sym_kind(self):
        if isinstance(self, Pkg):
            return "package"
        elif isinstance(self, Mod):
            return "module"
        elif isinstance(self, Const):
            return "constant"
        elif isinstance(self, Static):
            return "static"
        elif isinstance(self, Type):
            return "type"
        return "function"

    def __getitem__(self, idx):
        if isinstance(idx, str):
            return self.lookup(idx)
        return self.syms[idx]

class Pkg(Sym):
    pass

class Mod(Sym):
    pass

class Const(Sym):
    def __init__(self, vis, name, typ, expr):
        Sym.__init__(self, vis, name)
        self.expr = expr
        self.typ = typ

class Static(Sym):
    def __init__(self, vis, is_mut, name, typ):
        Sym.__init__(self, vis, name)
        self.is_mut = is_mut
        self.typ = typ

class Field:
    def __init__(self, name, is_mut, is_pub, typ):
        self.name = name
        self.is_mut = is_mut
        self.is_pub = is_pub
        self.typ = typ

class TypeKind(Enum):
    Placeholder = auto_enum()
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
    Alias = auto_enum()
    ErrType = auto_enum()
    Array = auto_enum()
    Slice = auto_enum()
    Tuple = auto_enum()
    Enum = auto_enum()
    Struct = auto_enum()
    Union = auto_enum()
    Trait = auto_enum()

# Type infos

class AliasInfo:
    def __init__(self, parent):
        self.parent = parent

class ErrTypeInfo:
    def __init__(self, nr):
        self.nr = nr

class ArrayInfo:
    def __init__(self, elem_typ, size):
        self.elem_typ = elem_typ
        self.size = size

class SliceInfo:
    def __init__(self, elem_typ):
        self.elem_typ = elem_typ

class TupleInfo:
    def __init__(self, types):
        self.types = types

class EnumInfo:
    def __init__(self, variants):
        self.variants = variants

class UnionInfo:
    def __init__(self, variants, no_tag):
        self.variants = variants
        self.no_tag = no_tag # C-like union

class Type(Sym):
    def __init__(self, vis, name, kind, fields=[], info=None):
        Sym.__init__(self, vis, name)
        self.kind = kind
        self.fields = fields
        self.info = info

    def lookup_field(self, name):
        for f in self.fields:
            if f.name == name:
                return f
        return None

    def has_field(self, name):
        if _ := self.lookup_field(name):
            return True
        return False

class Arg:
    def __init__(self, name, is_mut, typ, def_expr, has_def_expr, pos):
        self.name = name
        self.is_mut = is_mut
        self.typ = typ
        self.def_expr = def_expr
        self.has_def_expr = has_def_expr
        self.pos = pos

class Fn(Sym):
    def __init__(
        self, abi, vis, is_extern, is_unsafe, name, args, ret_is_mut, ret_typ
    ):
        Sym.__init__(self, vis, name)
        self.abi = abi
        self.is_extern = is_extern
        self.is_unsafe = is_unsafe
        self.args = args
        self.ret_is_mut = ret_is_mut
        self.ret_typ = ret_typ

    def typ(self):
        from .type import Fn, FnArg
        args = []
        for arg in self.args:
            args.append(FnArg(arg.is_mut, arg.typ))
        return Fn(
            self.is_unsafe, self.is_extern, self.abi, args, self.ret_is_mut,
            self.ret_typ
        )

def universe():
    uni = Sym(Visibility.Private, "universe")
    uni.add(Type(Visibility.Public, "c_void", TypeKind.CVoid))
    uni.add(Type(Visibility.Public, "void", TypeKind.Void))
    uni.add(Type(Visibility.Public, "ptr", TypeKind.Ptr))
    uni.add(Type(Visibility.Public, "bool", TypeKind.Bool))
    uni.add(Type(Visibility.Public, "rune", TypeKind.Rune))
    uni.add(Type(Visibility.Public, "i8", TypeKind.Int8))
    uni.add(Type(Visibility.Public, "i16", TypeKind.Int16))
    uni.add(Type(Visibility.Public, "i32", TypeKind.Int32))
    uni.add(Type(Visibility.Public, "i64", TypeKind.Int64))
    uni.add(Type(Visibility.Public, "isize", TypeKind.Isize))
    uni.add(Type(Visibility.Public, "u8", TypeKind.Uint8))
    uni.add(Type(Visibility.Public, "u16", TypeKind.Uint16))
    uni.add(Type(Visibility.Public, "u32", TypeKind.Uint32))
    uni.add(Type(Visibility.Public, "u64", TypeKind.Uint64))
    uni.add(Type(Visibility.Public, "usize", TypeKind.Usize))
    uni.add(Type(Visibility.Public, "f32", TypeKind.Float32))
    uni.add(Type(Visibility.Public, "f64", TypeKind.Float64))
    uni.add(
        Type(
            Visibility.Public,
            "str",
            TypeKind.Str,
            fields=[Field("len", True, False, None)]
        )
    )
    uni.add(
        Type(
            Visibility.Public,
            "error",
            TypeKind.Struct,
            fields=[Field("msg", True, False, None)]
        )
    )
    return uni
