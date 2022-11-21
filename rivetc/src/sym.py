# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from enum import IntEnum as Enum, auto as auto_enum

from .token import NO_POS
from .utils import CompilerError

SYMBOL_COUNT = 0

def new_symbol_id():
    global SYMBOL_COUNT
    ret = SYMBOL_COUNT
    SYMBOL_COUNT += 1
    return ret

class ObjLevel(Enum):
    Rec = auto_enum()
    Arg = auto_enum()
    Local = auto_enum()

class Obj:
    def __init__(self, is_mut, name, typ, level):
        self.name = name
        self.is_mut = is_mut
        self.is_changed = False
        self.is_used = False
        self.level = level
        self.typ = typ

class Scope:
    def __init__(self, start, parent = None):
        self.parent = parent
        self.detached_from_parent = False
        self.objects = []
        self.start = start
        self.end = 0

    def add(self, obj):
        if obj.name == "_":
            return # ignore special var
        if self.exists(obj.name):
            raise CompilerError(f"duplicate object `{obj.name}`")
        self.objects.append(obj)

    def exists(self, name):
        if _ := self.lookup(name):
            return True
        return False

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

    def dont_lookup_parent(self):
        return self.detached_from_parent or self.parent == None

    def update_type(self, name, typ):
        if obj := self.lookup(name):
            obj.typ = typ

class ABI(Enum):
    Rivet = auto_enum()
    C = auto_enum()

    @staticmethod
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

class Vis(Enum):
    Priv = auto_enum()
    Pub = auto_enum() # Public outside current module
    Prot = auto_enum() # Protected inside current class

    def is_pub(self):
        return self == Vis.Pub

    def __repr__(self):
        if self == Vis.Pub:
            return "pub"
        elif self == Vis.Prot:
            return "prot"
        return "" # private

    def __str__(self):
        return self.__repr__()

class Sym:
    def __init__(self, vis, name, abi = ABI.Rivet):
        self.attrs = None
        self.id = new_symbol_id()
        self.abi = abi
        self.vis = vis
        self.name = name
        self.mangled_name = ""
        self.qualified_name = ""
        self.parent = None
        self.syms = []
        self.is_universe = isinstance(self, Mod) and self.id == 0
        self.is_root = False

    def add(self, sym):
        if asym := self.find(sym.name):
            if isinstance(asym, Type) and asym.kind == TypeKind.Placeholder:
                # update placeholder
                asym.update(sym)
                return
            raise CompilerError(
                f"{self.typeof()} `{self.name}` has duplicate symbol `{sym.name}`"
            )
        sym.parent = self
        self.syms.append(sym)

    def add_and_return(self, sym):
        idx = len(self.syms)
        for i, asym in enumerate(self.syms):
            if asym.name == sym.name:
                if isinstance(asym, Type) and asym.kind == TypeKind.Placeholder:
                    # update placeholder
                    asym.update(sym)
                    return self.syms[i]
                raise CompilerError(
                    f"{self.typeof()} `{self.name}` has duplicate symbol `{sym.name}`"
                )
        sym.parent = self
        self.syms.append(sym)
        return self.syms[idx]

    def add_or_get_mod(self, sym):
        if m := self.find(sym.name):
            return m
        return self.add_and_return(sym)

    def get_public_syms(self):
        syms = []
        for s in self.syms:
            if s.vis.is_pub():
                syms.append(s)
        return syms

    def find(self, name):
        for sym in self.syms:
            if sym.name == name:
                return sym
        return None

    def exists(self, name):
        if _ := self.find(name):
            return True
        return False

    def mod(self):
        p = self
        while True:
            if isinstance(p, Mod):
                break
            p = p.parent
            if p == None:
                break
        return p

    def has_access_to(self, other):
        self_mod = self.mod()
        other_mod = other.mod()
        return (
            other_mod.is_universe or self_mod == other or self_mod == other_mod
            or self_mod == other_mod.parent or self_mod.parent == other.parent
        )

    def typeof(self):
        if isinstance(self, Mod):
            return "module"
        elif isinstance(self, Const):
            return "constant"
        elif isinstance(self, Var):
            return "variable"
        elif isinstance(self, Type):
            return "type"
        elif isinstance(self, Fn):
            if self.is_method:
                return "method"
            return "function"
        return "unknown symbol kind"

    def qualname(self):
        if len(self.qualified_name) > 0:
            return self.qualified_name
        if self.parent == None or self.parent.is_universe:
            self.qualified_name = self.name
            return self.qualified_name
        self.qualified_name = f"{self.parent.qualname()}.{self.name}"
        return self.qualified_name

    def is_runtime_mod(self):
        return isinstance(self, Mod) and self.name == "runtime"

    def __getitem__(self, idx):
        if isinstance(idx, str):
            if s := self.find(idx):
                return s
            raise Exception(f"cannot find symbol `{idx}` in `{self.name}`")
        return self.syms[idx]

    def __eq__(self, other):
        if other == None:
            return False
        return self.id == other.id

class SymRef(Sym):
    def __init__(self, vis, name, ref):
        Sym.__init__(self, vis, name)
        self.ref = ref

class Mod(Sym):
    def add_or_get_array(self, elem_typ, size):
        unique_name = f"[{elem_typ.qualstr()}; {size}]"
        if sym := self.find(unique_name):
            return sym
        from .type import Ptr, Type as type_Type
        return self.add_and_return(
            Type(
                Vis.Pub, unique_name, TypeKind.Array,
                info = ArrayInfo(elem_typ, size)
            )
        )

    def add_or_get_vec(self, elem_typ):
        unique_name = f"[{elem_typ.qualstr()}]"
        if sym := self.find(unique_name):
            return sym
        from .type import Ptr, Type as type_Type
        vec_sym = Type(
            Vis.Pub, unique_name, TypeKind.Vec, info = VecInfo(elem_typ),
            fields = [
                Field("len", False, Vis.Pub, type_Type(self[14])),
                Field("cap", False, Vis.Pub, type_Type(self[14]))
            ]
        )
        vec_sym.add(
            Fn(
                ABI.Rivet, Vis.Pub, False, False, True, False, "push",
                [Arg("value", False, elem_typ, None, False, NO_POS)],
                type_Type(self[0]), False, True, NO_POS, True, False
            )
        )
        vec_sym.add(
            Fn(
                ABI.Rivet, Vis.Pub, False, False, True, False, "pop", [],
                elem_typ, False, True, NO_POS, True, False
            )
        )
        vec_sym.add(
            Fn(
                ABI.Rivet, Vis.Pub, False, False, True, False, "is_empty", [],
                type_Type(self[3]), False, True, NO_POS, False, False
            )
        )
        vec_sym.add(
            Fn(
                ABI.Rivet, Vis.Pub, False, False, True, False, "clone", [],
                type_Type(vec_sym), False, True, NO_POS, False, False
            )
        )
        return self.add_and_return(vec_sym)

    def add_or_get_tuple(self, types):
        unique_name = f"({', '.join([t.qualstr() for t in types])})"
        if sym := self.find(unique_name):
            return sym
        return self.add_and_return(
            Type(Vis.Pub, unique_name, TypeKind.Tuple, info = TupleInfo(types))
        )

class Const(Sym):
    def __init__(self, vis, name, typ, expr):
        Sym.__init__(self, vis, name)
        self.expr = expr
        self.evaled_expr = None
        self.has_evaled_expr = False
        self.ir_expr = None
        self.has_ir_expr = False
        self.typ = typ

class Var(Sym):
    def __init__(self, vis, is_mut, is_extern, abi, name, typ):
        Sym.__init__(self, vis, name, abi)
        self.is_extern = is_extern
        self.is_mut = is_mut
        self.typ = typ

class Field:
    def __init__(
        self, name, is_mut, vis, typ, has_def_expr = False, def_expr = None
    ):
        self.name = name
        self.is_mut = is_mut
        self.vis = vis
        self.typ = typ
        self.has_def_expr = has_def_expr
        self.def_expr = def_expr

class TypeKind(Enum):
    Placeholder = auto_enum()
    Never = auto_enum()
    Void = auto_enum()
    Nil = auto_enum()
    Bool = auto_enum()
    Rune = auto_enum()
    Int8 = auto_enum()
    Int16 = auto_enum()
    Int32 = auto_enum()
    Int64 = auto_enum()
    Uint8 = auto_enum()
    Uint16 = auto_enum()
    Uint32 = auto_enum()
    Uint64 = auto_enum()
    Isize = auto_enum()
    Usize = auto_enum()
    UntypedInt = auto_enum()
    UntypedFloat = auto_enum()
    Float32 = auto_enum()
    Float64 = auto_enum()
    String = auto_enum()
    Alias = auto_enum()
    Array = auto_enum()
    Vec = auto_enum()
    Tuple = auto_enum()
    Enum = auto_enum()
    Trait = auto_enum()
    Class = auto_enum()
    Struct = auto_enum()

    def is_primitive(self):
        if self in (
            TypeKind.Void, TypeKind.Nil, TypeKind.Bool, TypeKind.Rune,
            TypeKind.Int8, TypeKind.Int16, TypeKind.Int32, TypeKind.Int64,
            TypeKind.Isize, TypeKind.Uint8, TypeKind.Uint16, TypeKind.Uint32,
            TypeKind.Uint64, TypeKind.Usize, TypeKind.UntypedInt,
            TypeKind.UntypedFloat, TypeKind.Float32, TypeKind.Float64,
            TypeKind.Never
        ):
            return True
        return False

    def __repr__(self):
        if self == TypeKind.Void:
            return "void"
        elif self == TypeKind.Nil:
            return "nil"
        elif self == TypeKind.Bool:
            return "bool"
        elif self == TypeKind.Rune:
            return "rune"
        elif self == TypeKind.Int8:
            return "i8"
        elif self == TypeKind.Int16:
            return "i16"
        elif self == TypeKind.Int32:
            return "i32"
        elif self == TypeKind.Int64:
            return "i64"
        elif self == TypeKind.Isize:
            return "isize"
        elif self == TypeKind.Uint8:
            return "u8"
        elif self == TypeKind.Uint16:
            return "u16"
        elif self == TypeKind.Uint32:
            return "u32"
        elif self == TypeKind.Uint64:
            return "u64"
        elif self == TypeKind.Usize:
            return "usize"
        elif self == TypeKind.UntypedInt:
            return "untyped_int"
        elif self == TypeKind.UntypedFloat:
            return "untyped_float"
        elif self == TypeKind.Float32:
            return "f32"
        elif self == TypeKind.Float64:
            return "f64"
        elif self == TypeKind.String:
            return "string"
        elif self == TypeKind.Alias:
            return "alias"
        elif self == TypeKind.Array:
            return "array"
        elif self == TypeKind.Vec:
            return "vector"
        elif self == TypeKind.Tuple:
            return "tuple"
        elif self == TypeKind.Trait:
            return "trait"
        elif self == TypeKind.Class:
            return "class"
        elif self == TypeKind.Struct:
            return "struct"
        elif self == TypeKind.Enum:
            return "enum"
        elif self == TypeKind.Never:
            return "never"
        return "placeholder"

    def __str__(self):
        return self.__repr__()

# Type infos

class AliasInfo:
    def __init__(self, parent):
        self.parent = parent

class ArrayInfo:
    def __init__(self, elem_typ, size):
        self.elem_typ = elem_typ
        self.size = size
        self.has_contains_method = False

class VecInfo:
    def __init__(self, elem_typ):
        self.elem_typ = elem_typ
        self.has_contains_method = False

class TupleInfo:
    def __init__(self, types):
        self.types = types

class EnumValue:
    def __init__(self, name, value):
        self.name = name
        self.value = value

class EnumInfo:
    def __init__(self, underlying_typ):
        self.underlying_typ = underlying_typ
        self.values = []

    def add_value(self, name, value):
        self.values.append(EnumValue(name, value))

    def get_value(self, name):
        for v in self.values:
            if v.name == name:
                return v
        return None

    def has_value(self, name):
        if _ := self.get_value(name):
            return True
        return False

class TraitInfo:
    def __init__(self):
        self.implements = []
        self.has_objects = False

    def indexof(self, sym):
        for idx, s in enumerate(self.implements):
            if sym == s:
                return idx
        return 0

class ClassInfo:
    def __init__(self):
        self.base = None
        self.is_base = False

class StructInfo:
    def __init__(self, is_opaque):
        self.is_opaque = is_opaque
        self.bases = []

class Type(Sym):
    def __init__(self, vis, name, kind, fields = [], info = None):
        Sym.__init__(self, vis, name)
        self.kind = kind
        self.fields = fields.copy()
        self.full_fields_ = []
        self.info = info
        self.size = -1
        self.align = -1

    def find_field(self, name):
        for f in self.fields:
            if f.name == name:
                return f
        if self.kind == TypeKind.Class and self.info.base:
            if f := self.info.base.find_field(name):
                if f.vis in (Vis.Pub, Vis.Prot):
                    return f
        elif self.kind == TypeKind.Struct:
            for base in self.info.bases:
                if f := base.find_field(name):
                    if f.vis in (Vis.Pub, Vis.Prot):
                        return f
        return None

    def has_field(self, name):
        if _ := self.find_field(name):
            return True
        return False

    def find(self, name):
        if s := Sym.find(self, name):
            return s
        if self.kind == TypeKind.Class and self.info.base:
            if s := Sym.find(self.info.base, name):
                return s
        elif self.kind == TypeKind.Struct:
            for base in self.info.bases:
                if s := Sym.find(base, name):
                    return s
        return None

    def full_fields(self):
        if len(self.full_fields_) > 0:
            return self.full_fields_
        fields = []
        if self.kind == TypeKind.Class and self.info.base:
            fields += self.info.base.full_fields()
        elif self.kind == TypeKind.Struct:
            for base in self.info.bases:
                fields += base.full_fields()
        for f in self.fields:
            fields.append(f)
        self.full_fields_ = fields
        return fields

    def update(self, other):
        if self.kind == TypeKind.Placeholder:
            # update placeholder
            self.vis = other.vis
            self.kind = other.kind
            self.fields = other.fields
            for ss in other.syms:
                self.add(ss)
            self.info = other.info

    def is_subtype_of(self, t):
        if self == t:
            return True
        if self.kind == TypeKind.Class and self.info.base:
            if self.info.base == t:
                return True
            return self.info.base.is_subtype_of(t)
        elif self.kind == TypeKind.Struct:
            for base in self.info.bases:
                if base.is_subtype_of(t):
                    return True
        return False

    def is_boxed(self):
        return self.kind in (
            TypeKind.Trait, TypeKind.Class, TypeKind.String, TypeKind.Vec
        )

class Arg:
    def __init__(self, name, is_mut, typ, def_expr, has_def_expr, pos):
        self.name = name
        self.is_mut = is_mut
        self.is_self = name == "self"
        self.typ = typ
        self.def_expr = def_expr
        self.has_def_expr = has_def_expr
        self.pos = pos

class Fn(Sym):
    def __init__(
        self, abi, vis, is_extern, is_unsafe, is_method, is_variadic, name,
        args, ret_typ, has_named_args, has_body, name_pos, self_is_mut,
        self_is_ref
    ):
        Sym.__init__(self, vis, name)
        self.is_main = False
        self.abi = abi
        self.is_extern = is_extern
        self.is_unsafe = is_unsafe
        self.is_method = is_method
        self.is_variadic = is_variadic
        self.self_typ = None
        self.self_is_mut = self_is_mut
        self.self_is_ref = self_is_ref
        self.args = args
        self.ret_typ = ret_typ
        self.has_named_args = has_named_args
        self.has_body = has_body
        self.name_pos = name_pos

    def args_len(self):
        from .type import Variadic
        len_ = 0
        for arg in self.args:
            if not (arg.is_self or isinstance(arg.typ, Variadic)):
                len_ += 1
        return len_

    def kind(self):
        if self.is_method:
            return "method"
        return "function"

    def typ(self):
        from .type import Fn
        return Fn(
            self.is_extern, self.abi, self.is_method, self.args,
            self.is_variadic, self.ret_typ, self.self_is_mut, self.self_is_ref
        )

def universe():
    from .type import Ptr, Type as type_Type

    uni = Mod(Vis.Priv, "universe")
    uni.add(Type(Vis.Pub, "void", TypeKind.Void))
    uni.add(Type(Vis.Pub, "never", TypeKind.Never))
    uni.add(Type(Vis.Pub, "nil", TypeKind.Nil))
    uni.add(Type(Vis.Pub, "bool", TypeKind.Bool))
    uni.add(Type(Vis.Pub, "rune", TypeKind.Rune))
    uni.add(Type(Vis.Pub, "i8", TypeKind.Int8))
    uni.add(Type(Vis.Pub, "i16", TypeKind.Int16))
    uni.add(Type(Vis.Pub, "i32", TypeKind.Int32))
    uni.add(Type(Vis.Pub, "i64", TypeKind.Int64))
    uni.add(Type(Vis.Pub, "isize", TypeKind.Isize))
    uni.add(Type(Vis.Pub, "u8", TypeKind.Uint8))
    uni.add(Type(Vis.Pub, "u16", TypeKind.Uint16))
    uni.add(Type(Vis.Pub, "u32", TypeKind.Uint32))
    uni.add(Type(Vis.Pub, "u64", TypeKind.Uint64))
    uni.add(Type(Vis.Pub, "usize", TypeKind.Usize))
    uni.add(Type(Vis.Pub, "untyped_int", TypeKind.UntypedInt))
    uni.add(Type(Vis.Pub, "untyped_float", TypeKind.UntypedFloat))
    uni.add(Type(Vis.Pub, "f32", TypeKind.Float32))
    uni.add(Type(Vis.Pub, "f64", TypeKind.Float64))
    uni.add(Type(Vis.Pub, "string", TypeKind.String))
    uni.add(Type(Vis.Pub, "Error", TypeKind.Class, info = ClassInfo()))

    return uni
