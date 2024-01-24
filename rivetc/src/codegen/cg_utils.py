# Copyright (C) 2023 Jose Mendoza. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

import os

from ..sym import TypeKind
from .. import ast, sym, type, utils
from ..token import OVERLOADABLE_OPERATORS_STR

def decode_escape(ch):
    if ch.startswith("\\"):
        code = ch[1:]
        code_b = utils.bytestr(code).buf[0]
        if code in ("\\", "'", '"'):
            return chr(code_b)
        elif code in ("a", "b", "f"):
            return chr(code_b - 90)
        elif code == "n":
            return "\n"
        elif code == "r":
            return "\r"
        elif code == "t":
            return "\t"
        elif code == "v":
            return "\v"
    return ch

def prefix_type(tt):
    prefix = ""
    if isinstance(tt, type.Ptr):
        _t = tt
        while isinstance(_t, type.Ptr):
            prefix += "ptr_"
            if _t.is_mut:
                prefix += "mut_"
            _t = _t.typ
        prefix += prefix_type(tt.typ)
    elif isinstance(tt, type.Ptr):
        prefix += "ref_"
        if tt.is_mut:
            prefix += "mut_"
        prefix += prefix_type(tt.typ)
    elif isinstance(tt, type.Option):
        prefix += "opt_" + prefix_type(tt.typ)
    return prefix

def mangle_type(typ):
    if isinstance(typ, type.Func):
        s = "fn_"
        if typ.is_unsafe:
            s += "unsafe_"
        if typ.is_extern:
            s += f"extern_{typ.abi}_"
        if typ.is_method:
            s += "m_"
        if typ.self_is_mut:
            s += "_sm_"
        elif typ.self_is_ptr:
            s += "_sr_"
        if typ.is_variadic:
            s += "_v_"
        s += f"_args{len(typ.args)}"
        return s
    return f"{prefix_type(typ)}{mangle_symbol(typ.symbol())}"

def mangle_symbol(s):
    if len(s.mangled_name) > 0:
        return s.mangled_name
    res = []
    root = s
    while True:
        if s.is_universe:
            break
        if isinstance(s, sym.Mod):
            s.mangled_name = "".join([
                f"{len(n)}{n}" for n in s.name.split(".")
            ])
            res.insert(0, s.mangled_name)
        elif isinstance(s, sym.Type):
            if s.kind == TypeKind.Tuple:
                name = "Tuple_"
                for i, tt in enumerate(s.info.types):
                    name += mangle_type(tt)
                    if i < len(s.info.types) - 1:
                        name += "_"
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == TypeKind.Slice:
                res.insert(0, "4core5Slice")
                s.mangled_name = "_R4core5Slice"
            elif s.kind == TypeKind.DynArray:
                res.insert(0, "4core8DynArray")
                s.mangled_name = "_R4core8DynArray"
            elif s.kind == TypeKind.Array:
                name = f"Array_{mangle_type(s.info.elem_typ)}_{s.info.size}"
                name = f"{len(name)}{name}"
                res.insert(0, name)
                s.mangled_name = name
            elif s.kind == TypeKind.String:
                res.insert(0, "4core6string")
                s.mangled_name = "_R4core6string"
            else:
                res.insert(0, f"{len(s.name)}{s.name}")
        elif s.name in OVERLOADABLE_OPERATORS_STR:
            name = OVERLOADABLE_OPERATORS_STR[s.name]
            name = f"{len(name)}{name}"
            res.insert(0, name)
            s.mangled_name = name
        else:
            res.insert(0, f"{len(s.name)}{s.name}")
        if s.parent == None:
            break
        else:
            s = s.parent
    res.insert(0, "_R")

    if isinstance(root, sym.Func):
        if root.is_method:
            res.append("M")
        else:
            res.append("F")

    root.mangled_name = "".join(res)
    return root.mangled_name
