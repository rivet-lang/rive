# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from .sym import TypeKind

class _Ptr: # ugly hack =/
    def __init__(self, val):
        self.val = val

    def store(self, val):
        self.val.__class__ = val.__class__
        self.val.__dict__ = val.__dict__

class BaseType:
    def unalias(self):
        if isinstance(self, Result):
            self.typ.unalias()
        elif isinstance(self, Optional):
            self.typ.unalias()
        elif isinstance(self, Fn):
            for at in self.arg_types:
                at.unalias()
            self.ret_typ.unalias()
        elif isinstance(self, Tuple):
            for t in self.types:
                t.unalias()
        elif isinstance(self, Array):
            self.typ.unalias()
        elif isinstance(self, Slice):
            self.typ.unalias()
        elif isinstance(self, Ptr):
            self.typ.unalias()
        elif isinstance(self, Ref):
            self.typ.unalias()
        elif isinstance(self, Type):
            if self.is_resolved() and self.sym.kind == TypeKind.Alias:
                self.sym.info.parent.unalias()
                _Ptr(self).store(self.sym.info.parent)

class Type(BaseType):
    def __init__(self, sym):
        self.sym = sym
        self.expr = None
        self.unresolved = False

    @staticmethod
    def unresolved(expr):
        typ = Type(None)
        typ.expr = expr
        typ.unresolved = True
        return typ

    def resolve(self, sym):
        self.sym = sym
        self.unresolved = False

    def is_resolved(self):
        return not self.unresolved

    def __str__(self):
        return str(self.sym.name)

class Ref(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return "&" + str(self.typ)

class Ptr(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return "*" + str(self.typ)

class Slice(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return f"[{self.typ}]"

class Array(BaseType):
    def __init__(self, typ, size):
        self.typ = typ
        self.size = size

    def __str__(self):
        return f"[{self.typ}; {self.size}]"

class Tuple(BaseType):
    def __init__(self, types):
        self.types = types

    def __str__(self):
        return f"({', '.join([str(t) for t in self.types])})"

class FnArg:
    def __init__(self, is_mut, typ):
        self.is_mut = is_mut
        self.typ = typ

class Fn(BaseType):
    def __init__(self, is_unsafe, is_extern, abi, args, ret_is_mut, ret_typ):
        self.is_unsafe = is_unsafe
        self.is_extern = is_extern
        self.abi = abi
        self.args = args
        self.ret_is_mut = ret_is_mut
        self.ret_typ = ret_typ

    def __str__(self):
        res = ""
        if self.is_unsafe:
            res += "unsafe "
        if self.is_extern:
            res += f'extern "{self.abi}" '
        res += "fn("
        for i, arg in enumerate(self.args):
            if arg.is_mut:
                res += "mut "
            res += str(arg.typ)
            if i < len(self.args) - 1:
                res += ", "
        res += f") "
        if self.ret_is_mut:
            res += "mut "
        res += str(self.ret_typ)
        return res

class Optional(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return f"?{self.typ}"

class Result(BaseType):
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return f"!{self.typ}"
