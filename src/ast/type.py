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
