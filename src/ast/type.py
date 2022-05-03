# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

class UnknownType:
    def __init__(self, expr):
        self.expr = expr

    def __str__(self):
        return str(self.expr)

class Type:
    def __init__(self, sym):
        self.sym = sym

    def __str__(self):
        return str(self.sym.name)

class Ref:
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return "&" + str(self.typ)

class Ptr:
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return "*" + str(self.typ)

class Slice:
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return f"[{self.typ}]"

class Array:
    def __init__(self, typ, size):
        self.typ = typ
        self.size = size

    def __str__(self):
        return f"[{self.typ}; {self.size}]"

class Tuple:
    def __init__(self, types):
        self.types = types

    def __str__(self):
        return f"({', '.join([str(t) for t in self.types])})"

class Optional:
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return f"?{self.typ}"

class Result:
    def __init__(self, typ):
        self.typ = typ

    def __str__(self):
        return f"!{self.typ}"
