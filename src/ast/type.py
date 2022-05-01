# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

class UnknownType:
    def __init__(self, expr):
        self.expr = expr

class Type:
    def __init__(self, sym):
        self.sym = sym

class Ref:
    def __init__(self, typ):
        self.typ = typ

class Ptr:
    def __init__(self, typ):
        self.typ = typ

class Slice:
    def __init__(self, typ):
        self.typ = typ

class Array:
    def __init__(self, typ, size):
        self.typ = typ
        self.size = size

class Tuple:
    def __init__(self, types):
        self.types = types

class Optional:
    def __init__(self, typ):
        self.typ = typ

class Result:
    def __init__(self, typ):
        self.typ = typ
