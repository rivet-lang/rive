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
    def __init__(self, ty):
        self.ty = ty

class Ptr:
    def __init__(self, ty):
        self.ty = ty

class Optional:
    def __init__(self, ty):
        self.ty = ty

class Result:
    def __init__(self, ty):
        self.ty = ty
