# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

class Type:
    def __init__(self, name):
        self.name = name

    def ptr(self):
        return Pointer(self)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return self.name

class Pointer:
    def __init__(self, typ):
        self.typ = typ

    def ptr(self):
        return Pointer(self)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return f"{self.typ}*"

class Array:
    def __init__(self, typ, size):
        self.typ = typ
        self.size = size

    def ptr(self):
        return Pointer(self)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return f"{self.typ}{[self.size]}"

class Function:
    def __init__(self, args, ret_typ):
        self.args = args
        self.ret_typ = ret_typ

    def ptr(self):
        return Pointer(self)

    def __repr__(self):
        return str(self)

    def __str__(self):
        return f"function({', '.join([str(arg) for arg in self.args])}) {self.ret_typ}"
