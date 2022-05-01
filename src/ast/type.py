# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import sym

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

# Primitive types.
# NOTE: the difference between `c_void` and `void` is that
# the former corresponds to C's `void`, while the latter,
# behind the scenes, is simply an alias to `u8`.
c_void_t = Type(sym.c_void)
void_t = Type(sym.void)
rawptr_t = Type(sym.rawptr)
bool_t = Type(sym.bool)
rune_t = Type(sym.rune)
int8_t = Type(sym.int8)
int16_t = Type(sym.int16)
int32_t = Type(sym.int32)
int64_t = Type(sym.int64)
isize_t = Type(sym.isize)
uint8_t = Type(sym.uint8)
uint16_t = Type(sym.uint16)
uint32_t = Type(sym.uint32)
uint64_t = Type(sym.uint64)
usize_t = Type(sym.usize)
float32_t = Type(sym.float32)
float64_t = Type(sym.float64)
str_t = Type(sym.str)
