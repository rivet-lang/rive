# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from ..utils import CompilerError

def universe():
    st = Sym("universe")
    st.add(Type("bool"))
    st.add(Type("char"))
    st.add(Type("i8"))
    st.add(Type("i16"))
    st.add(Type("i32"))
    st.add(Type("i64"))
    st.add(Type("u8"))
    st.add(Type("u16"))
    st.add(Type("u32"))
    st.add(Type("u64"))
    st.add(Type("isize"))
    st.add(Type("usize"))
    st.add(Type("f32"))
    st.add(Type("f64"))
    st.add(Type("str"))
    st.add(Type("rawptr"))
    return st

class Sym:
    def __init__(self, name):
        self.name = name
        self.syms = []

    def add(self, sym):
        if self.exists(sym.name):
            raise CompilerError(
                f"another symbol with this name already exists: `{sym.name}`"
            )
        self.syms.append(sym)

    def lookup(self, name):
        for sym in self.syms:
            if sym.name == name:
                return sym
        return None

    def exists(self, name):
        if _ := self.lookup(name):
            return True
        return False

class Pkg(Sym):
    pass

class Mod(Sym):
    pass

class Type(Sym):
    pass
