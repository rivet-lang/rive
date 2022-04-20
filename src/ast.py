# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.


class SourceFile:
    def __init__(self, file, decls):
        self.file = file
        self.decls = decls


# ---- Declarations ----
class EmptyDecl:
    pass


class ExternPkg:
    def __init__(self, pkg_name, pos):
        self.pkg_name = pkg_name
        self.pos = pos


class Mod:
    def __init__(self, name, decls):
        self.name = name
        self.decls = decls
        self.is_pub = False


# ------ Expressions -------
class Ident:
    def __init__(self, name):
        self.name = name
