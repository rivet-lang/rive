# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.


class ExternPkg:
    def __init__(self, pkg):
        self.pkg = pkg


class SourceFile:
    def __init__(self, name, decls, is_root=False):
        self.name = name
        self.decls = decls
        self.is_root = is_root


class Mod:
    def __init__(self, name, decls):
        self.name = name
        self.decls = decls
        self.is_pub = False


class Ident:
    def __init__(self, name):
        self.name = name
