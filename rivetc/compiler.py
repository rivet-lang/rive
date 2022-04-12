# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from llvmlite import binding as llvm

from . import prefs


class Compiler:
    def __init__(self, args: [str]):
        llvm.initialize()
        llvm.initialize_all_asmprinters()
        llvm.initialize_all_targets()

        self.prefs = prefs.Prefs(args)

        llvm.shutdown()
