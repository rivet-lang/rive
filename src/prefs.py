# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os
from enum import IntEnum as Enum, auto as auto_enum

from .utils import *

HELP = """Usage: rivetc [OPTIONS] INPUT

Compiler Options:
   --pkg-name NAME             Specify the name of the package being built.
   -v, --verbose               Use verbose output.

Help Options:
   -V, --version               Display compiler version.
   -h, --help                  Display this message."""


class OutputMode(Enum):
    BINARY = auto_enum()
    LIBRARY = auto_enum()


class Prefs:
    def __init__(self, args: [str]):
        if len(args) == 0:
            eprint(HELP)
            exit(0)

        self.input = ""
        self.pkg_name = "main"
        self.output_mode = OutputMode.BINARY
        self.is_verbose = False

        i = 0
        while i < len(args):
            arg = args[i]
            current_args = args[i:]

            # informative options
            if arg in ["-h", "--help"]:
                eprint(HELP)
                exit(0)
            elif arg in ["-V", "--version"]:
                eprint(f"Rivet {VERSION}")
                exit(0)

            # compiler options
            if arg.endswith(".ri"):
                if not os.path.exists(arg):
                    error(f"unable to read '{arg}': file not found")
                elif os.path.isdir(arg):
                    error(f"unable to read '{arg}': is a directory")
                elif self.input != "":
                    error("multiple input filenames provided")
                self.input = arg
            elif arg == "--pkg-name":
                if len(current_args) > 1:
                    self.pkg_name = option(current_args, arg, "main")
                    i += 1
                    if not self.pkg_name.isidentifier():
                        error(f"invalid package name `{self.pkg_name}`")
                else:
                    error("`--pkg-name` requires a name as argument")
            elif arg in ["-v", "--verbose"]:
                self.is_verbose = True
            else:
                error(f"unknown option: `{arg}`")

            i += 1

        if self.input == "":
            error("no input filename given")
