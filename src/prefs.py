# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os
import glob
from enum import IntEnum as Enum, auto as auto_enum

from .utils import error, eprint

VERSION = "0.1.0a"
HELP = """Usage: rivetc [OPTIONS] INPUTS

The compiler can receive both files and directories as input, example:
   rivetc my_file.ri my_folder/ my_folder2/ other_file.ri

Compiler Options:
   --pkg-name NAME             Specify the name of the package being built.
   -v, --verbose               Use verbose output.

Help Options:
   -V, --version               Display compiler version.
   -h, --help                  Display this message."""

def option(args, param):
    for (i, arg) in enumerate(args):
        if param == arg:
            if i + 1 < len(args):
                return args[i + 1]
            break
    return None

class PkgMode(Enum):
    BINARY = auto_enum()
    LIBRARY = auto_enum()

class Prefs:
    def __init__(self, args: [str]):
        if len(args) == 0:
            eprint(HELP)
            return

        self.inputs = []
        self.pkg_name = "main"
        self.pkg_mode = PkgMode.BINARY
        self.is_verbose = False

        i = 0
        while i < len(args):
            arg = args[i]
            current_args = args[i:]

            # informative options
            if arg in ("-h", "--help"):
                eprint(HELP)
                return
            elif arg in ("-V", "--version"):
                eprint(f"Rivet {VERSION}")
                return

            # compiler options
            if arg.endswith(".ri"):
                if os.path.isdir(arg):
                    error(f"unable to read '{arg}': is a directory")
                elif not os.path.exists(arg):
                    error(f"unable to read '{arg}': file not found")
                elif arg in self.inputs:
                    error(f"duplicate file '{arg}'")
                self.inputs.append(arg)
            elif arg == "--pkg-name":
                if pkg_name := option(current_args, arg):
                    self.pkg_name = pkg_name
                    i += 1
                    if not self.pkg_name.isidentifier():
                        error(f"invalid package name `{self.pkg_name}`")
                else:
                    error("`--pkg-name` requires a name as argument")
            elif arg in ("-v", "--verbose"):
                self.is_verbose = True
            elif os.path.isdir(arg):
                files = glob.glob(f"{arg}/*.ri")
                if len(files) == 0:
                    error(f"`{files}` does not have .ri files")
                for f in files:
                    self.inputs.append(f)
            else:
                error(f"unknown option: `{arg}`")

            i += 1

        if len(self.inputs) == 0:
            error("no input received")
