# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os, sys, glob
from enum import IntEnum as Enum, auto as auto_enum

from .utils import error, eprint

VERSION = "0.1.0"
HELP = """Usage: rivetc [OPTIONS] INPUTS

The compiler can receive both files and directories as input, example:
   rivetc my_file.ri my_folder/ my_folder2/ other_file.ri

Options:
   --pkg-name <name>
      Specify the name of the package being built.

   -os <name>, --target-os <name>
      Change the target OS that Rivet tries to compile for. By default, the
      target OS is the host system.

      Here is a list of the operating systems, supported by Rivet:
        `linux`

   -v, --verbose
      Print additional messages to the console.

   -V, --version
      Print compiler version.

   -h, --help
      Print this message."""

def option(args, param):
    for (i, arg) in enumerate(args):
        if param == arg:
            if i + 1 < len(args):
                return args[i + 1]
            break
    return None

class OS(Enum):
    Linux = auto_enum()
    # Windows = auto_enum()
    # Macos = auto_enum()

    @staticmethod
    def get_current():
        if os := OS.get_from_string(sys.platform):
            return os
        else:
            error(f"unknown or unsupported host OS: {sys.platform}")

    @staticmethod
    def get_from_string(name_):
        name = name_.lower()
        if name == "linux":
            return OS.Linux
        return None

    def equals_to_string(self, flag):
        if flag == "_LINUX_" and self == OS.Linux:
            return True
        return False

class PkgMode(Enum):
    Binary = auto_enum()
    Library = auto_enum()

class Prefs:
    def __init__(self, args: [str]):
        if len(args) == 0:
            eprint(HELP)
            exit(1)

        self.pkg_name = "main"
        self.pkg_mode = PkgMode.Binary
        self.os = OS.get_current()
        self.is_verbose = False

        self.inputs = []

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
            elif arg in ("-os", "--target-os"):
                if os_name := option(current_args, arg):
                    if os_flag := OS.get_from_string(os_name):
                        self.os = os_flag
                        i += 1
                    else:
                        error(f"unknown or unsupported OS name: `{os_name}`")
                else:
                    error(f"`{arg}` requires a name as argument")
            elif arg in ("-v", "--verbose"):
                self.is_verbose = True
            elif os.path.isdir(arg):
                files = glob.glob(f"{arg}/*.ri")
                if len(files) == 0:
                    error(f"`{arg}` does not have .ri files")
                for f in files:
                    self.inputs.append(f)
            else:
                error(f"unknown option: `{arg}`")

            i += 1

        if len(self.inputs) == 0:
            error("no input received")
