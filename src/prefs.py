# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os, sys, glob
from os import path
from enum import IntEnum as Enum, auto as auto_enum

from .utils import error, eprint, run_process

VERSION = "0.1.0"
COMMIT = run_process("git", "rev-parse", "--short", "HEAD").out
HELP = """Usage: rivetc [OPTIONS] INPUTS

The compiler can receive both files and directories as input, example:
   rivetc my_file.ri my_folder/ my_folder2/ other_file.ri

Options:
   --pkg-name <name>
      Specify the name of the package being built. By default: main.

   --pkg-type bin|lib|dylib|staticlib
      Specify the type of the package being built. By default: bin.

   -o <filename>, --output <filename>
      Force Rivet to output the package in a specific location
      (relative to the current working directory if not absolute).
      By default: main.

   -d <flag>, --define <flag>
      Define the provided flag.

   -os <name>, --target-os <name>
      Change the target OS that Rivet tries to compile for. By default, the
      target OS is the host system.

      Here is a list of the operating systems supported by Rivet:
        `linux`

   -arch <arch>, --target-arch <arch>
      Change the target architecture that Rivet tries to compile for. By
      default, the target architecture is the host arch.

      Here is a list of the architectures supported by Rivet:
        `amd64`, `i386`

   -x32, -x64
      Whether 32-bit or 64-bit machine code will be generated.

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
    def get():
        if os := OS.from_string(sys.platform):
            return os
        else:
            error(f"unknown or unsupported host OS: {sys.platform}")

    @staticmethod
    def from_string(name_):
        name = name_.lower()
        if name == "linux":
            return OS.Linux
        return None

    def equals_to_string(self, flag):
        if flag == "_LINUX_" and self == OS.Linux:
            return True
        return False

class Arch(Enum):
    Amd64 = auto_enum() # aka x86_64
    I386 = auto_enum() # aka x86

    @staticmethod
    def get():
        if os.uname().machine == "x86_64":
            return Arch.Amd64
        return Arch.I386

    @staticmethod
    def from_string(arch_):
        arch = arch_.lower()
        if arch == "amd64":
            return Arch.Amd64
        elif arch == "i386":
            return Arch.I386
        return None

class ByteOrder(Enum):
    Little = auto_enum()
    Big = auto_enum()

    @staticmethod
    def get():
        if sys.byteorder == "little":
            return ByteOrder.Little
        return ByteOrder.Big

class PkgType(Enum):
    Bin = auto_enum() # .exe
    Lib = auto_enum() # .rilib
    DyLib = auto_enum() # .so, .dll, .dylib
    StaticLib = auto_enum() # .a, .lib

    @staticmethod
    def from_string(typ):
        if typ == "bin":
            return PkgType.Bin
        elif typ == "lib":
            return PkgType.Lib
        elif typ == "dylib":
            return PkgType.DyLib
        elif typ == "static":
            return PkgType.StaticLib
        return None

class Prefs:
    def __init__(self, args: [str]):
        self.inputs = []

        self.pkg_name = "main"
        self.pkg_type = PkgType.Bin
        self.output = "main"
        self.os = OS.get()
        self.arch = Arch.get()
        self.x64 = True
        self.byte_order = ByteOrder.get()
        self.flags = []

        self.is_verbose = False

        if len(args) == 0:
            eprint(HELP)
            return

        i = 0
        while i < len(args):
            arg = args[i]
            current_args = args[i:]

            # informative options
            if arg in ("-h", "--help"):
                eprint(HELP)
                return
            elif arg in ("-V", "--version"):
                eprint(f"rivetc {VERSION} {COMMIT}")
                return

            # compiler options
            if arg.endswith(".ri"):
                if path.isdir(arg):
                    error(f"unable to read '{arg}': is a directory")
                elif not path.exists(arg):
                    error(f"unable to read '{arg}': file not found")
                elif arg in self.inputs:
                    error(f"duplicate file '{arg}'")
                self.inputs.append(arg)
            elif arg == "--pkg-name":
                if pkg_name := option(current_args, arg):
                    self.pkg_name = pkg_name
                    self.output = pkg_name
                    if not self.pkg_name.isidentifier():
                        error(f"invalid package name `{self.pkg_name}`")
                else:
                    error("`--pkg-name` requires a name as argument")
                i += 1
            elif arg == "--pkg-type":
                if typ := option(current_args, arg):
                    if pkg_typ := PkgType.from_string(typ):
                        self.pkg_typ = pkg_typ
                    else:
                        error(f"invalid package type: `{typ}`")
                else:
                    error("`--pkg-type` requires a package type as argument")
                i += 1
            elif arg in ("-o", "--output"):
                if out := option(current_args, arg):
                    self.output = out
                    if path.isdir(self.output):
                        error(f"{arg}: `{self.output}` is a directory")
                else:
                    error(f"`{arg}` requires a filename as argument")
                i += 1
            elif arg in ("-d", "--define"):
                if flag := option(current_args, arg):
                    if not flag.isupper():
                        error(f"flag `{flag}` should have a upper case name")
                    elif flag.startswith("_") and flag.endswith("_"):
                        error(
                            f"this form of declaration is reserved for the compiler: `{flag}`"
                        )
                    elif flag in self.flags:
                        error(f"duplicate flag: `{flag}`")
                    self.flags.append(flag)
                else:
                    error(f"`{arg}` requires a name as argument")
                i += 1
            elif arg in ("-os", "--target-os"):
                if os_name := option(current_args, arg):
                    if os_flag := OS.from_string(os_name):
                        self.os = os_flag
                    else:
                        error(f"unknown operating system target: `{os_name}`")
                else:
                    error(f"`{arg}` requires a name as argument")
                i += 1
            elif arg in ("-arch", "--target-arch"):
                if arch_name := option(current_args, arg):
                    if arch_flag := Arch.from_string(arch_name):
                        self.arch = arch_flag
                    else:
                        error(f"unknown architecture target: `{arch_name}`")
                else:
                    error(f"`{arg}` requires a name as argument")
                i += 1
            elif arg in ("-x32", "-x64"):
                self.x64 = arg == "-x64"
            elif arg in ("-v", "--verbose"):
                self.is_verbose = True
            elif path.isdir(arg):
                files = glob.glob(f"{arg}/*.ri")
                if len(files) == 0:
                    error(f"`{arg}` does not have .ri files")
                for f in files:
                    self.inputs.append(f)
            else:
                error(f"unknown option: `{arg}`")
            i += 1

        self.inputs = self.filter_files(self.inputs)
        if len(self.inputs) == 0:
            error("no input received")

        if not path.isabs(self.output):
            self.output = path.join(os.getcwd(), self.output)

    def filter_files(self, inputs):
        new_inputs = []
        for input in inputs:
            if input.count('.') == 1:
                new_inputs.append(input)
            exts = input[:-3].split('.')[1:]
            should_compile = False
            for ext in exts:
                if ext.startswith("d_") or ext.startswith("notd_"):
                    if ext.startswith("d_"):
                        should_compile = ext[2:] in self.flags
                    else:
                        should_compile = ext[5:] not in self.flags
                elif osf := OS.from_string(ext):
                    should_compile = osf == self.os
                elif arch := Arch.from_string(ext):
                    should_compile = arch == self.arch
            if should_compile:
                new_inputs.append(input)
        return new_inputs
