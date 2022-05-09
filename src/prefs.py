# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from os import path
import os, sys, glob
from ctypes import sizeof, c_voidp
from enum import IntEnum as Enum, auto as auto_enum

from . import ast, report, tokens
from .utils import error, eprint, run_process

VERSION = "0.1.0"
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

   -b <backend>, --backend <backend>
      Specify the backend to use while building the package.

      Current list of supported backends:
        `c` (default)
           Rivet outputs C source code which is passed to a C compiler to be compiled.

   -d <flag>, --define <flag>
      Define the provided flag.

   -L <path>
      Add a directory to the library search path.

   -os <name>, --target-os <name>
      Change the target OS that Rivet tries to compile for. By default, the
      target OS is the host system.

      Current list of supported operating systems:
        `linux`

   -arch <arch>, --target-arch <arch>
      Change the target architecture that Rivet tries to compile for. By
      default, the target architecture is the host arch.

      Current list of supported architectures:
        `amd64`, `i386`

   -x32, -x64
      Whether 32-bit or 64-bit machine code will be generated.

   -v, --verbose
      Print additional messages to the console.

   -V, --version
      Print compiler version.

   -h, --help
      Print this message."""

RIVET_DIR = path.join(path.expanduser("~"), ".rivet-lang")

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
            error(f"unknown target OS: {sys.platform}")

    @staticmethod
    def from_string(name):
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
        arch = os.uname().machine
        if arch == "x86_64":
            return Arch.Amd64
        elif arch == "x86":
            return Arch.I386
        else:
            error(f"unknown target architecture: `{arch}`")

    @staticmethod
    def from_string(arch):
        if arch == "amd64":
            return Arch.Amd64
        elif arch == "i386":
            return Arch.I386
        return None

    def equals_to_string(self, flag):
        if flag == "_AMD64_" and self == Arch.Amd64:
            return True
        elif flag == "_i386_" and self == Arch.I386:
            return True
        return False

class Bits(Enum):
    X32 = auto_enum()
    X64 = auto_enum()

    @staticmethod
    def get():
        if sizeof(c_voidp) == 8:
            return Bits.X64
        return Bits.X32

class Endian(Enum):
    Little = auto_enum()
    Big = auto_enum()

    @staticmethod
    def get():
        if sys.byteorder == "little":
            return Endian.Little
        return Endian.Big

class Backend(Enum):
    C = auto_enum()

    def from_string(bk):
        if bk == "c":
            return Backend.C
        return None

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

        # target info
        self.target_os = OS.get()
        self.target_arch = Arch.get()
        self.target_bits = Bits.get()
        self.target_endian = Endian.get()
        self.target_backend = Backend.C

        # package info
        self.pkg_name = "main"
        self.pkg_type = PkgType.Bin
        self.pkg_output = "main"

        self.library_path = [
            path.join(path.dirname(path.realpath(sys.argv[0])), "lib"),
            path.join(RIVET_DIR, "libs")
        ]
        self.flags = []
        self.is_verbose = False

        if len(args) == 0:
            eprint(HELP)
            return

        i = 0
        flags = []
        while i < len(args):
            arg = args[i]
            if arg[0] == '-' and arg not in (
                "-L", "-d", "--define"
            ) and arg in flags:
                error(f"duplicate flag `{arg}`")
            flags.append(arg)

            current_args = args[i:]

            # informative options
            if arg in ("-h", "--help"):
                eprint(HELP)
                return
            elif arg in ("-V", "--version"):
                commit_date = run_process(
                    "git", "log", "-n", "1", '--pretty=format:%h %as'
                ).out
                eprint(f"rivetc {VERSION} ({commit_date})")
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
                    self.pkg_output = out
                    if path.isdir(self.pkg_output):
                        error(f"{arg}: `{self.pkg_output}` is a directory")
                else:
                    error(f"`{arg}` requires a filename as argument")
                i += 1
            elif arg in ("-b", "--backend"):
                if b := option(current_args, arg):
                    if backend := Backend.from_string(b):
                        self.target_backend = backend
                    else:
                        error(f"unknown backend: `{b}`")
                else:
                    error(f"`{arg}` requires a name as argument")
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
            elif arg == "-L":
                if p := option(current_args, arg):
                    if path.isdir(p):
                        if p in self.library_path:
                            error(f"duplicate library path: `{p}`")
                        self.library_path.append(p)
                    else:
                        error(f"`{p}` is not a directory")
                else:
                    error("`-L` requires a directory as argument")
                i += 1
            elif arg in ("-os", "--target-os"):
                if os_name := option(current_args, arg):
                    if os_flag := OS.from_string(os_name):
                        self.target_os = os_flag
                    else:
                        error(f"unknown target operating system: `{os_name}`")
                else:
                    error(f"`{arg}` requires a name as argument")
                i += 1
            elif arg in ("-arch", "--target-arch"):
                if arch_name := option(current_args, arg):
                    if arch_flag := Arch.from_string(arch_name):
                        self.target_arch = arch_flag
                    else:
                        error(f"unknown target architecture: `{arch_name}`")
                else:
                    error(f"`{arg}` requires a name as argument")
                i += 1
            elif arg in ("-x32", "-x64"):
                self.target_bits = Bits.X32 if arg == "-x32" else Bits.X64
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

        self.build_rivet_dir()

        if not path.isabs(self.pkg_output):
            self.pkg_output = path.join(os.getcwd(), self.pkg_output)

    def filter_files(self, inputs):
        new_inputs = []
        for input in inputs:
            if input.count('.') == 1:
                new_inputs.append(input)
                continue
            exts = input[:-3].split('.')[1:]
            should_compile = False
            already_exts = []
            for ext in exts:
                if ext in already_exts:
                    error(f"duplicate special extension `{ext}` for `{input}`")
                already_exts.append(ext)
                if ext.startswith("d_") or ext.startswith("notd_"):
                    if ext.startswith("d_"):
                        should_compile = ext[2:] in self.flags
                    else:
                        should_compile = ext[5:] not in self.flags
                elif osf := OS.from_string(ext):
                    should_compile = osf == self.target_os
                elif arch := Arch.from_string(ext):
                    should_compile = arch == self.target_arch
            if should_compile:
                new_inputs.append(input)
        return new_inputs

    def build_rivet_dir(self):
        if not path.isdir(RIVET_DIR):
            os.mkdir(RIVET_DIR)
            os.mkdir(path.join(RIVET_DIR, "libs"))

    def evalue_comptime_condition(self, cond):
        if isinstance(cond, ast.Ident):
            if cond.is_comptime:
                report.error("invalid comptime condition", cond.pos)
            # operating systems
            elif cond.name == "_LINUX_":
                return self.target_os.equals_to_string(cond.name)
            # architectures
            elif cond.name in ("_AMD64_", "_i386_"):
                return self.target_arch.equals_to_string(cond.name)
            # bits
            elif cond.name in ("_x32_", "_x64_"):
                if cond.name == "_x32_":
                    return self.target_bits == Bits.X32
                else:
                    return self.target_bits == Bits.X64
            # endian
            elif cond.name in ("_LITTLE_ENDIAN_", "_BIG_ENDIAN_"):
                if cond.name == "_LITTLE_ENDIAN_":
                    return self.target_endian == Endian.Little
                else:
                    return self.target_endian == Endian.Big
            else:
                if cond.name.startswith("_") and cond.name.endswith("_"):
                    report.error(f"unknown builtin flag: `{cond}`", cond.pos)
                    return False
                return cond.name in self.flags
        elif isinstance(cond, ast.ParExpr):
            return self.evalue_comptime_condition(cond.expr)
        elif isinstance(cond, ast.UnaryExpr):
            if cond.op == tokens.Kind.Bang:
                return not self.evalue_comptime_condition(cond.right)
            else:
                report.error("invalid comptime condition", cond.pos)
        elif isinstance(cond, ast.BinaryExpr):
            if cond.op in (tokens.Kind.KeyAnd, tokens.Kind.KeyOr):
                if cond.op == tokens.Kind.KeyAnd:
                    return self.evalue_comptime_condition(
                        cond.left
                    ) and self.evalue_comptime_condition(cond.right)
                else:
                    return self.evalue_comptime_condition(
                        cond.left
                    ) or self.evalue_comptime_condition(cond.right)
            else:
                report.error("invalid comptime condition", cond.pos)
        else:
            report.error("invalid comptime condition", cond.pos)
        return False
