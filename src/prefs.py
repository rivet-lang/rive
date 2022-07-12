# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from os import path
import os, sys, glob, platform
from ctypes import sizeof, c_voidp
from enum import IntEnum as Enum, auto as auto_enum

from . import report
from .utils import error, eprint, execute, is_valid_name, full_version

HELP = """Usage: rivetc [OPTIONS] INPUTS

The compiler can receive both files and directories as input, example:
   rivetc my_file.ri my_folder/ my_folder2/ other_file.ri

Options:
   --pkg-name <name>
      Specify the name of the package being built. By default: main.

   --pkg-type bin|lib|dylib|staticlib
      Specify the type of the package being built. By default: bin.

   -r, --release
      Compile the executable in release mode, where most optimizations are enabled.
      Note that most Rivet warnings turn to errors, if you pass -r or --release, so
      you will have to fix them first.

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

   --check-syntax
      Only scan and parse the package, but then stop.

   --check
      Scans, parses, and checks the files without compiling the package.

   --emit-rir
      Emit Rivet Intermediate Representation to a file.

   --keep-c
      Don't remove the output C source file.

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
	Windows = auto_enum()
	Macos = auto_enum()

	@staticmethod
	def get():
		if os := OS.from_string(sys.platform):
			return os
		else:
			error(f"unknown target OS: {sys.platform}")

	@staticmethod
	def from_string(name):
		if name.startswith("linux"):
			return OS.Linux
		elif name in ("windows", "win32"):
			return OS.Windows
		#elif name=="macos":
		#    return OS.Macos
		return None

	def equals_to_string(self, flag):
		if flag == "_LINUX_" and self == OS.Linux:
			return True
		elif flag == "_WINDOWS_" and self == OS.Windows:
			return True
		return False

	def __str__(self):
		if self == OS.Linux: return "Linux"
		elif self == OS.Windows: return "Windows"
		elif self == OS.Macos: return "Macos"

class Arch(Enum):
	Amd64 = auto_enum() # aka x86_64
	I386 = auto_enum() # aka x86

	@staticmethod
	def get():
		arch = platform.uname().machine
		if arch in ("x86_64", "AMD64"):
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

	def __str__(self):
		if self == Arch.I386: return "i386"
		return "amd64"

class Bits(Enum):
	X32 = auto_enum()
	X64 = auto_enum()

	@staticmethod
	def get():
		if sizeof(c_voidp) == 8:
			return Bits.X64
		return Bits.X32

	def __str__(self):
		if self == Bits.X32: return "x32"
		return "x64"

class Endian(Enum):
	Little = auto_enum()
	Big = auto_enum()

	@staticmethod
	def get():
		if sys.byteorder == "little":
			return Endian.Little
		return Endian.Big

	def __str__(self):
		if self == Endian.Little: return "little"
		return "big"

class Backend(Enum):
	C = auto_enum()

	@staticmethod
	def from_string(bk):
		if bk == "c":
			return Backend.C
		return None

	def __str__(self):
		return "C"

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

class BuildMode(Enum):
	Debug = auto_enum()
	Release = auto_enum()
	Test = auto_enum()

	def __str__(self):
		if self == BuildMode.Release:
			return "release"
		elif self == BuildMode.Test:
			return "test"
		return "debug"

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
		self.pkg_name = "core" # TODO: temp, should be "main"
		self.pkg_type = PkgType.Bin
		self.pkg_output = "main"
		self.build_mode = BuildMode.Debug

		self.library_path = [
		    path.join(path.dirname(path.realpath(sys.argv[0])), "lib"),
		    path.join(RIVET_DIR, "libs")
		]
		self.library_to_link = []
		self.objects_to_link = []

		self.flags = []
		self.check_syntax = False
		self.check = False
		self.emit_rir = False
		self.keep_c = False
		self.is_verbose = False

		if len(args) == 0:
			eprint(HELP)
			exit(0)

		self.load_core_library()

		i = 0
		flags = []
		while i < len(args):
			arg = args[i]
			if len(arg) > 1 and arg[0] == '-' and arg not in (
			    "-L", "-d", "--define"
			) and arg in flags:
				error(f"duplicate flag `{arg}`")
			flags.append(arg)

			current_args = args[i:]

			# informative options
			if arg in ("-h", "--help"):
				eprint(HELP)
				exit(0)
			elif arg in ("-V", "--version"):
				eprint(full_version())
				exit(0)

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
					self.pkg_output = pkg_name
					if not is_valid_name(self.pkg_name):
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
			elif arg in ("-r", "--release"):
				self.build_mode = BuildMode.Release
				report.WARNS_ARE_ERRORS = True
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
			elif arg == "--check-syntax":
				self.check_syntax = True
			elif arg == "--check":
				self.check = True
			elif arg == "--emit-rir":
				self.emit_rir = True
			elif arg == "--keep-c":
				self.keep_c = True
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

		self.filter_files()
		if len(self.inputs) == 0:
			error("no input received")

		self.build_rivet_dir()

		if not path.isabs(self.pkg_output):
			self.pkg_output = path.join(os.getcwd(), self.pkg_output)

	def load_core_library(self):
		self.inputs = glob.glob("lib/core/src/*.ri")

	def filter_files(self):
		new_inputs = []
		for input in self.inputs:
			basename_input = path.basename(input)
			if basename_input.count('.') == 1:
				new_inputs.append(input)
				continue
			exts = basename_input[:-3].split('.')[1:]
			should_compile = False
			already_exts = []
			for ext in exts:
				if ext in already_exts:
					error(f"{input}: duplicate special extension `{ext}`")
				already_exts.append(ext)
				if ext.startswith("d_") or ext.startswith("notd_"):
					if ext.startswith("d_"):
						should_compile = should_compile and ext[2:] in self.flags
					else:
						should_compile = should_compile and ext[
						    5:] not in self.flags
				elif osf := OS.from_string(ext):
					should_compile = should_compile and self.target_os == osf
				elif arch := Arch.from_string(ext):
					should_compile = should_compile and self.target_arch == arch
				elif ext in ("x32", "x64"):
					if ext == "x32":
						should_compile = should_compile and self.target_bits == Bits.X32
					else:
						should_compile = should_compile and self.target_bits == Bits.X64
				elif ext in ("little_endian", "big_endian"):
					if ext == "little_endian":
						should_compile = should_compile and self.target_endian == Endian.Little
					else:
						should_compile = should_compile and self.target_endian == Endian.Big
				elif b := Backend.from_string(ext): # backends
					should_compile = should_compile and self.target_backend == b
				else:
					error(f"{input}: unknown special extension `{ext}`")
			if should_compile:
				new_inputs.append(input)
		self.inputs = new_inputs

	def build_rivet_dir(self):
		if not path.isdir(RIVET_DIR):
			os.mkdir(RIVET_DIR)
			os.mkdir(path.join(RIVET_DIR, "objs"))
			os.mkdir(path.join(RIVET_DIR, "libs"))
