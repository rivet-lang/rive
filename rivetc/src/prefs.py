# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from os import path
import os, sys, glob, platform
from ctypes import sizeof, c_voidp
from enum import IntEnum as Enum, auto as auto_enum

from . import report
from .utils import error, eprint, execute, is_valid_name, full_version, HELP

RIVET_DIR = path.join(path.expanduser("~"), ".rivet-lang")
RIVETC_DIR = path.dirname(path.realpath(sys.argv[0]))

def option(args, param):
	for i, arg in enumerate(args):
		if param == arg:
			if i + 1 < len(args):
				return args[i + 1]
			break
	return None

class OS(Enum):
	Linux = auto_enum()
	Windows = auto_enum()

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
	X86 = auto_enum()

	@staticmethod
	def get():
		arch = platform.uname().machine
		if arch in ("x86_64", "AMD64"):
			return Arch.Amd64
		elif arch in ("x86", "i386"):
			return Arch.X86
		else:
			error(f"unknown target architecture: `{arch}`")

	@staticmethod
	def from_string(arch):
		if arch == "amd64":
			return Arch.Amd64
		elif arch == "x86":
			return Arch.X86
		return None

	def equals_to_string(self, flag):
		if flag == "_AMD64_" and self == Arch.Amd64:
			return True
		elif flag == "_X86_" and self == Arch.X86:
			return True
		return False

	def __str__(self):
		if self == Arch.X86: return "x86"
		return "amd64"

class Bits(Enum):
	X32 = auto_enum()
	X64 = auto_enum()

	@staticmethod
	def get():
		return Bits.X64 if sizeof(c_voidp) == 8 else Bits.X32

	def __str__(self):
		return "x64" if self == Bits.X64 else "x32"

class Endian(Enum):
	Little = auto_enum()
	Big = auto_enum()

	@staticmethod
	def get():
		return Endian.Little if sys.byteorder == "little" else Endian.Big

	def __str__(self):
		return "little" if self == Endian.Little else "big"

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
		# target info
		self.target_os = OS.get()
		self.target_arch = Arch.get()
		self.target_bits = Bits.get()
		self.target_endian = Endian.get()
		self.target_backend = Backend.C

		# package info
		self.input = ""
		self.pkg_name = ""
		self.pkg_type = PkgType.Bin
		self.pkg_output = "main.exe" if self.target_os == OS.Windows else "main"
		self.build_mode = BuildMode.Debug

		self.library_path = [
		    path.join(RIVET_DIR, "libs"),
		    path.join(RIVETC_DIR, "lib")
		]

		self.libraries_to_link = []
		self.objects_to_link = []

		self.backend_compiler = "gcc"
		self.flags = []
		self.check_syntax = False
		self.check = False
		self.emit_rir = False
		self.keep_c = False
		self.is_verbose = False

		if len(args) == 0:
			eprint(HELP)
			exit(0)

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
			if arg == "--pkg-name":
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
			elif arg == "--backend-compiler":
				if backend_compiler := option(current_args, arg):
					self.backend_compiler = backend_compiler
				else:
					error("`--backend-compiler` requires a name as argument")
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
			elif arg.startswith("-"):
				error(f"unknown option: `{arg}`")
			else:
				if len(self.input) > 0:
					error("the compiler can only receive one package")
				elif not path.exists(arg):
					error(f"`{arg}` does not exist")
				else:
					self.input = arg
					if self.pkg_name == "":
						if path.isfile(arg):
							self.pkg_name = path.splitext(path.basename(arg))[0]
						else:
							self.pkg_name = path.basename(path.normpath(arg))
			i += 1

		self.build_rivet_dir()

		if not path.isabs(self.pkg_output):
			self.pkg_output = path.join(os.getcwd(), self.pkg_output)

		if self.target_os == OS.Windows and not self.pkg_output.endswith(
		    ".exe"
		):
			self.pkg_output += ".exe"

	def build_rivet_dir(self):
		if not path.isdir(RIVET_DIR):
			os.mkdir(RIVET_DIR)
			os.mkdir(path.join(RIVET_DIR, "objs"))
			os.mkdir(path.join(RIVET_DIR, "libs"))
