# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os

from . import c_headers
from .. import prefs, utils

# NOTE: some of the words in `C_RESERVED` are not reserved in C, but are
# in C++, thus need escaping too. `small` should not be needed, but see:
# https://stackoverflow.com/questions/5874215/what-is-rpcndr-h
C_RESERVED = [
    'auto', 'bool', 'case', 'char', 'complex',
    'default', 'delete', 'do', 'double', 'export',
    'float', 'goto', 'inline', 'int', 'long',
    'namespace', 'new', 'register', 'restrict', 'short', 'signed',
    'sizeof', 'static', 'typedef', 'typename', 'union',
    'unix', 'unsigned', 'void', 'volatile', 'template', 'small'
]

def c_escape(kw):
    if kw in C_RESERVED:
        return f"_ri_{kw}"
    return kw

class CGen:
    def __init__(self, comp):
        self.comp = comp
        self.out = utils.Builder()

    def gen(self, out_rir):
        c_file = f"{self.comp.prefs.pkg_name}.ri.c"
        with open(c_file, "w+") as out:
            out.write(c_headers.HEADER)
            out.write(str(self.out).strip())
            out.write("int main(i32 __argc, char** __argv) {\n")
            pkg_main = f"_R{len(self.comp.prefs.pkg_name)}{self.comp.prefs.pkg_name}4mainF"
            out.write(
                f"  _R4core10rivet_mainF(__argc, (u8**)__argv, {pkg_main});\n"
            )
            out.write("  return 0;\n")
            out.write("}\n")

        args = [
            self.comp.prefs.backend_compiler, "-o", self.comp.prefs.pkg_output,
            "-fno-builtin", "-Werror",
            "-m64" if self.comp.prefs.target_bits == prefs.Bits.X64 else "-m32",
            *[f"-l{l}" for l in self.comp.prefs.libraries_to_link],
            *[f"-L{l}" for l in self.comp.prefs.library_path],
            *self.comp.prefs.objects_to_link,
            c_file
        ]
        if self.comp.prefs.build_mode == prefs.BuildMode.Release:
            args.append("-flto")
            args.append("-O3")
        else:
            args.append("-g")
        self.comp.vlog(f"C compiler options: {args}")

        res = utils.execute(*args)
        if res.exit_code == 0:
            if not self.comp.prefs.keep_c:
                os.remove(c_file)
        else:
            utils.error(
                f"error while compiling the output C file `{c_file}`:\n{res.err}"
            )

        def write(self, txt):
            self.out.write(txt)

        def writeln(self, txt = ""):
            self.out.writeln(txt)
