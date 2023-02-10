# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

import os, sys, subprocess

VERSION = "0.1.0a"
HELP = """Usage: rivetc [OPTIONS] INPUT

The compiler can receive a file or a directory as input, examples:
   `rivetc my_file.ri` or `rivetc my_folder/`

Options:
   --mod-name <name>
      Specify the name of the module being built. By default: main.

   --mod-type bin|dylib|staticlib
      Specify the type of the module being built. By default: bin.

   -r, --release
      Compile the executable in release mode, where most optimizations are enabled.
      Note that most Rivet warnings turn to errors, if you pass -r or --release, so
      you will have to fix them first.

   -t, --test
      Compile the current project for testing.

   -o <filename>, --output <filename>
      Force Rivet to output the module in a specific location
      (relative to the current working directory if not absolute).
      By default: main.

   -b <backend>, --backend <backend>
      Specify the backend to use while building the module.

      Current list of supported backends:
        `c` (default): Rivet outputs C source code which is passed to a C compiler
        to be compiled.

   --backend-compiler <compiler>
      Change the backend compiler Rivet invokes to the specified compiler.

      Officially supported/tested backend compilers include:
        C: `clang`, `gcc` and `mingw`

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
      Only scan and parse the module, but then stop.

   --check
      Scans, parses, and checks the files without compiling the module.

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

INVALID_ESCAPES = ["(", "{", "$", "`", "."]
BACKSLASH = chr(92)
BACKSLASH_R = chr(13)
BACKSLASH_N = chr(10)
DOUBLE_QUOTE = chr(34)
DOUBLE_ESCAPE = "\\\\"

def supports_escape_sequences(fd):
    if sys.platform == "nt":
        return False
    if os.getenv("TERM") == "dumb":
        return False
    return os.isatty(fd) > 0

def can_show_color_on_stdout():
    return supports_escape_sequences(1)

def can_show_color_on_stderr():
    return supports_escape_sequences(2)

def format(msg, open, close):
    if not (can_show_color_on_stdout() and can_show_color_on_stderr()):
        return msg
    return f"\x1b[{open}m{msg}\x1b[{close}m"

def bold(msg):
    return format(msg, "1", "22")

def green(msg):
    return format(msg, "32", "39")

def yellow(msg):
    return format(msg, "33", "39")

def blue(msg):
    return format(msg, "34", "39")

def cyan(msg):
    return format(msg, "36", "39")

def white(msg):
    return format(msg, "37", "39")

def red(msg):
    return format(msg, "91", "39")

def commit_hash():
    return execute("git", "log", "-n", "1", '--pretty=format:%h').out

def full_version():
    commit_date = execute("git", "log", "-n", "1", '--pretty=format:%h %as').out
    return f"rivetc {VERSION} ({commit_date})"

# Rounds the number `n` up to the next multiple `multiple`.
# NOTE: `multiple` must be a power of 2.
def round_up(n, multiple):
    return (n + multiple - 1) & (-multiple)

class ProcessResult:
    def __init__(self, out, err, exit_code):
        self.out = out
        self.err = err
        self.exit_code = exit_code

def execute(*args):
    res = subprocess.run(args, capture_output = True, encoding = 'utf-8')
    stdout = res.stdout.strip() if res.stdout else ""
    stderr = res.stderr.strip() if res.stderr else ""
    return ProcessResult(stdout, stderr, res.returncode)

class Builder:
    def __init__(self):
        self.buf = ""

    def write(self, txt):
        self.buf += txt

    def write_octal_escape(self, c):
        self.buf += chr(92) # '\'
        self.buf += chr(48 + (c >> 6)) # octal digit 2
        self.buf += chr(48 + ((c >> 3) & 7)) # octal digit 1
        self.buf += chr(48 + (c & 7)) # octal digit 0

    def writeln(self, txt = ""):
        if len(txt) == 0:
            self.buf += "\n"
        else:
            self.buf += f"{txt}\n"

    def clear(self):
        self.buf = ""

    def len(self):
        return len(self)

    def __len__(self):
        return len(self.buf)

    def __repr__(self):
        return self.buf

    def __str__(self):
        return self.buf

class CompilerError(Exception):
    pass

def eprint(*s, end = "\n"):
    print(*s, end = end, file = sys.stderr)

def error(msg):
    bg = bold(f'rivetc: {red("error:")}')
    eprint(f"{bg} {msg}")
    exit(1)

def is_valid_name(ch):
    return (ch >= "A" and ch <= "Z") or (ch >= "a" and ch <= "z") or ch == "_"

class Bytestr:
    def __init__(self, buf, len_):
        self.buf = buf
        self.len = len_

def bytestr(s):
    buf = s.encode("utf-8")
    return Bytestr(buf, len(buf))

def index_any(s, chars):
    for i, ss in enumerate(s):
        for c in chars:
            if c == ss:
                return i
    return -1

def escape_nonascii(original):
    sb = Builder()
    for c in original.encode("utf-8"):
        if c < 32 or c > 126:
            # Encode with a 3 digit octal escape code, which has the
            # advantage to be limited/non dependant on what character
            # will follow next, unlike hex escapes:
            sb.write_octal_escape(c)
        else:
            sb.write(chr(c))
    return str(sb)

def decode_h_escapes(s_, start, escapes_pos):
    if len(escapes_pos) == 0:
        return s_
    s = s_.encode("utf-8")
    ss = [s[:escapes_pos[0] - start].decode()]
    for i, pos in enumerate(escapes_pos):
        idx = pos - start
        end_idx = idx + 4 # len("\xXX") == 4
        try:
            ss.append(chr(int(s[idx + 2:end_idx], 16)))
        except:
            ss.append(chr(0))
        if i + 1 < len(escapes_pos):
            ss.append(s[end_idx:escapes_pos[i + 1] - start].decode())
        else:
            ss.append(s[end_idx:].decode())
    return "".join(ss)

def smart_quote(str, raw: bool):
    len_ = len(str)
    if len_ == 0:
        return ""
    if len_ < 256:
        is_pure = True
        for i in range(0, len_):
            ch = str[i]
            if ((ch >= chr(37) and ch <= chr(90))
                or (ch >= chr(95) and ch <= chr(126))
                or (ch in (" ", "!", "#", "[", "]"))):
                # safe punctuation + digits + big latin letters, small latin
                # letters + more safe punctuation, important punctuation exceptions,
                # that are not placed conveniently in a consequitive span in
                # the ASCII table.
                continue
            is_pure = False
            break
        if is_pure:
            return str
    result = ""
    pos = -1
    last = chr(0)
    current = chr(0)
    next = chr(0)
    skip_next = False
    while True:
        pos += 1
        if skip_next:
            skip_next = False
            pos += 1
        if pos >= len_:
            break
        last = current
        current = str[pos]
        if pos + 1 < len_:
            next = str[pos + 1]
        else:
            next = 0
        if current == DOUBLE_QUOTE:
            current = 0
            result += BACKSLASH
            result += DOUBLE_QUOTE
            continue
        if current == BACKSLASH:
            if raw:
                result += DOUBLE_ESCAPE
                continue
            if next == BACKSLASH:
                # escaped backslash - keep as is
                current = 0
                skip_next = True
                result += DOUBLE_ESCAPE
                continue
            if next != chr(0):
                if raw:
                    skip_next = True
                    result += DOUBLE_ESCAPE
                    continue
                if next in INVALID_ESCAPES:
                    current = 0
                    skip_next = True
                    result += next
                    continue
                # keep all valid escape sequences
                skip_next = True
                result += current
                result += next
                current = 0
                continue
        if current == BACKSLASH_N:
            # keep newlines in string
            current = chr(0)
            result += BACKSLASH
            result += "n"
            continue
        if current == BACKSLASH_R and next == BACKSLASH_N:
            result += current
            result += next
            current = chr(0)
            skip_next = True
            continue
        result += current
    return result

class OrderedDepMap:
    def __init__(self, keys = [], data = {}):
        self.keys = keys.copy()
        self.data = data.copy()

    def set(self, name, deps):
        if name not in self.data:
            self.keys.append(name)
        self.data[name] = deps

    def add(self, name, deps):
        d = self.get(name)
        for dep in deps:
            if dep not in d:
                d.append(dep)
        self.set(name, d)

    def get(self, name):
        return self.data[name] if name in self.data else []

    def delete(self, name):
        if name not in self.data:
            raise KeyError(f"OrderedDepMap.delete: no such key `{name}`")
        for i, _ in enumerate(self.keys):
            if self.keys[i] == name:
                self.keys.pop(i)
                break
        self.data.pop(name)

    def apply_diff(self, name, deps):
        diff = []
        deps_of_name = self.get(name)
        for dep in deps_of_name:
            if dep not in deps:
                diff.append(dep)
        self.set(name, diff)

    def size(self):
        return len(self.data)

class DepGraphNode:
    def __init__(self, name, deps):
        self.name = name
        self.deps = deps

class NodeNames:
    def __init__(self, is_cycle = {}, names = {}):
        self.is_cycle = is_cycle.copy()
        self.names = names.copy()

    def is_part_of_cycle(self, name, already_seen):
        seen = False
        new_already_seen = already_seen.copy()
        if name in self.is_cycle:
            return self.is_cycle[name], new_already_seen

        if name in already_seen:
            new_already_seen.append(name)
            self.is_cycle[name] = True
            return True, new_already_seen

        new_already_seen.append(name)
        deps = self.names[name] if name in self.names else []
        if len(deps) == 0:
            self.is_cycle[name] = False
            return False, new_already_seen

        for d in deps:
            d_already_seen = new_already_seen.copy()
            seen, d_already_seen = self.is_part_of_cycle(d, d_already_seen)
            if seen:
                new_already_seen = d_already_seen.copy()
                self.is_cycle[name] = True
                return True, new_already_seen
        self.is_cycle[name] = False
        return False, new_already_seen

class DepGraph:
    def __init__(self, acyclic = True, nodes = []):
        self.acyclic = acyclic
        self.nodes = nodes.copy()

    def add(self, name, deps):
        self.nodes.append(DepGraphNode(name, deps))

    def resolve(self):
        node_names = OrderedDepMap()
        node_deps = OrderedDepMap()
        for node in self.nodes:
            node_names.add(node.name, node.deps)
            node_deps.add(node.name, node.deps)
        iterations = 0
        resolved = DepGraph()
        while node_deps.size() != 0:
            iterations += 1
            ready_set = []
            for name in node_deps.keys:
                if len(node_deps.get(name)) == 0:
                    ready_set.append(name)
            if len(ready_set) == 0:
                g = DepGraph()
                g.acyclic = False
                for name in node_deps.keys:
                    g.add(name, node_names.get(name))
                return g
            for name in ready_set:
                node_deps.delete(name)
                resolved.add(name, node_names.get(name))
            for name in node_deps.keys:
                node_deps.apply_diff(name, ready_set)
        return resolved

    def last_node(self):
        return self.nodes[-1]

    def display(self):
        out = []
        for node in self.nodes:
            for dep in node.deps:
                out.append(f" > {node.name} -> {dep}")
        return "\n".join(out)

    def display_cycles(self):
        seen = False
        out = []
        nn = NodeNames()
        for node in self.nodes:
            nn.names[node.name] = node.deps
        for k, _ in nn.names.items():
            cycle_names = []
            if k in nn.is_cycle:
                continue
            seen, cycle_names = nn.is_part_of_cycle(k, cycle_names)
            if seen:
                out.append(" > " + " -> ".join(cycle_names))
        return "\n".join(out)
