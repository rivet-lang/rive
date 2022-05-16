# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import sys, subprocess

from . import colors

INVALID_ESCAPES = ["(", "{", "$", "`", "."]
BACKSLASH = chr(92)
BACKSLASH_R = chr(13)
BACKSLASH_N = chr(10)
DOUBLE_QUOTE = chr(34)
DOUBLE_ESCAPE = "\\\\"

class ProcessResult:
    def __init__(self, out, err, exit_code):
        self.out = out
        self.err = err
        self.exit_code = exit_code

def run_process(*args):
    res = subprocess.run(args, capture_output=True)
    return ProcessResult(
        res.stdout.decode().strip(),
        res.stderr.decode().strip(), res.returncode
    )

class CompilerError(Exception):
    pass

def bytestr(string):
    buf = bytearray((' ' * len(string)).encode('ascii'))
    buf = string.encode('utf-8')
    return (buf, len(buf))

def eprint(*s, end="\n"):
    print(*s, end=end, file=sys.stderr)

def error(msg):
    bg = colors.bold(f'rivetc: {colors.red("error:")}')
    eprint(f"{bg} {msg}")
    exit(1)

def is_valid_name(ch):
    return (ch >= "A" and ch <= "Z") or (ch >= "a" and ch <= "z") or ch == "_"

def smart_quote(str, raw: bool):
    len_ = len(str)
    if len_ == 0:
        return ""
    if len_ < 256:
        is_pure = True
        for i in range(0, len_):
            ch = str[i]
            if (
                (ch >= chr(37) and ch <= chr(90))
                or (ch >= chr(95) and ch <= chr(126))
                or (ch in [" ", "!", "#", "[", "]"])
            ):
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
                skip_next = True
                result += DOUBLE_ESCAPE
                continue
            if next != chr(0):
                if raw:
                    skip_next = True
                    result += DOUBLE_ESCAPE
                    continue
                if next in INVALID_ESCAPES:
                    skip_next = True
                    result += next
                    continue
                # keep all valid escape sequences
                skip_next = True
                result += current
                result += next
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
        if not raw:
            if current == BACKSLASH_R and next == BACKSLASH_N:
                # Windows style new line \r\n
                skip_next = True
                result += BACKSLASH
                result += "n"
                continue
        result += current
    return result
