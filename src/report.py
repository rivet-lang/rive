# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import utils, colors

ERRORS = 0
WARNS = 0

# This dictionary saves the lines of the files that have
# had reports to avoid having to open them over and over
# again.
FILES_ALREADY_READ = {}

LAST_LINE_NR_LEN = -1
SEP = colors.bold("|")
MARK = colors.bold(colors.green("^"))


def count_digits(n):
    if n == 0:
        return 1
    c = 0
    while n != 0:
        n //= 10
        c += 1
    return c


def _readline(file, line_nr):
    global FILES_ALREADY_READ
    if file in FILES_ALREADY_READ:
        return FILES_ALREADY_READ[file][line_nr]
    lines = open(file).read().splitlines()
    FILES_ALREADY_READ[file] = lines
    return lines[line_nr]


def readline(pos):
    global LAST_LINE_NR_LEN
    line_nr = max(0, pos.line + 1)
    line = _readline(pos.file, line_nr)
    LAST_LINE_NR_LEN = count_digits(line_nr) + 3
    # TODO(StunxFS): it would be better if the marker was the width
    # of the token.
    marker = (" " * (pos.col - 1)) + MARK
    return f"  {colors.bold(line_nr)} {SEP} {line}\n    {SEP} {marker}"


def fmt_msg(pos, kind, msg):
    kind = colors.red(kind) if kind == "error:" else colors.yellow(kind)
    return f"{colors.bold(f'{pos}: {kind} {msg}')}"


def error(msg, pos):
    global ERRORS
    utils.eprint(fmt_msg(pos, "error:", msg))
    utils.eprint(readline(pos))
    ERRORS += 1


def warn(msg, pos):
    global WARNS
    utils.eprint(fmt_msg(pos, "warning:", msg))
    utils.eprint(readline(pos))
    WARNS += 1


def note(msg):
    utils.eprint(f"{' ' * LAST_LINE_NR_LEN}{colors.bold(colors.cyan('= note:'))} {msg}")


def help(msg):
    utils.eprint(f"{' ' * LAST_LINE_NR_LEN}{colors.bold('= help:')} {msg}")
