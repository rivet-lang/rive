# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import utils, colors

ERRORS = 0
WARNS = 0

# This dictionary saves the lines of the files that have
# had reports to avoid having to open them over and over
# again.
FILE_LINES = {}

LAST_LINE_NR_LEN = -1
SEP = colors.bold("|")
MARK = colors.bold(colors.green("^"))

def _readline(file, line_nr):
    global FILE_LINES
    if file in FILE_LINES:
        lines = FILE_LINES[file]
        line_nr = min(line_nr, len(lines) - 1)
        return lines[line_nr]

    lines = open(file).read().splitlines()
    FILE_LINES[file] = lines
    line_nr = min(line_nr, len(lines) - 1)
    return lines[line_nr]

def readline(pos):
    global LAST_LINE_NR_LEN
    line = _readline(pos.file, pos.line)
    line_str = f"  {colors.bold(pos.line+1)}"
    LAST_LINE_NR_LEN = len(f"  {pos.line+1}")
    # TODO(StunxFS): it would be better if the marker was the width
    # of the token.
    marker = (" " * (pos.col - 1)) + MARK
    return f"{line_str} {SEP} {line}\n{' ' * LAST_LINE_NR_LEN} {SEP} {marker}"

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
    utils.eprint(
        f"{' ' * LAST_LINE_NR_LEN}{colors.bold(colors.cyan(' = note:'))} {msg}"
    )

def help(msg):
    utils.eprint(f"{' ' * LAST_LINE_NR_LEN}{colors.bold(' = help:')} {msg}")
