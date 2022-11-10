# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import textwrap

<<<<<<< HEAD
from . import utils, colors
=======
from . import colors, utils
>>>>>>> fd5cbb707991f17d1cc05e277c0ef9c401dd652c

WARNS_ARE_ERRORS = False
ERRORS = 0
WARNS = 0

<<<<<<< HEAD
# This dictionary saves the lines of the files that have
# had reports to avoid having to open them over and over
# again.
FILE_LINES = {}

LAST_LINE_NR_LEN = -1
SEP = colors.bold(colors.blue("|"))
MARK = colors.bold("^")
=======
>>>>>>> fd5cbb707991f17d1cc05e277c0ef9c401dd652c
FOOT = colors.bold(colors.blue("="))

def color(kind, msg):
    return colors.red(msg) if kind == "error:" else colors.yellow(msg)

<<<<<<< HEAD
def _readline(file, line_nr):
    global FILE_LINES
    if file in FILE_LINES:
        lines = FILE_LINES[file]
        line_nr = min(line_nr, len(lines) - 1)
        return lines[line_nr]

    lines = open(file, encoding = 'UTF-8').read().splitlines()
    FILE_LINES[file] = lines
    line_nr = min(line_nr, len(lines) - 1)
    return lines[line_nr]

def readline(pos, kind):
    global LAST_LINE_NR_LEN
    line = _readline(pos.file, pos.line)
    line_str = f"  {colors.bold(colors.blue(pos.line + 1))}"
    LAST_LINE_NR_LEN = len(f"  {pos.line+1}")
    # TODO(StunxFS): it would be better if the marker was the width of
    # the token.
    marker = (" " * (pos.col - 1)) + color(kind, MARK)
    return f"{line_str} {SEP} {line}\n{' ' * LAST_LINE_NR_LEN} {SEP} {marker}"

=======
>>>>>>> fd5cbb707991f17d1cc05e277c0ef9c401dd652c
def fmt_msg(pos, kind, msg):
    return f"{colors.bold(f'{pos}: {color(kind,kind)}')} {msg}"

def error(msg, pos):
    global ERRORS
    utils.eprint(fmt_msg(pos, "error:", msg))
<<<<<<< HEAD
    utils.eprint(readline(pos, "error:"))
=======
>>>>>>> fd5cbb707991f17d1cc05e277c0ef9c401dd652c
    ERRORS += 1

def warn(msg, pos):
    if WARNS_ARE_ERRORS:
        error(msg, pos)
        return
    global WARNS
    utils.eprint(fmt_msg(pos, "warning:", msg))
<<<<<<< HEAD
    utils.eprint(readline(pos, "warning:"))
    WARNS += 1

def wrap_text(msg):
    return f"\n{' ' * LAST_LINE_NR_LEN}   ".join(textwrap.wrap(msg, width = 80))

def note(msg):
    utils.eprint(
        f"{' ' * LAST_LINE_NR_LEN} {FOOT} {colors.bold(colors.green('note:'))} {wrap_text(msg)}"
=======
    WARNS += 1

def wrap_text(msg):
    return f"\n   ".join(textwrap.wrap(msg, width = 80))

def note(msg):
    utils.eprint(
        f"   {FOOT} {colors.bold(colors.green('note:'))} {wrap_text(msg)}"
>>>>>>> fd5cbb707991f17d1cc05e277c0ef9c401dd652c
    )

def help(msg):
    utils.eprint(
<<<<<<< HEAD
        f"{' ' * LAST_LINE_NR_LEN} {FOOT} {colors.bold(colors.cyan('help:'))} {wrap_text(msg)}"
=======
        f"   {FOOT} {colors.bold(colors.cyan('help:'))} {wrap_text(msg)}"
>>>>>>> fd5cbb707991f17d1cc05e277c0ef9c401dd652c
    )
