# Copyright (C) 2023 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

import os
import textwrap

from . import utils

WARNS_ARE_ERRORS = False
ERRORS = 0
WARNS = 0

# This dictionary saves the lines of the files that have
# had reports to avoid having to open them over and over
# again.
FILE_LINES = {}

SEP = utils.bold(utils.blue("|"))
MARK = utils.bold(utils.blue("^"))
FOOT = utils.bold(utils.blue("="))

def format_number(num):
    num = str(num)
    if len(num) == 5:
        return num
    return " " * (5 - len(num)) + num

def color(kind, msg):
    return utils.red(msg) if kind == "error:" else utils.yellow(msg)

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
    line = _readline(pos.file, pos.line)
    line_str = f"{utils.bold(utils.blue(format_number(pos.line + 1)))}"
    marker = (" " * (pos.col - 1)) + MARK
    return f"{line_str} {SEP} {line}\n      {SEP} {marker}"

def fmt_msg(pos, kind, msg):
    file = os.path.relpath(pos.file).replace("\\", "/")
    return utils.bold(
        f'{file}:{pos.line + 1}:{pos.col}: {color(kind,kind)} {msg}'
    )

def error(msg, pos):
    global ERRORS
    utils.eprint(fmt_msg(pos, "error:", msg))
    utils.eprint(readline(pos, "error:"))
    ERRORS += 1

def warn(msg, pos):
    if WARNS_ARE_ERRORS:
        error(msg, pos)
        return
    global WARNS
    utils.eprint(fmt_msg(pos, "warning:", msg))
    utils.eprint(readline(pos, "warning:"))
    WARNS += 1

def wrap_text(msg):
    return f"\n        ".join(textwrap.wrap(msg, width = 80))

def note(msg):
    utils.eprint(
        f"      {FOOT} {utils.bold(utils.blue('note:'))} {wrap_text(msg)}"
    )

def help(msg):
    utils.eprint(f"      {FOOT} {utils.bold('help:')} {wrap_text(msg)}")
