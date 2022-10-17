# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import textwrap

from . import colors, utils

WARNS_ARE_ERRORS = False
ERRORS = 0
WARNS = 0

FOOT = colors.bold(colors.blue("="))

def color(kind, msg):
	return colors.red(msg) if kind == "error:" else colors.yellow(msg)

def fmt_msg(pos, kind, msg):
	return f"{colors.bold(f'{pos}: {color(kind,kind)} {msg}')}"

def error(msg, pos):
	global ERRORS
	utils.eprint(fmt_msg(pos, "error:", msg))
	ERRORS += 1

def warn(msg, pos):
	if WARNS_ARE_ERRORS:
		error(msg, pos)
		return
	global WARNS
	utils.eprint(fmt_msg(pos, "warning:", msg))
	WARNS += 1

def wrap_text(msg):
	return f"\n   ".join(textwrap.wrap(msg, width = 80))

def note(msg):
	utils.eprint(
	    f"   {FOOT} {colors.bold(colors.green('note:'))} {wrap_text(msg)}"
	)

def help(msg):
	utils.eprint(
	    f"   {FOOT} {colors.bold(colors.cyan('help:'))} {wrap_text(msg)}"
	)
