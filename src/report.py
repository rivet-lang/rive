# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from . import utils

ERRORS = 0
WARNS = 0


def error(msg, pos):
    utils.eprint(f"{pos}: error: {msg}")
    ERRORS += 1


def warn(msg, pos):
    utils.eprint(f"{pos}: warning: {msg}")
    WARNS += 1


def note(msg):
    utils.eprint(f"    note: {msg}")


def help(msg):
    utils.eprint(f"    help: {msg}")
