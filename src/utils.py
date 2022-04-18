# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import sys

VERSION = "0.1.0b"


def eprint(s, end="\n"):
    print(s, end=end, file=sys.stderr)


def error(msg):
    eprint(f"rivet: error: {msg}")
    exit(1)


def option(args, param, def_=""):
    for (i, arg) in enumerate(args):
        if param == arg:
            if i + 1 < len(args):
                return args[i + 1]
            break
    return def_
