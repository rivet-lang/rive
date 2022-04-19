# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os


def supports_escape_sequences(fd):
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


def red(msg):
    return format(msg, "91", "39")


def yellow(msg):
    return format(msg, "33", "39")


def cyan(msg):
    return format(msg, "36", "39")


def white(msg):
    return format(msg, "37", "39")


def bold(msg):
    return format(msg, "1", "22")
