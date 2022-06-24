# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os, sys, subprocess

def filename(path):
	return os.path.splitext(os.path.basename(path))[0]

def eprint(*s, end = "\n"):
	print(*s, end = end, file = sys.stderr)

class ProcessResult:
	def __init__(self, out, err, exit_code):
		self.out = out
		self.err = err
		self.exit_code = exit_code

def run_process(*args):
	res = subprocess.run(args, capture_output = True)
	return ProcessResult(
	    res.stdout.decode().strip(),
	    res.stderr.decode().strip(), res.returncode
	)

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

def green(msg):
	return format(msg, "32", "39")

def bold(msg):
	return format(msg, "1", "22")
