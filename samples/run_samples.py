# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

from os import path
import glob, sys, os, subprocess

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
	    res.stdout.decode(encoding = 'UTF-8').strip(),
	    res.stderr.decode(encoding = 'UTF-8').strip(), res.returncode
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

def green(msg):
	return format(msg, "32", "39")

def bold(msg):
	return format(msg, "1", "22")

def run_samples():
	ok, fail = 0, 0
	exit_code = 0

	SAMPLES = glob.glob(os.path.join("samples", "*.ri"))
	for i, file in enumerate(SAMPLES):
		start = f" [{i+1}/{len(SAMPLES)}]"
		res = run_process(sys.executable, "rivetc", file)
		if res.exit_code == 0:
			res = run_process("./" + filename(file))
			if res.exit_code == 0:
				eprint(start, file, bold(green("-> OK")))
				ok += 1
			else:
				eprint(start, file, bold(red("-> FAIL")))
				fail += 1
			os.remove(filename(file))
		else:
			eprint(start, file, bold(red("-> FAIL")))
			fail += 1
	eprint(bold("Summary for all samples: "), end = "")
	if ok > 0:
		eprint(bold(green(f"{ok} passed")) + ", ", end = "")
	if fail > 0:
		eprint(bold(red(f"{fail} failed")) + ", ", end = "")
	eprint(bold(f"{len(SAMPLES)} total."))

	return exit_code

exit(run_samples())
