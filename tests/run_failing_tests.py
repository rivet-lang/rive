# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from os import path
import glob, sys, os

import utils

CC = os.getenv("RIVET_CC_TEST")
CC = CC if CC else "gcc"

def run_fail_tests():
	exit_code = 0

	IS_WINDOWS = sys.platform == "win32"
	FAIL_FILES = glob.glob(os.path.join("tests", "failing", "**", "*.ri"))
	FAIL_FILES.sort()
	HEADER = f"------------------ Running {len(FAIL_FILES)} failing tests ------------------"

	utils.eprint(utils.bold(HEADER))
	for file in FAIL_FILES:
		res = utils.run_process(
		    sys.executable,
		    "rivetc.py", # TODO: "--pkg-name", utils.filename(file),
		    file, "-cc", CC
		)
		try:
			outf = open(file.replace(".ri", ".out"), encoding = 'UTF-8').read()
			if outf.strip() == res.err:
				utils.eprint(utils.bold(utils.green(" [ PASS ] ")), file)
			else:
				utils.eprint(utils.bold(utils.red(" [ FAIL ] ")), file)
			out = outf.strip()
			if IS_WINDOWS:
				out = out.replace("\\", "/")
			if out != res.err:
				utils.eprint(utils.bold("Expected:"))
				utils.eprint(outf)
				utils.eprint(utils.bold("Got:"))
				utils.eprint(res.err)
				exit_code = 1
			if res.exit_code == 0:
				utils.eprint("Exit code: 0")
		except FileNotFoundError:
			utils.eprint(
			    utils.bold(utils.yellow(" [ SKIP (.out file not found) ] ")),
			    file
			)
			utils.eprint(file)
	utils.eprint(utils.bold("-" * len(HEADER)))

	return exit_code

if __name__ == "__main__":
	exit(run_fail_tests())
