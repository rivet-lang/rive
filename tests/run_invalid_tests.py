# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

from os import path
import glob, sys, os, utils

CC = os.getenv("RIVET_CC_TEST")
CC = CC if CC else "gcc"

def run_fail_tests():
	ok, fail, skip = 0, 0, 0
	exit_code = 0

	IS_WINDOWS = sys.platform == "win32"
	FAIL_FILES = glob.glob(os.path.join("tests", "invalid", "*.ri"))
	FAIL_FILES.sort()
	for i, file in enumerate(FAIL_FILES):
		start = f" [{i+1}/{len(FAIL_FILES)}]"
		res = utils.run_process(sys.executable, "rivetc", file)
		try:
			outf = open(file.replace(".ri", ".out"), encoding = 'UTF-8').read()
			if outf.strip() == res.err:
				utils.eprint(start, file, utils.bold(utils.green("-> OK")))
				ok += 1
			else:
				utils.eprint(start, file, utils.bold(utils.red("-> FAIL")))
				fail += 1
			out = outf.strip()
			if IS_WINDOWS:
				res.err = res.err.replace("\\", "/").replace("\r\n", "\n")
			if out != res.err:
				utils.eprint(utils.bold("Expected:"))
				utils.eprint(out)
				utils.eprint(utils.bold("Got:"))
				utils.eprint(res.err)
				exit_code = 1
			if res.exit_code == 0:
				utils.eprint("Exit code: 0")
		except FileNotFoundError:
			utils.eprint(
			    utils.bold(utils.yellow("-> SKIP (.out file not found)")), file
			)
			skip += 1
	utils.eprint(utils.bold("Summary for all tests: "), end = "")
	if ok > 0:
		utils.eprint(utils.bold(utils.green(f"{ok} passed")) + ", ", end = "")
	if fail > 0:
		utils.eprint(utils.bold(utils.red(f"{fail} failed")) + ", ", end = "")
	if skip > 0:
		utils.eprint(utils.bold(utils.yellow(f"{skip} skipped")), end = "")
	utils.eprint(utils.bold(f"{len(FAIL_FILES)} total."))

	return exit_code

exit(run_fail_tests())
