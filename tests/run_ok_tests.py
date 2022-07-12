# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import glob, sys, os

import utils

CC = os.getenv("RIVET_CC_TEST")
CC = CC if CC else "gcc"

def run_tests():
	exit_code = 0

	IS_WINDOWS = sys.platform == "win32"
	OK_FILES = glob.glob(os.path.join("tests", "ok", "*.ri"))
	HEADER = f"---------------------- Running {len(OK_FILES)} tests ----------------------"
	TEST_EXE = "test.exe" if IS_WINDOWS else "test"

	utils.eprint(utils.bold(HEADER))
	for file in OK_FILES:
		res = utils.run_process(
		    sys.executable, "rivetc.py", "-o", "test", "-cc", CC, file
		)
		if res.exit_code == 0:
			res = utils.run_process(".\\test.exe" if IS_WINDOWS else "./test")
			if res.exit_code == 0:
				utils.eprint(utils.bold(utils.green(" [ PASS ] ")), file)
				os.remove("test")
			else:
				utils.eprint(utils.bold(utils.red(" [ FAIL ] ")), file)
				utils.eprint(res.err)
				exit_code = res.exit_code
		else:
			utils.eprint(utils.bold(utils.red(" [ FAIL ] ")), file)
			utils.eprint(res.err)
			exit_code = res.exit_code
	utils.eprint(utils.bold("-" * len(HEADER)))

	return exit_code

if __name__ == "__main__":
	exit(run_tests())
