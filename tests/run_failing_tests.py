# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import glob, sys
from os import path

import utils

def run_fail_tests():
    exit_code = 0

    FAIL_FILES = glob.glob(f"tests/failing/**/*.ri")
    HEADER = f"------------------ Running {len(FAIL_FILES)} failing tests ------------------"

    print(utils.bold(HEADER))
    for file in FAIL_FILES:
        res = utils.run_process(
            sys.executable, "rivetc.py", "--pkg-name", utils.filename(file),
            file
        )
        try:
            outf = open(file.replace(".ri", ".out")).read()
            if outf.strip() == res.err:
                print(utils.bold(utils.green(" [ PASS ] ")), end="")
            else:
                print(utils.bold(utils.red(" [ FAIL ] ")), end="")
            print(file)
            if outf.strip() != res.err:
                print(utils.bold("Expected:"))
                print(outf)
                print(utils.bold("Got:"))
                print(res.err)
                exit_code = 1
            if res.exit_code == 0:
                print("Exit code: 0")
        except FileNotFoundError:
            print(
                utils.bold(utils.yellow(" [ SKIP (.out file not found) ] ")),
                end=""
            )
            print(file)
    print(utils.bold("-" * len(HEADER)))

    return exit_code

if __name__ == "__main__":
    exit(run_fail_tests())
