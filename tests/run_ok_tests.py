# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import glob, sys
from os import path

import utils

def run_tests():
    exit_code = 0

    OK_FILES = glob.glob(f"tests/ok/*.ri")
    HEADER = f"---------------------- Running {len(OK_FILES)} tests ----------------------"

    print(utils.bold(HEADER))
    for file in OK_FILES:
        res = utils.run_process(sys.executable, "rivetc.py", file)
        if res.exit_code == 0:
            print(utils.bold(utils.green(" [ PASS ] ")), end="")
        else:
            print(utils.bold(utils.red(" [ FAIL ] ")), end="")
        print(file)
        if res.exit_code != 0:
            print(res.err)
            exit_code = 1
    print(utils.bold("-" * len(HEADER)))

    return exit_code

if __name__ == "__main__":
    exit(run_tests())
