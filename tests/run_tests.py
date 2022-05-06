# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import glob
import utils
from os import path

def run_tests():
    exit_code = 0

    OK_FILES = glob.glob(f"tests/*.ri")

    print(
        utils
        .bold("---------------------- Running tests ----------------------")
    )
    for file in OK_FILES:
        res = utils.run_process("python3", "rivetc.py", file)
        if res.exit_code == 0:
            print(utils.bold(utils.green(" [ PASS ] ")), end="")
        else:
            print(utils.bold(utils.red(" [ FAIL ] ")), end="")
        print(file)
        if res.exit_code != 0:
            print(res.err)
            exit_code = 1
    print(
        utils
        .bold("-----------------------------------------------------------")
    )

    return exit_code

if __name__ == "__main__":
    exit(run_tests())
