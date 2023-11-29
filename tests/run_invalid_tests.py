# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

from os import path
import glob, sys, os, utils

def run_fail_tests():
    ok, fail, skip = 0, 0, 0
    exit_code = 0

    FAIL_FILES = glob.glob(os.path.join("tests", "invalid", "*.ri"))
    if not os.path.exists("rivet"):
        print("> building Rivet self-hosted compiler...")
        res = utils.run_process("python3", "rivetc", "-o", "rivet", "cmd")
        if res.exit_code != 0:
            print("    > fail:", res.err)
            exit(1)
    for i, file in enumerate(FAIL_FILES):
        start = f" [{i+1}/{len(FAIL_FILES)}]"
        res = utils.run_process("./rivet", "check", "--show-color=false", file)
        try:
            outf = open(file.replace(".ri", ".out"), encoding = 'UTF-8').read()
            if outf.strip() == res.err:
                utils.eprint(start, file, utils.bold(utils.green("-> OK")))
                ok += 1
            else:
                utils.eprint(start, file, utils.bold(utils.red("-> FAIL")))
                fail += 1
            out = outf.strip()
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
                start, file,
                utils.bold(utils.yellow("-> SKIP (.out file not found)"))
            )
            skip += 1
    utils.eprint(utils.bold("Summary for all tests: "), end = "")
    if ok > 0:
        utils.eprint(utils.bold(utils.green(f"{ok} passed")) + ", ", end = "")
    if fail > 0:
        utils.eprint(utils.bold(utils.red(f"{fail} failed")) + ", ", end = "")
    if skip > 0:
        utils.eprint(
            utils.bold(utils.yellow(f"{skip} skipped")) + ", ", end = ""
        )
    utils.eprint(utils.bold(f"{len(FAIL_FILES)} total."))

    return exit_code

exit(run_fail_tests())
