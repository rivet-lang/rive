# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

# Script used to generate the `.out` files

import os, glob, sys

import utils

FAIL_FILES = glob.glob(f"tests/failing/**/*.ri")
for file in FAIL_FILES:
    out_name = file.replace(".ri", ".out")
    res = utils.run_process(
        sys.executable, "rivetc.py", "--pkg-name", utils.filename(file), file
    )
    if res.exit_code != 0:
        with open(out_name, "w") as f:
            f.write(res.err)
    else:
        print(f"[BAD: exit_code == 0] {file}")
        # os.remove(utils.filename(file))
