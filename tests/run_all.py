# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

import os, sys

py_exe = sys.orig_argv[0]

if os.system(f"{py_exe} rivetc -t tests/valid/") != 0:
    exit(1)
print()
if os.system(f"{py_exe} tests/run_b_invalid_tests.py") != 0:
    exit(1)
print()
if os.system(f"{py_exe} tests/run_invalid_tests.py") != 0:
    exit(1)
