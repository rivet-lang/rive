# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import os

if os.system("python3.11 rivetc -t tests/valid/") != 0:
    exit(1)
print()
if os.system("python3.11 tests/run_invalid_tests.py") != 0:
    exit(1)
