# Copyright (C) 2022 The Rivet Developers. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

# Script used to generate the `.out` files

import os, glob, sys, utils

FAIL_FILES = glob.glob(f"tests/invalid/*.ri")
for file in FAIL_FILES:
	out_name = file.replace(".ri", ".out")
	if os.path.exists(out_name):
		continue
	res = utils.run_process(sys.executable, "rivetc", file)
	if res.exit_code != 0:
		utils.eprint(f"[OK] {file}")
		with open(out_name, "w", encoding = 'UTF-8') as f:
			f.write(res.err)
	else:
		utils.eprint(f"[BAD: exit_code == 0] {file}")
