# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import sys

from src.compiler import Compiler

if __name__ == "__main__":
	Compiler(sys.argv[1:]).run()
