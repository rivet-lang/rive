# Copyright (C) 2023 Jose Mendoza. All rights reserved.
# Use of this source code is governed by an MIT license that can
# be found in the LICENSE file.

import sys

from src import Compiler

Compiler(sys.argv[1:]).run()
