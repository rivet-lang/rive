# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import run_fail_tests, run_tests

exit_code = run_tests.run_tests()
exit_code = run_fail_tests.run_fail_tests()
exit(exit_code)
