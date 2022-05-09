# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

import run_failing_tests, run_ok_tests

exit_code = run_ok_tests.run_tests()
exit_code = run_failing_tests.run_fail_tests()
exit(exit_code)
