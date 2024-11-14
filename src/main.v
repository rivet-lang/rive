// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module main

import os
import builder

fn main() {
	mut b := builder.new(os.args[1..])
	b.run()
}
