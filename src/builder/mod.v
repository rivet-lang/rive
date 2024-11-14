// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module builder

import prefs
import util

pub struct Builder {
pub mut:
	prefs &prefs.Prefs
}

@[inline]
pub fn new(args []string) Builder {
	return Builder{
		prefs: prefs.parse_args(args) or { util.error(err.msg()) }
	}
}

pub fn (mut b Builder) run() {}
