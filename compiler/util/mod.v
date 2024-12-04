// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module util

@[inline]
pub fn is_valid_name(c u8) bool {
	return c == `_` || c.is_letter()
}
