// Copyright (C) 2024-present Jose Mendoza - All rights reserved. Use of this
// source code is governed by an MIT license that can be found in the LICENSE
// file.

module util

import os
import compiler.report

@[inline]
pub fn is_valid_name(c u8) bool {
	return c == `_` || c.is_letter()
}

pub fn read_file(path string) string {
	return skip_bom(os.read_file(path) or {
		// we use `ic_fatal` because this should not happen
		report.ic_fatal(err.msg())
	})
}

pub fn skip_bom(file_content string) string {
	mut raw_text := file_content
	// BOM check
	if raw_text.len >= 3 {
		unsafe {
			c_text := raw_text.str
			if c_text[0] == 0xEF && c_text[1] == 0xBB && c_text[2] == 0xBF {
				// skip three BOM bytes
				offset_from_begin := 3
				raw_text = tos(c_text[offset_from_begin], vstrlen(c_text) - offset_from_begin)
			}
		}
	}
	return raw_text
}
