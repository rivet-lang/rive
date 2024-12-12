// Copyright (C) 2024-present The Rivet programming language. Use of this source code
// is governed by an MIT license that can be found in the LICENSE file.

module ast

@[heap]
pub struct Scope {
pub mut:
	owner Symbol
mut:
	parent &Scope
	syms   []Symbol
}

@[inline]
pub fn Scope.new(parent &Scope) &Scope {
	return &Scope{
		parent: parent
	}
}

@[inline]
pub fn (sc &Scope) derive() &Scope {
	return &Scope{
		owner:  sc.owner
		parent: sc
	}
}

pub fn (mut sc Scope) add_symbol(sym Symbol) ! {
	if _ := sc.lookup(sym.name) {
		return error('duplicate variable `${sym.name}`')
	}
	sc.syms << sym
}

pub fn (mut sc Scope) add_local_symbol(sym Symbol) ! {
	if _ := sc.find(sym.name) {
		return error('duplicate symbol `${sym.name}`')
	}
	sc.syms << sym
}

pub fn (sc &Scope) find(name string) ?Symbol {
	for sym in sc.syms {
		if sym.name == name {
			return sym
		}
	}
	return none
}

pub fn (sc &Scope) lookup(name string) ?Symbol {
	if sym := sc.find(name) {
		return sym
	}
	if sc.owner is Function || isnil(sc.parent) {
		return none
	}
	return sc.parent.lookup(name)
}
