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
pub fn Scope.new(parent &Scope, owner ?Symbol) &Scope {
	mut sc := &Scope{
		parent: parent
	}
	if owner != none {
		sc.owner = owner
	}
	return sc
}

@[inline]
pub fn (sc &Scope) derive() &Scope {
	return &Scope{
		owner:  sc.owner
		parent: sc
	}
}

pub fn (mut sc Scope) add_symbol(sym Symbol) ! {
	if other := sc.lookup(sym.name) {
		m := if other is Variable && other.is_arg {
			'${sym.type_of()} `${sym.name}` has the same name as an argument'
		} else if other.type_of() == sym.type_of() {
			'duplicate ${sym.type_of()} `${sym.name}`'
		} else {
			'another symbol exists with the same name as `${sym.name}`'
		}
		return error(m)
	}
	sc.syms << sym
}

pub fn (mut sc Scope) add_local_symbol(sym Symbol) ! {
	if other := sc.find(sym.name) {
		m := if other.type_of() == sym.type_of() {
			'duplicate ${sym.type_of()} `${sym.name}`'
		} else {
			'another symbol exists with the same name as `${sym.name}`'
		}
		return error(m)
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
	if sc.owner !is Function || isnil(sc.parent) {
		return none
	}
	return sc.parent.lookup(name)
}
