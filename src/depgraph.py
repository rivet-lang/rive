# Copyright (C) 2022 The Rivet Team. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

class OrderedDepMap:
    def __init__(self, keys=[], data={}):
        self.keys = keys
        self.data = data

    def set(self, name, deps):
        if name not in self.data:
            self.keys.append(name)
        self.data[name] = deps

    def add(self, name, deps):
        d = self.get(name)
        for dep in deps:
            if dep not in d:
                d.append(dep)
        self.set(name, d)

    def get(self, name):
        try:
            return self.data[name]
        except:
            return []

    def delete(self, name):
        if name not in self.data:
            raise KeyError(f"OrderedDepMap.delete: no such key `{name}`")
        for i, _ in enumerate(self.keys):
            if self.keys[i] == name:
                self.keys.pop(i)
                break
        self.data.pop(name)

    def apply_diff(self, name, deps):
        diff = []
        deps_of_name = self.get(name)
        for dep in deps_of_name:
            if dep not in deps:
                diff.append(dep)
        self.set(name, diff)

    def size(self):
        return len(self.data)

class DepGraphNode:
    def __init__(self, name, deps):
        self.name = name
        self.deps = deps

class NodeNames:
    def __init__(self, is_cycle={}, names={}):
        self.is_cycle = is_cycle
        self.names = names

    def is_part_of_cycle(self, name, already_seen):
        seen = False
        new_already_seen = already_seen.copy()
        if name in self.is_cycle:
            return self.is_cycle[name], new_already_seen
        if name in already_seen:
            new_already_seen.append(name)
            self.is_cycle[name] = True
            return True, new_already_seen
        new_already_seen.append(name)
        deps = self.names[name] if name in self.names else []
        if len(deps) == 0:
            self.is_cycle[name] = False
            return False, new_already_seen
        for d in deps:
            d_already_seen = new_already_seen.copy()
            seen, d_already_seen = self.is_part_of_cycle(d, d_already_seen)
            if seen:
                new_already_seen = d_already_seen.copy()
                self.is_cycle[name] = True
                return True, new_already_seen
        self.is_cycle[name] = False
        return False, new_already_seen

class DepGraph:
    def __init__(self, acyclic=True, nodes=[]):
        self.acyclic = acyclic
        self.nodes = nodes

    def add(self, name, deps):
        self.nodes.append(DepGraphNode(name, deps))

    def resolve(self):
        node_names = OrderedDepMap()
        node_deps = OrderedDepMap()
        for node in self.nodes:
            node_names.add(node.name, node.deps)
            node_deps.add(node.name, node.deps)
        iterations = 0
        resolved = DepGraph()
        resolved.nodes = [] # fix bug
        while node_deps.size() != 0:
            iterations += 1
            ready_set = []
            for name in node_deps.keys:
                if len(node_deps.get(name)) == 0:
                    ready_set.append(name)
            if len(ready_set) == 0:
                g = DepGraph()
                g.acyclic = False
                for name in node_deps.keys:
                    g.add(name, node_names.get(name))
                return g
            for name in ready_set:
                node_deps.delete(name)
                resolved.add(name, node_names.get(name))
            for name in node_deps.keys:
                node_deps.apply_diff(name, ready_set)
        return resolved

    def last_node(self):
        return self.nodes[-1]

    def display(self):
        out = []
        for node in self.nodes:
            for dep in node.deps:
                out.append(f" * {node.name} -> {dep}")
        return "\n".join(out)

    def display_cycles(self):
        seen = False
        out = []
        nn = NodeNames()
        for node in self.nodes:
            nn.names[node.name] = node.deps
        for k, _ in nn.names.items():
            cycle_names = []
            if k in nn.is_cycle:
                continue
            seen, cycle_names = nn.is_part_of_cycle(k, cycle_names)
            if seen:
                out.append(" * " + " -> ".join(cycle_names))
        return "\n".join(out)
