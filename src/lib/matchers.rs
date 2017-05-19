// Copyright (c) 2017 King's College London
// created by the Software Development Team <http://soft-dev.org/>
//
// The Universal Permissive License (UPL), Version 1.0
//
// Subject to the condition set forth below, permission is hereby granted to any
// person obtaining a copy of this software, associated documentation and/or
// data (collectively the "Software"), free of charge and under any and all
// copyright rights in the Software, and any and all patent rights owned or
// freely licensable by each licensor hereunder covering either (i) the
// unmodified Software as contributed to or provided by such licensor, or (ii)
// the Larger Works (as defined below), to deal in both
//
// (a) the Software, and
// (b) any piece of software and/or hardware listed in the lrgrwrks.txt file
// if one is included with the Software (each a “Larger Work” to which the Software
// is contributed by such licensors),
//
// without restriction, including without limitation the rights to copy, create
// derivative works of, display, perform, and distribute the Software and make,
// use, sell, offer for sale, import, export, have made, and have sold the
// Software and the Larger Work(s), and to sublicense the foregoing rights on
// either these or other terms.
//
// This license is subject to the following condition: The above copyright
// notice and either this complete permission notice or at a minimum a reference
// to the UPL must be included in all copies or substantial portions of the
// Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#![warn(missing_docs)]

use std::collections::HashMap;

use ast::{Arena, NodeId};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
/// Type of mapping.
///
/// Not needed by matching algorithms, but useful for debugging.
pub enum MappingType {
    /// Anchor mappings are found by the top-down GumTree matcher.
    ANCHOR,
    /// Container mappings are found by the phase one of the bottom-up GumTree matcher.
    CONTAINER,
    /// Recovery mappings are found by phase two of the bottom-up GumTree matcher.
    RECOVERY,
}

impl Default for MappingType {
    fn default() -> MappingType {
        MappingType::ANCHOR
    }
}

/// A store of mappings between nodes in different arenas.
/// Direction is important.
pub struct MappingStore<T: Clone> {
    /// Mappings from the source tree to the destination.
    ///
    /// Should contain the same information as `to_map`.
    pub from_map: HashMap<NodeId, (NodeId, MappingType)>,
    /// Mappings from the destination tree to the source.
    ///
    /// Should contain the same information as `from_map`.
    pub to_map: HashMap<NodeId, (NodeId, MappingType)>,

    /// Source arena (treat as immutable).
    pub from: Arena<T>,
    /// Destination arena (treat as immutable).
    pub to: Arena<T>,
}

impl<T: Clone> MappingStore<T> {
    /// Create a new mapping store.
    pub fn new(base: Arena<T>, diff: Arena<T>) -> MappingStore<T> {
        MappingStore {
            from_map: HashMap::new(),
            to_map: HashMap::new(),
            from: base,
            to: diff,
        }
    }

    /// Push a new mapping into the store.
    pub fn push(&mut self, from: NodeId, to: NodeId, ty: MappingType) {
        self.from_map.insert(from, (to, ty.clone()));
        self.to_map.insert(to, (from, ty.clone()));
    }

    /// Remove mapping from store.
    pub fn remove(&mut self, from: &NodeId, to: &NodeId) {
        self.from_map.remove(from);
        self.to_map.remove(to);
    }

    /// `true` if the store has a mapping from `from` to another node.
    pub fn has_from(&self, from: &NodeId) -> bool {
        self.from_map.contains_key(from)
    }

    /// `true` if the store has a mapping from a node to `to`.
    pub fn has_to(&self, to: &NodeId) -> bool {
        self.to_map.contains_key(to)
    }

    /// `true` if `node` is involved in a mapping, `false` otherwise.
    pub fn is_mapped(&self, node: NodeId, is_from: bool) -> bool {
        if is_from {
            return self.from_map.contains_key(&node);
        }
        self.to_map.contains_key(&node)
    }

    /// Two sub-trees are isomorphic if they have the same structure.
    ///
    /// Two single-node trees are isomorphic if they have the same labels
    /// (although the nodes may have different values). Isomorphic subtrees must
    /// have the same *shape* i.e. the subtrees must have isomorphic children.
    ///
    /// Described in more detail in Chawathe et al. (1996).
    pub fn is_isomorphic(&self, from: NodeId, to: NodeId) -> bool {
        // Case 1: both nodes are leaves.
        if from.is_leaf(&self.from) && to.is_leaf(&self.to) &&
           self.from[from].label == self.to[to].label {
            return true;
        }
        // Case 2: one node is a leaf and the other is a branch.
        if from.is_leaf(&self.from) && !to.is_leaf(&self.to) ||
           !from.is_leaf(&self.from) && to.is_leaf(&self.to) {
            return false;
        }
        // Case 3: both nodes are branches.
        if self.from[from].label != self.to[to].label ||
           from.height(&self.from) != to.height(&self.to) {
            return false;
        }
        let f_children = from.children(&self.from).collect::<Vec<NodeId>>();
        let t_children = to.children(&self.to).collect::<Vec<NodeId>>();
        if f_children.len() != t_children.len() {
            return false;
        }
        for index in 0..f_children.len() {
            if !self.is_isomorphic(f_children[index], t_children[index]) {
                return false;
            }
        }
        true
    }

    /// `true` if `from` and `to` may be mapped to one another, `false` otherwise.
    pub fn is_mapping_allowed(&self, from: &NodeId, to: &NodeId) -> bool {
        self.from[*from].label == self.to[*to].label && !(self.has_from(from) || self.has_to(to))
    }
}

/// Match two trees and return a store of mappings between them.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait MatchTrees<T: Clone> {
    /// Match two trees and return a store of mappings between them.
    fn match_trees(&self, base: Arena<T>, diff: Arena<T>) -> MappingStore<T>;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_mult_arena() -> Arena<String> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("+"), String::from("Expr"), 0);
        let n1 = arena.new_node(String::from("1"), String::from("INT"), 2);
        arena.make_child_of(n1, root).unwrap();
        let n2 = arena.new_node(String::from("*"), String::from("Expr"), 2);
        arena.make_child_of(n2, root).unwrap();
        let n3 = arena.new_node(String::from("3"), String::from("INT"), 4);
        arena.make_child_of(n3, n2).unwrap();
        let n4 = arena.new_node(String::from("4"), String::from("INT"), 4);
        arena.make_child_of(n4, n2).unwrap();
        let format1 = "Expr +
  INT 1
  Expr *
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        arena
    }

    fn create_plus_arena() -> Arena<String> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("+"), String::from("Expr"), 0);
        let n1 = arena.new_node(String::from("3"), String::from("INT"), 4);
        arena.make_child_of(n1, root).unwrap();
        let n2 = arena.new_node(String::from("4"), String::from("INT"), 4);
        arena.make_child_of(n2, root).unwrap();
        let format1 = "Expr +
    INT 3
    INT 4
";
        assert_eq!(format1, format!("{}", arena));
        arena
    }

    #[test]
    fn is_isomorphic() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MappingStore::new(plus, mult);
        assert!(store.is_isomorphic(NodeId::new(0), NodeId::new(2)));
        assert!(store.is_isomorphic(NodeId::new(1), NodeId::new(3)));
        assert!(store.is_isomorphic(NodeId::new(2), NodeId::new(4)));
        assert!(store.is_isomorphic(NodeId::new(1), NodeId::new(4)));
        assert!(store.is_isomorphic(NodeId::new(2), NodeId::new(3)));
        // Not isomorphic.
        assert!(!store.is_isomorphic(NodeId::new(0), NodeId::new(0)));
        assert!(!store.is_isomorphic(NodeId::new(0), NodeId::new(1)));
        assert!(!store.is_isomorphic(NodeId::new(0), NodeId::new(3)));
        assert!(!store.is_isomorphic(NodeId::new(0), NodeId::new(4)));
    }

    #[test]
    fn is_mapping_allowed() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let mut store = MappingStore::new(plus, mult);
        assert!(store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(2)));
        assert!(store.is_mapping_allowed(&NodeId::new(1), &NodeId::new(3)));
        assert!(store.is_mapping_allowed(&NodeId::new(2), &NodeId::new(4)));
        assert!(store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(0)));
        // Not allowed.
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(1)));
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(3)));
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(4)));
        // Mapping already exists.
        store.push(NodeId::new(0), NodeId::new(0), MappingType::ANCHOR);
        store.push(NodeId::new(2), NodeId::new(4), MappingType::ANCHOR);
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(2)));
        assert!(store.is_mapping_allowed(&NodeId::new(1), &NodeId::new(3)));
        assert!(!store.is_mapping_allowed(&NodeId::new(2), &NodeId::new(4)));
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(0)));
    }
}
