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
// if one is included with the Software (each a "Larger Work" to which the Software
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

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;

use action::EditScript;
use ast::{Arena, ArenaError, FromNodeId, NodeId, ToNodeId};
use emitters::RenderJson;

/// Result type returned by the edit script generator.
pub type EditScriptResult<T> = Result<EditScript<T>, ArenaError>;

/// Type of mapping.
///
/// Not needed by matching algorithms, but useful for debugging.
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum MappingType {
    /// Anchor mappings are found by the top-down GumTree matcher.
    ANCHOR,
    /// Container mappings are found by the phase one of the bottom-up GumTree matcher.
    CONTAINER,
    /// Recovery mappings are found by phase two of the bottom-up GumTree matcher.
    RECOVERY,
    /// A mapping added by the algorithm that generates the edit script.
    ///
    /// See Chawathe et al. (1996).
    EDIT
}

impl Default for MappingType {
    fn default() -> MappingType {
        MappingType::ANCHOR
    }
}

/// A store of mappings between nodes in different arenas.
/// Direction is important.
pub struct MappingStore<T: Clone + Debug + ToString> {
    /// Mappings from the source tree to the destination.
    ///
    /// Should contain the same information as `to_map`.
    pub from: RefCell<HashMap<NodeId<FromNodeId>, (NodeId<ToNodeId>, MappingType)>>,
    /// Mappings from the destination tree to the source.
    ///
    /// Should contain the same information as `from_map`.
    pub to: RefCell<HashMap<NodeId<ToNodeId>, (NodeId<FromNodeId>, MappingType)>>,

    /// Source arena (treat as mutable).
    pub from_arena: RefCell<Arena<T, FromNodeId>>,
    /// Destination arena (treat as immutable).
    pub to_arena: RefCell<Arena<T, ToNodeId>>
}

impl<T: Clone + Debug + ToString> RenderJson for MappingStore<T> {
    fn render_json(&self, indent: usize) -> String {
        let ind_2 = " ".repeat(indent * 2);
        let ind_3 = " ".repeat(indent * 3);
        let mut json = vec![];
        for (key, value) in self.from.borrow().iter() {
            json.push(format!("{}{{\n{}\"src\": {},\n{}\"dest\": {}\n{}}}",
                              ind_2, ind_3, key, ind_3, value.0, ind_2));
        }
        format!("{}{}{}{}{}{}",
                " ".repeat(indent),
                "\"matches\": [\n",
                json.join(",\n"),
                "\n",
                " ".repeat(indent),
                "]")
    }
}

impl<T: Clone + Debug + Eq + ToString + 'static> MappingStore<T> {
    /// Create a new mapping store.
    pub fn new(base: Arena<T, FromNodeId>, diff: Arena<T, ToNodeId>) -> MappingStore<T> {
        MappingStore { from: RefCell::new(HashMap::new()),
                       to: RefCell::new(HashMap::new()),
                       from_arena: RefCell::new(base),
                       to_arena: RefCell::new(diff) }
    }

    /// Return the number of mappings in this store.
    pub fn size(&self) -> usize {
        self.from.borrow().len()
    }

    /// Push a new mapping into the store.
    pub fn push(&self, from: NodeId<FromNodeId>, to: NodeId<ToNodeId>, ty: &MappingType) {
        self.from.borrow_mut().insert(from, (to, ty.clone()));
        self.to.borrow_mut().insert(to, (from, ty.clone()));
        info!("Mapping: {:?} {:} -> {:?} {:} ({:?})",
              self.from_arena.borrow()[from].ty,
              self.from_arena.borrow()[from].label,
              self.to_arena.borrow()[to].ty,
              self.to_arena.borrow()[to].label,
              ty);
    }

    /// Remove mapping from store.
    pub fn remove(&self, from: &NodeId<FromNodeId>, to: &NodeId<ToNodeId>) {
        self.from.borrow_mut().remove(from);
        self.to.borrow_mut().remove(to);
    }

    /// `true` if the store has a mapping from `from` to another node.
    pub fn contains_from(&self, from: &NodeId<FromNodeId>) -> bool {
        self.from.borrow().contains_key(from)
    }

    /// `true` if the store has a mapping from a node to `to`.
    pub fn contains_to(&self, to: &NodeId<ToNodeId>) -> bool {
        self.to.borrow().contains_key(to)
    }

    /// Get the `NodeId` that `to` is mapped from.
    pub fn get_from(&self, to: &NodeId<ToNodeId>) -> Option<NodeId<FromNodeId>> {
        self.to.borrow().get(to).and_then(|x| Some(x.0))
    }

    /// Get the `NodeId` that `from` is mapped to.
    pub fn get_to(&self, from: &NodeId<FromNodeId>) -> Option<NodeId<ToNodeId>> {
        self.from.borrow().get(from).and_then(|x| Some(x.0))
    }

    /// Test whether `from` is mapped to `to` in this store.
    pub fn is_mapped(&self, from: &NodeId<FromNodeId>, to: &NodeId<ToNodeId>) -> bool {
        if !self.contains_from(from) {
            return false;
        }
        self.get_to(from).map_or(false, |x| x == *to)
    }

    /// Compute whether two sub-trees are isomorphic, based on their hashes.
    ///
    /// Isomorphism in this case is defined has having the same hash and the
    /// same hash serialization.
    pub fn is_isomorphic_hash(&self, from: NodeId<FromNodeId>, to: NodeId<ToNodeId>) -> bool {
        let is_isomorphic = from.to_static_hash_string(&self.from_arena.borrow())
                            == to.to_static_hash_string(&self.to_arena.borrow());
        debug_assert!(!is_isomorphic
                      || (self.from_arena.borrow()[from].label == self.to_arena.borrow()[to].label)
                         && (self.from_arena.borrow()[from].ty == self.to_arena.borrow()[to].ty));
        is_isomorphic
    }

    /// Two sub-trees are isomorphic if they have the same structure.
    ///
    /// Two single-node trees are isomorphic if they have the same types
    /// and values. Isomorphic subtrees must have the same *shape* i.e. the
    /// subtrees must have isomorphic children.
    ///
    /// Described in more detail in Chawathe et al. (1996).
    pub fn is_isomorphic(&self, from: NodeId<FromNodeId>, to: NodeId<ToNodeId>) -> bool {
        // Case 1: both nodes are leaves.
        if from.is_leaf(&self.from_arena.borrow()) && to.is_leaf(&self.to_arena.borrow())
           && self.from_arena.borrow()[from].label == self.to_arena.borrow()[to].label
           && self.from_arena.borrow()[from].ty == self.to_arena.borrow()[to].ty
        {
            return true;
        }
        // Case 2: one node is a leaf and the other is a branch.
        if from.is_leaf(&self.from_arena.borrow()) && !to.is_leaf(&self.to_arena.borrow())
           || !from.is_leaf(&self.from_arena.borrow()) && to.is_leaf(&self.to_arena.borrow())
        {
            return false;
        }
        // Case 3: both nodes are branches.
        if self.from_arena.borrow()[from].label != self.to_arena.borrow()[to].label
           || self.from_arena.borrow()[from].ty != self.to_arena.borrow()[to].ty
           || from.height(&self.from_arena.borrow()) != to.height(&self.to_arena.borrow())
        {
            return false;
        }
        let f_children =
            from.children(&self.from_arena.borrow()).collect::<Vec<NodeId<FromNodeId>>>();
        let t_children = to.children(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>();
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
    pub fn is_mapping_allowed(&self, from: &NodeId<FromNodeId>, to: &NodeId<ToNodeId>) -> bool {
        self.from_arena.borrow()[*from].ty == self.to_arena.borrow()[*to].ty
        && !(self.contains_from(from) || self.contains_to(to))
    }
}

/// Match two trees and return a store of mappings between them.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait MatchTrees<T: Clone + Debug + ToString> {
    /// Match two trees and return a store of mappings between them.
    fn match_trees(&mut self,
                   base: Arena<T, FromNodeId>,
                   diff: Arena<T, ToNodeId>)
                   -> MappingStore<T>;

    /// Describe the matcher for the user.
    ///
    /// This is the string that is printed when the user passes in the --list
    /// CLI option.
    fn describe(&self) -> String;
}

/// Test that two nodes have the same label and type.
pub fn has_same_type_and_label<T: Clone + Debug + Eq>(n1: &NodeId<FromNodeId>,
                                                      arena1: &Arena<T, FromNodeId>,
                                                      n2: &NodeId<ToNodeId>,
                                                      arena2: &Arena<T, ToNodeId>)
                                                      -> bool {
    arena1[*n1].label == arena2[*n2].label && arena1[*n1].ty == arena2[*n2].ty
}

/// Test that two nodes have the same type.
pub fn has_same_type<T: Clone + Debug + Eq>(n1: &NodeId<FromNodeId>,
                                            arena1: &Arena<T, FromNodeId>,
                                            n2: &NodeId<ToNodeId>,
                                            arena2: &Arena<T, ToNodeId>)
                                            -> bool {
    arena1[*n1].ty == arena2[*n2].ty
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_common::{create_mult_arena, create_plus_arena};

    #[test]
    fn is_isomorphic() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store_p =
            MappingStore::new(plus.clone(), Arena::<String, ToNodeId>::from(plus.clone()));
        let store_m =
            MappingStore::new(mult.clone(), Arena::<String, ToNodeId>::from(mult.clone()));
        let store = MappingStore::new(plus, Arena::<String, ToNodeId>::from(mult));
        // Isomorphic.
        assert!(store_p.is_isomorphic(NodeId::new(0), NodeId::new(0)));
        assert!(store_p.is_isomorphic(NodeId::new(1), NodeId::new(1)));
        assert!(store_p.is_isomorphic(NodeId::new(2), NodeId::new(2)));
        assert!(store_m.is_isomorphic(NodeId::new(0), NodeId::new(0)));
        assert!(store_m.is_isomorphic(NodeId::new(1), NodeId::new(1)));
        assert!(store_m.is_isomorphic(NodeId::new(2), NodeId::new(2)));
        assert!(store_m.is_isomorphic(NodeId::new(3), NodeId::new(3)));
        assert!(store_m.is_isomorphic(NodeId::new(4), NodeId::new(4)));
        assert!(store.is_isomorphic(NodeId::new(1), NodeId::new(3)));
        assert!(store.is_isomorphic(NodeId::new(2), NodeId::new(4)));
        // Not isomorphic.
        assert!(!store.is_isomorphic(NodeId::new(0), NodeId::new(0)));
        assert!(!store.is_isomorphic(NodeId::new(1), NodeId::new(1)));
        assert!(!store.is_isomorphic(NodeId::new(2), NodeId::new(2)));
        assert!(!store.is_isomorphic(NodeId::new(2), NodeId::new(3)));
    }

    #[test]
    fn is_isomorphic_hash() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store_p =
            MappingStore::new(plus.clone(), Arena::<String, ToNodeId>::from(plus.clone()));
        let store_m =
            MappingStore::new(mult.clone(), Arena::<String, ToNodeId>::from(mult.clone()));
        let store = MappingStore::new(plus, Arena::<String, ToNodeId>::from(mult));
        // Isomorphic.
        assert!(store_p.is_isomorphic(NodeId::new(0), NodeId::new(0)));
        assert!(store_p.is_isomorphic(NodeId::new(1), NodeId::new(1)));
        assert!(store_p.is_isomorphic(NodeId::new(2), NodeId::new(2)));
        assert!(store_m.is_isomorphic(NodeId::new(0), NodeId::new(0)));
        assert!(store_m.is_isomorphic(NodeId::new(1), NodeId::new(1)));
        assert!(store_m.is_isomorphic(NodeId::new(2), NodeId::new(2)));
        assert!(store_m.is_isomorphic(NodeId::new(3), NodeId::new(3)));
        assert!(store_m.is_isomorphic(NodeId::new(4), NodeId::new(4)));
        assert!(store.is_isomorphic(NodeId::new(1), NodeId::new(3)));
        assert!(store.is_isomorphic(NodeId::new(2), NodeId::new(4)));
        // Not isomorphic.
        assert!(!store.is_isomorphic(NodeId::new(0), NodeId::new(0)));
        assert!(!store.is_isomorphic(NodeId::new(1), NodeId::new(1)));
        assert!(!store.is_isomorphic(NodeId::new(2), NodeId::new(2)));
        assert!(!store.is_isomorphic(NodeId::new(2), NodeId::new(3)));
    }

    #[test]
    fn is_mapping_allowed() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MappingStore::new(plus, Arena::<String, ToNodeId>::from(mult));
        assert!(store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(2)));
        assert!(store.is_mapping_allowed(&NodeId::new(1), &NodeId::new(3)));
        assert!(store.is_mapping_allowed(&NodeId::new(2), &NodeId::new(4)));
        assert!(store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(0)));
        // Not allowed.
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(1)));
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(3)));
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(4)));
        // Mapping already exists.
        store.push(NodeId::new(0), NodeId::new(0), &MappingType::ANCHOR);
        store.push(NodeId::new(2), NodeId::new(4), &MappingType::ANCHOR);
        assert_eq!(2, store.size());
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(2)));
        assert!(store.is_mapping_allowed(&NodeId::new(1), &NodeId::new(3)));
        assert!(!store.is_mapping_allowed(&NodeId::new(2), &NodeId::new(4)));
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(0)));
    }

    #[test]
    fn is_mapped() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MappingStore::new(plus, Arena::<String, ToNodeId>::from(mult));
        store.push(NodeId::new(0), NodeId::new(0), &MappingType::ANCHOR);
        store.push(NodeId::new(2), NodeId::new(4), &MappingType::ANCHOR);
        assert!(store.is_mapped(&NodeId::new(0), &NodeId::new(0)));
        assert!(store.is_mapped(&NodeId::new(2), &NodeId::new(4)));
        assert_eq!(2, store.size());
        // Not mapped.
        assert!(!store.is_mapped(&NodeId::new(0), &NodeId::new(1)));
        assert!(!store.is_mapped(&NodeId::new(0), &NodeId::new(2)));
        assert!(!store.is_mapped(&NodeId::new(0), &NodeId::new(3)));
        assert!(!store.is_mapped(&NodeId::new(0), &NodeId::new(4)));
        assert!(!store.is_mapped(&NodeId::new(1), &NodeId::new(1)));
        assert!(!store.is_mapped(&NodeId::new(1), &NodeId::new(2)));
        assert!(!store.is_mapped(&NodeId::new(1), &NodeId::new(3)));
        assert!(!store.is_mapped(&NodeId::new(1), &NodeId::new(4)));
        assert!(!store.is_mapped(&NodeId::new(2), &NodeId::new(1)));
        assert!(!store.is_mapped(&NodeId::new(2), &NodeId::new(2)));
        assert!(!store.is_mapped(&NodeId::new(2), &NodeId::new(3)));
    }

    #[test]
    fn render_json() {
        use myers_matcher::MyersConfig;
        let plus = create_plus_arena();
        let mult = create_mult_arena();
        let mut matcher = MyersConfig::new();
        let store = matcher.match_trees(plus, Arena::<String, ToNodeId>::from(mult));
        let expected_str = vec!["\"matches\": [",
                                "{\n\"src\": 1,\n\"dest\": 3\n}",
                                "{\n\"src\": 0,\n\"dest\": 0\n}",
                                "{\n\"src\": 2,\n\"dest\": 4\n}"];
        let expected = expected_str.iter()
                                   .map(|s| String::from(*s))
                                   .collect::<Vec<String>>();
        let got = store.render_json(0);
        for item in expected {
            assert!(got.contains(&item));
        }
    }
}
