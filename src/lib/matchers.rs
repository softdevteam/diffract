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

/// Matchers create mappings between two abstract syntax trees.
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;

use action::EditScript;
use ast::{Arena, ArenaError, DstNodeId, NodeId, SrcNodeId};
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
    /// Should contain the same information as `dst` map.
    pub src: RefCell<HashMap<NodeId<SrcNodeId>, (NodeId<DstNodeId>, MappingType)>>,
    /// Mappings from the destination tree to the source.
    ///
    /// Should contain the same information as `src` map.
    pub dst: RefCell<HashMap<NodeId<DstNodeId>, (NodeId<SrcNodeId>, MappingType)>>,

    /// Source arena (treat as mutable).
    pub src_arena: RefCell<Arena<T, SrcNodeId>>,
    /// Destination arena (treat as immutable).
    pub dst_arena: RefCell<Arena<T, DstNodeId>>
}

impl<T: Clone + Debug + ToString> RenderJson for MappingStore<T> {
    fn render_json(&self, indent: usize) -> String {
        let ind_2 = " ".repeat(indent * 2);
        let ind_3 = " ".repeat(indent * 3);
        let mut json = vec![];
        for (key, value) in self.src.borrow().iter() {
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
    pub fn new(src: Arena<T, SrcNodeId>, dst: Arena<T, DstNodeId>) -> MappingStore<T> {
        MappingStore { src: RefCell::new(HashMap::new()),
                       dst: RefCell::new(HashMap::new()),
                       src_arena: RefCell::new(src),
                       dst_arena: RefCell::new(dst) }
    }

    /// Return the number of mappings in this store.
    pub fn size(&self) -> usize {
        self.src.borrow().len()
    }

    /// Push a new mapping into the store.
    pub fn push(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>, ty: &MappingType) {
        self.src.borrow_mut().insert(src, (dst, ty.clone()));
        self.dst.borrow_mut().insert(dst, (src, ty.clone()));
        info!("Mapping: {:?} {:} -> {:?} {:} ({:?})",
              self.src_arena.borrow()[src].ty,
              self.src_arena.borrow()[src].label,
              self.dst_arena.borrow()[dst].ty,
              self.dst_arena.borrow()[dst].label,
              ty);
    }

    /// Remove mapping from store.
    pub fn remove(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) {
        self.src.borrow_mut().remove(&src);
        self.dst.borrow_mut().remove(&dst);
    }

    /// `true` if the store has a mapping from `src` to another node.
    pub fn contains_src(&self, src: NodeId<SrcNodeId>) -> bool {
        self.src.borrow().contains_key(&src)
    }

    /// `true` if the store has a mapping from a node to `dst`.
    pub fn contains_dst(&self, dst: NodeId<DstNodeId>) -> bool {
        self.dst.borrow().contains_key(&dst)
    }

    /// Get the `NodeId` that `dst` is mapped from.
    pub fn get_src(&self, dst: NodeId<DstNodeId>) -> Option<NodeId<SrcNodeId>> {
        self.dst.borrow().get(&dst).and_then(|x| Some(x.0))
    }

    /// Get the `NodeId` that `src` is mapped to.
    pub fn get_dst(&self, src: NodeId<SrcNodeId>) -> Option<NodeId<DstNodeId>> {
        self.src.borrow().get(&src).and_then(|x| Some(x.0))
    }

    /// Test whether `src` is mapped to `dst` in this store.
    pub fn is_mapped(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) -> bool {
        if !self.contains_src(src) {
            return false;
        }
        self.get_dst(src).map_or(false, |x| x == dst)
    }

    /// Compute whether two sub-trees are isomorphic, based on their hashes.
    ///
    /// Isomorphism in this case is defined has having the same hash and the
    /// same hash serialization.
    pub fn is_isomorphic_hash(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) -> bool {
        let is_isomorphic = src.to_static_hash_string(&self.src_arena.borrow())
                            == dst.to_static_hash_string(&self.dst_arena.borrow());
        debug_assert!(!is_isomorphic
                      || (self.src_arena.borrow()[src].label == self.dst_arena.borrow()[dst].label)
                         && (self.src_arena.borrow()[src].ty == self.dst_arena.borrow()[dst].ty));
        is_isomorphic
    }

    /// Two sub-trees are isomorphic if they have the same structure.
    ///
    /// Two single-node trees are isomorphic if they have the same types
    /// and values. Isomorphic subtrees must have the same *shape* i.e. the
    /// subtrees must have isomorphic children.
    ///
    /// Described in more detail in Chawathe et al. (1996).
    pub fn is_isomorphic(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) -> bool {
        // Case 1: both nodes are leaves.
        if src.is_leaf(&self.src_arena.borrow())
           && dst.is_leaf(&self.dst_arena.borrow())
           && self.src_arena.borrow()[src].label == self.dst_arena.borrow()[dst].label
           && self.src_arena.borrow()[src].ty == self.dst_arena.borrow()[dst].ty
        {
            return true;
        }
        // Case 2: one node is a leaf and the other is a branch.
        if src.is_leaf(&self.src_arena.borrow()) && !dst.is_leaf(&self.dst_arena.borrow())
           || !src.is_leaf(&self.src_arena.borrow()) && dst.is_leaf(&self.dst_arena.borrow())
        {
            return false;
        }
        // Case 3: both nodes are branches.
        if self.src_arena.borrow()[src].label != self.dst_arena.borrow()[dst].label
           || self.src_arena.borrow()[src].ty != self.dst_arena.borrow()[dst].ty
           || src.height(&self.src_arena.borrow()) != dst.height(&self.dst_arena.borrow())
        {
            return false;
        }
        let f_children =
            src.children(&self.src_arena.borrow()).collect::<Vec<NodeId<SrcNodeId>>>();
        let t_children =
            dst.children(&self.dst_arena.borrow()).collect::<Vec<NodeId<DstNodeId>>>();
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

    /// `true` if `src` and `dst` may be mapped to one another, `false` otherwise.
    pub fn is_mapping_allowed(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) -> bool {
        self.src_arena.borrow()[src].ty == self.dst_arena.borrow()[dst].ty
        && !(self.contains_src(src) || self.contains_dst(dst))
    }
}

/// Match two trees and return a store of mappings between them.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait MatchTrees<T: Clone + Debug + ToString> {
    /// Match two trees and return a store of mappings between them.
    fn match_trees(&mut self,
                   src: Arena<T, SrcNodeId>,
                   dst: Arena<T, DstNodeId>)
                   -> MappingStore<T>;

    /// Describe the matcher for the user.
    ///
    /// This is the string that is printed when the user passes in the --list
    /// CLI option.
    fn describe(&self) -> String;
}

/// Test that two nodes have the same label and type.
pub fn has_same_type_and_label<T: Clone + Debug + Eq>(n1: NodeId<SrcNodeId>,
                                                      arena1: &Arena<T, SrcNodeId>,
                                                      n2: NodeId<DstNodeId>,
                                                      arena2: &Arena<T, DstNodeId>)
                                                      -> bool {
    arena1[n1].label == arena2[n2].label && arena1[n1].ty == arena2[n2].ty
}

/// Test that two nodes have the same type.
pub fn has_same_type<T: Clone + Debug + Eq>(n1: NodeId<SrcNodeId>,
                                            arena1: &Arena<T, SrcNodeId>,
                                            n2: NodeId<DstNodeId>,
                                            arena2: &Arena<T, DstNodeId>)
                                            -> bool {
    arena1[n1].ty == arena2[n2].ty
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
            MappingStore::new(plus.clone(), Arena::<String, DstNodeId>::from(plus.clone()));
        let store_m =
            MappingStore::new(mult.clone(), Arena::<String, DstNodeId>::from(mult.clone()));
        let store = MappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
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
            MappingStore::new(plus.clone(), Arena::<String, DstNodeId>::from(plus.clone()));
        let store_m =
            MappingStore::new(mult.clone(), Arena::<String, DstNodeId>::from(mult.clone()));
        let store = MappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
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
        let store = MappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
        assert!(store.is_mapping_allowed(NodeId::new(0), NodeId::new(2)));
        assert!(store.is_mapping_allowed(NodeId::new(1), NodeId::new(3)));
        assert!(store.is_mapping_allowed(NodeId::new(2), NodeId::new(4)));
        assert!(store.is_mapping_allowed(NodeId::new(0), NodeId::new(0)));
        // Not allowed.
        assert!(!store.is_mapping_allowed(NodeId::new(0), NodeId::new(1)));
        assert!(!store.is_mapping_allowed(NodeId::new(0), NodeId::new(3)));
        assert!(!store.is_mapping_allowed(NodeId::new(0), NodeId::new(4)));
        // Mapping already exists.
        store.push(NodeId::new(0), NodeId::new(0), &MappingType::ANCHOR);
        store.push(NodeId::new(2), NodeId::new(4), &MappingType::ANCHOR);
        assert_eq!(2, store.size());
        assert!(!store.is_mapping_allowed(NodeId::new(0), NodeId::new(2)));
        assert!(store.is_mapping_allowed(NodeId::new(1), NodeId::new(3)));
        assert!(!store.is_mapping_allowed(NodeId::new(2), NodeId::new(4)));
        assert!(!store.is_mapping_allowed(NodeId::new(0), NodeId::new(0)));
    }

    #[test]
    fn is_mapped() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
        store.push(NodeId::new(0), NodeId::new(0), &MappingType::ANCHOR);
        store.push(NodeId::new(2), NodeId::new(4), &MappingType::ANCHOR);
        assert!(store.is_mapped(NodeId::new(0), NodeId::new(0)));
        assert!(store.is_mapped(NodeId::new(2), NodeId::new(4)));
        assert_eq!(2, store.size());
        // Not mapped.
        assert!(!store.is_mapped(NodeId::new(0), NodeId::new(1)));
        assert!(!store.is_mapped(NodeId::new(0), NodeId::new(2)));
        assert!(!store.is_mapped(NodeId::new(0), NodeId::new(3)));
        assert!(!store.is_mapped(NodeId::new(0), NodeId::new(4)));
        assert!(!store.is_mapped(NodeId::new(1), NodeId::new(1)));
        assert!(!store.is_mapped(NodeId::new(1), NodeId::new(2)));
        assert!(!store.is_mapped(NodeId::new(1), NodeId::new(3)));
        assert!(!store.is_mapped(NodeId::new(1), NodeId::new(4)));
        assert!(!store.is_mapped(NodeId::new(2), NodeId::new(1)));
        assert!(!store.is_mapped(NodeId::new(2), NodeId::new(2)));
        assert!(!store.is_mapped(NodeId::new(2), NodeId::new(3)));
    }
    #[test]
    fn render_json() {
        use myers_matcher::MyersConfig;
        let plus = create_plus_arena();
        let mult = create_mult_arena();
        let mut matcher = MyersConfig::new();
        let store = matcher.match_trees(plus, Arena::<String, DstNodeId>::from(mult));
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
