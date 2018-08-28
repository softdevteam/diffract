// Copyright (c) 2018 King's College London
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

/// A multi-mapping store holds non-unique mappings between two abstract syntax trees.
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use ast::{Arena, DstNodeId, NodeId, SrcNodeId};

/// A store of mappings between nodes in different arenas.
/// Direction is important.
pub struct MultiMappingStore<T: Clone + Debug + ToString> {
    /// Mappings from the source tree to the destination.
    ///
    /// Should contain the same information as `dst` map.
    pub src: RefCell<HashMap<NodeId<SrcNodeId>, HashSet<NodeId<DstNodeId>>>>,
    /// Mappings from the destination tree to the source.
    ///
    /// Should contain the same information as `src` map.
    pub dst: RefCell<HashMap<NodeId<DstNodeId>, HashSet<NodeId<SrcNodeId>>>>,

    /// Source arena (treat as mutable).
    pub src_arena: RefCell<Arena<T, SrcNodeId>>,
    /// Destination arena (treat as immutable).
    pub dst_arena: RefCell<Arena<T, DstNodeId>>
}

impl<T: Clone + Debug + Eq + ToString + 'static> MultiMappingStore<T> {
    /// Create a new mapping store.
    pub fn new(src: Arena<T, SrcNodeId>, dst: Arena<T, DstNodeId>) -> MultiMappingStore<T> {
        MultiMappingStore { src: RefCell::new(HashMap::new()),
                            dst: RefCell::new(HashMap::new()),
                            src_arena: RefCell::new(src),
                            dst_arena: RefCell::new(dst) }
    }

    /// Push a new mapping into the store.
    pub fn push(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) {
        if !self.src.borrow().contains_key(&src) {
            self.src.borrow_mut().insert(src, HashSet::new());
        }
        self.src.borrow_mut().get_mut(&src).unwrap().insert(dst);
        if !self.dst.borrow().contains_key(&dst) {
            self.dst.borrow_mut().insert(dst, HashSet::new());
        }
        self.dst.borrow_mut().get_mut(&dst).unwrap().insert(src);
        info!("Mapping: {:?} {:} -> {:?} {:}",
              self.src_arena.borrow()[src].ty,
              self.src_arena.borrow()[src].label,
              self.dst_arena.borrow()[dst].ty,
              self.dst_arena.borrow()[dst].label);
    }

    /// Remove mapping from store.
    pub fn remove(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) {
        self.src.borrow_mut().get_mut(&src).unwrap().remove(&dst);
        if self.src.borrow().get(&src).unwrap().is_empty() {
            self.src.borrow_mut().remove(&src);
        }
        self.dst.borrow_mut().get_mut(&dst).unwrap().remove(&src);
        if self.dst.borrow().get(&dst).unwrap().is_empty() {
            self.dst.borrow_mut().remove(&dst);
        }
    }

    /// `true` if the store has a mapping from `src` to other nodes.
    pub fn contains_src(&self, src: NodeId<SrcNodeId>) -> bool {
        self.src.borrow().contains_key(&src)
    }

    /// `true` if the store has a mapping from a node to `dst`.
    pub fn contains_dst(&self, dst: NodeId<DstNodeId>) -> bool {
        self.dst.borrow().contains_key(&dst)
    }

    /// `true` if the store has a mapping from `src` to `dst`.
    pub fn contains_mapping(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) -> bool {
        self.src.borrow().contains_key(&src) && self.src.borrow().get(&src).unwrap().contains(&dst)
    }

    /// Return a set of all nodes mapped to `dst`.
    pub fn get_src(&self, src: NodeId<SrcNodeId>) -> Option<HashSet<NodeId<DstNodeId>>> {
        self.src.borrow().get(&src).cloned()
    }

    /// Return a set of all nodes mapped to `src`.
    pub fn get_dst(&self, dst: NodeId<DstNodeId>) -> Option<HashSet<NodeId<SrcNodeId>>> {
        self.dst.borrow().get(&dst).cloned()
    }

    /// Return a set of all mapped `src` nodes.
    pub fn get_srcs(&self) -> HashSet<NodeId<SrcNodeId>> {
        self.src.borrow().keys().cloned().collect()
    }

    /// Return a set of all mapped `dst` nodes.
    pub fn get_dsts(&self) -> HashSet<NodeId<DstNodeId>> {
        self.dst.borrow().keys().cloned().collect()
    }

    /// `true` if `src` is mapped to exactly one `dst` node, and vice versa.
    ///
    /// Returns `false` if `src` is not mapped at all.
    pub fn is_src_unique(&self, src: NodeId<SrcNodeId>) -> bool {
        if !self.contains_src(src) {
            return false;
        }
        let num_mapped = self.dst
                             .borrow()
                             .get(&self.src.borrow().get(&src).unwrap().iter().next().unwrap())
                             .unwrap()
                             .len();
        (*self.src.borrow().get(&src).unwrap()).len() == 1 && num_mapped == 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_common::{create_mult_arena, create_plus_arena};

    #[test]
    fn multi_map_get_src_get_dst() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MultiMappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
        let mut get_src_expected: HashSet<NodeId<SrcNodeId>> = HashSet::new();
        let mut get_dst_expected: HashSet<NodeId<DstNodeId>> = HashSet::new();
        assert_eq!(None, store.get_dst(NodeId::new(0)));
        assert_eq!(None, store.get_src(NodeId::new(0)));
        store.push(NodeId::new(0), NodeId::new(0));
        get_src_expected.insert(NodeId::new(0));
        get_dst_expected.insert(NodeId::new(0));
        assert_eq!(Some(&get_dst_expected),
                   store.get_src(NodeId::new(0)).as_ref());
        assert_eq!(Some(&get_src_expected),
                   store.get_dst(NodeId::new(0)).as_ref());
        store.push(NodeId::new(0), NodeId::new(1));
        get_dst_expected.insert(NodeId::new(1));
        assert_eq!(Some(&get_dst_expected),
                   store.get_src(NodeId::new(0)).as_ref());
        assert_eq!(Some(&get_src_expected),
                   store.get_dst(NodeId::new(0)).as_ref());
        store.push(NodeId::new(0), NodeId::new(2));
        get_dst_expected.insert(NodeId::new(2));
        assert_eq!(Some(&get_dst_expected),
                   store.get_src(NodeId::new(0)).as_ref());
        assert_eq!(Some(&get_src_expected),
                   store.get_dst(NodeId::new(0)).as_ref());
        store.push(NodeId::new(0), NodeId::new(2));
        assert_eq!(Some(&get_dst_expected),
                   store.get_src(NodeId::new(0)).as_ref());
        assert_eq!(Some(&get_src_expected),
                   store.get_dst(NodeId::new(0)).as_ref());
    }

    #[test]
    fn multi_get_srcs_get_dsts() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MultiMappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
        let mut srcs_expected: HashSet<NodeId<SrcNodeId>> = HashSet::new();
        let mut dsts_expected: HashSet<NodeId<DstNodeId>> = HashSet::new();
        store.push(NodeId::new(0), NodeId::new(0));
        srcs_expected.insert(NodeId::new(0));
        dsts_expected.insert(NodeId::new(0));
        assert_eq!(srcs_expected, store.get_srcs());
        assert_eq!(dsts_expected, store.get_dsts());
        store.push(NodeId::new(0), NodeId::new(1));
        dsts_expected.insert(NodeId::new(1));
        assert_eq!(srcs_expected, store.get_srcs());
        assert_eq!(dsts_expected, store.get_dsts());
        store.push(NodeId::new(0), NodeId::new(2));
        dsts_expected.insert(NodeId::new(2));
        assert_eq!(srcs_expected, store.get_srcs());
        assert_eq!(dsts_expected, store.get_dsts());
        store.push(NodeId::new(1), NodeId::new(0));
        srcs_expected.insert(NodeId::new(1));
        assert_eq!(srcs_expected, store.get_srcs());
        assert_eq!(dsts_expected, store.get_dsts());
        store.push(NodeId::new(0), NodeId::new(2));
        assert_eq!(srcs_expected, store.get_srcs());
        assert_eq!(dsts_expected, store.get_dsts());
    }

    #[test]
    fn multi_map_remove() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MultiMappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
        store.push(NodeId::new(0), NodeId::new(0));
        store.push(NodeId::new(0), NodeId::new(1));
        store.push(NodeId::new(0), NodeId::new(2));
        store.push(NodeId::new(1), NodeId::new(0));
        store.push(NodeId::new(0), NodeId::new(2));
        assert_eq!(2, store.get_srcs().len());
        assert_eq!(3, store.get_dsts().len());
        store.remove(NodeId::new(0), NodeId::new(0));
        assert_eq!(2, store.get_srcs().len());
        assert_eq!(3, store.get_dsts().len());
        store.remove(NodeId::new(0), NodeId::new(1));
        assert_eq!(2, store.get_srcs().len());
        assert_eq!(2, store.get_dsts().len());
        store.remove(NodeId::new(0), NodeId::new(2));
        assert_eq!(1, store.get_srcs().len());
        assert_eq!(1, store.get_dsts().len());
        store.remove(NodeId::new(1), NodeId::new(0));
        assert_eq!(0, store.get_srcs().len());
        assert_eq!(0, store.get_dsts().len());
    }

    #[test]
    fn multi_map_contains() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MultiMappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
        store.push(NodeId::new(0), NodeId::new(0));
        store.push(NodeId::new(0), NodeId::new(1));
        store.push(NodeId::new(0), NodeId::new(2));
        store.push(NodeId::new(1), NodeId::new(0));
        store.push(NodeId::new(0), NodeId::new(2));
        assert!(store.contains_src(NodeId::new(0)));
        assert!(store.contains_src(NodeId::new(1)));
        assert!(!store.contains_src(NodeId::new(2)));
        assert!(store.contains_dst(NodeId::new(0)));
        assert!(store.contains_dst(NodeId::new(1)));
        assert!(store.contains_dst(NodeId::new(2)));
        assert!(!store.contains_dst(NodeId::new(3)));
    }

    #[test]
    fn multi_map_contains_mapping() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MultiMappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
        store.push(NodeId::new(0), NodeId::new(0));
        store.push(NodeId::new(0), NodeId::new(1));
        store.push(NodeId::new(0), NodeId::new(2));
        store.push(NodeId::new(1), NodeId::new(0));
        store.push(NodeId::new(0), NodeId::new(2));
        assert!(store.contains_mapping(NodeId::new(0), NodeId::new(0)));
        assert!(store.contains_mapping(NodeId::new(0), NodeId::new(1)));
        assert!(store.contains_mapping(NodeId::new(0), NodeId::new(2)));
        assert!(store.contains_mapping(NodeId::new(1), NodeId::new(0)));
        assert!(store.contains_mapping(NodeId::new(0), NodeId::new(2)));
        assert!(!store.contains_mapping(NodeId::new(2), NodeId::new(0)));
        assert!(!store.contains_mapping(NodeId::new(1), NodeId::new(1)));
        assert!(!store.contains_mapping(NodeId::new(2), NodeId::new(2)));
        assert!(!store.contains_mapping(NodeId::new(1), NodeId::new(2)));
        assert!(!store.contains_mapping(NodeId::new(2), NodeId::new(1)));
    }

    #[test]
    fn multi_map_is_src_unique() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MultiMappingStore::new(plus, Arena::<String, DstNodeId>::from(mult));
        store.push(NodeId::new(0), NodeId::new(0));
        store.push(NodeId::new(0), NodeId::new(1));
        store.push(NodeId::new(0), NodeId::new(2));
        store.push(NodeId::new(1), NodeId::new(3));
        store.push(NodeId::new(0), NodeId::new(2));
        assert!(!store.is_src_unique(NodeId::new(0)));
        assert!(store.is_src_unique(NodeId::new(1)));
        assert!(!store.is_src_unique(NodeId::new(2)));
    }
}
