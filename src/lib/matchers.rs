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

use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use action::EditScript;
use ast::{Arena, ArenaError, NodeId};
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
    EDIT,
}

impl Default for MappingType {
    fn default() -> MappingType {
        MappingType::ANCHOR
    }
}

/// A store of mappings between nodes in different arenas.
/// Direction is important.
pub struct MappingStore<T: Clone + Debug> {
    /// Mappings from the source tree to the destination.
    ///
    /// Should contain the same information as `to_map`.
    pub from: HashMap<NodeId, (NodeId, MappingType)>,
    /// Mappings from the destination tree to the source.
    ///
    /// Should contain the same information as `from_map`.
    pub to: HashMap<NodeId, (NodeId, MappingType)>,

    /// Source arena (treat as immutable).
    pub from_arena: Arena<T>,
    /// Destination arena (treat as immutable).
    pub to_arena: Arena<T>,
}

impl<T: Clone + Debug> RenderJson for MappingStore<T> {
    fn render_json(&self, indent: usize) -> String {
        let ind_2 = " ".repeat(indent * 2);
        let ind_3 = " ".repeat(indent * 3);
        let mut json = vec![];
        for (key, value) in &self.from {
            json.push(format!("{}{{\n{}\"src\": {},\n{}\"dest\": {}\n{}}}",
                              ind_2,
                              ind_3,
                              key,
                              ind_3,
                              value.0,
                              ind_2));
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

impl<T: Clone + Debug + Eq + 'static> MappingStore<T> {
    /// Create a new mapping store.
    pub fn new(base: Arena<T>, diff: Arena<T>) -> MappingStore<T> {
        MappingStore {
            from: HashMap::new(),
            to: HashMap::new(),
            from_arena: base,
            to_arena: diff,
        }
    }

    /// Push a new mapping into the store.
    pub fn push(&mut self, from: NodeId, to: NodeId, ty: &MappingType) {
        self.from.insert(from, (to, ty.clone()));
        self.to.insert(to, (from, ty.clone()));
    }

    /// Remove mapping from store.
    pub fn remove(&mut self, from: &NodeId, to: &NodeId) {
        self.from.remove(from);
        self.to.remove(to);
    }

    /// `true` if the store has a mapping from `from` to another node.
    pub fn contains_from(&self, from: &NodeId) -> bool {
        self.from.contains_key(from)
    }

    /// `true` if the store has a mapping from a node to `to`.
    pub fn contains_to(&self, to: &NodeId) -> bool {
        self.to.contains_key(to)
    }

    /// Get the `NodeId` that `to` is mapped from.
    pub fn get_from(&self, to: &NodeId) -> Option<NodeId> {
        self.to.get(to).and_then(|x| Some(x.0))
    }

    /// Get the `NodeId` that `from` is mapped to.
    pub fn get_to(&self, from: &NodeId) -> Option<NodeId> {
        self.from.get(from).and_then(|x| Some(x.0))
    }

    /// Test whether `from` is mapped to `to` in this store.
    pub fn is_mapped(&self, from: &NodeId, to: &NodeId) -> bool {
        if !self.contains_from(from) {
            return false;
        }
        self.get_to(from).map_or(false, |x| x == *to)
    }

    /// Test whether `from` is mapped to `to` by the matcher.
    /// As opposed to the edit script generator.
    pub fn is_mapped_by_matcher(&self, from: &NodeId, to: &NodeId) -> bool {
        if !self.contains_from(from) {
            return false;
        }
        match self.from.get(from) {
            None => false,
            Some(&(val, ref ty)) => val == *to && *ty != MappingType::EDIT
        }
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
        if from.is_leaf(&self.from_arena) && to.is_leaf(&self.to_arena) &&
           self.from_arena[from].label == self.to_arena[to].label &&
           self.from_arena[from].value == self.to_arena[to].value {
            return true;
        }
        // Case 2: one node is a leaf and the other is a branch.
        if from.is_leaf(&self.from_arena) && !to.is_leaf(&self.to_arena) ||
           !from.is_leaf(&self.from_arena) && to.is_leaf(&self.to_arena) {
            return false;
        }
        // Case 3: both nodes are branches.
        if self.from_arena[from].label != self.to_arena[to].label ||
           self.from_arena[from].value != self.to_arena[to].value ||
           from.height(&self.from_arena) != to.height(&self.to_arena) {
            return false;
        }
        let f_children = from.children(&self.from_arena).collect::<Vec<NodeId>>();
        let t_children = to.children(&self.to_arena).collect::<Vec<NodeId>>();
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
        self.from_arena[*from].label == self.to_arena[*to].label &&
        !(self.contains_from(from) || self.contains_to(to))
    }

    /// Dice measure of similarity between subtrees.
    pub fn dice_sim(&self, from: &NodeId, to: &NodeId) -> f64 {
        let n_from = from.breadth_first_traversal(&self.from_arena)
            .collect::<Vec<NodeId>>()
            .len() as f64;
        let n_to = to.breadth_first_traversal(&self.to_arena)
            .collect::<Vec<NodeId>>()
            .len() as f64;
        let dice = 2.0 * f64::from(self.num_common_descendants(from, to)) / (n_from + n_to);
        debug_assert!(dice >= 0. && dice <= 1.);
        dice
    }

    /// Jaccard measure of similarity between subtrees.
    pub fn jaccard_sim(&self, from: &NodeId, to: &NodeId) -> f64 {
        let n_from = from.breadth_first_traversal(&self.from_arena)
            .collect::<Vec<NodeId>>()
            .len() as f64;
        let n_to = to.breadth_first_traversal(&self.to_arena)
            .collect::<Vec<NodeId>>()
            .len() as f64;
        let common = f64::from(self.num_common_descendants(from, to));
        let jaccard = common / (n_from + n_to - common);
        debug_assert!(jaccard >= 0. && jaccard <= 1.);
        jaccard
    }

    /// Measure of similarity between subtrees Described in Chawathe et al. (1996).
    pub fn chawathe_sim(&self, from: &NodeId, to: &NodeId) -> f64 {
        let n_from = from.breadth_first_traversal(&self.from_arena)
            .collect::<Vec<NodeId>>()
            .len() as f64;
        let n_to = to.breadth_first_traversal(&self.to_arena)
            .collect::<Vec<NodeId>>()
            .len() as f64;
        let common = f64::from(self.num_common_descendants(from, to));
        let chawathe = common / n_from.max(n_to);
        debug_assert!(chawathe >= 0. && chawathe <= 1.);
        chawathe
    }

    /// Find the number of "common" descendants in two matched subtrees.
    ///
    /// To nodes are common if they have already been matched.
    fn num_common_descendants(&self, from: &NodeId, to: &NodeId) -> u32 {
        let mut dst_desc = HashSet::new();
        for node in to.breadth_first_traversal(&self.to_arena) {
            dst_desc.insert(node);
        }
        let mut common = 0;
        let mut to: Option<NodeId>;
        for node in from.descendants(&self.from_arena) {
            to = self.get_to(&node);
            if to.is_some() && dst_desc.contains(&to.unwrap()) {
                common += 1;
            }
        }
        common
    }
}

/// Match two trees and return a store of mappings between them.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait MatchTrees<T: Clone + Debug> {
    /// Match two trees and return a store of mappings between them.
    fn match_trees(&self, base: Arena<T>, diff: Arena<T>) -> MappingStore<T>;

    /// Describe the matcher for the user.
    ///
    /// This is the string that is printed when the user passes in the --list
    /// CLI option.
    fn describe(&self) -> String;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_mult_arena() -> Arena<String> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("+"),
                                  String::from("Expr"),
                                  None,
                                  None,
                                  None,
                                  None);
        let n1 = arena.new_node(String::from("1"),
                                String::from("INT"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("*"),
                                String::from("Expr"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("3"),
                                String::from("INT"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node(String::from("4"),
                                String::from("INT"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(n2, &mut arena).unwrap();
        let format1 = "Expr \"+\"
  INT \"1\"
  Expr \"*\"
    INT \"3\"
    INT \"4\"
";
        assert_eq!(format1, format!("{:?}", arena));
        arena
    }

    fn create_plus_arena() -> Arena<String> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("+"),
                                  String::from("Expr"),
                                  None,
                                  None,
                                  None,
                                  None);
        let n1 = arena.new_node(String::from("3"),
                                String::from("INT"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("4"),
                                String::from("INT"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let format1 = "Expr \"+\"
  INT \"3\"
  INT \"4\"
";
        assert_eq!(format1, format!("{:?}", arena));
        arena
    }

    #[test]
    fn is_isomorphic() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store_p = MappingStore::new(plus.clone(), plus.clone());
        let store_m = MappingStore::new(mult.clone(), mult.clone());
        let store = MappingStore::new(plus, mult);
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
        store.push(NodeId::new(0), NodeId::new(0), &MappingType::ANCHOR);
        store.push(NodeId::new(2), NodeId::new(4), &MappingType::ANCHOR);
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(2)));
        assert!(store.is_mapping_allowed(&NodeId::new(1), &NodeId::new(3)));
        assert!(!store.is_mapping_allowed(&NodeId::new(2), &NodeId::new(4)));
        assert!(!store.is_mapping_allowed(&NodeId::new(0), &NodeId::new(0)));
    }

    #[test]
    fn is_mapped() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let mut store = MappingStore::new(plus, mult);
        store.push(NodeId::new(0), NodeId::new(0), &MappingType::ANCHOR);
        store.push(NodeId::new(2), NodeId::new(4), &MappingType::ANCHOR);
        assert!(store.is_mapped(&NodeId::new(0), &NodeId::new(0)));
        assert!(store.is_mapped(&NodeId::new(2), &NodeId::new(4)));
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
    fn is_mapped_by_matcher() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let mut store = MappingStore::new(plus, mult);
        store.push(NodeId::new(0), NodeId::new(0), &MappingType::CONTAINER);
        store.push(NodeId::new(2), NodeId::new(4), &MappingType::EDIT);
        assert!(store.is_mapped_by_matcher(&NodeId::new(0), &NodeId::new(0)));
        // Not mapped.
        assert!(!store.is_mapped_by_matcher(&NodeId::new(0), &NodeId::new(1)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(0), &NodeId::new(2)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(0), &NodeId::new(3)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(0), &NodeId::new(4)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(1), &NodeId::new(1)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(1), &NodeId::new(2)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(1), &NodeId::new(3)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(1), &NodeId::new(4)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(2), &NodeId::new(1)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(2), &NodeId::new(2)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(2), &NodeId::new(3)));
        assert!(!store.is_mapped_by_matcher(&NodeId::new(2), &NodeId::new(4)));
    }

    #[test]
    fn num_common_descendants() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let mut store = MappingStore::new(plus, mult);
        store.push(NodeId::new(0), NodeId::new(2), &Default::default());
        store.push(NodeId::new(1), NodeId::new(3), &Default::default());
        store.push(NodeId::new(2), NodeId::new(4), &Default::default());
        assert_eq!(2,
                   store.num_common_descendants(&NodeId::new(0), &NodeId::new(2)));
        assert_eq!(2,
                   store.num_common_descendants(&NodeId::new(0), &NodeId::new(0)));
        assert_eq!(0,
                   store.num_common_descendants(&NodeId::new(1), &NodeId::new(0)));
    }

    #[test]
    fn render_json() {
        use myers_matcher::MyersConfig;
        let plus = create_plus_arena();
        let mult = create_mult_arena();
        let matcher = MyersConfig::new();
        let store = matcher.match_trees(plus, mult);
        let expected_str = vec!["\"matches\": [",
                                "{\n\"src\": 1,\n\"dest\": 3\n}",
                                "{\n\"src\": 0,\n\"dest\": 0\n}",
                                "{\n\"src\": 2,\n\"dest\": 4\n}"];
        let expected = expected_str
            .iter()
            .map(|s| String::from(*s))
            .collect::<Vec<String>>();
        let got = store.render_json(0);
        for item in expected {
            assert!(got.contains(&item));
        }
    }
}
