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

use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use action::{ApplyAction, Delete, EditScript, Insert, Move, Update};
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
#[derive(Debug)]
pub struct TemporaryMappingStore {
    /// Mappings from the source tree to the destination.
    ///
    /// Should contain the same information as `to_map`.
    pub from: HashMap<NodeId, NodeId>,
    /// Mappings from the destination tree to the source.
    ///
    /// Should contain the same information as `from_map`.
    pub to: HashMap<NodeId, NodeId>,
}

impl TemporaryMappingStore {
    /// Create a new mapping store.
    pub fn new() -> TemporaryMappingStore {
        Default::default()
    }

    /// Push a new mapping into the store.
    pub fn push(&mut self, from: NodeId, to: NodeId) {
        self.from.insert(from, to);
        self.to.insert(to, from);
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
        self.to.get(to).and_then(|x| Some(*x))
    }

    /// Get the `NodeId` that `from` is mapped to.
    pub fn get_to(&self, from: &NodeId) -> Option<NodeId> {
        self.from.get(from).and_then(|x| Some(*x))
    }
}

impl Default for TemporaryMappingStore {
    fn default() -> TemporaryMappingStore {
        TemporaryMappingStore {
            from: HashMap::new(),
            to: HashMap::new(),
        }
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
    pub fn push(&mut self, from: NodeId, to: NodeId, ty: MappingType) {
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
           self.from_arena[from].label == self.to_arena[to].label {
            return true;
        }
        // Case 2: one node is a leaf and the other is a branch.
        if from.is_leaf(&self.from_arena) && !to.is_leaf(&self.to_arena) ||
           !from.is_leaf(&self.from_arena) && to.is_leaf(&self.to_arena) {
            return false;
        }
        // Case 3: both nodes are branches.
        if self.from_arena[from].label != self.to_arena[to].label ||
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
        let dice = 2.0 * self.num_common_descendants(from, to) as f64 / (n_from + n_to);
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
        let common = self.num_common_descendants(from, to) as f64;
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
        let common = self.num_common_descendants(from, to) as f64;
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

    /// Given a mapping store, generate an edit script.
    ///
    /// This function implements the optimal algorithm of Chawathe et al. (1996).
    /// Variable names as in Figures 8 and 9 of the paper.
    pub fn generate_edit_script(&mut self, root: NodeId) -> EditScriptResult<T> {
        let mut script: EditScript<T> = EditScript::new();
        let mut from_in_order: HashSet<NodeId> = HashSet::new();
        let mut to_in_order: HashSet<NodeId> = HashSet::new();
        let mut new_mappings = TemporaryMappingStore::new();
        // Copy current mappings over to new_mappings.
        for (key, value) in &self.from {
            new_mappings.push(*key, value.0);
        }
        // Combined update, insert, align and move phases.
        let tmp_to_arena = self.to_arena.clone();
        for x in root.breadth_first_traversal(&tmp_to_arena) {
            let mut w = root; // Overwritten later.
            // Insertion phase.
            if !new_mappings.to.contains_key(&x) && x.is_root(&self.to_arena) {
                let k = self.find_pos(x, &new_mappings, &to_in_order);
                debug!("Edit script: INS {} {:?} No parent",
                       self.to_arena[x].label,
                       self.to_arena[x].value);
                w = self.from_arena.new_node(self.to_arena[x].value.clone(),
                                             self.to_arena[x].label.clone(),
                                             self.to_arena[x].indent,
                                             self.to_arena[x].col_no,
                                             self.to_arena[x].line_no,
                                             self.to_arena[x].char_no,
                                             self.to_arena[x].token_len);
                new_mappings.push(w, x);
                let mut ins = Insert::new(w, None, k);
                ins.apply(&mut self.from_arena)?;
                script.push(ins);
            } else if !new_mappings.to.contains_key(&x) {
                let y = self.to_arena[x].parent().unwrap();
                let z = new_mappings.get_from(&y).unwrap();
                let k = self.find_pos(x, &new_mappings, &to_in_order);
                debug!("Edit script: INS {} {:?} Parent: {} {:?}",
                       self.to_arena[x].label,
                       self.to_arena[x].value,
                       self.from_arena[z].label,
                       self.from_arena[z].value);
                w = self.from_arena.new_node(self.to_arena[x].value.clone(),
                                             self.to_arena[x].label.clone(),
                                             self.to_arena[x].indent,
                                             self.to_arena[x].col_no,
                                             self.to_arena[x].line_no,
                                             self.to_arena[x].char_no,
                                             self.to_arena[x].token_len);
                new_mappings.push(w, x);
                let mut ins = Insert::new(w, Some(z), k);
                ins.apply(&mut self.from_arena)?;
                script.push(ins);
            } else if !x.is_root(&self.to_arena) {
                // Insertion and update phases.
                w = new_mappings.get_from(&x).unwrap();
                let v = self.from_arena[w].parent().unwrap();
                if self.from_arena[w].value != self.to_arena[x].value {
                    debug!("Edit script: UPD {} {:?} -> {} {:?}",
                           self.from_arena[w].label,
                           self.from_arena[w].value,
                           self.to_arena[x].label,
                           self.to_arena[x].value);
                    let mut upd = Update::new(w,
                                              self.to_arena[x].value.clone(),
                                              self.to_arena[x].label.clone());
                    upd.apply(&mut self.from_arena)?;
                    script.push(upd.clone());
                }
                // MOVE phase.
                let y = self.to_arena[x].parent().unwrap();
                let z = new_mappings.get_from(&y).unwrap();
                if z != v {
                    let k = self.find_pos(x, &new_mappings, &to_in_order);
                    let mut mov = Move::new(w, z, k);
                    debug!("Edit script: MOV {} {:?} Parent: {} {:?}",
                           self.from_arena[w].label,
                           self.from_arena[w].value,
                           self.from_arena[z].label,
                           self.from_arena[z].value);
                    mov.apply(&mut self.from_arena)?;
                    script.push(mov);
                }
            }
            self.align_children(w,
                                x,
                                &mut script,
                                &new_mappings,
                                &mut from_in_order,
                                &mut to_in_order)?;
        }
        // Delete phase.
        let mut actions = EditScript::new();
        for w in root.post_order_traversal(&self.from_arena) {
            if !new_mappings.contains_from(&w) {
                debug!("Edit script: DEL {} {:?}",
                       self.from_arena[w].label,
                       self.from_arena[w].value);
                let del = Delete::new(w);
                script.push(del);
                actions.push(del);
            }
        }
        actions.apply(&mut self.from_arena)?;
        // Add new mappings to the existing store.
        debug!("Append new mappings to store.");
        for (from, to) in new_mappings.from {
            if !self.contains_from(&from) {
                debug!("New mapping due to edit script: {} -> {}", from, to);
                self.push(from, to, MappingType::EDIT);
            }
        }
        Ok(script)
    }

    fn align_children(&mut self,
                      w: NodeId,
                      x: NodeId,
                      script: &mut EditScript<T>,
                      new_mappings: &TemporaryMappingStore,
                      from_in_order: &mut HashSet<NodeId>,
                      to_in_order: &mut HashSet<NodeId>)
                      -> Result<(), ArenaError> {
        debug!("align_children({}, {})", w, x);
        for child in x.children(&self.to_arena) {
            to_in_order.remove(&child);
        }
        let mut s1: Vec<NodeId> = vec![];
        for child in w.children(&self.from_arena) {
            from_in_order.remove(&child);
            if new_mappings.contains_from(&child) {
                let mapped = new_mappings.get_to(&child).unwrap();
                if x.children(&self.to_arena).any(|n| n == mapped) {
                    s1.push(child);
                }
            }
        }
        let s2: Vec<NodeId> = vec![];
        for child in x.children(&self.to_arena) {
            if new_mappings.contains_to(&child) {
                let mapped = new_mappings.get_from(&child).unwrap();
                if w.children(&self.from_arena).any(|n| n == mapped) {
                    s1.push(child);
                }
            }
        }
        let lcs = self.lcss(&s1, &s2);
        for &(from, to) in &lcs {
            from_in_order.insert(from);
            to_in_order.insert(to);
        }
        for a in &s1 {
            for b in &s2 {
                if new_mappings.contains_from(a) && new_mappings.get_to(a).unwrap() == *b &&
                   !lcs.contains(&(*a, *b)) {
                    let k = self.find_pos(*b, new_mappings, to_in_order);
                    let mut mov = Move::new(*a, w, k);
                    debug!("Edit script: MOV {} {:?} Parent: {} {:?}",
                           self.from_arena[*a].label,
                           self.from_arena[*a].value,
                           self.to_arena[w].label,
                           self.to_arena[w].value);
                    script.push(mov);
                    mov.apply(&mut self.from_arena)?;
                    from_in_order.insert(*a);
                    to_in_order.insert(*b);
                }
            }
        }
        Ok(())
    }

    /// Find the position of node x in to_arena.
    fn find_pos(&self,
                x: NodeId,
                new_mappings: &TemporaryMappingStore,
                to_in_order: &HashSet<NodeId>)
                -> u16 {
        debug!("find_pos({})", x);
        if x.is_root(&self.to_arena) {
            return 0;
        }
        let y = self.to_arena[x].parent().unwrap();
        let siblings = y.children(&self.to_arena).collect::<Vec<NodeId>>();
        for child in &siblings {
            if to_in_order.contains(child) {
                if x == *child {
                    return 0;
                } else {
                    break;
                }
            }
        }
        let x_pos = x.get_child_position(&self.to_arena).unwrap();
        let mut v: Option<NodeId> = None;
        for (i, child) in x.children(&self.to_arena).enumerate() {
            if to_in_order.contains(&child) {
                v = Some(child);
            }
            if i >= x_pos {
                break;
            }
        }
        if v.is_none() {
            return 0; // No right-most sibling in order.
        }
        let u = new_mappings.get_from(&v.unwrap()).unwrap();
        let u_pos = u.get_child_position(&self.to_arena).unwrap();
        u_pos as u16 + 1
    }

    fn lcss(&self, seq1: &[NodeId], seq2: &[NodeId]) -> Vec<(NodeId, NodeId)> {
        let mut lcss: Vec<(NodeId, NodeId)> = vec![];
        if seq1.is_empty() || seq2.is_empty() {
            return lcss;
        }
        let mut grid = vec![];
        for _ in 0..seq1.len() + 1 {
            grid.push(vec![0; seq2.len() + 1]);
        }
        debug_assert_eq!(seq1.len() + 1, grid.len());
        debug_assert_eq!(seq2.len() + 1, grid[0].len());
        for (i, n1) in seq1.iter().enumerate() {
            for (j, n2) in seq2.iter().enumerate() {
                if self.contains_from(n1) && self.get_to(n1).unwrap() == *n2 {
                    grid[i + 1][j + 1] = 1 + grid[i][j];
                } else {
                    grid[i + 1][j + 1] = max(grid[i + 1][j], grid[i][j + 1]);
                }
            }
        }
        let mut i = seq1.len();
        let mut j = seq2.len();
        while i != 0 && j != 0 {
            if grid[i][j] == grid[i - 1][j] {
                i -= 1;
            } else if grid[i][j] == grid[i][j - 1] {
                j -= 1;
            } else {
                lcss.push((seq1[i - 1], seq2[j - 1]));
                i -= 1;
                j -= 1;
            }
        }
        lcss.reverse();
        lcss
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
                                  0,
                                  None,
                                  None,
                                  None,
                                  None);
        let n1 = arena.new_node(String::from("1"),
                                String::from("INT"),
                                2,
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("*"),
                                String::from("Expr"),
                                2,
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("3"),
                                String::from("INT"),
                                4,
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node(String::from("4"),
                                String::from("INT"),
                                4,
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(n2, &mut arena).unwrap();
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
        let root = arena.new_node(String::from("+"),
                                  String::from("Expr"),
                                  0,
                                  None,
                                  None,
                                  None,
                                  None);
        let n1 = arena.new_node(String::from("3"),
                                String::from("INT"),
                                4,
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("4"),
                                String::from("INT"),
                                4,
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
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

    #[test]
    fn num_common_descendants() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let mut store = MappingStore::new(plus, mult);
        store.push(NodeId::new(0), NodeId::new(2), Default::default());
        store.push(NodeId::new(1), NodeId::new(3), Default::default());
        store.push(NodeId::new(2), NodeId::new(4), Default::default());
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
