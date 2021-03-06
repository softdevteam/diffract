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
use std::collections::HashSet;
use std::fmt::Debug;

use action::{ApplyAction, Delete, EditScript, Insert, Move, Update};
use ast::{ArenaError, DstNodeId, NodeId, SrcNodeId};
use matchers::{EditScriptResult, MappingStore, MappingType};

/// Label to use when introducing temporary root nodes in an AST.
///
/// Temporary root nodes should be removed before the edit script generator
/// completes. This constant is exposed for use in integration tests.
pub const TMP_ROOT: &str = "_____DIFFRACT_TMP_ROOT";

/// Given a matching between two ASTs, generate a complete edit script.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait EditScriptGenerator<T: Clone + Debug + Eq + ToString + 'static> {
    /// Given a matching between two ASTs, generate a complete edit script.
    fn generate_script(&self, store: &MappingStore<T>) -> EditScriptResult<T>;
}

/// The Chawathe et al. (1996) algorithm does not require any configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct Chawathe98Config {}

impl Default for Chawathe98Config {
    fn default() -> Chawathe98Config {
        Chawathe98Config {}
    }
}
impl Chawathe98Config {
    /// Create a new configuration object, with default values.
    pub fn new() -> Chawathe98Config {
        Default::default()
    }
}

impl<T: Clone + Debug + Eq + ToString + 'static> EditScriptGenerator<T> for Chawathe98Config {
    /// This function should generate the edit script for GLUE and COPY
    fn generate_script(&self, _store: &MappingStore<T>) -> EditScriptResult<T> {
        Ok(EditScript::new())
    }
}

/// The Chawathe et al. (1996) algorithm does not require any configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct Chawathe96Config {}

impl Default for Chawathe96Config {
    fn default() -> Chawathe96Config {
        Chawathe96Config {}
    }
}

impl Chawathe96Config {
    /// Create a new configuration object, with default values.
    pub fn new() -> Chawathe96Config {
        Default::default()
    }

    /// Align children of w (From AST) and x (To AST) which are out of order.
    fn align_children<T: Clone + Debug + Eq + ToString + 'static>(
        &self,
        store: &MappingStore<T>,
        w: NodeId<SrcNodeId>,
        x: NodeId<DstNodeId>,
        script: &mut EditScript<T>,
        src_in_order: &mut HashSet<NodeId<SrcNodeId>>,
        dst_in_order: &mut HashSet<NodeId<DstNodeId>>)
        -> Result<(EditScript<T>), ArenaError> {
        let mut actions = EditScript::new();
        // Variable key:
        // a, w in store.src_arena (T_1 in paper).
        // b, x in store.dst_arena (T_2 in paper).
        debug!("align_children({}, {})", w, x);
        // If neither node has any children there is nothing to align. This
        // optimisation does not appear in the paper.
        if w.is_leaf(&store.src_arena.borrow()) || x.is_leaf(&store.dst_arena.borrow()) {
            debug!("align_children: {:?} or {:?} is a leaf node.",
                   store.src_arena.borrow()[w],
                   store.dst_arena.borrow()[x]);
            return Ok(actions);
        }
        // 1. Mark all children of w and all children of x "out of order".
        for child in w.children(&store.src_arena.borrow()) {
            src_in_order.remove(&child);
        }
        for child in x.children(&store.dst_arena.borrow()) {
            dst_in_order.remove(&child);
        }
        // 2. Let s1 be the sequence of children of w whose partners are
        // children of x and let s2 be the sequence of children of x whose
        // partners are children of w.
        let mut s1: Vec<NodeId<SrcNodeId>> = vec![];
        for child in w.children(&store.src_arena.borrow()) {
            if store.contains_src(child) {
                let mapped = store.get_dst(child).unwrap();
                if x.children(&store.dst_arena.borrow()).any(|c| c == mapped) {
                    s1.push(child);
                }
            }
        }
        let mut s2: Vec<NodeId<DstNodeId>> = vec![];
        for child in x.children(&store.dst_arena.borrow()) {
            if store.contains_dst(child) {
                let mapped = store.get_src(child).unwrap();
                if w.children(&store.src_arena.borrow()).any(|c| c == mapped) {
                    s2.push(child);
                }
            }
        }
        // 3. Let equal(a, b) iff (a, b) in store.
        // 4. Let lcs be lcss(s1, s2, equal).
        let lcs = self.lcss(store, &s1, &s2);
        // 5. For each (a, b) in lcs mark nodes a and b "in order".
        for &(a, b) in &lcs {
            src_in_order.insert(a);
            dst_in_order.insert(b);
        }
        // 6. For each a in s1, b in s2 such that (a, b) in store (M') but
        // (a, b) not in lcs: (i) let k = find_pos(b); (ii) append MOV(a, w, k)
        // to the script and apply MOV(a, w, k) to T_1; (iii) Mark a, b as "in
        // order". Note, the paper says M (for the store), but should say M'.
        for a in &s1 {
            for b in &s2 {
                if store.is_mapped(*a, *b) && !lcs.contains(&(*a, *b)) {
                    let k = self.find_pos(store, *b, src_in_order, dst_in_order);
                    let mut mov = Move::new(*a, w, k);
                    debug!("Edit script align_children: MOV {:?} {} Parent: {:?} {} Child: {}",
                           store.src_arena.borrow()[*a].ty,
                           store.src_arena.borrow()[*a].label,
                           store.src_arena.borrow()[w].ty.clone(),
                           store.src_arena.borrow()[w].label.clone(),
                           k);
                    script.push(mov.clone());
                    actions.push(mov);
                    src_in_order.insert(*a);
                    dst_in_order.insert(*b);
                }
            }
        }
        Ok(actions)
    }

    /// Find the position of node x in dst_arena.
    fn find_pos<T: Clone + Debug + Eq + ToString + 'static>(&self,
                                                 store: &MappingStore<T>,
                                                 x: NodeId<DstNodeId>,
                                                 src_in_order: &HashSet<NodeId<SrcNodeId>>,
                                                 dst_in_order: &HashSet<NodeId<DstNodeId>>)
-> u16{
        // 1. Let y=p(x) in T_2 and let w be the partner of x in T_1.
        // N.B. as the algorithm is presented in the paper, w is unused in the
        // remainder of this method. Likely this is just a typo, and so we
        // don't assign a variable w in this implementation.
        let y = store.dst_arena.borrow()[x].parent().unwrap();
        // 2. If x is the leftmost child of y that is marked "in order" return
        // 0 (the paper says 1 but we count child nodes from 0).
        let siblings =
            y.children(&store.dst_arena.borrow()).collect::<Vec<NodeId<DstNodeId>>>();
        for child in &siblings {
            if dst_in_order.contains(child) {
                if x == *child {
                    debug!("find_pos({}) <- 0. Node in order.", x);
                    return 0;
                } else {
                    break;
                }
            }
        }
        let x_pos = x.get_child_position(&store.dst_arena.borrow()).unwrap();
        // 3. Find v in T_2, where v is the rightmost sibling of x that is to
        // the left of x and is marked "in order".
        let v = siblings.iter()
                        .take(x_pos)
                        .filter(|c| dst_in_order.contains(c))
                        .last();
        // This check is in GumTree and ChangeDistiller, but not in the paper.
        if v.is_none() {
            debug!("find_pos({}) <- 0. No right-most sibling in order.", x);
            return 0;
        }
        // 4. Let u be the partner of v in T_1.
        let u_opt = store.get_src(*v.unwrap());
        assert!(u_opt.is_some(), "find_pos() could not find a partner for v");
        let u = u_opt.unwrap();
        // 5. Suppose u is the i'th child of its parent that is marked
        // "in order". Return i + 1.
        let u_p = store.src_arena.borrow()[u].parent().unwrap();
        let u_pos = u.get_child_position(&store.src_arena.borrow()).unwrap();
        let position = u_p.children(&store.src_arena.borrow())
                          .take(u_pos)
                          .filter(|c| src_in_order.contains(c))
                          .count() as u16 + 1;
        debug!("find_pos({}) <- {}", x, position);
        position
    }

    fn lcss<T: Clone + Debug + Eq + ToString + 'static>(
        &self,
        store: &MappingStore<T>,
        seq1: &[NodeId<SrcNodeId>],
        seq2: &[NodeId<DstNodeId>])
        -> Vec<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> {
        let mut lcss: Vec<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = vec![];
        if seq1.is_empty() || seq2.is_empty() {
            return lcss;
        }
        let mut grid = Vec::with_capacity(seq1.len() + 1);
        for _ in 0..seq1.len() + 1 {
            grid.push(vec![0; seq2.len() + 1]);
        }
        debug_assert_eq!(seq1.len() + 1,
                         grid.len(),
                         "Cost matrix not sized correctly.");
        debug_assert_eq!(seq2.len() + 1,
                         grid[0].len(),
                         "Cost matrix not sized correctly.");
        for (i, n1) in seq1.iter().enumerate() {
            for (j, n2) in seq2.iter().enumerate() {
                if store.is_mapped(*n1, *n2) {
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

impl<T: Clone + Debug + Default + Eq + ToString + 'static> EditScriptGenerator<T>
    for Chawathe96Config
{
    /// This function implements the optimal algorithm of Chawathe et al. (1996).
    /// Variable names as in Figures 8 and 9 of the paper.
    /// Will panic if either `Arena` in `store` is empty (and has no root node).
    fn generate_script(&self, store: &MappingStore<T>) -> EditScriptResult<T> {
        // 1. E <- e, M' <- M.
        let mut script: EditScript<T> = EditScript::new();
        let mut src_in_order: HashSet<NodeId<SrcNodeId>> = HashSet::new();
        let mut dst_in_order: HashSet<NodeId<DstNodeId>> = HashSet::new();
        if store.src_arena.borrow().is_empty() && store.dst_arena.borrow().is_empty() {
            return Ok(script);
        }
        // Combined update, insert, align and move phases.
        // 2. Visit the nodes of T_2 in breadth-first order.
        assert!(store.src_arena.borrow().root().is_some(),
                "'src' AST has no root node.");
        assert!(store.dst_arena.borrow().root().is_some(),
                "'dst' AST has no root node.");
        // Check for unmatched root nodes. Chawathe et al. (1996) says: If the
        // roots of T_1 and T_2 are not matched in M, then we add new (dummy)
        // root nodes x to T_1 and y to T_2, and add (x; y)to M. The old root of
        // T_1 is made the lone child of x and the old root of T_2 is made the
        // lone child of y. Hereafter we assume without loss of generality that
        // the roots of T_1 and T_2 are matched in M.
        let mut fake_roots = false;
        if !store.is_mapped(store.src_arena.borrow().root().unwrap(),
                            store.dst_arena.borrow().root().unwrap())
        {
            fake_roots = true;
            assert!(!store.src_arena
                          .borrow()
                          .contains_type_and_label(&Default::default(), TMP_ROOT),
                    "'src' AST already contains a node like TMP_ROOT.");
            let new_src_root = store.src_arena.borrow_mut().new_node(Default::default(),
                                                                     String::from(TMP_ROOT),
                                                                     None,
                                                                     None,
                                                                     None,
                                                                     None);
            store.src_arena.borrow_mut().new_root(new_src_root)?;
            assert!(!store.dst_arena
                          .borrow()
                          .contains_type_and_label(&Default::default(), TMP_ROOT),
                    "'dst' AST already contains a node like TMP_ROOT.");
            let new_dst_root = store.dst_arena.borrow_mut().new_node(Default::default(),
                                                                     String::from(TMP_ROOT),
                                                                     None,
                                                                     None,
                                                                     None,
                                                                     None);
            store.dst_arena.borrow_mut().new_root(new_dst_root)?;
            store.push(new_src_root, new_dst_root, &MappingType::EDIT);
        }
        let root_src = store.src_arena.borrow().root().unwrap();
        let root_dst = store.dst_arena.borrow().root().unwrap();
        // (a) Let x be the current node in the breadth-first search of T_2
        // and let y be the parent of x. Let z be the partner of y in M'.
        for x in root_dst.breadth_first_traversal(&store.dst_arena.borrow()) {
            let mut w: Option<NodeId<SrcNodeId>> = None;
            // Insertion phase.
            if !store.contains_dst(x) {
                let y = store.dst_arena.borrow()[x].parent().unwrap();
                let z = store.get_src(y).unwrap();
                // (b) if x has no partner in M': i. let k<-find_pos(x),
                let k = self.find_pos(store, x, &src_in_order, &dst_in_order);
                debug!("Edit script: INS {:?} Parent: {:?}",
                       store.dst_arena.borrow()[x],
                       store.src_arena.borrow()[z]);
                w = Some(store.src_arena
                              .borrow_mut()
                              .new_node(store.dst_arena.borrow()[x].ty.clone(),
                                        store.dst_arena.borrow()[x].label.clone(),
                                        store.dst_arena.borrow()[x].col_no,
                                        store.dst_arena.borrow()[x].line_no,
                                        store.dst_arena.borrow()[x].char_no,
                                        store.dst_arena.borrow()[x].token_len));
                // iii. Add (w, x) to M' and apply INS((w, a, v(x)), z, k) to T_1.
                store.push(w.unwrap(), x, &MappingType::EDIT);
                // ii. Append INS((w, a, v(x)), z, k) to E for new identifier w
                let mut ins = Insert::new(w.unwrap(), Some(z), k);
                ins.apply(&mut store.src_arena.borrow_mut())?;
                script.push(ins);
            } else if !x.is_root(&store.dst_arena.borrow()) {
                // Insertion and update phases.
                // (c) else if x is not a root (x has a partner in M').
                // i. Let w be the partner of x in M' and let v be the parent
                // of w in T_1.
                w = Some(store.get_src(x).unwrap());
                let v = store.src_arena.borrow()[w.unwrap()].parent().unwrap();
                // ii. if value_of(w) != value_of(x):
                if store.src_arena.borrow()[w.unwrap()].label != store.dst_arena.borrow()[x].label {
                    debug!("Edit script: UPD {:?} -> {:?}",
                           store.src_arena.borrow()[w.unwrap()],
                           store.dst_arena.borrow()[x]);
                    let mut upd = Update::new(w.unwrap(),
                                              store.dst_arena.borrow()[x].ty.clone(),
                                              store.dst_arena.borrow()[x].label.clone());
                    // B. Apply UPD(w, v(x)) to T_1.
                    upd.apply(&mut store.src_arena.borrow_mut())?;
                    // A. Append UPD(w, v(x)) to E.
                    script.push(upd.clone());
                }
                // MOVE phase.
                // iii. If (y, v) not in M':
                let y = store.dst_arena.borrow()[x].parent().unwrap();
                // A. Let z be the partner of y in M'.
                let z = store.get_src(y).unwrap();
                if z != v {
                    // B. Let k<-find_pos(x).
                    let k = self.find_pos(store, x, &src_in_order, &dst_in_order);
                    let mut mov = Move::new(w.unwrap(), z, k);
                    debug!("Edit script: MOV {:?} {} Parent: {:?} {} Child: {}",
                           store.src_arena.borrow()[w.unwrap()].ty,
                           store.src_arena.borrow()[w.unwrap()].label,
                           store.src_arena.borrow()[z].ty,
                           store.src_arena.borrow()[z].label,
                           k);
                    // D. Apply MOV(w, z, k) to T_1.
                    mov.apply(&mut store.src_arena.borrow_mut())?;
                    // C. Append MOV(w, z, k) to E.
                    script.push(mov);
                }
            }
            // (d) align_children(w, x).
            if w.is_some()
               && !w.unwrap().is_root(&store.src_arena.borrow())
               && !x.is_root(&store.dst_arena.borrow())
            {
                let mut actions =
                    self.align_children(store,
                                         store.src_arena.borrow()[w.unwrap()].parent().unwrap(),
                                         store.dst_arena.borrow()[x].parent().unwrap(),
                                         &mut script,
                                         &mut src_in_order,
                                         &mut dst_in_order)?;
                actions.apply(&mut store.src_arena.borrow_mut())?;
            }
        }
        // Delete phase.
        // 3. Perform a post-order traversal of T_1.
        let mut actions = EditScript::new();
        // (a) Let w be the current node in the post-order traversal of T_1.
        for w in root_src.post_order_traversal(&store.src_arena.borrow()) {
            // (b) If w has no partner in M' then append DEL(w) to E and apply
            // DEL(w) to T_1.
            if !store.contains_src(w) {
                debug!("Edit script: DEL {:?} {}",
                       store.src_arena.borrow()[w].ty,
                       store.src_arena.borrow()[w].label);
                let del = Delete::new(w);
                script.push(del.clone());
                actions.push(del);
            }
        }
        actions.apply(&mut store.src_arena.borrow_mut())?;
        if fake_roots {
            debug!("Removing fake roots.");
            let tmp_src_root = store.src_arena.borrow().root().unwrap();
            let tmp_dst_root = store.dst_arena.borrow().root().unwrap();
            store.src_arena.borrow_mut().delete_root()?;
            store.dst_arena.borrow_mut().delete_root()?;
            store.remove(tmp_src_root, tmp_dst_root);
        }
        debug_assert!(store.is_isomorphic(root_src, root_dst));
        Ok(script)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use action::{Delete, Insert, Move, Update};
    use ast::Arena;

    #[test]
    fn test_chawathe96_empty_asts() {
        let ast_src = Arena::<u8, SrcNodeId>::new();
        let ast_dst = Arena::<u8, DstNodeId>::new();
        let store = MappingStore::new(ast_src, ast_dst);
        let edit_script_res = Chawathe96Config::new().generate_script(&store);
        assert!(edit_script_res.is_ok());
        let edit_script = edit_script_res.unwrap();
        assert_eq!(0, edit_script.size());
    }

    #[test]
    fn test_chawathe96_fully_matched_asts() {
        let mut ast_src = Arena::<u8, SrcNodeId>::new();
        let root_src = ast_src.new_node(0, String::from("INT"), None, None, None, None);
        let mut ast_dst = Arena::<u8, DstNodeId>::new();
        let root_dst = ast_dst.new_node(0, String::from("INT"), None, None, None, None);
        let store = MappingStore::new(ast_src, ast_dst);
        store.push(root_src, root_dst, &MappingType::EDIT);
        let edit_script_res = Chawathe96Config::new().generate_script(&store);
        assert!(edit_script_res.is_ok());
        let edit_script = edit_script_res.unwrap();
        assert_eq!(0, edit_script.size());
    }

    #[test]
    fn test_chawathe96_single_delete() {
        let mut ast_src = Arena::<&'static str, SrcNodeId>::new();
        let root_src = ast_src.new_node("Expr", String::from("+"), None, None, None, None);
        let n1 = ast_src.new_node("INT", String::from("1"), None, None, None, None);
        n1.make_child_of(root_src, &mut ast_src).unwrap();
        let mut ast_dst = Arena::<&'static str, DstNodeId>::new();
        let root_dst = ast_dst.new_node("Expr", String::from("+"), None, None, None, None);
        let store = MappingStore::new(ast_src, ast_dst);
        store.push(root_src, root_dst, &MappingType::ANCHOR);
        let edit_script_res = Chawathe96Config::new().generate_script(&store);
        assert!(edit_script_res.is_ok());
        let edit_script = edit_script_res.unwrap();
        assert_eq!(1, edit_script.size());
        let mut expected: EditScript<&'static str> = EditScript::new();
        expected.push(Delete::new(n1));
        assert_eq!(expected, edit_script);
    }

    #[test]
    fn test_chawathe96_single_insert() {
        let mut ast_src = Arena::<&'static str, SrcNodeId>::new();
        let root_src = ast_src.new_node("Expr", String::from("+"), None, None, None, None);
        let mut ast_dst = Arena::<&'static str, DstNodeId>::new();
        let root_dst = ast_dst.new_node("Expr", String::from("+"), None, None, None, None);
        let n1 = ast_dst.new_node("INT", String::from("1"), None, None, None, None);
        n1.make_child_of(root_dst, &mut ast_dst).unwrap();
        let store = MappingStore::new(ast_src, ast_dst);
        store.push(root_src, root_dst, &MappingType::ANCHOR);
        let edit_script_res = Chawathe96Config::new().generate_script(&store);
        assert!(edit_script_res.is_ok());
        let edit_script = edit_script_res.unwrap();
        assert_eq!(1, edit_script.size());
        let mut expected: EditScript<&'static str> = EditScript::new();
        expected.push(Insert::new(NodeId::new(1), Some(root_src), 0));
        assert_eq!(expected, edit_script);
    }

    #[test]
    fn test_chawathe96_single_update() {
        let mut ast_src = Arena::<&'static str, SrcNodeId>::new();
        let root_src = ast_src.new_node("Expr", String::from("+"), None, None, None, None);
        let n1 = ast_src.new_node("INT", String::from("-1"), None, None, None, None);
        n1.make_child_of(root_src, &mut ast_src).unwrap();
        let mut ast_dst = Arena::<&'static str, DstNodeId>::new();
        let root_dst = ast_dst.new_node("Expr", String::from("+"), None, None, None, None);
        let n2 = ast_dst.new_node("INT", String::from("1"), None, None, None, None);
        n2.make_child_of(root_dst, &mut ast_dst).unwrap();
        let store = MappingStore::new(ast_src, ast_dst);
        store.push(root_src, root_dst, &MappingType::ANCHOR);
        store.push(n1, n2, &MappingType::ANCHOR);
        let edit_script_res = Chawathe96Config::new().generate_script(&store);
        assert!(edit_script_res.is_ok());
        let edit_script = edit_script_res.unwrap();
        assert_eq!(1, edit_script.size());
        let mut expected: EditScript<&'static str> = EditScript::new();
        expected.push(Update::new(n1, "INT", String::from("1")));
        assert_eq!(expected, edit_script);
    }

    #[test]
    fn test_chawathe96_single_move() {
        let mut ast_src = Arena::<&'static str, SrcNodeId>::new();
        let root_src = ast_src.new_node("Expr", String::from("+"), None, None, None, None);
        let n1 = ast_src.new_node("INT", String::from("-1"), None, None, None, None);
        n1.make_child_of(root_src, &mut ast_src).unwrap();
        let n2 = ast_src.new_node("INT", String::from("1"), None, None, None, None);
        n2.make_child_of(root_src, &mut ast_src).unwrap();
        let mut ast_dst = Arena::<&'static str, DstNodeId>::new();
        let root_dst = ast_dst.new_node("Expr", String::from("+"), None, None, None, None);
        let n3 = ast_dst.new_node("INT", String::from("-1"), None, None, None, None);
        n3.make_child_of(root_dst, &mut ast_dst).unwrap();
        let n4 = ast_dst.new_node("INT", String::from("1"), None, None, None, None);
        n4.make_child_of(n3, &mut ast_dst).unwrap();
        let store = MappingStore::new(ast_src, ast_dst);
        store.push(root_src, root_dst, &MappingType::ANCHOR);
        store.push(n1, n3, &MappingType::ANCHOR);
        store.push(n2, n4, &MappingType::ANCHOR);
        let edit_script_res = Chawathe96Config::new().generate_script(&store);
        assert!(edit_script_res.is_ok());
        let edit_script = edit_script_res.unwrap();
        assert_eq!(1, edit_script.size());
        let mut expected: EditScript<&'static str> = EditScript::new();
        expected.push(Move::new(n2, n1, 0));
        assert_eq!(expected, edit_script);
    }
}
