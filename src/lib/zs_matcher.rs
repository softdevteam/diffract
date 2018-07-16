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
use std::f64;
use std::fmt::Debug;
use std::hash::Hash;

use ast::{Arena, DstNodeId, NodeId, SrcNodeId};
use f64_eq;
use matchers::{has_same_type, MappingStore, MappingType, MatchTrees};
use qgram::trigram_distance;

/// Cost of deleting a sub-forest.
const DELETION_COST: f64 = 1.0;

/// Cost of inserting a sub-forest.
const INSERTION_COST: f64 = 1.0;

/// Z-S Configuration includes tree and forest distance matrices.
///
/// The `src` and `dst` trees are indexed in left-right postorder numbering,
/// as described in Section 3.1 of the paper.
#[derive(Debug, Clone, PartialEq)]
pub struct ZhangShashaConfig {
    src: ZSTree<SrcNodeId>,
    dst: ZSTree<DstNodeId>,
    tree_dist: Vec<Vec<f64>>,
    forest_dist: Vec<Vec<f64>>
}

impl ZhangShashaConfig {
    /// Create a new configuration object.
    pub fn new<T: Clone + PartialEq + ToString>(src: &Arena<T, SrcNodeId>,
                                                dst: &Arena<T, DstNodeId>)
                                                -> ZhangShashaConfig {
        ZhangShashaConfig { src: ZSTree::new(src),
                            dst: ZSTree::new(dst),
                            tree_dist: vec![],
                            forest_dist: vec![] }
    }

    /// Compute the tree distance matrix. See Section 3.2 of the paper.
    fn compute_tree_distance<T: Clone + Debug + Eq + ToString + 'static>(&mut self,
                                                                         store: &MappingStore<T>)
    {
        for _ in 0..self.src.node_count + 1 {
            self.tree_dist.push(vec![0.0; self.dst.node_count + 1]);
            self.forest_dist.push(vec![0.0; self.dst.node_count + 1]);
        }
        debug_assert_eq!(self.tree_dist.len(), self.src.node_count + 1);
        debug_assert_eq!(self.tree_dist[0].len(), self.dst.node_count + 1);
        debug_assert_eq!(self.forest_dist.len(), self.src.node_count + 1);
        debug_assert_eq!(self.forest_dist[0].len(), self.dst.node_count + 1);
        for i in 1..self.src.key_roots.len() {
            for j in 1..self.dst.key_roots.len() {
                let kr_src = self.src.key_roots[i];
                let kr_dst = self.dst.key_roots[j];
                self.compute_forest_dist(kr_src, kr_dst, store);
            }
        }
    }

    /// Compute the forest distance matrix. See Section 3.2 of the paper.
    fn compute_forest_dist<T: Clone + Debug + Eq + ToString + 'static>(&mut self,
                                                                       i: usize,
                                                                       j: usize,
                                                                       store: &MappingStore<T>)
    {
        self.forest_dist[self.src.lld(i) - 1][self.dst.lld(j) - 1] = 0.0;
        for di in self.src.lld(i)..i + 1 {
            let cost_del = get_deletion_cost(self.src.id(di), &store.src_arena.borrow());
            self.forest_dist[di][self.dst.lld(j) - 1] =
                self.forest_dist[di - 1][self.dst.lld(j) - 1] + cost_del;
            for dj in self.dst.lld(j)..j + 1 {
                let cost_ins = get_insertion_cost(self.dst.id(dj), &store.dst_arena.borrow());
                self.forest_dist[self.src.lld(i) - 1][dj] =
                    self.forest_dist[self.src.lld(i) - 1][dj - 1] + cost_ins;
                if self.src.lld(di) == self.src.lld(i) && self.dst.lld(dj) == self.dst.lld(j) {
                    let cost_upd = get_update_cost(self.src.id(di),
                                                   &store.src_arena.borrow(),
                                                   self.dst.id(dj),
                                                   &store.dst_arena.borrow());
                    self.forest_dist[di][dj] =
                        f64::min(f64::min(self.forest_dist[di - 1][dj] + cost_del,
                                          self.forest_dist[di][dj - 1] + cost_ins),
                                 self.forest_dist[di - 1][dj - 1] + cost_upd);
                    self.tree_dist[di][dj] = self.forest_dist[di][dj];
                } else {
                    self.forest_dist[di][dj] =
                        f64::min(f64::min(self.forest_dist[di - 1][dj] + cost_del,
                                          self.forest_dist[di][dj - 1] + cost_ins),
                                 self.forest_dist[self.src.lld(di) - 1][self.dst.lld(dj) - 1]
                                 + self.tree_dist[di][dj]);
                }
            }
        }
    }
}

impl<T: Clone + Debug + Eq + ToString + 'static> MatchTrees<T> for ZhangShashaConfig {
    fn describe(&self) -> String {
        let desc = "
The Zhang Shasha tree matching algorithm -- works by attempting to match
successive subtrees. For more information see Zhang and Shasha (1989) Simple
Fast Algorithms for the Editing Distance Between Trees and Related Problems.";
        String::from(desc)
    }

    /// Match locations in distinct ASTs (not described in the paper).
    fn match_trees(&mut self,
                   src: Arena<T, SrcNodeId>,
                   dst: Arena<T, DstNodeId>)
                   -> MappingStore<T> {
        let store = MappingStore::new(src, dst);
        self.compute_tree_distance(&store);
        // Implementation of Algorithm Y src Wagner and Fischer (1974), with
        // an outer loop to iterate over pairs of sub-forests.
        let mut tree_pairs: Vec<(usize, usize)> = vec![];
        tree_pairs.push((self.src.node_count, self.dst.node_count));
        while !tree_pairs.is_empty() {
            let tree_pair = tree_pairs.pop().unwrap();
            let last_row = tree_pair.0;
            let last_col = tree_pair.1;
            let first_row = self.src.lld(last_row) - 1;
            let first_col = self.dst.lld(last_col) - 1;
            let mut row = last_row;
            let mut col = last_col;
            // Algorithm Y.
            while row > first_row || col > first_col {
                if row > first_row
                   && f64_eq(self.forest_dist[row - 1][col] + DELETION_COST,
                             self.forest_dist[row][col])
                {
                    // Node with left-right postorder id `row` is DELETEd src
                    // edit distance tree `src`.
                    row -= 1;
                } else if col > first_col
                          && f64_eq(self.forest_dist[row][col - 1] + INSERTION_COST,
                                    self.forest_dist[row][col])
                {
                    // Node with left-right postorder id `col` is INSERTed into
                    // edit distance tree `dst`.
                    col -= 1;
                } else if self.src.lld(row) - 1 == self.src.lld(last_row) - 1
                          && self.dst.lld(col) - 1 == self.dst.lld(last_col) - 1
                          && has_same_type(self.src.id(row),
                                           &store.src_arena.borrow(),
                                           self.dst.id(col),
                                           &store.dst_arena.borrow())
                {
                    // Both sub-forests are trees, so match id node row (in src)
                    // to col (in dst). The matcher does not use left-right
                    // postorder notation, so we retrieve the original node ids.
                    let src_node = self.src.id(row);
                    let dst_node = self.dst.id(col);
                    store.push(src_node, dst_node, &MappingType::ANCHOR);
                    row -= 1;
                    col -= 1;
                } else {
                    tree_pairs.push((row, col));
                    // Continue computing match with the forest to the left of
                    // the current edit distance tree pair.
                    row = self.src.lld(row) - 1;
                    col = self.dst.lld(col) - 1;
                }
            }
        }
        store
    }
}

/// Return cost of a deletion action.
fn get_deletion_cost<T: Clone>(_node: NodeId<SrcNodeId>, _arena: &Arena<T, SrcNodeId>) -> f64 {
    DELETION_COST
}

/// Return cost of an insertion action.
fn get_insertion_cost<T: Clone>(_node: NodeId<DstNodeId>, _arena: &Arena<T, DstNodeId>) -> f64 {
    INSERTION_COST
}

/// Return cost of an update action.
fn get_update_cost<T: Clone + Eq + ToString>(src: NodeId<SrcNodeId>,
                                             src_arena: &Arena<T, SrcNodeId>,
                                             dst: NodeId<DstNodeId>,
                                             dst_arena: &Arena<T, DstNodeId>)
                                             -> f64 {
    if src_arena[src].ty == dst_arena[dst].ty {
        let src_s = src_arena[src].label.to_string();
        let dst_s = dst_arena[dst].label.to_string();
        if src_s.is_empty() || dst_s.is_empty() {
            return 1.0;
        } else {
            return 1.0 - trigram_distance(&src_s, &dst_s);
        }
    }
    f64::MAX
}

/// A `ZSTree` represents an `Arena` AST in left-right postorder numbering.
///
/// The `ids` vector maps left-right postorder ids back to the original
/// `NodeIds` in the AST `Arena` struct. The `llds` vector holds leftmost leaf
/// descendants of each node. `key_roots` are the values described in Section
/// 3.2 of the paper as `LR_keyroots` and defined thus:
///
///  `LR_Keyroots(T) = { k | there exists no k_ > k such that l(k) = l(k_) }`
///
/// in this notation, `T` is a tree and `l(i)` is the id of node `i` described
/// in left-right postorder notation.
///
/// If node `k` is in `LR_Keyroots(T)` then either `k` is the root node of `T`
/// or `l(k) != l(p(k))` where `p(k)` is the parent of node `k`. i.e.
/// `LR_Keyroots` is the set of roots of all subtrees of `T` that require
/// separate distance computations.
#[derive(Debug, Clone, PartialEq)]
struct ZSTree<U: Copy + Eq + Hash + PartialEq> {
    // Internal vector position of leaf-most leaf descendant of the root node.
    node_count: usize,
    leaf_count: usize,
    llds: Vec<usize>,
    ids: Vec<NodeId<U>>,
    key_roots: Vec<usize>
}

impl<U: Copy + Eq + Hash + PartialEq> ZSTree<U> {
    pub fn new<T: Clone + PartialEq + ToString>(ast: &Arena<T, U>) -> ZSTree<U> {
        let node_count = ast.size();
        let mut tree = ZSTree { node_count,
                                leaf_count: 0,
                                llds: Vec::with_capacity(node_count),
                                ids: Vec::with_capacity(node_count),
                                key_roots: vec![] };
        for _ in 0..node_count {
            tree.llds.push(0);
            tree.ids.push(NodeId::new(0));
        }
        debug_assert_eq!(tree.llds.len(), node_count);
        debug_assert_eq!(tree.ids.len(), node_count);
        let mut idx = 1;
        let mut tmp_data: HashMap<NodeId<U>, usize> = HashMap::new();
        if ast.root().is_none() {
            // Assume client code has just constructed a tree in order to call
            // describe() on the ZhangShashaConfig trait.
            return tree;
        }
        let root = ast.root().unwrap();
        for node in root.post_order_traversal(ast) {
            tmp_data.insert(node, idx);
            tree.set_id(idx, node);
            tree.set_lld(idx, tmp_data[&node.get_first_leaf(ast)]);
            if node.is_leaf(ast) {
                tree.leaf_count += 1;
            }
            idx += 1;
        }
        // Compute key roots.
        tree.key_roots = Vec::with_capacity(tree.leaf_count + 1);
        for _ in 0..tree.leaf_count + 1 {
            tree.key_roots.push(0);
        }
        debug_assert_eq!(tree.key_roots.len(), tree.leaf_count + 1);
        let mut visited: Vec<bool> = vec![false; tree.node_count + 1];
        debug_assert_eq!(visited.len(), tree.node_count + 1);
        let mut k = tree.key_roots.len() - 1;
        for i in (1..tree.node_count + 1).rev() {
            if !visited[tree.lld(i)] {
                tree.key_roots[k] = i;
                visited[tree.lld(i)] = true;
                k -= 1;
            }
        }
        tree
    }

    /// Get the left-most leaf descendent of node `i`.
    ///
    /// `i` and the result are given in left-right postorder numbering.
    fn lld(&self, i: usize) -> usize {
        self.llds[i - 1] + 1
    }

    /// Set the left-most leaf descendent of node `i`.
    ///
    /// `i` and `lld` are given in left-right postorder numbering.
    fn set_lld(&mut self, i: usize, lld: usize) {
        self.llds[i - 1] = lld - 1;
        if self.node_count < i {
            self.node_count = i;
        }
    }

    /// Get the id of node `i`.
    ///
    /// `i` is given in left-right postorder numbering, the result is the
    /// id of the node in its original `Arena` representation.
    fn id(&self, i: usize) -> NodeId<U> {
        self.ids[i - 1]
    }

    /// Set the index of `node`.
    ///
    /// `i` is given in left-right postorder numbering, `node` is the
    /// id of the node in its original `Arena` representation.
    fn set_id(&mut self, i: usize, node: NodeId<U>) {
        self.ids[i - 1] = node;
        if self.node_count < i {
            self.node_count = i;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_common::load_xml_ast;
    use test_common::{create_mult_arena, create_slide_dst_arena, create_slide_src_arena};
    use test_common::{create_zs_dst_arena, create_zs_src_arena};
    use test_common::{create_zs_paper_dst_arena, create_zs_paper_src_arena};

    #[test]
    fn test_zs_single_identical_node() {
        let ast_src = load_xml_ast("<Tree ty=\"program\" label=\"\" />");
        let ast_dst =
            Arena::<String, DstNodeId>::from(load_xml_ast("<Tree ty=\"program\" label=\"\" />"));
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(1, store.size());
        assert!(store.is_mapped(NodeId::new(0), NodeId::new(0)));
    }

    #[test]
    fn test_zs_single_nonidentical_node() {
        let ast_src = load_xml_ast("<Tree ty=\"program\" label=\"mylabel\" />");
        let ast_dst =
            Arena::<String, DstNodeId>::from(load_xml_ast("<Tree ty=\"notaprog\" label=\"\" />"));
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(0, store.size());
    }

    #[test]
    fn test_zs_single_update() {
        let ast_src = load_xml_ast("<Tree ty=\"int\" label=\"This sentence is similar. A quirky thing it is.\" />");
        let ast_dst =
            Arena::<String, DstNodeId>::from(load_xml_ast("<Tree ty=\"int\" label=\"A quirky thing it is. This is a sentence.\" />"));
        let src_root = ast_src.root().unwrap();
        let dst_root = ast_dst.root().unwrap();
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(1, store.size());
        assert!(store.is_mapped(src_root, dst_root));
    }

    #[test]
    fn test_zs_single_insert() {
        let ast_src = load_xml_ast(
                                   "<Tree ty=\"Expr\" label=\"+\">
    </Tree>"
        );
        let ast_dst =
            Arena::<String, DstNodeId>::from(load_xml_ast(
                "<Tree ty=\"Expr\" label=\"+\">
        <Tree ty=\"INT\" label=\"1\"/>
    </Tree>"
            ));
        let src_root = ast_src.root().unwrap();
        let dst_root = ast_dst.root().unwrap();
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(1, store.size());
        assert!(store.is_mapped(src_root, dst_root));
    }

    #[test]
    fn test_zs_single_delete() {
        let ast_src = load_xml_ast(
                                   "<Tree ty=\"Expr\" label=\"+\">
        <Tree ty=\"INT\" label=\"1\"/>
    </Tree>"
        );
        let ast_dst =
            Arena::<String, DstNodeId>::from(load_xml_ast(
                "<Tree ty=\"Expr\" label=\"+\">
    </Tree>"
            ));
        let src_root = ast_src.root().unwrap();
        let dst_root = ast_dst.root().unwrap();
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(1, store.size());
        assert!(store.is_mapped(src_root, dst_root));
    }

    #[test]
    fn test_zs_same_tree() {
        let ast_src = create_mult_arena();
        let ast_dst = Arena::<String, DstNodeId>::from(ast_src.clone());
        assert_eq!(5, ast_src.size());
        assert_eq!(5, ast_dst.size());
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(5, store.size());
        // These are not in a loop because we want assertion failures to
        // give the node id at the command line.
        assert!(store.is_mapped(NodeId::new(0), NodeId::new(0)));
        assert!(store.is_mapped(NodeId::new(1), NodeId::new(1)));
        assert!(store.is_mapped(NodeId::new(2), NodeId::new(2)));
        assert!(store.is_mapped(NodeId::new(3), NodeId::new(3)));
        assert!(store.is_mapped(NodeId::new(4), NodeId::new(4)));
    }

    #[test]
    fn test_zs_workshop_example() {
        // Example src:
        // https://www.slideshare.net/hecfran/tree-distance-algorithm
        let ast_src = load_xml_ast(
                                   "<Tree ty=\"node\" label=\"a\">
    <Tree ty=\"node\" label=\"a\">
        <Tree ty=\"node\" label=\"a\">
                <Tree ty=\"node\" label=\"b\"/>
        </Tree>
        <Tree ty=\"node\" label=\"b\"/>
        <Tree ty=\"node\" label=\"b\"/>
    </Tree>
</Tree>
"
        );
        let ast_dst =
            Arena::<String, DstNodeId>::from(load_xml_ast(
                "<Tree ty=\"node\" label=\"c\">
    <Tree ty=\"node\" label=\"a\">
            <Tree ty=\"node\" label=\"b\"/>
    </Tree>
    <Tree ty=\"node\" label=\"a\">
        <Tree ty=\"node\" label=\"b\"/>
    </Tree>
    <Tree ty=\"node\" label=\"b\"/>
</Tree>
"
            ));
        assert_eq!(6, ast_src.size());
        assert_eq!(6, ast_dst.size());
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(vec![0, 0, 2, 3, 0, 0], matcher_config.src.llds);
        assert_eq!(vec![0, 0, 2, 2, 4, 0], matcher_config.dst.llds);
        assert_eq!(vec![0, 3, 4, 6], matcher_config.src.key_roots);
        assert_eq!(vec![0, 4, 5, 6], matcher_config.dst.key_roots);
        let tree_dist = vec![vec![0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                             vec![0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 5.0],
                             vec![0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 4.0],
                             vec![0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 5.0],
                             vec![0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 5.0],
                             vec![0.0, 4.0, 3.0, 4.0, 3.0, 4.0, 2.0],
                             vec![0.0, 5.0, 4.0, 5.0, 4.0, 5.0, 3.0]];
        assert_eq!(tree_dist, matcher_config.tree_dist);
        assert_eq!(5, store.size());
        // These are not in a loop because we want assertion failures to
        // give the node id at the command line.
        assert!(store.is_mapped(NodeId::new(1), NodeId::new(0)));
        assert!(store.is_mapped(NodeId::new(2), NodeId::new(1)));
        assert!(store.is_mapped(NodeId::new(5), NodeId::new(5)));
        assert!(store.is_mapped(NodeId::new(3), NodeId::new(4)));
        assert!(store.is_mapped(NodeId::new(4), NodeId::new(3)));
    }

    #[test]
    fn test_zs_same_structure() {
        // Each node should appear in the match store as an UPDATE. Labels here
        // should be interesting enough to produce non-rational update costs.
        let ast_src = load_xml_ast(
                                   "<Tree ty=\"node\" label=\"KAIZHONG ZHANG AND DENNIS SHASHA\">
    <Tree ty=\"node\" label=\"HUMAN\">
        <Tree ty=\"node\" label=\"AAACCGTGAGTTATTCGTTCTAGAA\"/>
        <Tree ty=\"node\" label=\"XMJYAUZ\">
            <Tree ty=\"node\" label=\"tent\"/>
        </Tree>
    </Tree>
    <Tree ty=\"node\" label=\"Saturday\">
    </Tree>
</Tree>
"
        );
        let ast_dst = Arena::<String, DstNodeId>::from(load_xml_ast("<Tree ty=\"node\" label=\"DENNIS SHASHA AND KAIZHONG ZHANG\">
    <Tree ty=\"node\" label=\"CHIMPANZEE\">
        <Tree ty=\"node\" label=\"CACCCCTAAGGTACCTTTGGTTC\"/>
        <Tree ty=\"node\" label=\"MZJAWXU\">
            <Tree ty=\"node\" label=\"test\"/>
        </Tree>
    </Tree>
    <Tree ty=\"node\" label=\"Sunday\">
    </Tree>
</Tree>
"));
        assert_eq!(6, ast_src.size());
        assert_eq!(6, ast_dst.size());
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(6, store.size());
        // These are not in a loop because we want assertion failures to
        // give the node id at the command line.
        assert!(store.is_mapped(NodeId::new(0), NodeId::new(0)));
        assert!(store.is_mapped(NodeId::new(1), NodeId::new(1)));
        assert!(store.is_mapped(NodeId::new(2), NodeId::new(2)));
        assert!(store.is_mapped(NodeId::new(3), NodeId::new(3)));
        assert!(store.is_mapped(NodeId::new(4), NodeId::new(4)));
        assert!(store.is_mapped(NodeId::new(5), NodeId::new(5)));
    }

    #[test]
    fn test_zs_paper_example() {
        // Example src Fig. 4 of the paper.
        let ast_src = create_zs_paper_src_arena();
        let ast_dst = create_zs_paper_dst_arena();
        assert_eq!(6, ast_src.size());
        assert_eq!(6, ast_dst.size());
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        assert_eq!(vec![0, 3, 5, 6], matcher_config.src.key_roots);
        assert_eq!(vec![0, 2, 5, 6], matcher_config.dst.key_roots);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        let tree_dist = vec![vec![0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                             vec![0.0, 0.0, 1.0, 2.0, 3.0, 1.0, 5.0],
                             vec![0.0, 1.0, 0.0, 2.0, 3.0, 1.0, 5.0],
                             vec![0.0, 2.0, 1.0, 2.0, 2.0, 2.0, 4.0],
                             vec![0.0, 3.0, 3.0, 1.0, 2.0, 4.0, 4.0],
                             vec![0.0, 1.0, 1.0, 3.0, 4.0, 0.0, 5.0],
                             vec![0.0, 5.0, 5.0, 3.0, 3.0, 5.0, 2.0]];
        assert_eq!(tree_dist, matcher_config.tree_dist);
        assert_eq!(5, store.size());
        assert!(store.is_mapped(NodeId::new(0), NodeId::new(0)));
        assert!(store.is_mapped(NodeId::new(2), NodeId::new(2)));
        assert!(store.is_mapped(NodeId::new(1), NodeId::new(3)));
        assert!(store.is_mapped(NodeId::new(3), NodeId::new(4)));
        assert!(store.is_mapped(NodeId::new(5), NodeId::new(5)));
    }

    #[test]
    fn test_zs_custom_example() {
        let ast_src = create_zs_src_arena();
        let ast_dst = create_zs_dst_arena();
        let src_root = ast_src.root().unwrap();
        let dst_root = ast_dst.root().unwrap();
        let src_children = &src_root.children(&ast_src)
                                    .collect::<Vec<NodeId<SrcNodeId>>>();
        let dst_children = &dst_root.children(&ast_dst)
                                    .collect::<Vec<NodeId<DstNodeId>>>();
        let dst_grandchild = dst_children[0].children(&ast_dst).nth(1).unwrap();
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(5, store.size());
        assert!(store.is_mapped(src_root, dst_children[0]));
        assert!(store.is_mapped(src_children[0],
                                dst_children[0].children(&store.dst_arena.borrow())
                                               .nth(0)
                                               .unwrap()));
        assert!(store.is_mapped(src_children[1], dst_grandchild));
        assert!(store.is_mapped(src_children[1].children(&store.src_arena.borrow())
                                               .nth(0)
                                               .unwrap(),
                                dst_grandchild.children(&store.dst_arena.borrow())
                                              .nth(0)
                                              .unwrap()));
        assert!(store.is_mapped(src_children[1].children(&store.src_arena.borrow())
                                               .nth(2)
                                               .unwrap(),
                                dst_grandchild.children(&store.dst_arena.borrow())
                                              .nth(2)
                                              .unwrap()));
    }

    #[test]
    fn test_zs_slide_example() {
        let ast_src = create_slide_src_arena();
        let ast_dst = create_slide_dst_arena();
        let src_root = ast_src.root().unwrap();
        let dst_root = ast_dst.root().unwrap();
        let src_children = &src_root.children(&ast_src)
                                    .collect::<Vec<NodeId<SrcNodeId>>>();
        let src_grandchild = src_children[0].children(&ast_src).nth(0).unwrap();
        let dst_children = &dst_root.children(&ast_dst)
                                    .collect::<Vec<NodeId<DstNodeId>>>();
        let mut matcher_config = ZhangShashaConfig::new(&ast_src, &ast_dst);
        let store = matcher_config.match_trees(ast_src, ast_dst);
        assert_eq!(5, store.size());
        assert!(store.is_mapped(src_root, dst_root));
        assert!(store.is_mapped(src_grandchild, dst_children[0]));
        assert!(store.is_mapped(src_grandchild.children(&store.src_arena.borrow())
                                              .nth(0)
                                              .unwrap(),
                                dst_children[0].children(&store.dst_arena.borrow())
                                               .nth(0)
                                               .unwrap()));
        assert!(store.is_mapped(src_children[0].children(&store.src_arena.borrow())
                                               .nth(1)
                                               .unwrap(),
                                dst_children[1].children(&store.dst_arena.borrow())
                                               .nth(0)
                                               .unwrap()));
        assert!(store.is_mapped(src_children[0].children(&store.src_arena.borrow())
                                               .nth(2)
                                               .unwrap(),
                                dst_children[2]));
    }
}
