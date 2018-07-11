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

use ast::{Arena, NodeId};
use label_maps::LabelMap;
/// Stores information about a single AST (as needed by RTED) in vectors.
use std::ops::{Index, IndexMut};
use std::rc::Rc;

// Size of the `info` vector.
const INFO_SIZE: usize = 16;
// Size of the paths vector.
const PATHS_SIZE: usize = 3;
// Size of the relative subtrees vector.
const REL_SUBTREES_SIZE: usize = 3;
// Size of the node type vector.
const NODE_TYPE_SIZE: usize = 3;

/// Constants related to forward and reverse paths through trees.
#[allow(missing_docs)]
pub enum PathIdx {
    Left = 0,
    Right = 1,
    Heavy = 2,
    Both = 3,
    RevLeft = 4,
    RevRight = 5,
    RevHeavy = 6
}

/// Index a struct with the `PathIdx` enum.
macro_rules! table_index_trait {
    ($table:ident, $ty:ty) => {
        impl Index<PathIdx> for $table {
            type Output = $ty;
            fn index(&self, idx: PathIdx) -> &Self::Output {
                &self.table[idx as usize]
            }
        }
        impl IndexMut<PathIdx> for $table {
            fn index_mut(&mut self, idx: PathIdx) -> &mut Self::Output {
                &mut self.table[idx as usize]
            }
        }
    };
}

// Flags (left, right, heavy) for each node.
#[derive(Clone, Debug, Eq, PartialEq)]
struct NodeTypeTable {
    pub table: Vec<Vec<bool>>
}

impl NodeTypeTable {
    fn new(size: usize) -> NodeTypeTable {
        let mut node_types = NodeTypeTable { table: vec![] };
        for _ in 0..NODE_TYPE_SIZE {
            node_types.table.push(vec![false; size]);
        }
        node_types
    }
}

table_index_trait!(NodeTypeTable, Vec<bool>);

// Paths maps paths (left / right / heavy) to nodes.
#[derive(Clone, Debug, Eq, PartialEq)]
struct PathTable {
    pub table: Vec<Vec<Option<usize>>>
}

impl PathTable {
    fn new(size: usize) -> PathTable {
        let mut paths = PathTable { table: vec![] };
        for idx in 0..PATHS_SIZE {
            if idx == PathIdx::Both as usize {
                paths.table.push(vec![Some(0); size]);
            } else {
                paths.table.push(vec![None; size]);
            }
        }
        paths
    }
}

table_index_trait!(PathTable, Vec<Option<usize>>);

// Paths maps relative subtrees (left / right / heavy) to nodes.
#[derive(Clone, Debug, Eq, PartialEq)]
struct RelativeSubtreesTable {
    pub table: Vec<Vec<Vec<usize>>>
}

impl RelativeSubtreesTable {
    fn new(size: usize) -> RelativeSubtreesTable {
        let mut subtrees = RelativeSubtreesTable { table: vec![] };
        for idx in 0..REL_SUBTREES_SIZE {
            subtrees.table.push(vec![]);
            for _ in 0..size {
                subtrees.table[idx].push(vec![]);
            }
        }
        subtrees
    }
}

table_index_trait!(RelativeSubtreesTable, Vec<Vec<usize>>);

/// Constants related to information held about trees.
///
/// Vectors whose index is prefixed with `POST2` hold node ids in left-right
/// postorder numbering (not the original `NodeId`s). `RPOST2` means reversed
/// postorder numbering (i.e. right-left).
#[allow(missing_docs)]
pub enum InfoIdx {
    Post2Size = 0,
    Post2KRSum = 1,
    Post2RevKRSum = 2,
    /// Number of subforests in full decomposition.
    Post2DescSum = 3,
    Post2Pre = 4,
    Post2Parent = 5,
    Post2Label = 6,
    /// Key root nodes (size of this array = leaf count).
    KR = 7,
    /// Left-most leaf descendants.
    Post2LLD = 8,
    /// Minimum key root nodes index in key root array.
    Post2MinKR = 9,
    /// Reversed key root nodes.
    RKR = 10,
    /// Reversed postorder 2 right-most leaf descendants.
    RPost2RLD = 11,
    RPost2MinRKR = 12,
    /// Reversed postorder -> postorder.
    RPost2Post = 13,
    /// Strategy for Demaine.
    Post2Strategy = 14,
    /// Convert preorder to postorder id.
    Pre2Post = 15
}

/// Vector to hold information about an AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InfoTable {
    table: Vec<Vec<Option<usize>>>
}

impl InfoTable {
    fn new(size: usize) -> InfoTable {
        let mut info = InfoTable { table: vec![] };
        for idx in 0..INFO_SIZE {
            if idx == InfoIdx::Post2Parent as usize
               || idx == InfoIdx::Post2MinKR as usize
               || idx == InfoIdx::RPost2MinRKR as usize
               || idx == InfoIdx::Post2Strategy as usize
            {
                info.table.push(vec![None; size]);
            } else {
                info.table.push(vec![Some(0); size]);
            }
        }
        info
    }
}

impl Index<InfoIdx> for InfoTable {
    type Output = Vec<Option<usize>>;

    fn index(&self, idx: InfoIdx) -> &Self::Output {
        &self.table[idx as usize]
    }
}

impl IndexMut<InfoIdx> for InfoTable {
    fn index_mut(&mut self, idx: InfoIdx) -> &mut Self::Output {
        &mut self.table[idx as usize]
    }
}

/// Information held about an AST.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InfoTree<'a> {
    /// All information related to a given AST and necessary for RTED.
    pub info: InfoTable,
    // Node labels stored as numbers (as an optimisation).
    labels: Rc<LabelMap<'a>>,
    // Flags (left, right, heavy) for each node.
    node_type: NodeTypeTable,
    // Paths maps paths (left / right / heavy) to nodes.
    paths: PathTable,
    // Paths maps relative subtrees (left / right / heavy) to nodes.
    relative_subtrees: RelativeSubtreesTable,
    tmp_size: usize,
    tmp_descendant_sizes: usize,
    tmp_key_root_sizes: usize,
    tmp_rev_key_root_sizes: usize,
    tmp_preorder: usize,
    // Postorder number of the current node in the TED recursion.
    current_node: usize,
    // Remember if the tree order was switched during the recursion (by
    // comparison with the order of the input tree).
    switched: bool,
    leaf_count: usize,
    tree_size: usize
}

impl<'a> InfoTree<'a> {
    /// Create a new information tree for a non-empty AST.
    pub fn new<T: Clone + PartialEq, U: Copy + PartialEq>(ast: &'a Arena<T, U>,
                                                          label_map: Rc<LabelMap<'a>>)
                                                          -> InfoTree<'a> {
        assert!(ast.root().is_some(),
                "Cannot create an InfoTree on an empty Arena.");
        let mut tree = InfoTree { info: InfoTable::new(ast.size()),
                                  labels: label_map,
                                  node_type: NodeTypeTable::new(ast.size()),
                                  paths: PathTable::new(ast.size()),
                                  relative_subtrees: RelativeSubtreesTable::new(ast.size()),
                                  tmp_size: 0,
                                  tmp_descendant_sizes: 0,
                                  tmp_key_root_sizes: 0,
                                  tmp_rev_key_root_sizes: 0,
                                  tmp_preorder: 0,
                                  // We asserted above that `ast` has a root node, so `ast.size > 0`.
                                  current_node: ast.size() - 1,
                                  switched: false,
                                  leaf_count: 0,
                                  tree_size: ast.size() };
        tree.gather_info(ast.root().unwrap(), &ast, None);
        tree.post_traversal_processing();
        tree
    }

    /// Return the size of the original AST.
    pub fn size(&self) -> usize {
        self.tree_size
    }

    /// Return `true` if a given node (postorder id) is left / right / heavy.
    pub fn if_node_of_type(&self, postorder: usize, ty: PathIdx) -> bool {
        self.node_type[ty][postorder]
    }

    /// Return the type array for a subtree.
    pub fn get_node_type_array(&self, ty: PathIdx) -> &Vec<bool> {
        &self.node_type[ty]
    }

    /// For given infoCode and postorder of a node returns requested information of that node.
    pub fn get_info(&self, info_code: InfoIdx, nodes_postorder: usize) -> usize {
        self.info[info_code][nodes_postorder].unwrap()
    }

    /// For given infoCode returns an info array (index array)
    pub fn get_info_vec(&self, info_code: InfoIdx) -> &Vec<Option<usize>> {
        &self.info[info_code]
    }

    /// Returns relevant subtrees for given node.
    ///
    /// Assuming that child v of given node belongs to given path, all children
    /// of given node are returned but node v.
    pub fn get_node_relevant_subtrees(&self,
                                      path_type: PathIdx,
                                      node_postorder: usize)
                                      -> &Vec<usize> {
        &self.relative_subtrees[path_type][node_postorder]
    }

    /// Returns an array representation of a given path's type.
    pub fn get_path(&self, path_type: PathIdx) -> &Vec<Option<usize>> {
        &self.paths[path_type]
    }

    /// Returns the postorder of current root node.
    pub fn get_current_node(&self) -> usize {
        self.current_node
    }

    /// Sets postorder of the current node in the recursion.
    pub fn set_current_node(&mut self, postorder: usize) {
        self.current_node = postorder;
    }

    /// Gathers information of a given tree in corresponding arrays.
    ///
    /// At this point the given tree is traversed once, but there is a loop
    /// over current nodes children to assign them their parents.
    fn gather_info<T: Clone + PartialEq, U: Copy + PartialEq>(&mut self,
                                                              id: NodeId<U>,
                                                              ast: &'a Arena<T, U>,
                                                              mut postorder: Option<usize>)
                                                              -> usize {
        let mut current_size = 0;
        let mut children_count = 0;
        let mut desc_sizes = 0;
        let mut kr_sizes_sum = 0;
        let mut reverse_kr_sizes_sum = 0;
        let preorder = self.tmp_preorder;

        let mut heavy_child: Option<usize> = None;
        let mut left_child: Option<usize> = None;
        let mut right_child: Option<usize> = None;
        let mut weight: usize;
        let mut max_weight = 0;
        let mut current_postorder: usize;
        let mut old_heavy_child: Option<usize> = None;

        let mut tmp_heavy_rel_subtrees: Vec<usize> = vec![];
        let mut tmp_left_rel_subtrees: Vec<usize> = vec![];
        let mut tmp_right_rel_subtrees: Vec<usize> = vec![];
        let mut children_postorders: Vec<usize> = vec![];

        self.tmp_preorder += 1;

        let mut iterable = id.children(&ast).peekable();
        while let Some(child) = iterable.next() {
            children_count += 1;
            postorder = Some(self.gather_info(child, &ast, postorder));
            children_postorders.push(postorder.unwrap());
            current_postorder = postorder.unwrap();
            // Heavy path.
            weight = self.tmp_size;
            if weight >= max_weight {
                max_weight = weight;
                old_heavy_child = heavy_child;
                heavy_child = Some(current_postorder);
            } else {
                tmp_heavy_rel_subtrees.push(current_postorder);
            }
            if old_heavy_child.is_some() {
                tmp_heavy_rel_subtrees.push(old_heavy_child.unwrap());
                old_heavy_child = None;
            }
            // Left path.
            if children_count == 1 {
                left_child = Some(current_postorder);
            } else {
                tmp_left_rel_subtrees.push(current_postorder);
            }
            // Right path.
            right_child = Some(current_postorder);
            if iterable.peek().is_some() {
                tmp_right_rel_subtrees.push(current_postorder);
            }
            // Subtree size.
            current_size += 1 + self.tmp_size;
            desc_sizes += self.tmp_descendant_sizes;
            if children_count > 1 {
                kr_sizes_sum += self.tmp_key_root_sizes + self.tmp_size + 1;
            } else {
                kr_sizes_sum += self.tmp_key_root_sizes;
                self.node_type[PathIdx::Left][current_postorder] = true;
            }
            if iterable.peek().is_some() {
                reverse_kr_sizes_sum += self.tmp_rev_key_root_sizes + self.tmp_size + 1;
            } else {
                reverse_kr_sizes_sum += self.tmp_rev_key_root_sizes;
                self.node_type[PathIdx::Right][current_postorder] = true;
            }
        }
        if postorder.is_none() {
            postorder = Some(0);
        } else {
            postorder = Some(postorder.unwrap() + 1);
        }
        let current_desc_sizes = desc_sizes + current_size + 1;
        self.info[InfoIdx::Post2DescSum][postorder.unwrap()] =
            Some((current_size + 1) * (current_size + 1 + 3) / 2 - current_desc_sizes);
        self.info[InfoIdx::Post2KRSum][postorder.unwrap()] = Some(kr_sizes_sum + current_size + 1);
        self.info[InfoIdx::Post2RevKRSum][postorder.unwrap()] =
            Some(reverse_kr_sizes_sum + current_size + 1);
        // POST2_LABEL
        self.info[InfoIdx::Post2Label][postorder.unwrap()] =
            Rc::make_mut(&mut self.labels).store(&ast[id].label);
        // POST2_PARENT
        for i in children_postorders {
            self.info[InfoIdx::Post2Parent][i] = postorder;
        }
        // POST2_SIZE
        self.info[InfoIdx::Post2Size][postorder.unwrap()] = Some(current_size + 1);
        if current_size == 0 {
            self.leaf_count += 1;
        }
        // POST2_PRE
        self.info[InfoIdx::Post2Pre][postorder.unwrap()] = Some(preorder);
        // PRE2_POST
        self.info[InfoIdx::Pre2Post][preorder] = postorder;
        // RPOST2_POST
        self.info[InfoIdx::RPost2Post][self.tree_size - 1 - preorder] = postorder;
        // Heavy path.
        if heavy_child.is_some() {
            self.paths[PathIdx::Heavy][postorder.unwrap()] = heavy_child;
            self.node_type[PathIdx::Heavy][heavy_child.unwrap()] = true;
            if left_child < heavy_child && heavy_child < right_child {
                self.info[InfoIdx::Post2Strategy][postorder.unwrap()] =
                    Some(PathIdx::Both as usize);
            } else if heavy_child == left_child {
                self.info[InfoIdx::Post2Strategy][postorder.unwrap()] =
                    Some(PathIdx::Right as usize);
            } else if heavy_child == right_child {
                self.info[InfoIdx::Post2Strategy][postorder.unwrap()] =
                    Some(PathIdx::Left as usize);
            }
        } else {
            self.info[InfoIdx::Post2Strategy][postorder.unwrap()] = Some(PathIdx::Right as usize);
        }
        // Left path.
        if left_child.is_some() {
            self.paths[PathIdx::Left][postorder.unwrap()] = left_child;
        }
        // Right path.
        if right_child.is_some() {
            self.paths[PathIdx::Right][postorder.unwrap()] = right_child;
        }
        // Heavy / left / right relevant subtrees.
        self.relative_subtrees[PathIdx::Heavy][postorder.unwrap()] = tmp_heavy_rel_subtrees;
        self.relative_subtrees[PathIdx::Right][postorder.unwrap()] = tmp_right_rel_subtrees;
        self.relative_subtrees[PathIdx::Left][postorder.unwrap()] = tmp_left_rel_subtrees;

        self.tmp_descendant_sizes = current_desc_sizes;
        self.tmp_size = current_desc_sizes;
        self.tmp_key_root_sizes = kr_sizes_sum;
        self.tmp_rev_key_root_sizes = reverse_kr_sizes_sum;

        postorder.unwrap()
    }

    /// Gathers information that couldn't be collected while tree traversal.
    fn post_traversal_processing(&mut self) {
        self.info[InfoIdx::KR] = vec![None; self.leaf_count];
        self.info[InfoIdx::RKR] = vec![None; self.leaf_count];
        // Compute left-most leaf descendants.
        // Move along the left-most path, remember each node and assign to it
        // the path's leaf. Compute right-most leaf descendants (in reverse postorder).
        for i in 0..self.tree_size {
            if self.paths[PathIdx::Left][i].is_none() {
                self.info[InfoIdx::Post2LLD][i] = Some(i);
            } else {
                self.info[InfoIdx::Post2LLD][i] =
                    self.info[InfoIdx::Post2LLD][self.paths[PathIdx::Left][i].unwrap()];
            }
            if self.paths[PathIdx::Right][i].is_none() {
                let post_pre = self.info[InfoIdx::Post2Pre][i].unwrap();
                self.info[InfoIdx::RPost2RLD][self.tree_size - 1 - post_pre] =
                    Some(self.tree_size - 1 - self.info[InfoIdx::Post2Pre][i].unwrap());
            } else {
                let tmp1 = self.info[InfoIdx::Post2Pre][i].unwrap();
                self.info[InfoIdx::RPost2RLD][self.tree_size - 1 - tmp1] = self.info
                    [InfoIdx::RPost2RLD]
                    [self.tree_size
                     - 1
                     - self.info[InfoIdx::Post2Pre][self.paths[PathIdx::Right][i].unwrap()].unwrap()];
            }
        }
        // Compute reversed key root nodes (in reversed postorder).
        let mut visited: Vec<bool> = vec![false; self.tree_size];
        let mut visited_rev: Vec<bool> = vec![false; self.tree_size]; // was empty
        let mut k = self.leaf_count;
        let mut k_rev = self.leaf_count;
        for i in (0..self.tree_size).rev() {
            if k > 0 && !visited[self.info[InfoIdx::Post2LLD][i].unwrap()] {
                self.info[InfoIdx::KR][k - 1] = Some(i);
                visited[self.info[InfoIdx::Post2LLD][i].unwrap()] = true;
                k -= 1;
            }
            if k_rev > 0 && !visited_rev[self.info[InfoIdx::RPost2RLD][i].unwrap()] {
                self.info[InfoIdx::RKR][k_rev - 1] = Some(i);
                visited_rev[self.info[InfoIdx::RPost2RLD][i].unwrap()] = true;
                k_rev -= 1;
            }
        }
        // Compute minimal reversed key roots for every subtree (in reverse postorder).
        let mut parent: Option<usize>;
        let mut parent_rev: Option<usize>;
        for i in 0..self.leaf_count {
            parent = self.info[InfoIdx::KR][i];
            while parent.is_some() && self.info[InfoIdx::Post2MinKR][parent.unwrap()].is_some() {
                self.info[InfoIdx::Post2MinKR][parent.unwrap()] = Some(i);
                parent = self.info[InfoIdx::Post2Parent][parent.unwrap()];
            }
            parent_rev = self.info[InfoIdx::RKR][i];
            while parent_rev.is_some()
                  && self.info[InfoIdx::RPost2MinRKR][parent_rev.unwrap()].is_some()
            {
                self.info[InfoIdx::RPost2MinRKR][parent_rev.unwrap()] = Some(i);
                parent_rev = self.info[InfoIdx::Post2Parent]
                    [self.info[InfoIdx::RPost2Post][parent_rev.unwrap()].unwrap()];
                if parent_rev.is_some() {
                    parent_rev = Some(self.tree_size
                                      - 1
                                      - self.info[InfoIdx::Post2Pre][parent_rev.unwrap()].unwrap());
                }
            }
        }
    }

    /// Set a flag if the tree order was switched during the recursion.
    pub fn set_switched(&mut self, value: bool) {
        self.switched = value
    }

    /// Was the tree order was switched during the recursion?
    pub fn switched(&self) -> bool {
        self.switched
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use label_maps::LabelMap;
    use test_common::create_mult_arena;

    #[test]
    fn test_new_info_trees() {
        let ast = create_mult_arena();
        let map = Rc::new(LabelMap::new());
        let _ = InfoTree::new(&ast, map);
    }
}
