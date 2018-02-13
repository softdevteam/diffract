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

///
/// This matcher is based on the paper Meaningful Change Detection in Structured Data.
/// Written by Sudarshan S. Chawathe and Hector Garcia-Molina.
/// This matcher would use the additional operation which are GLUE and COPY.
/// In addition to Insert, Delete, Move, Update.
/// This is supposed to be an alternative matcher to chawathe96.

use std::fmt::Debug;

use ast::{Arena, FromNodeId, NodeId, ToNodeId};

use std::cell::RefCell;
use std::collections::HashSet;
use std::cmp;

/// Not needed by matching algorithms, but useful for debugging.
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum EdgeType {
    /// Edge corresponds to a UPDATE operation.
    UPDATE,
    /// Edge corresponds to a INSERT operation.
    INSERT,
    /// Edge corresponds to a DELETE operation.
    DELETE,
    /// Edge corresponds to a MOVE operation.
    MOVE,
    /// Edge corresponds to a COPY operation.
    COPY,
    /// Edge corresponds to a GLUE operation.
    GLUE,
    /// Edge corresponds to a NULL operation.
    NULL,
    /// Edge corresponds to a OK operation.
    OK,
}
#[derive(Debug, Clone)]
/// The intial cost of each edge
pub struct CostEdge {
    /// Edge corresponds to a UPDATE cost operation.
    pub update: usize,
    /// Edge corresponds to a INSERT cost operation.
    pub insert: usize,
    /// Edge corresponds to a DELETE cost operation.
    pub delete: usize,
    /// Edge corresponds to a MOVE cost operation.
    pub move_: usize,
    /// Edge corresponds to a COPY cost operation.
    pub copy: usize,
    /// Edge corresponds to a GLUE cost operation.
    pub glue: usize,
    /// Edge corresponds to a NULL cost operation.
    pub null: usize,
    /// Edge corresponds to a OK cost operation.
    pub ok: usize,
}

/// This shows the Cost Edge Struct where the user can define the cost of different edge type.
impl CostEdge {
    /// Create a new cost edge
    /// This shows the cost for each edge defined by the user
    pub fn new(cost_update_1: usize,
               cost_insert_1: usize,
               cost_delete_1: usize,
               cost_move_1: usize,
               cost_copy_1: usize,
               cost_glue_1: usize,
               cost_null_1: usize,
               cost_ok_1: usize)
               -> CostEdge {
        CostEdge { update: cost_update_1,
                   insert: cost_insert_1,
                   delete: cost_delete_1,
                   move_: cost_move_1,
                   copy: cost_copy_1,
                   glue: cost_glue_1,
                   null: cost_null_1,
                   ok: cost_ok_1, }
    }
}

#[derive(Debug, Clone)]
/// Edge construct which shows the edge between two nodes and their value
pub struct Edge {
    /// from node shows the node id from the From-Tree
    pub from_node: NodeId<FromNodeId>,
    /// to node shows the node id from To-Tree
    pub to_node: NodeId<ToNodeId>,
    /// The value of the edge
    pub value: usize,
    /// The edge type
    pub edge_type: EdgeType,
}

impl Edge {
    /// Create a new edge
    pub fn new(new_from_node: NodeId<FromNodeId>,
               new_to_node: NodeId<ToNodeId>,
               new_value: usize,
               new_edge_type: EdgeType)
               -> Edge {
        Edge { from_node: new_from_node,
               to_node: new_to_node,
               value: new_value,
               edge_type: new_edge_type, }
    }
}

/// A store of mappings between nodes in different arenas.
/// Direction is important.
#[derive(Debug, Clone)]
pub struct MappingStoreGraph<T: Clone + Debug> {
    /// List Of edges
    pub list_edges: RefCell<Vec<Edge>>,
    /// Source arena (treat as immutable).
    pub from_arena: RefCell<Arena<T, FromNodeId>>,
    /// Destination arena (treat as immutable).
    pub to_arena: RefCell<Arena<T, ToNodeId>>,
    /// List of nodes to be inserted
    pub list_node_inserted: RefCell<Vec<NodeId<ToNodeId>>>,
    /// List of nodes to be deleted
    pub list_node_deleted: RefCell<Vec<NodeId<FromNodeId>>>,
    /// Find the Cost of all edges
    pub all_edge_cost: CostEdge,
}

impl<T: Clone + Debug + Eq + 'static> MappingStoreGraph<T> {
    /// Implementation of new in mapping store the second version
    pub fn new(base: Arena<T, FromNodeId>, diff: Arena<T, ToNodeId>) -> MappingStoreGraph<T> {
        MappingStoreGraph { from_arena: RefCell::new(base),
                            to_arena: RefCell::new(diff),
                            list_edges: RefCell::new(Vec::new()),
                            list_node_inserted: RefCell::new(Vec::new()),
                            list_node_deleted: RefCell::new(Vec::new()),
            // The cost could change in the future but,
            // we can make the default cost for every edge cost the value of 1.
                            all_edge_cost: CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1), }
    }
    /// Implementation of the update of the cost edge.
    /// This will renew the cost value for each edge type.
    pub fn update_cost(&mut self,
                       cost_update: usize,
                       cost_insert: usize,
                       cost_delete: usize,
                       cost_move: usize,
                       cost_copy: usize,
                       cost_glue: usize,
                       cost_null: usize,
                       cost_ok: usize) {
        self.all_edge_cost = CostEdge::new(cost_update,
                                           cost_insert,
                                           cost_delete,
                                           cost_move,
                                           cost_copy,
                                           cost_glue,
                                           cost_null,
                                           cost_ok);
    }

    /// Push New Edge
    pub fn push(&mut self,
                new_from_node: NodeId<FromNodeId>,
                new_to_node: NodeId<ToNodeId>,
                new_value: usize,
                new_edge_type: EdgeType) {
        let new_edge = Edge::new(new_from_node, new_to_node, new_value, new_edge_type);
        self.list_edges.borrow_mut().push(new_edge);
    }
    /// Remove Edge
    pub fn remove(&mut self, index: usize) {
        self.list_edges.borrow_mut().remove(index);
    }
    /// Check contains the from_node
    pub fn contains_from(&self, from: NodeId<FromNodeId>) -> bool {
        let list_edges = self.list_edges.borrow();
        for edge in list_edges.iter() {
            if edge.from_node == from {
                return true;
            }
        }
        return false;
    }

    /// Check if the edges contains
    ///     1) from node id &
    ///     2) To node id
    ///     3) Edge Type
    pub fn contains_edge(&self, from: usize, to: usize, edge_type: EdgeType, list_edges: &mut Vec<Edge>) -> bool {
        for edge in list_edges {
            if edge.from_node.id() == from && edge.to_node.id() == to && edge.edge_type == edge_type {
                return true;
            }
        }
        return false;
    }

    /// Check if the edge contains the to_node
    pub fn contains_to(&self, to: NodeId<ToNodeId>) -> bool {
        let list_edges = self.list_edges.borrow();
        for edge in list_edges.iter() {
            if edge.to_node == to {
                return true;
            }
        }
        return false;
    }

    /// Print for all the pre order traversal tree2: <Vec<NodeId<ToNodeId>>>
    pub fn print_preorder_traversal(&self, tree1: Vec<NodeId<FromNodeId>>, tree2: Vec<NodeId<ToNodeId>>) -> () {
        println!();
        println!("Tree 1");
        print!("|");
        for node in tree1 {
            print!("{:?},{:?}|",
                   node.id(),
                   self.from_arena.borrow()[node].label);
        }
        println!();
        println!("Tree 2");
        print!("|");
        for node in tree2 {
            print!("{:?},{:?}|", node.id(), self.to_arena.borrow()[node].label);
        }
        println!();
    }
    /// Checks if the subtree in node id in From_node is the same same in to_node
    /// This overall checks the subtrees of two trees and whether they are the same.
    /// The check is done by checking if the nodes are equal and as well as their descendants.
    pub fn check_subtree(&self, from_node: NodeId<FromNodeId>, to_node: NodeId<ToNodeId>) -> bool {
        //        let mut output = true;
        let descendants_from_node = from_node.descendants(&self.from_arena.borrow())
                                             .collect::<Vec<NodeId<FromNodeId>>>();
        let descendants_to_node = to_node.descendants(&self.to_arena.borrow())
                                         .collect::<Vec<NodeId<ToNodeId>>>();
        if eq(&from_node,
              &self.from_arena.borrow(),
              &to_node,
              &self.to_arena.borrow())
        {
            if descendants_from_node.len() == descendants_to_node.len() {
                for i in 0..descendants_to_node.len() {
                    if !eq(&descendants_from_node[i],
                           &self.from_arena.borrow(),
                           &descendants_to_node[i],
                           &self.to_arena.borrow())
                    {
                        // This means the descendants are !perfect by label and expression
                        return false;
                        //output = false;
                    }
                }
            } else {
                return false;
                //output = false;
            }
        } else {
            return false;
            // output = false;
        }
        return true;
    }
    /// Check if the subtree is deleted from from_node against to_node.
    /// This checks whether the the descendants are in the deletion edges.
    pub fn check_tree_deletion(&self,
                               from_node_ancestor: NodeId<FromNodeId>,
                               delete_tree2: NodeId<ToNodeId>,
                               hash_set_deletion: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)>)
                               -> bool {
        let mut output = true;
        let descendants_from_node = from_node_ancestor.descendants(&self.from_arena.borrow())
                                                      .collect::<Vec<NodeId<FromNodeId>>>();
        for descendant in descendants_from_node {
            if !hash_set_deletion.contains(&(descendant, delete_tree2)) {
                output = false;
            }
        }
        return output;
    }

    /// check if subtree 1 exists in tree 2.
    /// This overall checks from node exists in the same position in tree 2 (To Node Id)
    /// If it does then true otherwise false.
    pub fn check_tree_1_exists_in_tree2(&self, from_node_ancestor: NodeId<FromNodeId>) -> bool {
        let mut output = true;
        let node_id_to_node = from_node_ancestor.id();

        let to_node = NodeId::<ToNodeId>::new(node_id_to_node);
        if self.check_subtree(from_node_ancestor, to_node) {
            output = true;
        } else {
            output = false;
        }

        return output;
    }
    /// Check if the descendants of to_node are in the insertion edges.
    /// From node would be the insertion node.
    pub fn check_tree_insertion(&self,
                                insert_tree1: NodeId<FromNodeId>,
                                to_node: NodeId<ToNodeId>,
                                hash_set_insertion: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)>)
                                -> bool {
        let mut output = true;
        let descendants_from_node = to_node.descendants(&self.to_arena.borrow())
                                           .collect::<Vec<NodeId<ToNodeId>>>();
        for descendant in descendants_from_node {
            if !hash_set_insertion.contains(&(insert_tree1, descendant)) {
                output = false;
            }
        }

        return output;
    }

    /// Converts vector(To_node_id) to hash set
    pub fn vector_to_hash_set_to_node_id(&self, tree2: Vec<NodeId<ToNodeId>>) -> HashSet<NodeId<ToNodeId>> {
        let mut hashset = HashSet::<NodeId<ToNodeId>>::new();
        for node in tree2.clone() {
            hashset.insert(node);
        }
        return hashset;
    }

    /// Converts vector(From_node_id) to hash set
    pub fn vector_to_hash_set_from_node_id(&self, tree2: Vec<NodeId<FromNodeId>>) -> HashSet<NodeId<FromNodeId>> {
        let mut hashset = HashSet::<NodeId<FromNodeId>>::new();
        for node in tree2.clone() {
            hashset.insert(node);
        }
        return hashset;
    }

    /// Calculate lower bound cost
    pub fn lower_bound_edge(self, edge1: Edge, edges: Vec<Edge>) -> usize {
        // Vector to hash map
        let mut edges_hash_map = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();

        for edge in edges {
            edges_hash_map.insert((edge.from_node, edge.to_node));
        }
        // Cost of the lower bound is
        // Addition of Cost of updating M -> N
        // Addition of 1/2 * min ()

        // Get the cost of update
        let cost_update = self.all_edge_cost.update;
        let cost_move = self.all_edge_cost.move_;
        let m = edge1.from_node.clone();
        let n = edge1.to_node.clone();

        // Sum of Cost of forced move with m',n where m' is the child of m
        let m_children = m.children(&self.from_arena.borrow()).collect::<Vec<NodeId<FromNodeId>>>();
        let n_children = n.children(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>();
        let mut cmf_m1_n = 0; // Cost of sum of Cmf(m',n)
        let mut cmf_m_n1 = 0; // Cost of sum of Cmf(m,n')
        for m_child in &m_children {
            let mut calculate_cost = true;
            /*
            If n_child does not exist.
            If M_child & n_child doesn't exist IN Edges.
                calculate_cost = true
            IF it exists then there is no calculate cost.
            */
            for n_child in &n_children {
                if edges_hash_map.contains(&(*m_child, *n_child)) {
                    calculate_cost = false;
                }
            }
            if calculate_cost {
                cmf_m1_n += cost_move;
            }
        }

        for n_child in n_children {
            let mut calculate_cost = true;
            for m_child in m_children.clone() {
                if edges_hash_map.contains(&(m_child, n_child)) {
                    calculate_cost = false;
                }
            }
            if calculate_cost {
                cmf_m_n1 += cost_move;
            }
        }

        let lower_bound_edge = cost_update + 1 / 2 * (cmp::min(cmf_m1_n, cmf_m_n1));
        return lower_bound_edge;
    }
    /// Calculate lower bound cost
    pub fn upper_bound_edge(self, edge1: Edge, edges: Vec<Edge>) -> usize {
        // Vector to hash map
        let mut edges_hash_map = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();

        for edge in edges.clone() {
            edges_hash_map.insert((edge.from_node, edge.to_node));
        }

        // Cost of the lower bound is
        // Addition of Cost of updating M -> N
        // Addition of 1/2 * min ()

        // Get the cost of update
        let cost_update = self.all_edge_cost.update;
        let cost_move = self.all_edge_cost.move_;
        let m = edge1.from_node.clone();
        let n = edge1.to_node.clone();

        // Sum of Cost of forced move with m',n where m' is the child of m
        let m_children = m.children(&self.from_arena.borrow()).collect::<Vec<NodeId<FromNodeId>>>();
        let n_children = n.children(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>();
        let mut cm_m1_n = 0; // Cost of sum of Cmf(m',n)
        let mut cm_m_n1 = 0; // Cost of sum of Cmf(m,n')
        for m_child in &m_children {
            //
            let mut calculate_cost = false;
            // Count of number of edges
            let mut em1 = 0; // E(m')
            for e1 in edges.clone() {
                if *m_child == e1.from_node {
                    em1 += 1;
                }
            }
            em1 = em1 - 1;

            for n_child in n_children.clone() {
                if edges_hash_map.contains(&(*m_child, n_child)) {
                    calculate_cost = true;
                }
            }
            if calculate_cost {
                cm_m1_n += cost_move;
            }
            cm_m1_n = self.all_edge_cost.copy + cm_m1_n + em1;
        }

        for n_child in &n_children {
            let mut calculate_cost = false;
            let mut en1 = 0;
            for e1 in edges.clone() {
                if *n_child == e1.to_node {
                    en1 += 1;
                }
            }
            en1 = en1 - 1;

            for m_child in &m_children {
                if edges_hash_map.contains(&(*m_child, *n_child)) {
                    calculate_cost = true;
                }
            }
            if calculate_cost {
                cm_m_n1 += cost_move;
            }
            cm_m_n1 = self.all_edge_cost.glue + cm_m_n1 + en1;
        }

        let mut cost_cw = 0;

        if edge1.edge_type == EdgeType::UPDATE {
            cost_cw = self.all_edge_cost.update;
        } else if edge1.edge_type == EdgeType::INSERT {
            cost_cw = self.all_edge_cost.insert;
        } else if edge1.edge_type == EdgeType::DELETE {
            cost_cw = self.all_edge_cost.delete;
        }
        // Upper bound edge cost
        let upper_bound_edge = cost_cw + 1 / 2 * (cm_m1_n) + 1 / 2 * (cm_m_n1);

        return upper_bound_edge;
    }

    /// Algorithm Comes from Figure 10
    pub fn annotate(&self, edges: Vec<Edge>) {
        // For every edge that is not annotated do:
        let mut list_edges: Vec<Edge> = Vec::new();
        for edge in edges.clone() {
            // Edge e = [m,n] are part of edges K
            //  m -->
            let m = edge.from_node.clone();
            // n  -->
            let n = edge.to_node.clone();
            // Get M = { Mi is part of tree 1 : [Mi,n] part of list of edges }
            let mut list_m: Vec<NodeId<FromNodeId>> = Vec::new();
            let mut list_n: Vec<NodeId<ToNodeId>> = Vec::new();
            for mi in edges.clone() {
                if mi.to_node.id() == n.id() {
                    // add  Mi when the n is the same
                    list_m.push(mi.from_node);
                }
            }
            for ni in edges.clone() {
                if ni.from_node.id() == m.id() {
                    // add Ni when the m is the same
                    list_n.push(ni.to_node);
                }
            }
            // Case 1
            if list_m.len() == 1 && list_n.len() == 1 {
                // //// CASE 1.1 //// //
                // (1)list_m[0].id() == self.from_arena.len()-1
                // (1:R)The node in M its id is the insertion which is the last node in the from node
                // (2) list_n[0].id() != self.to_arena.len()-1
                // (2:R) The node in N where it doesn't equal the deletion node which is the last node in to_arena
                // (3:R) Check if node in N, is the same as our -- n --
                if list_m[0].id() == self.from_arena.borrow().size() - 1
                   && list_n[0].id() != self.to_arena.borrow().size() - 1 && list_n[0].id() == n.id()
                {
                    // Last Node
                    //
                    let mut edge = Edge::new(list_m[0], n, 1, EdgeType::INSERT);
                    list_edges.push(edge);
                }
                // //// CASE 1.2 //// //
                else if list_m[0].id() == m.id() && m.id() != self.from_arena.borrow().size() - 1
                          && list_n[0].id() == self.to_arena.borrow().size() - 1
                {
                    let mut edge = Edge::new(m, list_n[0], 1, EdgeType::DELETE);
                    list_edges.push(edge);
                }
                // //// CASE 1.3 //// //
                else if list_m[0].id() == self.from_arena.borrow().size() - 1
                          && list_n[0].id() == self.to_arena.borrow().size() - 1
                {
                    let mut edge = Edge::new(list_m[0], list_n[0], 1, EdgeType::NULL);
                    list_edges.push(edge);
                }
                // //// CASE 1.4 //// //
                else if list_m[0].id() == m.id() && m.id() != self.from_arena.borrow().size() - 1
                          && list_n[0].id() == n.id()
                          && n.id() != self.to_arena.borrow().size() - 1
                {
                    // Check if parent of m,n is in edges
                    for check in edges.clone() {
                        let m_parent = self.from_arena.borrow()[m].parent().unwrap();
                        let n_parent = self.to_arena.borrow()[n].parent().unwrap();
                        if m_parent.id() == check.from_node.id() && n_parent.id() == check.to_node.id() {
                            let mut edge = Edge::new(m, n, 1, EdgeType::NULL);
                            list_edges.push(edge);
                        } else {
                            let mut edge = Edge::new(m, n, 1, EdgeType::MOVE);
                            list_edges.push(edge);
                        }
                    }
                }
            }
            // ///// CASE 2 //// //
            else if list_m.len() == 1 && list_n.len() > 1 {
                // //// CASE 2.1 //// //
                // Figure 16, appendix D
                // //// CASE 2.2 //// //
                // Figure 17, appendix D
            }
        }
    }

    /// add to_nodes to vector to be inserted
    pub fn insert_node(&mut self, node_to_insert: NodeId<ToNodeId>) {
        self.list_node_inserted.borrow_mut().push(node_to_insert);
    }
    /// add from_node to vector to be deleted
    pub fn delete_node(&mut self, node_to_delete: NodeId<FromNodeId>) {
        self.list_node_deleted.borrow_mut().push(node_to_delete);
    }
    /// Print
    pub fn print(&self) {
        let size_from_node = NodeId::new(self.from_arena.borrow().size() - 1);
        println!("{:?}", size_from_node);
        let size_to_node = NodeId::<ToNodeId>::new(self.to_arena.borrow().size() - 1);
        let last_node_from_node = &self.from_arena.borrow()[size_from_node];
        println!("The Last node From_NODE is --> {:?}", last_node_from_node);
        let last_node_to_node = &self.to_arena.borrow()[size_to_node];
        println!("The Last node From_NODE is --> {:?}", last_node_to_node);
        println!("List Of edges already mapped to");
        let mut index = 0;
        let list_edges = self.list_edges.borrow();
        for edges in list_edges.iter() {
            println!(" -- {:?} -- ", index);

            println!("TY        :   From node    -> {:?} || to node -> {:?}",
                     self.from_arena.borrow()[edges.from_node].ty,
                     self.to_arena.borrow()[edges.to_node].ty);
            println!("Value     :   From node -> {:?} || to node -> {:?}",
                     self.from_arena.borrow()[edges.from_node].label,
                     self.to_arena.borrow()[edges.to_node].label);
            println!("From node -> {:?} || to node -> {:?} || Edge type -> {:?}",
                     edges.from_node, edges.to_node, edges.edge_type);
            index += 1;
            println!("--");
        }
    }
}

/// The Config2 matcher needs no configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct Config2 {}

impl Default for Config2 {
    fn default() -> Config2 {
        Config2 {}
    }
}

impl Config2 {
    /// Create a new configuration object, with default values.
    pub fn new() -> Config2 {
        Default::default()
    }
}

impl<T: Clone + Debug + Eq + 'static> MatchingTreesScriptor<T> for Config2 {
    /// Perform matches
    fn match_trees(&self, base: Arena<T, FromNodeId>, diff: Arena<T, ToNodeId>) -> MappingStoreGraph<T> {
        let mut store = MappingStoreGraph::new(base.clone(), diff.clone());
        if store.from_arena.borrow().is_empty() || store.to_arena.borrow().is_empty() {
            return store;
        }

        let last_node_from_node = NodeId::<FromNodeId>::new(base.size() - 1);
        let last_node_to_node = NodeId::<ToNodeId>::new(diff.clone().size() - 1);

        let base_pre = store.from_arena
                            .borrow()
                            .root()
                            .unwrap()
                            .pre_order_traversal(&store.from_arena.borrow())
                            .collect::<Vec<NodeId<FromNodeId>>>();
        let diff_pre = store.to_arena
                            .borrow()
                            .root()
                            .unwrap()
                            .pre_order_traversal(&store.to_arena.borrow())
                            .collect::<Vec<NodeId<ToNodeId>>>();

        // Hash Set See if it contains insertion and deletion
        let mut set_contains_insertion = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        let mut set_contains_deletion = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        for from_node_id in 0..base_pre.len() {
            let mut bool_check = false;
            for to_node_id in 0..diff_pre.len() {
                if eq(&base_pre[from_node_id],
                      &store.from_arena.borrow(),
                      &diff_pre[to_node_id],
                      &store.to_arena.borrow())
                {
                    if from_node_id == to_node_id {
                        // Perfect matching
                        store.push(base_pre[from_node_id],
                                   diff_pre[to_node_id],
                                   1,
                                   EdgeType::OK);
                    } else {
                        // The nodes have the same value
                        store.push(base_pre[from_node_id],
                                   diff_pre[to_node_id],
                                   1,
                                   EdgeType::MOVE)
                    }
                    bool_check = true;
                } else if eq_label(&base_pre[from_node_id],
                                   &store.from_arena.borrow(),
                                   &diff_pre[to_node_id],
                                   &store.to_arena.borrow())
                {
                    if from_node_id == to_node_id {
                        // the same label and value but different position
                        store.push(base_pre[from_node_id],
                                   diff_pre[to_node_id],
                                   1,
                                   EdgeType::UPDATE);
                    }
                }

                // We have reached the last node in the diff tree
                if bool_check == false && to_node_id == diff_pre.len() - 1 {
                    store.delete_node(base_pre[from_node_id]);
                    store.push(base_pre[from_node_id],
                               last_node_to_node,
                               1,
                               EdgeType::DELETE);
                    set_contains_deletion.insert((base_pre[from_node_id], last_node_to_node));
                }
            }
        }
        for to_node_id in 0..diff_pre.len() {
            let mut bool_check = false;
            for from_node_id in 0..base_pre.len() {
                if eq(&base_pre[from_node_id],
                      &store.from_arena.borrow(),
                      &diff_pre[to_node_id],
                      &store.to_arena.borrow())
                {
                    bool_check = true;
                }
                if bool_check == false && from_node_id == base_pre.len() - 1 {
                    store.push(last_node_from_node,
                               diff_pre[to_node_id],
                               1,
                               EdgeType::INSERT);
                    set_contains_insertion.insert((last_node_from_node, diff_pre[to_node_id]));
                }
            }
        }
        let get_all_edges = store.list_edges.borrow().clone();
        let mut all_edges_move = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType)>::new();
        let mut all_leaves_move = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType)>::new();
        for edge in get_all_edges.clone() {
            if edge.edge_type == EdgeType::MOVE {
                let edge_copy = edge.clone();
                all_edges_move.insert((edge.from_node, edge.to_node, edge.edge_type));
                let m_check = edge.from_node.is_leaf(&store.from_arena.borrow());
                let n_check = edge.to_node.is_leaf(&store.to_arena.borrow());

                if m_check && n_check {
                    all_leaves_move.insert((edge_copy.from_node, edge_copy.to_node, edge_copy.edge_type));
                }
            }
        }
        let mut all_leaves_parent =
            HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>, NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        // Check if its nodes are in the leaf move operation
        for edge in all_leaves_move.clone() {
            let e1_parent = store.from_arena.borrow()[edge.0].parent().unwrap(); //edge.0.parent;
            let e1_siblings = e1_parent.children(&store.from_arena.borrow())
                                       .collect::<Vec<NodeId<FromNodeId>>>();
            let e1_to_node_parent = store.to_arena.borrow()[edge.1].parent().unwrap(); // edge to node parent

            let size_e1_siblings = e1_siblings.len();
            for sibling in e1_siblings {
                // are the siblings map to the same subtree with the parent being the same
                let mut check_siblings_mapped_parent = 0;
                // Check if siblings exists in move hashset
                /*
                loop through all the from node's siblings
                    Main goal is to check if the move operation for its siblings exists
                    1. loop though all the move leaves operations
                        1.1) Checks
                            1.1.1) if the parent the same as the current edge (From nodes)
                            1.1.2) if the parent the same as the current edge (to nodes)
                    Overall checking all the leaves and its parents the last two years
                */
                for s in all_leaves_move.clone() {
                    if store.check_subtree(e1_parent, e1_to_node_parent) {
                        check_siblings_mapped_parent += 1;
                    }
                }

                if check_siblings_mapped_parent > 0 {
                    // The sibling do map to the same move node
                    if check_siblings_mapped_parent >= size_e1_siblings {
                        all_leaves_parent.insert((e1_parent, e1_to_node_parent, edge.0, edge.1));
                    }
                } else {
                    break;
                }
            }
        }
        // Loop through parents and check if they are in move hash set
        let mut end_copy_tree = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        //        println!("all leaves parent => {:?}",all_leaves_parent);
        for edge in all_leaves_parent.clone() {
            let mut edge_copy = edge.clone();
            let mut edge_change = (edge_copy.0, edge_copy.1);
            let mut vector_input: Vec<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = Vec::new();

            loop {
                if all_edges_move.contains(&(edge_change.0, edge_change.1, EdgeType::MOVE)) {
                    vector_input.push((edge_change.0, edge_change.1));
                    let parent_m = store.from_arena.borrow()[edge_change.0].parent().unwrap();
                    let parent_n = store.to_arena.borrow()[edge_change.1].parent().unwrap();
                    if !parent_m.is_root(&store.from_arena.borrow()) && !parent_n.is_root(&store.to_arena.borrow()) {
                        edge_change = (parent_m, parent_n);
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            if vector_input.len() > 0 {
                end_copy_tree.insert(vector_input[vector_input.len() - 1]);
            }
        }

        // Add Nodes Copy Tree
        for nodes in end_copy_tree {
            if store.check_subtree(nodes.0, nodes.1) {
                /*
                - If insertion in sub-tree from tree 2
                - If deletion in sub-tree from tree 1
                    -> Then it means the glue operations has occurred.
                */
                // If node.0 in tree1 Exists in tree 2 then its copy
                // Otherwise its glue
                //println!("Store Check tree 1 in tree2 {:?}",store.check_tree_1_exists_in_tree2(nodes.0));
                if !store.check_tree_1_exists_in_tree2(nodes.0) {
                    store.push(nodes.0, nodes.1, 1, EdgeType::GLUE);
                } else {
                    store.push(nodes.0, nodes.1, 1, EdgeType::COPY);
                }
            }
        }

        store
    }

    /// Describe this matcher for the user.
    fn describe(&self) -> String {
        let desc = "This matcher performs matching operation from the paper Chawathe et al. (1998).";
        String::from(desc)
    }

    /// Prune Edges
    fn prune_edges(&self, input: MappingStoreGraph<T>) -> (Vec<Edge>, Vec<Edge>) {
        // Loop Though all the edges
        let ct1 = cmp::max(input.all_edge_cost.move_, input.all_edge_cost.copy);

        let ct = cmp::max(ct1, input.all_edge_cost.glue); // ct the max value of Move , copy and glue
        let all_edges = input.list_edges.borrow().clone();
        let mut edges_to_be_pruned: Vec<Edge> = Vec::new();
        let mut edges_still_left: Vec<Edge> = Vec::new();
        // Initial Pruning Guess
        for edge in &all_edges {
            // edge variable will be E1 --> (m,n)
            // Get All The edges for E2 --> where (m,n1)
            // Where the from_node_id is the same but to_id is different
            // Get All The edges for E3 ---> where (m1,n)
            // Where the to_node_id is the same but to from_id is different
            let mut upper_bound_e2_and_e3 = 0;

            // E2 = (m,n1) &&   E3 = (m1,n)

            upper_bound_e2_and_e3 = input.clone()
                                         .upper_bound_edge(edge.clone(), all_edges.clone());

            let mut lower_bound_e1 = 0;
            lower_bound_e1 = input.clone()
                                  .lower_bound_edge(edge.clone(), all_edges.clone());

            if lower_bound_e1 >= upper_bound_e2_and_e3 + 2 * ct {
                edges_to_be_pruned.push(edge.clone());
            } else {
                edges_still_left.push(edge.clone());
            }
        }

        let remaining_edges = edges_still_left.clone();
        // Remove the update edges if the number of descendants are not the same.
        let mut edges_after_pruning_update: Vec<Edge> = Vec::new();
        for e1_update in remaining_edges.clone() {
            if e1_update.edge_type == EdgeType::UPDATE {
                let m = e1_update.from_node
                                 .descendants(&input.from_arena.borrow())
                                 .collect::<Vec<NodeId<FromNodeId>>>()
                                 .len();
                let n = e1_update.to_node
                                 .descendants(&input.to_arena.borrow())
                                 .collect::<Vec<NodeId<ToNodeId>>>()
                                 .len();
                // if the number of descendants are the same then ok to update
                // Otherwise you may need to insert nodes and delete etc so update won't make any sense.
                if m == n {
                    edges_after_pruning_update.push(e1_update.clone());
                }
            } else {
                edges_after_pruning_update.push(e1_update.clone());
            }
        }

        edges_still_left = edges_after_pruning_update;

        // Need to Rethink Logic
        // The Root should not move just update.
        // If it's root it should not have move, then it should have insert as well
        // Which doesn't make any sense
        // It could have glue, copy

        let mut remaining_edges_remove_root_move: Vec<Edge> = Vec::new();

        let remaining_edges = edges_still_left.clone();

        for e1_root in remaining_edges.clone() {
            // Check if its root
            if e1_root.from_node.is_root(&input.from_arena.borrow()) {
                // If it's root it should not have move
                // It could have glue, copy
                if e1_root.edge_type != EdgeType::MOVE {
                    remaining_edges_remove_root_move.push(e1_root.clone());
                }
            } else if e1_root.to_node.is_root(&input.to_arena.borrow()) {
                // To node should have anything mapped to the root
                // Same logic as the if statement above
                if e1_root.edge_type != EdgeType::MOVE {
                    remaining_edges_remove_root_move.push(e1_root.clone());
                }
            } else {
                remaining_edges_remove_root_move.push(e1_root.clone());
            }
        }

        edges_still_left = remaining_edges_remove_root_move;

        // Remove weird move operation and copy operation
        // If move and copy exist exist together take the one which has the same descendants
        // And Both Map to the same node
        let all_edges_remaining = edges_still_left.clone();
        let mut remaining_edges: Vec<Edge> = Vec::new();
        for edge_move in all_edges_remaining.clone() {
            let m_move = edge_move.from_node;
            let n_move = edge_move.to_node;
            let mut check = false;
            if edge_move.edge_type == EdgeType::MOVE {
                for edge_glue in all_edges_remaining.clone() {
                    let m_glue = edge_glue.from_node;
                    let n_glue = edge_glue.to_node;

                    if edge_glue.edge_type == EdgeType::GLUE && edge_move.edge_type == EdgeType::MOVE
                       && m_move == m_glue && n_move == n_glue
                    {
                        // We have found a copy so check is true
                        // so we have found (m,n) which is move and copy
                        check = true;
                        // We add copy edge to the remaining edges
                        remaining_edges.push(edge_glue.clone());
                    }
                }
            } else if edge_move.edge_type != EdgeType::GLUE {
                remaining_edges.push(edge_move.clone());
            }
            if check == false && edge_move.edge_type == EdgeType::MOVE {
                remaining_edges.push(edge_move.clone());
            }
        }
        edges_still_left = remaining_edges;

        /*
        Questions.
                 1) Do we need to consider where when there multiple (m,n) edge ?
        */

        return (edges_still_left, edges_to_be_pruned);
    }
}

/// Test that two nodes have the same label and type.
fn eq<T: Clone + Debug + Eq>(n1: &NodeId<FromNodeId>,
                             arena1: &Arena<T, FromNodeId>,
                             n2: &NodeId<ToNodeId>,
                             arena2: &Arena<T, ToNodeId>)
                             -> bool {
    arena1[*n1].label == arena2[*n2].label && arena1[*n1].ty == arena2[*n2].ty
}

/// Test that two nodes have the same types.
fn eq_label<T: Clone + Debug + Eq>(n1: &NodeId<FromNodeId>,
                                   arena1: &Arena<T, FromNodeId>,
                                   n2: &NodeId<ToNodeId>,
                                   arena2: &Arena<T, ToNodeId>)
                                   -> bool {
    arena1[*n1].ty == arena2[*n2].ty
}

/// Match two trees and return a store of mappings between them.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait MatchingTreesScriptor<T: Clone + Debug> {
    /// Match two trees and return a store of mappings between them.
    fn match_trees(&self, base: Arena<T, FromNodeId>, diff: Arena<T, ToNodeId>) -> MappingStoreGraph<T>;

    /// Describe the matcher for the user.
    ///
    /// This is the string that is printed when the user passes in the --list
    /// CLI option.
    fn describe(&self) -> String;

    /// This shows the trait for the pruning for the induced edges.
    /// The result should logically reduce the number of edges produced in induced edges.
    fn prune_edges(&self, input: MappingStoreGraph<T>) -> (Vec<Edge>, Vec<Edge>);
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Creates a simple tree for tests which is FromNodeId
    pub fn create_arena() -> Arena<String, FromNodeId> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("Expr"),
                                  String::from("+"),
                                  None,
                                  None,
                                  None,
                                  None);

        let n2 = arena.new_node(String::from("Expr"),
                                String::from("*"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("INT"),
                                String::from("3"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node(String::from("INT"),
                                String::from("4"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(n2, &mut arena).unwrap();

        let n1 = arena.new_node(String::from("INT"),
                                String::from("1"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        arena
    }

    /// Creates a simple tree for tests which is ToNodeId
    pub fn create_arena_2() -> Arena<String, ToNodeId> {
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("Expr"),
                                  String::from("+"),
                                  None,
                                  None,
                                  None,
                                  None);

        let n2 = arena.new_node(String::from("Expr"),
                                String::from("*"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("INT"),
                                String::from("3"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node(String::from("INT"),
                                String::from("4"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(n2, &mut arena).unwrap();

        let n1 = arena.new_node(String::from("INT"),
                                String::from("1"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();

        arena
    }

    #[test]
    /// Test for perfect Matching
    pub fn test_chawathe_matching_1() -> () {
        let mut tree1 = create_arena();
        tree1.new_node(String::from("INSERT"),
                       String::from("INSERT"),
                       None,
                       None,
                       None,
                       None);

        let mut tree2 = create_arena_2();
        tree2.new_node(String::from("DELETE"),
                       String::from("DELETE"),
                       None,
                       None,
                       None,
                       None);

        let matching_config = Config2::new();
        let matcher = matching_config.match_trees(tree1.clone(), tree2.clone());
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, EdgeType::OK, &mut checker_edges_induced));
    }

    #[test]
    // Tree 1 has more nodes and tree 2 stayst he same as create arena
    pub fn test_chawathe_matching_2() -> () {
        // Tree 1
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("Expr"),
                                  String::from("+"),
                                  None,
                                  None,
                                  None,
                                  None);

        let n4 = arena.new_node(String::from("Expr"),
                                String::from("*"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(root, &mut arena).unwrap();
        let n5 = arena.new_node(String::from("INT"),
                                String::from("3"),
                                None,
                                None,
                                None,
                                None);
        n5.make_child_of(n4, &mut arena).unwrap();
        let n6 = arena.new_node(String::from("INT"),
                                String::from("4"),
                                None,
                                None,
                                None,
                                None);
        n6.make_child_of(n4, &mut arena).unwrap();

        let n1 = arena.new_node(String::from("Expr"),
                                String::from("*"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("INT"),
                                String::from("1"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(n1, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("INT"),
                                String::from("2"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n1, &mut arena).unwrap();

        arena.new_node(String::from("INSERT"),
                       String::from("INSERT"),
                       None,
                       None,
                       None,
                       None);
        // Tree 2
        let mut tree2 = create_arena_2();
        tree2.new_node(String::from("DELETE"),
                       String::from("DELETE"),
                       None,
                       None,
                       None,
                       None);
        //
        let matching_config = Config2::new();
        let matcher = matching_config.match_trees(arena.clone(), tree2.clone());
        matcher.print();
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 4, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(6, 5, EdgeType::DELETE, &mut checker_edges_induced));
    }

    #[test]
    // Tree 2 has more nodes and tree 2 stays the same as create arena
    pub fn test_chawathe_matching_3() -> () {
        // Tree 2
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("Expr"),
                                  String::from("+"),
                                  None,
                                  None,
                                  None,
                                  None);

        let n4 = arena.new_node(String::from("Expr"),
                                String::from("*"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(root, &mut arena).unwrap();
        let n5 = arena.new_node(String::from("INT"),
                                String::from("3"),
                                None,
                                None,
                                None,
                                None);
        n5.make_child_of(n4, &mut arena).unwrap();
        let n6 = arena.new_node(String::from("INT"),
                                String::from("4"),
                                None,
                                None,
                                None,
                                None);
        n6.make_child_of(n4, &mut arena).unwrap();

        //
        let n1 = arena.new_node(String::from("Expr"),
                                String::from("*"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("INT"),
                                String::from("1"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(n1, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("INT"),
                                String::from("2"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n1, &mut arena).unwrap();

        arena.new_node(String::from("DELETE"),
                       String::from("DELETE"),
                       None,
                       None,
                       None,
                       None);
        // Tree 1
        let mut tree1 = create_arena();
        tree1.new_node(String::from("INSERT"),
                       String::from("INSERT"),
                       None,
                       None,
                       None,
                       None);
        //
        let descendants1 = NodeId::new(0).descendants(&tree1)
                                         .collect::<Vec<NodeId<FromNodeId>>>();
        let descendants2 = NodeId::new(0).descendants(&arena)
                                         .collect::<Vec<NodeId<ToNodeId>>>();

        let matching_config = Config2::new();
        let matcher = matching_config.match_trees(tree1.clone(), arena.clone());
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 4, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 5, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 6, EdgeType::INSERT, &mut checker_edges_induced));
    }

    #[test]
    pub fn test_chawathe_matching_4() -> () {
        // Tree 2
        let mut arena = Arena::new();
        let root = arena.new_node(String::from("Expr"),
                                  String::from("a"),
                                  None,
                                  None,
                                  None,
                                  None);

        // subtree 1
        let n1 = arena.new_node(String::from("Expr"),
                                String::from("f"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("Expr"),
                                String::from("b"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(n1, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("Expr"),
                                String::from("d"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n2, &mut arena).unwrap();
        /*
                          n1
                         /
                       n2
                      /
                    n3
    */
        // End subtree 1
        // Subtree 2
        let n4 = arena.new_node(String::from("Expr"),
                                String::from("e"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(root, &mut arena).unwrap();
        let n5 = arena.new_node(String::from("Expr"),
                                String::from("a"),
                                None,
                                None,
                                None,
                                None);
        n5.make_child_of(n4, &mut arena).unwrap();
        let n6 = arena.new_node(String::from("Expr"),
                                String::from("f"),
                                None,
                                None,
                                None,
                                None);
        n6.make_child_of(n4, &mut arena).unwrap();
        let n7 = arena.new_node(String::from("Expr"),
                                String::from("b"),
                                None,
                                None,
                                None,
                                None);
        n7.make_child_of(n6, &mut arena).unwrap();
        let n8 = arena.new_node(String::from("Expr"),
                                String::from("d"),
                                None,
                                None,
                                None,
                                None);
        n8.make_child_of(n7, &mut arena).unwrap();
        /*
            n4
           /  \
         n5    n6
                 \
                 n7
                   \
                   n8

    */

        // End of Subtree 2

        // Subtree 3
        let n9 = arena.new_node(String::from("Expr"),
                                String::from("g"),
                                None,
                                None,
                                None,
                                None);
        n9.make_child_of(root, &mut arena).unwrap();
        let n10 = arena.new_node(String::from("Expr"),
                                 String::from("cc"),
                                 None,
                                 None,
                                 None,
                                 None);
        n10.make_child_of(n9, &mut arena).unwrap();
        let n11 = arena.new_node(String::from("Expr"),
                                 String::from("ac"),
                                 None,
                                 None,
                                 None,
                                 None);
        n11.make_child_of(n10, &mut arena).unwrap();
        let n12 = arena.new_node(String::from("Expr"),
                                 String::from("cd"),
                                 None,
                                 None,
                                 None,
                                 None);
        n12.make_child_of(n9, &mut arena).unwrap();
        let n13 = arena.new_node(String::from("Expr"),
                                 String::from("ad"),
                                 None,
                                 None,
                                 None,
                                 None);
        n13.make_child_of(n12, &mut arena).unwrap();

        /*
                n9
               /  \
             n10   n12
            /        \
         n11         n13

    */

        // End of Subtree 3

        arena.new_node(String::from("DELETE"),
                       String::from("DELETE"),
                       None,
                       None,
                       None,
                       None);
        // Tree 1
        let mut tree1 = Arena::new();
        /*

    */
        let root = tree1.new_node(String::from("Expr"),
                                  String::from("a"),
                                  None,
                                  None,
                                  None,
                                  None);

        //    // Subtree 2
        let n4 = tree1.new_node(String::from("Expr"),
                                String::from("e"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(root, &mut tree1).unwrap();
        let n5 = tree1.new_node(String::from("Expr"),
                                String::from("a"),
                                None,
                                None,
                                None,
                                None);
        n5.make_child_of(n4, &mut tree1).unwrap();
        let n6 = tree1.new_node(String::from("Expr"),
                                String::from("f"),
                                None,
                                None,
                                None,
                                None);
        n6.make_child_of(n4, &mut tree1).unwrap();
        let n7 = tree1.new_node(String::from("Expr"),
                                String::from("b"),
                                None,
                                None,
                                None,
                                None);
        n7.make_child_of(n6, &mut tree1).unwrap();
        let n8 = tree1.new_node(String::from("Expr"),
                                String::from("d"),
                                None,
                                None,
                                None,
                                None);
        n8.make_child_of(n7, &mut tree1).unwrap();
        //    /*
        //            n4
        //           /  \
        //         n5    n6
        //                 \
        //                 n7
        //                   \
        //                   n8
        //
        //    */
        //
        //    // End of Subtree 2
        //
        //    // Subtree 3
        let n9 = tree1.new_node(String::from("Expr"),
                                String::from("g"),
                                None,
                                None,
                                None,
                                None);
        n9.make_child_of(root, &mut tree1).unwrap();
        let n10 = tree1.new_node(String::from("Expr"),
                                 String::from("cc"),
                                 None,
                                 None,
                                 None,
                                 None);
        n10.make_child_of(n9, &mut tree1).unwrap();
        let n11 = tree1.new_node(String::from("Expr"),
                                 String::from("ac"),
                                 None,
                                 None,
                                 None,
                                 None);
        n11.make_child_of(n10, &mut tree1).unwrap();
        let n12 = tree1.new_node(String::from("Expr"),
                                 String::from("cd"),
                                 None,
                                 None,
                                 None,
                                 None);
        n12.make_child_of(n9, &mut tree1).unwrap();
        let n13 = tree1.new_node(String::from("Expr"),
                                 String::from("ad"),
                                 None,
                                 None,
                                 None,
                                 None);
        n13.make_child_of(n12, &mut tree1).unwrap();

        /*
                n9
               /  \
             n10   n12
            /        \
         n11         n13

    */

        // End of Subtree 3

        tree1.new_node(String::from("INSERT"),
                       String::from("INSERT"),
                       None,
                       None,
                       None,
                       None);
        let matching_config = Config2::new();
        let matcher = matching_config.match_trees(arena.clone(), tree1.clone());
        let file_location = "./test_AS_4.dot";

        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(0, 2, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 3, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 4, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 5, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 0, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 2, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 5, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(6, 3, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(6, 6, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(7, 4, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(7, 7, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(8, 5, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(8, 8, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(9, 6, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(9, 9, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(10, 7, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(10, 10, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(11, 8, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(12, 9, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(13, 10, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(9, 6, EdgeType::GLUE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, EdgeType::GLUE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 3, EdgeType::GLUE, &mut checker_edges_induced));
    }

    #[test]
    pub fn test_chawathe_matching_5() -> () {
        // Tree 2
        let mut arena: Arena<String, FromNodeId> = Arena::new();
        let root = arena.new_node(String::from("NUMB"),
                                  String::from("1"),
                                  None,
                                  None,
                                  None,
                                  None);
        // subtree 1
        let n1 = arena.new_node(String::from("NUMB"),
                                String::from("2"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("NUMB"),
                                String::from("3"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("NUMB"),
                                String::from("4"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n1, &mut arena).unwrap();
        let n4 = arena.new_node(String::from("NUMB"),
                                String::from("5"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(n1, &mut arena).unwrap();
        arena.new_node(String::from("INSERT"),
                       String::from("INSERT"),
                       None,
                       None,
                       None,
                       None);
        // Tree 1
        let mut tree1: Arena<String, ToNodeId> = Arena::new();
        let root = tree1.new_node(String::from("NUMB"),
                                  String::from("1"),
                                  None,
                                  None,
                                  None,
                                  None);
        let n2 = tree1.new_node(String::from("NUMB"),
                                String::from("3"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut tree1).unwrap();
        let n1 = tree1.new_node(String::from("NUMB"),
                                String::from("2"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(n2, &mut tree1).unwrap();
        let n3 = tree1.new_node(String::from("NUMB"),
                                String::from("4"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n1, &mut tree1).unwrap();
        let n4 = tree1.new_node(String::from("NUMB"),
                                String::from("5"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(n1, &mut tree1).unwrap();

        tree1.new_node(String::from("DELETE"),
                       String::from("DELETE"),
                       None,
                       None,
                       None,
                       None);

        let matching_config = Config2::new();
        let matcher = matching_config.match_trees(arena.clone(), tree1.clone());
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();

        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 2, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 2, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 3, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 1, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 4, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 2, EdgeType::GLUE, &mut checker_edges_induced));

        let file_location = "./test_AS_5.dot";
    }

    #[test]
    pub fn test_chawathe_matching_6() -> () {
        // Tree 1
        let mut arena: Arena<String, FromNodeId> = Arena::new();
        let root = arena.new_node(String::from("Exp"),
                                  String::from("a"),
                                  None,
                                  None,
                                  None,
                                  None);
        // subtree 1
        let n1 = arena.new_node(String::from("Exp"),
                                String::from("b"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n1_1 = arena.new_node(String::from("Exp"),
                                  String::from("d"),
                                  None,
                                  None,
                                  None,
                                  None);
        n1_1.make_child_of(n1, &mut arena).unwrap();

        // Subtree 2
        let n2 = arena.new_node(String::from("Exp"),
                                String::from("e"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n2_1 = arena.new_node(String::from("Exp"),
                                  String::from("a"),
                                  None,
                                  None,
                                  None,
                                  None);
        n2_1.make_child_of(n2, &mut arena).unwrap();
        let n2_2 = arena.new_node(String::from("Exp"),
                                  String::from("f"),
                                  None,
                                  None,
                                  None,
                                  None);
        n2_2.make_child_of(n2, &mut arena).unwrap();
        // Subtree 3
        let n3 = arena.new_node(String::from("Exp"),
                                String::from("cc"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(root, &mut arena).unwrap();
        let n3_1 = arena.new_node(String::from("Exp"),
                                  String::from("ac"),
                                  None,
                                  None,
                                  None,
                                  None);
        n3_1.make_child_of(n3, &mut arena).unwrap();
        // Subtree 4
        let n4 = arena.new_node(String::from("Exp"),
                                String::from("cd"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(root, &mut arena).unwrap();
        let n4_1 = arena.new_node(String::from("Exp"),
                                  String::from("ad"),
                                  None,
                                  None,
                                  None,
                                  None);
        n4_1.make_child_of(n4, &mut arena).unwrap();
        arena.new_node(String::from("INSERT"),
                       String::from("INSERT"),
                       None,
                       None,
                       None,
                       None);
        // Tree 2
        let mut tree1: Arena<String, ToNodeId> = Arena::new();
        let root = tree1.new_node(String::from("Exp"),
                                  String::from("a"),
                                  None,
                                  None,
                                  None,
                                  None);
        // Subtree 1
        let n1 = tree1.new_node(String::from("Exp"),
                                String::from("f"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut tree1).unwrap();
        let n1_1 = tree1.new_node(String::from("Exp"),
                                  String::from("b"),
                                  None,
                                  None,
                                  None,
                                  None);
        n1_1.make_child_of(n1, &mut tree1).unwrap();
        let n1_2 = tree1.new_node(String::from("Exp"),
                                  String::from("d"),
                                  None,
                                  None,
                                  None,
                                  None);
        n1_2.make_child_of(n1_1, &mut tree1).unwrap();

        // Subtree 2
        let n2 = tree1.new_node(String::from("Exp"),
                                String::from("e"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut tree1).unwrap();
        let n2_1 = tree1.new_node(String::from("Exp"),
                                  String::from("a"),
                                  None,
                                  None,
                                  None,
                                  None);
        n2_1.make_child_of(n2, &mut tree1).unwrap();
        let n2_2 = tree1.new_node(String::from("Exp"),
                                  String::from("f"),
                                  None,
                                  None,
                                  None,
                                  None);
        n2_2.make_child_of(n2, &mut tree1).unwrap();
        let n2_3 = tree1.new_node(String::from("Exp"),
                                  String::from("b"),
                                  None,
                                  None,
                                  None,
                                  None);
        n2_3.make_child_of(n2_2, &mut tree1).unwrap();
        let n2_4 = tree1.new_node(String::from("Exp"),
                                  String::from("d"),
                                  None,
                                  None,
                                  None,
                                  None);
        n2_4.make_child_of(n2_3, &mut tree1).unwrap();

        // Subtree 3

        let n3 = tree1.new_node(String::from("Exp"),
                                String::from("g"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(root, &mut tree1).unwrap();
        let n3_1 = tree1.new_node(String::from("Exp"),
                                  String::from("cc"),
                                  None,
                                  None,
                                  None,
                                  None);
        n3_1.make_child_of(n3, &mut tree1).unwrap();
        let n3_2 = tree1.new_node(String::from("Exp"),
                                  String::from("ac"),
                                  None,
                                  None,
                                  None,
                                  None);
        n3_2.make_child_of(n3_1, &mut tree1).unwrap();
        let n3_3 = tree1.new_node(String::from("Exp"),
                                  String::from("cd"),
                                  None,
                                  None,
                                  None,
                                  None);
        n3_3.make_child_of(n3, &mut tree1).unwrap();
        let n3_4 = tree1.new_node(String::from("Exp"),
                                  String::from("ad"),
                                  None,
                                  None,
                                  None,
                                  None);
        n3_4.make_child_of(n3_3, &mut tree1).unwrap();

        tree1.new_node(String::from("DELETE"),
                       String::from("DELETE"),
                       None,
                       None,
                       None,
                       None);

        let matching_config = Config2::new();
        let mut matcher = matching_config.match_trees(arena.clone(), tree1.clone());
        matcher.update_cost(1, 1, 1, 1, 1, 1, 1, 1);

        let pruning = matching_config.prune_edges(matcher.clone());
        // Pruning
        let mut new_matcher_pruning = matcher.clone();
        // Length Of Edges Induced
        let length_induced_edges = matcher.list_edges.borrow().clone().len();
        //
        let edges_pruned = pruning.0.clone();
        new_matcher_pruning.list_edges = RefCell::new(edges_pruned.clone());

        let mut checker_edges_pruned = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(0, 5, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 1, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 2, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 7, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 2, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 3, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 8, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(3, 3, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(3, 4, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 0, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 4, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 5, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(5, 1, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(5, 5, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(5, 6, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6, 6, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6, 10, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(7, 7, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(7, 11, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8, 8, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8, 12, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(9, 9, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(9, 13, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(10, 9, EdgeType::INSERT, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 7, EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 2, EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8, 12, EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6, 10, EdgeType::GLUE, &mut checker_edges_pruned));
    }
}
