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
use matchers::{has_same_type, has_same_type_and_label};

use std::cell::RefCell;
use std::collections::HashSet;
use std::cmp;
use std::collections::HashMap;
use action::{ActionType, ApplyAction, Copy, Delete, EditScript, Glue, Insert, Move,
             Patchify, Update};
use term;
use patch::{hunkify, Patch};

/// Not needed by matching algorithms, but useful for debugging.
#[derive(Debug, Clone, Eq, Hash, PartialEq, Copy)]
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

#[derive(Debug, Clone, Copy)]
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
    /// Final mappings for nodes when the pruning are done in particular min-edge cover
    pub from: RefCell<HashMap<NodeId<FromNodeId>, (NodeId<ToNodeId>, EdgeType)>>,
    /// Mappings from the destination tree to the source.
    ///
    /// Should contain the same information as `from_map`.
    pub to: RefCell<HashMap<NodeId<ToNodeId>, (NodeId<FromNodeId>, EdgeType)>>,
}

impl<T: Clone + Debug + Eq + 'static> MappingStoreGraph<T> {
    /// Implementation of new in mapping store the second version
    pub fn new(base: Arena<T, FromNodeId>,
               diff: Arena<T, ToNodeId>)
               -> MappingStoreGraph<T> {
        MappingStoreGraph { from_arena: RefCell::new(base),
                            to_arena: RefCell::new(diff),
                            list_edges: RefCell::new(Vec::new()),
                            list_node_inserted: RefCell::new(Vec::new()),
                            list_node_deleted: RefCell::new(Vec::new()),
                            // The cost could change in the future but,
                            // we can make the default cost for every edge
                            // which is cost the value of 1.
                            all_edge_cost: CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1),
                            from: RefCell::new(HashMap::new()),
                            to: RefCell::new(HashMap::new()), }
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
    /// Push a new mapping into the store.
    pub fn push(&self, from: NodeId<FromNodeId>, to: NodeId<ToNodeId>, ty: &EdgeType) {
        self.from.borrow_mut().insert(from, (to, *ty));
        self.to.borrow_mut().insert(to, (from, *ty));
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
        self.to.borrow().get(to).map(|x| x.0)
    }

    /// Get the `NodeId` that `from` is mapped to.
    pub fn get_to(&self, from: &NodeId<FromNodeId>) -> Option<NodeId<ToNodeId>> {
        self.from.borrow().get(from).map(|x| x.0)
    }

    /// Test whether `from` is mapped to `to` in this store.
    pub fn is_mapped(&self, from: &NodeId<FromNodeId>, to: &NodeId<ToNodeId>) -> bool {
        if !self.contains_from(from) {
            return false;
        }
        self.get_to(from).map_or(false, |x| x == *to)
    }

    /// Push New Edge
    pub fn push_edge(&mut self,
                     new_from_node: NodeId<FromNodeId>,
                     new_to_node: NodeId<ToNodeId>,
                     new_value: usize,
                     new_edge_type: EdgeType) {
        let new_edge = Edge::new(new_from_node, new_to_node, new_value, new_edge_type);
        self.list_edges.borrow_mut().push(new_edge);
    }
    /// Remove Edge
    pub fn remove_edge(&mut self, index: usize) {
        self.list_edges.borrow_mut().remove(index);
    }
    /// Check contains the from_node
    pub fn contains_edge_from(&self, from: NodeId<FromNodeId>) -> bool {
        let list_edges = self.list_edges.borrow();
        for edge in list_edges.iter() {
            if edge.from_node == from {
                return true;
            }
        }
        false
    }

    /// Check if the edges contains
    ///     1) from node id &
    ///     2) To node id
    ///     3) Edge Type
    pub fn contains_edge(&self,
                         from: usize,
                         to: usize,
                         edge_type: &EdgeType,
                         list_edges: &[Edge])
                         -> bool {
        for edge in list_edges {
            if edge.from_node.id() == from && edge.to_node.id() == to
               && edge.edge_type == *edge_type
            {
                return true;
            }
        }
        false
    }

    /// Check if the edge contains the to_node
    pub fn contains_edge_to(&self, to: NodeId<ToNodeId>) -> bool {
        let list_edges = self.list_edges.borrow();
        for edge in list_edges.iter() {
            if edge.to_node == to {
                return true;
            }
        }
        false
    }

    /// Print for all the pre order traversal tree2: <Vec<NodeId<ToNodeId>>>
    pub fn print_preorder_traversal(&self,
                                    tree1: Vec<NodeId<FromNodeId>>,
                                    tree2: Vec<NodeId<ToNodeId>>)
                                    -> () {
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
        if has_same_type_and_label(&from_node,
                                   &self.from_arena.borrow(),
                                   &to_node,
                                   &self.to_arena.borrow())
        {
            if descendants_from_node.len() == descendants_to_node.len() {
                for i in 0..descendants_to_node.len() {
                    if !has_same_type_and_label(&descendants_from_node[i],
                                                &self.from_arena.borrow(),
                                                &descendants_to_node[i],
                                                &self.to_arena.borrow())
                    {
                        // This means the descendants are !perfect by label and expression
                        return false;
                    }
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
        true
    }
    /// Check if the subtree is deleted from from_node against to_node.
    /// This checks whether the the descendants are in the deletion edges.
    pub fn check_tree_deletion(&self,
                               from_node_ancestor: NodeId<FromNodeId>,
                               delete_tree2: NodeId<ToNodeId>,
                               hash_set_deletion: &HashSet<(NodeId<FromNodeId>,
                                        NodeId<ToNodeId>)>)
                               -> bool {
        let mut output = true;
        let descendants_from_node =
            from_node_ancestor.descendants(&self.from_arena.borrow())
                              .collect::<Vec<NodeId<FromNodeId>>>();
        for descendant in descendants_from_node {
            if !hash_set_deletion.contains(&(descendant, delete_tree2)) {
                output = false;
            }
        }
        output
    }

    /// check if subtree 1 exists in tree 2.
    /// This checks from node exists in the same position in tree 2 (To Node Id)
    /// If it does then true otherwise false.
    pub fn check_tree_1_exists_in_tree2(&self,
                                        from_node_ancestor: NodeId<FromNodeId>)
                                        -> bool {
        let mut output = true;
        let node_id_to_node = from_node_ancestor.id();
        if node_id_to_node >= self.to_arena.borrow().size() {
            return false;
        }
        let to_node = NodeId::<ToNodeId>::new(node_id_to_node);
        self.check_subtree(from_node_ancestor, to_node)
    }
    /// Check if the descendants of to_node are in the insertion edges.
    /// From node would be the insertion node.
    pub fn check_tree_insertion(&self,
                                insert_tree1: NodeId<FromNodeId>,
                                to_node: NodeId<ToNodeId>,
                                hash_set_insertion: &HashSet<(NodeId<FromNodeId>,
                                         NodeId<ToNodeId>)>)
                                -> bool {
        let mut output = true;
        let descendants_from_node = to_node.descendants(&self.to_arena.borrow())
                                           .collect::<Vec<NodeId<ToNodeId>>>();
        for descendant in descendants_from_node {
            if !hash_set_insertion.contains(&(insert_tree1, descendant)) {
                output = false;
            }
        }
        output
    }

    /// Converts vector(To_node_id) to hash set
    pub fn vector_to_hash_set_to_node_id(&self,
                                         tree2: &[NodeId<ToNodeId>])
                                         -> HashSet<NodeId<ToNodeId>> {
        let mut hashset = HashSet::<NodeId<ToNodeId>>::new();
        for node in tree2.to_owned() {
            hashset.insert(node);
        }
        hashset
    }

    /// Converts vector(From_node_id) to hash set
    pub fn vector_to_hash_set_from_node_id(&self,
                                           tree2: &[NodeId<FromNodeId>])
                                           -> HashSet<NodeId<FromNodeId>> {
        let mut hashset = HashSet::<NodeId<FromNodeId>>::new();
        for node in tree2.to_owned() {
            hashset.insert(node);
        }
        hashset
    }

    /// Calculate lower bound cost
    pub fn lower_bound_edge(self, edge1: &Edge, edges: Vec<Edge>) -> usize {
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
        let m = edge1.from_node;
        let n = edge1.to_node;
        // Sum of Cost of forced move with m',n where m' is the child of m
        let m_children =
            m.children(&self.from_arena.borrow()).collect::<Vec<NodeId<FromNodeId>>>();
        let n_children =
            n.children(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>();
        let mut cmf_m1_n = 0; // Cost of sum of Cmf(m',n)
        let mut cmf_m_n1 = 0; // Cost of sum of Cmf(m,n')
        for m_child in &m_children {
            let mut calculate_cost = true;
            // If n_child does not exist.
            // If M_child & n_child doesn't exist IN Edges.
            // If it exists then there is no calculate cost.
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
        cost_update + ((cmp::min(cmf_m1_n, cmf_m_n1)) / 2)
    }

    /// Calculate lower bound cost
    pub fn upper_bound_edge(self, edge1: &Edge, edges: &[Edge]) -> usize {
        // Vector to hash map
        let mut edges_hash_map = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        for edge in edges.to_owned() {
            edges_hash_map.insert((edge.from_node, edge.to_node));
        }
        // Cost of the lower bound is
        // Addition of Cost of updating M -> N
        // Addition of 1/2 * min ()

        // Get the cost of update
        let cost_update = self.all_edge_cost.update;
        let cost_move = self.all_edge_cost.move_;
        let m = edge1.from_node;
        let n = edge1.to_node;
        // Sum of Cost of forced move with m',n where m' is the child of m
        let mut m_children: Vec<NodeId<FromNodeId>> = vec![];
        if !m.is_leaf(&self.from_arena.borrow()) {
            m_children = m.children(&self.from_arena.borrow()).collect::<Vec<NodeId<FromNodeId>>>();
        }
        let mut n_children: Vec<NodeId<ToNodeId>> = vec![];
        if !n.is_leaf(&self.to_arena.borrow()) {
            let n_children =
                n.children(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>();
        }
        let mut cm_m1_n = 0; // Cost of sum of Cmf(m',n)
        let mut cm_m_n1 = 0; // Cost of sum of Cmf(m,n')
        for m_child in &m_children {
            let mut calculate_cost = false;
            // Count of number of edges
            let mut em1 = 0; // E(m')
            for e1 in edges.to_owned() {
                if *m_child == e1.from_node {
                    em1 += 1;
                }
            }
            if em1 > 0 {
                em1 -= 1;
            }
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
            for e1 in edges.to_owned() {
                if *n_child == e1.to_node {
                    en1 += 1;
                }
            }
            if en1 > 0 {
                en1 -= 1;
            }
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
        cost_cw + ((cm_m1_n) + (cm_m_n1)) / 2
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
        for (index, edge) in self.list_edges.borrow().iter().enumerate() {
            println!(" -- {:?} -- ", index);
            println!("TY        :   From node    -> {:?} || to node -> {:?}",
                     self.from_arena.borrow()[edge.from_node].ty,
                     self.to_arena.borrow()[edge.to_node].ty);
            println!("Value     :   From node -> {:?} || to node -> {:?}",
                     self.from_arena.borrow()[edge.from_node].label,
                     self.to_arena.borrow()[edge.to_node].label);
            println!("From node -> {:?} || to node -> {:?} || Edge type -> {:?}",
                     edge.from_node, edge.to_node, edge.edge_type);
            println!("--");
        }
    }
}

/// The Config2 matcher needs no configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct Config2 {}

/// Uses the edges and makes hash map that used for min edge cover solver.
/// 1 - To Node << Mapping to >> From Nodes, Edge Type, Cost.
/// 2 - From Node << Mapping to >> To Nodes, Edge Type, Cost.
pub fn edgecover_helper(
    new_matcher_pruning: &MappingStoreGraph<String>)
    -> (HashMap<NodeId<ToNodeId>,
                (HashSet<(NodeId<FromNodeId>, EdgeType, usize)>, usize)>,
       HashMap<NodeId<FromNodeId>,
                (HashSet<(NodeId<ToNodeId>, EdgeType, usize)>, usize)>) {
    // To Node -> From Node
    let mut list_t_f: HashMap<NodeId<ToNodeId>,
                              (HashSet<(NodeId<FromNodeId>, EdgeType, usize)>,
                              usize)> = HashMap::new();
    for edge1 in new_matcher_pruning.list_edges.borrow().clone() {
        let mut set: HashSet<(NodeId<FromNodeId>, EdgeType, usize)> = HashSet::new();
        let mut size: usize = 1;
        for edge2 in new_matcher_pruning.list_edges.borrow().clone() {
            if !(edge1.from_node == edge2.from_node && edge1.to_node == edge2.to_node
                 && edge1.edge_type == edge2.edge_type)
            {
                if edge2.to_node == edge1.to_node {
                    let temp: NodeId<FromNodeId> = edge2.from_node;
                    set.insert((temp, edge2.edge_type, edge2.value));
                    size += 1;
                }
            } else {
                let temp: NodeId<FromNodeId> = edge2.from_node;
                set.insert((temp, edge2.edge_type, edge2.value));
            }
        }
        let temp_to_node: NodeId<ToNodeId> = edge1.to_node;
        list_t_f.insert(temp_to_node, (set, size));
    }

    // From_ Node -> To Node
    let mut list_f_t: HashMap<NodeId<FromNodeId>,
                              (HashSet<(NodeId<ToNodeId>, EdgeType, usize)>, usize)> =
        HashMap::new();
    for edge1 in new_matcher_pruning.list_edges.borrow().clone() {
        let mut set: HashSet<(NodeId<ToNodeId>, EdgeType, usize)> = HashSet::new();
        let mut size: usize = 1;
        for edge2 in new_matcher_pruning.list_edges.borrow().clone() {
            if !(edge1.from_node == edge2.from_node && edge1.to_node == edge2.to_node
                 && edge1.edge_type == edge2.edge_type)
            {
                if edge2.from_node == edge1.from_node {
                    let temp: NodeId<ToNodeId> = edge2.to_node;
                    set.insert((temp, edge2.edge_type, edge2.value));
                    size += 1;
                }
            } else {
                let temp: NodeId<ToNodeId> = edge2.to_node;
                set.insert((temp, edge2.edge_type, edge2.value));
            }
        }
        let temp_to_node: NodeId<FromNodeId> = edge1.from_node;
        list_f_t.insert(temp_to_node, (set, size));
    }
    (list_t_f, list_f_t)
}

/// Min Edge Cover Solver Substitution.
/// Gets the removed edges and adds their available subtitution.
pub fn edgecover_annotate(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    // Algorithm [2.2.1]
    // Stage 1: Get all small edges with lowest cost.
    // Stage 2: Get all edges who has the same cost and pick by the edge type.
    //          If they are same edge type pick the first one.
    // Stage 3: Get all the remove edges and find the substitution.

    // Remove all the move opeartions
    // If they have a glue or copy ancestor
    let matcher_pruner =
        edgecover_annotate_helper_remove_move(new_matcher_pruning.clone());
    let returned_mapper = edgecover_helper(&matcher_pruner);
    // List of all remove edges.
    let mut all_removed_edges: HashSet<(NodeId<FromNodeId>,
                                       NodeId<ToNodeId>,
                                       EdgeType,
                                       usize)> = HashSet::new();
    // List of remaining edges.
    let mut all_remaining_edges: HashSet<(NodeId<FromNodeId>,
                                         NodeId<ToNodeId>,
                                         EdgeType,
                                         usize)> = HashSet::new();
    // Loop through all the from_node -> to_node who have multiple edges.
    for (key, val) in returned_mapper.1 {
        // val (0 -- To nodes) , (1 -- size of To Nodes)
        if val.1 > 1 {
            let mut curr_edges: Option<(NodeId<FromNodeId>,
                                       NodeId<ToNodeId>,
                                       EdgeType,
                                       usize)> = None;
            let mut min_edge_value = <usize>::max_value();
            // Loop through all the To_nodes
            for &(to_node, ref edge_type1, edge_cost) in &val.0.clone() {
                let edge_type = edge_type1;
                if min_edge_value == <usize>::max_value() {
                    curr_edges = Some((key, to_node, *edge_type, edge_cost));
                    min_edge_value = edge_cost;
                } else {
                    if !curr_edges.is_some() {
                        continue;
                    } else {
                        let current_edge = curr_edges.unwrap();
                        // Look at if the current edge cost is smaller
                        if min_edge_value == current_edge.3 {
                            // Look at edges if the current edge has the same cost as the loop edges.
                            // If loop_edge is group of lower edges pick the lower edges.
                            if (current_edge.2 != EdgeType::UPDATE
                                || current_edge.2 != EdgeType::INSERT
                                || current_edge.2 != EdgeType::DELETE)
                               && (*edge_type == EdgeType::UPDATE
                                   || *edge_type == EdgeType::INSERT
                                   || *edge_type == EdgeType::DELETE)
                            {
                                all_removed_edges.insert(current_edge);
                                curr_edges = Some((key, to_node, *edge_type, edge_cost));
                                min_edge_value = edge_cost;
                            } else {
                                all_removed_edges.insert((key,
                                                         to_node,
                                                         *edge_type,
                                                         edge_cost));
                            }
                        } else if min_edge_value < current_edge.3 {
                            all_removed_edges.insert(current_edge);
                            curr_edges = Some((key, to_node, *edge_type, edge_cost));
                            min_edge_value = edge_cost;
                        } else {
                            all_removed_edges.insert((key,
                                                     to_node,
                                                     *edge_type,
                                                     edge_cost));
                        }
                    }
                }
            }
            all_remaining_edges.insert(curr_edges.unwrap());
        } else {
            for &(to_node, ref edge_type1, edge_cost) in &val.0.clone() {
                let edge_type = edge_type1;
                all_remaining_edges.insert((key, to_node, *edge_type, edge_cost));
            }
        }
    }
    // All the removed edges should have their own replacement.
    // Insert Node
    let last_node_from_node_insert =
        NodeId::<FromNodeId>::new(new_matcher_pruning.from_arena.borrow().size() - 1);
    for &(_, to, edge_type, cost) in &all_removed_edges {
        if edge_type == EdgeType::MOVE {
            // Direct insert.
            if !all_remaining_edges.contains(&(last_node_from_node_insert,
                                             to,
                                             EdgeType::INSERT,
                                             cost))
            {
                all_remaining_edges.insert((last_node_from_node_insert,
                                           to,
                                           EdgeType::INSERT,
                                           cost));
            }
        } else if edge_type == EdgeType::GLUE {
            let all_descendants = to.descendants(&new_matcher_pruning.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>();
            if !all_remaining_edges.contains(&(last_node_from_node_insert,
                                             to,
                                             EdgeType::INSERT,
                                             cost))
            {
                all_remaining_edges.insert((last_node_from_node_insert,
                                           to,
                                           EdgeType::INSERT,
                                           cost));
            }
            for to_node_descendants in all_descendants {
                if !all_remaining_edges.contains(&(last_node_from_node_insert,
                                                 to_node_descendants,
                                                 EdgeType::INSERT,
                                                 cost))
                {
                    all_remaining_edges.insert((last_node_from_node_insert,
                                               to_node_descendants,
                                               EdgeType::INSERT,
                                               cost));
                }
            }
        }
    }
    // Create new edges
    let mut vec_edges: Vec<Edge> = Vec::new();
    for (from, to, edge_type, cost) in all_remaining_edges {
        let edge = Edge::new(from, to, cost, edge_type);
        vec_edges.push(edge);
    }
    new_matcher_pruning.list_edges = RefCell::new(vec_edges);
}

/// Apply edit script to the from_arena
pub fn edgecover_apply_edit_script(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    // Final Edit script
    let mut script: EditScript<String> = EditScript::new();
    let root_to = new_matcher_pruning.to_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.to_arena.borrow().clone();
    let bfs_root_to_tree = root_to.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search
    let check_mapper = edgecover_helper(new_matcher_pruning); // Mappings (1) To -> From    (2) From -> To
    let bfs_root_to_tree_node = root_to.breadth_first_traversal(reference_matcher); // Root to Traversal BFS Search Copy
    let mut from_arena = new_matcher_pruning.from_arena.borrow_mut();
    let mut to_arena = new_matcher_pruning.to_arena.borrow();
    // Parent Mapping hash map.
    let mut mapping_t_f: HashMap<NodeId<ToNodeId>, NodeId<FromNodeId>> = HashMap::new();
    // Do Delete Edit operation first.
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if edge.edge_type == EdgeType::DELETE {
            let mut delete_action = Delete::new(edge.from_node);
            delete_action.apply(&mut from_arena);
            script.push(delete_action);
        }
    }
    for to_node in bfs_root_to_tree_node {
        let get_edge = (check_mapper.0).get(&to_node);
        if get_edge.is_some() {
            for &(from, _, _) in &get_edge.unwrap().0 {
                mapping_t_f.insert(to_node, from);
            }
        }
    }
    let mut indexes_t_f: HashMap<usize, usize> = HashMap::new();
    let mut hash_from_ids_inserted: HashSet<NodeId<FromNodeId>> = HashSet::new();
    // Apply all the edit script operation.
    for to_node in bfs_root_to_tree {
        let get_edge = (check_mapper.0).get(&to_node);
        if get_edge.is_some() {
            for &(from, edge_type, _) in &get_edge.unwrap().0 {
                if edge_type == EdgeType::MOVE {
                    let mut parent_from_node: NodeId<FromNodeId>;
                    let wrapper = mapping_t_f.get(&to_arena[to_node].parent().unwrap());
                    if wrapper.is_some() {
                        parent_from_node = *wrapper.unwrap();
                    } else {
                        let parent_index = to_arena[to_node].parent().unwrap().id();
                        parent_from_node = NodeId::<FromNodeId>::new(parent_index);
                    }
                    // Check if the from node parent exists
                    if new_matcher_pruning.contains_edge_from(parent_from_node) {
                        let get_position = to_node.get_child_position(&(to_arena));
                        // if the parent exists then do the operation
                        // if the parent are the same and the position are the same then we dont need to perform the operation
                        let parent_from_node_checker = from_arena[from].parent().unwrap();
                        if !(from_arena[parent_from_node_checker].ty
                             == from_arena[parent_from_node].ty
                             && from_arena[parent_from_node_checker].label
                                == from_arena[parent_from_node].label
                             && from.get_child_position(&from_arena).unwrap()
                                == (get_position.unwrap())
                             && parent_from_node_checker == parent_from_node)
                        {
                            let mut move_action = Move::new(from,
                                                            parent_from_node,
                                                            get_position.unwrap() as u16);
                            move_action.apply(&mut from_arena);
                            script.push(move_action);
                            hash_from_ids_inserted.insert(from);
                        }
                        // The if above checks if they were in the same position if they are not then move otherwise "Perform the move operation"
                        indexes_t_f.insert(to_node.id(), from.id());
                    }
                } else if edge_type == EdgeType::GLUE {
                    let get_position = to_node.get_child_position(&(to_arena));
                    // Get Parent
                    let mut parent_from_node: NodeId<FromNodeId>;
                    let parent_index_1 = to_arena[to_node].parent().unwrap().id();
                    let wrapper = mapping_t_f.get(&to_arena[to_node].parent().unwrap());
                    if wrapper.is_some() {
                        parent_from_node = *wrapper.unwrap();
                    } else {
                        let parent_index = to_arena[to_node].parent().unwrap().id();
                        parent_from_node = NodeId::<FromNodeId>::new(parent_index);
                    }
                    // Check if the from node parent exists
                    if new_matcher_pruning.contains_edge_from(parent_from_node) {
                        // if the parent exists then do the operation
                        let index_parent_from_index;
                        match get_index_from_node(&indexes_t_f.clone(), parent_index_1) {
                            Some(x) => index_parent_from_index = *x,
                            None => index_parent_from_index = from_arena.size() + 1,
                        };

                        if index_parent_from_index < from_arena.size() {
                            let from_node_parent: NodeId<FromNodeId> = NodeId::new(index_parent_from_index);
                            let mut glue_action = Glue::new(from,
                                                            from_node_parent,
                                                            get_position.unwrap() as u16);
                            glue_action.apply(&mut from_arena);
                            script.push(glue_action);
                        }
                        indexes_t_f.insert(to_node.id(), from.id());
                    }
                } else if edge_type == EdgeType::COPY {
                    let get_position = to_node.get_child_position(&(to_arena));
                    // Get Parent
                    let parent_from_node: NodeId<FromNodeId>;
                    let wrapper = mapping_t_f.get(&to_arena[to_node].parent().unwrap());
                    if wrapper.is_some() {
                        parent_from_node = *wrapper.unwrap();
                    } else {
                        let parent_index = to_arena[to_node].parent().unwrap().id();
                        parent_from_node = NodeId::<FromNodeId>::new(parent_index);
                    }
                    // Check if the from node parent exists
                    if new_matcher_pruning.contains_edge_from(parent_from_node) {
                        // if the parent exists then do the operation
                        let mut copy_action = Copy::new(from,
                                                        parent_from_node,
                                                        get_position.unwrap() as u16);
                        copy_action.apply(&mut from_arena);
                        script.push(copy_action);
                        indexes_t_f.insert(to_node.id(), from.id());
                    }
                } else if edge_type == EdgeType::INSERT {
                    let get_position = to_node.get_child_position(&(to_arena));
                    // Get Parent
                    let parent_index_1 = to_arena[to_node].parent().unwrap().id();
                    let index_parent_from_index;
                    match get_index_from_node(&indexes_t_f.clone(), parent_index_1) {
                        Some(x) => index_parent_from_index = *x,
                        None => index_parent_from_index = from_arena.size() + 1,
                    };
                    if index_parent_from_index < from_arena.size() {
                        let from_node_parent: NodeId<FromNodeId> =
                            NodeId::new(index_parent_from_index);
                        let mut new_node: NodeId<FromNodeId> =
                            from_arena.new_node(to_arena[to_node].ty.clone(),
                                                to_arena[to_node].label.clone(),
                                                to_arena[to_node].col_no,
                                                to_arena[to_node].line_no,
                                                None,
                                                None);
                        let mut insert_action = Insert::new(new_node,
                                                            Some(from_node_parent),
                                                            get_position.unwrap() as u16);
                        // Parent exists
                        insert_action.apply(&mut from_arena);
                        script.push(insert_action);
                        indexes_t_f.insert(to_node.id(), new_node.id());
                    }
                } else if edge_type == EdgeType::UPDATE {
                    // Check if the from node parent exists
                    let parent_from_node: NodeId<FromNodeId>;
                    let wrapper = mapping_t_f.get(&to_arena[to_node].parent().unwrap());
                    if wrapper.is_some() {
                        parent_from_node = *wrapper.unwrap();
                    } else {
                        let parent_index = to_arena[to_node].parent().unwrap().id();
                        parent_from_node = NodeId::<FromNodeId>::new(parent_index);
                    }
                    if new_matcher_pruning.contains_edge_from(parent_from_node) {
                        // if the parent exists then do the operation
                        let mut update_action = Update::new(from,
                                                            to_arena[to_node].ty.clone(),
                                                            to_arena[to_node].label
                                                                             .clone());
                        update_action.apply(&mut from_arena);
                        script.push(update_action);
                        indexes_t_f.insert(to_node.id(), from.id());
                    }
                } else if edge_type == EdgeType::OK {
                    indexes_t_f.insert(to_node.id(), from.id());
                }
            }
        }
    }
}

/// Min Edge Cover Solver.
/// Uses the remaining pruned edges and gets the ones with the lowest cost or edge type.
fn edgecover_solver(
    mut new_matcher_pruning: MappingStoreGraph<String>)
    -> (MappingStoreGraph<String>,
       HashMap<NodeId<ToNodeId>,
                (HashSet<(NodeId<FromNodeId>, EdgeType, usize)>, usize)>,
       HashMap<NodeId<FromNodeId>,
                (HashSet<(NodeId<ToNodeId>, EdgeType, usize)>, usize)>) {
    // Tree 1 = T1
    // Tree 1 nodes = T1N_0....100000
    // Same for Tree 2.
    // Cases
    //
    //  Case 1: T1_1 && T1_2 map to T2_1
    //
    //    1) Check if whether they are single mapping or not
    //      1.1) There doesn't exist a mapping that has
    //          >>>>> T1_1 --(Maps To) -->  Something That is not T2_1
    //                 ____ Same For T2_1
    //
    //      1.2) If one them are true then choose the one with the single mapping
    //           and prune the the other one.
    //  Case 2: "The Cost"
    //     2.1) If Both are Single mappings Pick One that is cheaper
    //     2.2) Same Cost Choose One Depending on type
    //
    //  Case 3: Need To Think More
    let get_cover_mapping = edgecover_helper(&new_matcher_pruning);
    // Checking To Node edges with more than size 1
    let mut get_all_1_job_2_worker: HashMap<NodeId<ToNodeId>,
                                            (HashSet<(NodeId<FromNodeId>,
                                                     EdgeType,
                                                     usize)>,
                                            usize)> = HashMap::new();
    // Results For Edges
    let mut result_unchanged: HashSet<(NodeId<FromNodeId>,
                                      NodeId<ToNodeId>,
                                      EdgeType,
                                      usize)> = HashSet::new();
    for (key, val) in get_cover_mapping.0.clone() {
        if val.1 > 1 {
            get_all_1_job_2_worker.insert(key, val);
        } else {
            for set in &val.0 {
                result_unchanged.insert((set.0, key, set.1, set.2));
            }
        }
    }
    // Case 1 -- Remove Double mappings and keep single ones
    // Data structure to keep track of edges to remove
    let mut keeping_edges: HashSet<(NodeId<FromNodeId>,
                                   NodeId<ToNodeId>,
                                   EdgeType,
                                   usize)> = HashSet::new();
    for (key, val) in &get_all_1_job_2_worker {
        // Loop through all From Nodes Connected to This "ToNodeId"
        let mut set_of_one_to_one_mapping: HashSet<(NodeId<FromNodeId>,
                                                   NodeId<ToNodeId>,
                                                   EdgeType,
                                                   usize)> = HashSet::new();
        for &(from_node, edge, value) in &val.0 {
            // Only one to one mapping exists
            if &get_cover_mapping.1[&from_node].1 <= &1 {
                set_of_one_to_one_mapping.insert((from_node, (*key), edge, value));
            }
        }
        // We have a collections of from_nodes -> To Node which have one to one mapping
        // Keep the one with the lowest Cost
        let mut sets_of_lowest_cost: HashSet<(NodeId<FromNodeId>,
                                             NodeId<ToNodeId>,
                                             EdgeType,
                                             usize)> = HashSet::new();
        let mut lowest_cost = <usize>::max_value();
        // Get the lowest edge value
        for (from_node, to_node, edge, value) in set_of_one_to_one_mapping {
            if lowest_cost > value {
                sets_of_lowest_cost.clear();
                sets_of_lowest_cost.insert((from_node, to_node, edge, value));
            } else if lowest_cost == value {
                sets_of_lowest_cost.insert((from_node, to_node, edge, value));
            }
        }
        // List of edges with the same cost then pick the one with the edge type.
        // If they are the same edge type pick the first one.
        let mut result_edge: Option<(NodeId<FromNodeId>,
                                    NodeId<ToNodeId>,
                                    EdgeType,
                                    usize)> = None;
        let mut check_base1 = false;
        let mut check_base2 = false;
        for (from_node, to_node, edge1, value) in sets_of_lowest_cost {
            let edge = edge1;
            match result_edge {
                None => result_edge = Some((from_node, to_node, edge, value)),
                Some(_) => {}
            }
            check_base1 = false;
            if edge == EdgeType::INSERT || edge == EdgeType::DELETE
               || edge == EdgeType::UPDATE
            {
                check_base1 = true
            } else if edge == EdgeType::COPY || edge == EdgeType::MOVE
                      || edge == EdgeType::GLUE
            {
                check_base2 = true;
            }
            let result_edge_1_type = result_edge.unwrap().2;
            if (result_edge_1_type == EdgeType::COPY
                || result_edge_1_type == EdgeType::MOVE
                || result_edge_1_type == EdgeType::GLUE) && check_base1
            {
                result_edge = Some((from_node, to_node, edge, value));
            }
        }

        keeping_edges.insert(result_edge.unwrap());
    }
    for (from_node, to_node, edge, value) in keeping_edges {
        result_unchanged.insert((from_node, to_node, edge, value));
    }
    let mut new_edges = new_matcher_pruning.list_edges.borrow().clone();
    // List of index to remove
    let list_tuple_checker = result_unchanged;
    let _iter = new_edges.clone();
    let mut new_edges_final: Vec<Edge> = Vec::new();
    for edge in _iter.iter() {
        if list_tuple_checker.contains(&(edge.from_node,
                                       edge.to_node,
                                       edge.edge_type,
                                       edge.value))
        {
            let e = Edge::new(edge.from_node, edge.to_node, edge.value, edge.edge_type);
            new_edges_final.push(e);
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(new_edges_final.clone());
    let returner_mapper = edgecover_helper(&new_matcher_pruning);
    edgecover_annotate(&mut new_matcher_pruning);
    edgecover_apply_edit_script(&mut new_matcher_pruning);
    (new_matcher_pruning, returner_mapper.0, returner_mapper.1)
}

/// Remove all edges of copy and glue that have move as their descendants
pub fn edgecover_annotate_helper_remove_move(mut new_matcher_pruning : MappingStoreGraph<String>) -> MappingStoreGraph<String>{
    let mut all_edges = new_matcher_pruning.list_edges.borrow_mut().clone(); // Vec<Edge> -- type
    let all_edge_run = all_edges.clone();
    let mut remove_index: HashSet<(NodeId<FromNodeId>,
                                  NodeId<ToNodeId>,
                                  EdgeType,
                                  usize)> = HashSet::new();
    // get list of moves and remove them.
    for edge in &all_edge_run {
        if edge.edge_type == EdgeType::COPY || edge.edge_type == EdgeType::GLUE {
            // Look at their descendants
            let all_from_descendants =
                edge.from_node.descendants(&new_matcher_pruning.from_arena.borrow())
                    .collect::<HashSet<NodeId<FromNodeId>>>();
            // counter
            let mut counter: usize = 0;
            for edge_move in all_edge_run.clone() {
                // Look for edge move
                if edge_move.edge_type == EdgeType::MOVE
                   && all_from_descendants.contains(&edge_move.from_node)
                {
                    if !remove_index.contains(&(edge_move.from_node,
                                              edge_move.to_node,
                                              edge_move.edge_type,
                                              edge_move.value))
                    {
                        remove_index.insert((edge_move.from_node,
                                            edge_move.to_node,
                                            edge_move.edge_type,
                                            edge_move.value));
                    }
                }
                counter += 1;
            }
        }
    }

    let mut all_edges_output: Vec<Edge> = Vec::new();
    for edge_iter in all_edges.clone() {
        if !remove_index.contains(&(edge_iter.from_node,
                                  edge_iter.to_node,
                                  edge_iter.edge_type,
                                  edge_iter.value))
        {
            all_edges_output.push(edge_iter);
        }
    }

    new_matcher_pruning.list_edges = RefCell::new(all_edges_output.to_vec()); // Make reassign the list edges
    new_matcher_pruning
}

// ********** START - Printing in terminal ************
/// Testing to be made to show the edit script operations visually to the end user in the terminal.
pub fn write_diff_to_terminal(store: &MappingStoreGraph<String>,
                              script: &EditScript<String>,
                              from_path: &str,
                              to_path: &str) {
    let colours = build_colour_map();
    // Patches
    // Turn edit script and mappings into hunks of related patches.
    let mut from_patches: Vec<Patch> = vec![];
    let mut to_patches: Vec<Patch> = vec![];
    script.patchify_chawathe98(store, &mut from_patches, &mut to_patches);
    // Check if patches are empty
    if from_patches.is_empty() && to_patches.is_empty() {}
}

// Map action types to terminal colours.
fn build_colour_map() -> HashMap<ActionType, term::color::Color> {
    let mut map: HashMap<ActionType, term::color::Color> = HashMap::new();
    map.insert(ActionType::DELETE, term::color::BRIGHT_RED);
    map.insert(ActionType::INSERT, term::color::BRIGHT_GREEN);
    map.insert(ActionType::MOVE, term::color::BRIGHT_BLUE);
    map.insert(ActionType::UPDATE, term::color::BRIGHT_YELLOW);
    map.insert(ActionType::COPY, term::color::BRIGHT_MAGENTA);
    map.insert(ActionType::GLUE, term::color::BRIGHT_CYAN);
    map
}
// ********* END  - Printing in terminal ************

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

/// Gets the corresponding From Node Index using the To Node Index.
pub fn get_index_from_node(map: &HashMap<usize, usize>,
                           to_node: usize)
                           -> Option<&usize> {
    map.get(&to_node)
}

impl<T: Clone + Debug + Eq + 'static> MatchingTreesScriptor<T> for Config2 {
    /// Perform matches
    fn match_trees(&self,
                   base: Arena<T, FromNodeId>,
                   diff: Arena<T, ToNodeId>)
                   -> MappingStoreGraph<T> {
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
        let mut set_contains_insertion =
            HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        let mut set_contains_deletion =
            HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        for (from_node_id, from_node) in base_pre.iter().enumerate() {
            let mut bool_check = false;
            for (to_node_id, to_node) in diff_pre.iter().enumerate() {
                if has_same_type_and_label(from_node,
                                           &store.from_arena.borrow(),
                                           to_node,
                                           &store.to_arena.borrow())
                {
                    if from_node_id == to_node_id {
                        // Perfect matching
                        store.push(*from_node, *to_node, 1, EdgeType::OK);
                    } else {
                        // The nodes have the same value
                        store.push(*from_node, *to_node, 1, EdgeType::MOVE)
                    }
                    bool_check = true;
                } else if has_same_type(from_node,
                                        &store.from_arena.borrow(),
                                        to_node,
                                        &store.to_arena.borrow())
                          && from_node_id == to_node_id
                {
                    // the same label and value but different position
                    store.push(*from_node, *to_node, 1, EdgeType::UPDATE);
                }

                // We have reached the last node in the diff tree
                if !bool_check && to_node_id == diff_pre.len() - 1 {
                    store.delete_node(base_pre[from_node_id]);
                    store.push_edge(base_pre[from_node_id],
                                    last_node_to_node,
                                    1,
                                    EdgeType::DELETE);
                    set_contains_deletion.insert((base_pre[from_node_id],
                                                 last_node_to_node));
                }
            }
        }
        for to_node_id in 0..diff_pre.len() {
            let mut bool_check = false;
            for (from_node_id, from_node) in base_pre.iter().enumerate() {
                if has_same_type_and_label(from_node,
                                           &store.from_arena.borrow(),
                                           to_node,
                                           &store.to_arena.borrow())
                {
                    bool_check = true;
                }
                if !bool_check && from_node_id == base_pre.len() - 1 {
                    store.push_edge(last_node_from_node,
                                    diff_pre[to_node_id],
                                    1,
                                    EdgeType::INSERT);
                    set_contains_insertion.insert((last_node_from_node,
                                                  diff_pre[to_node_id]));
                }
            }
        }
        let get_all_edges = store.list_edges.borrow().clone();
        let mut all_edges_move = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType)>::new();
        let mut all_leaves_move =
            HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType)>::new();
        for edge in get_all_edges.clone() {
            if edge.edge_type == EdgeType::MOVE {
                let edge_copy = edge.clone();
                all_edges_move.insert((edge.from_node, edge.to_node, edge.edge_type));
                let m_check = edge.from_node.is_leaf(&store.from_arena.borrow());
                let n_check = edge.to_node.is_leaf(&store.to_arena.borrow());
                if m_check && n_check {
                    all_leaves_move.insert((edge_copy.from_node,
                                           edge_copy.to_node,
                                           edge_copy.edge_type));
                }
            }
        }
        let mut all_leaves_parent =
            HashSet::<(NodeId<FromNodeId>,
                      NodeId<ToNodeId>,
                      NodeId<FromNodeId>,
                      NodeId<ToNodeId>)>::new();
        // Check if its nodes are in the leaf move operation
        for edge in all_leaves_move.clone() {
            let e1_parent = store.from_arena.borrow()[edge.0].parent().unwrap(); // edge.0.parent;
            let e1_siblings = e1_parent.children(&store.from_arena.borrow())
                                       .collect::<Vec<NodeId<FromNodeId>>>();
            // edge to node parent
            let e1_to_node_parent = store.to_arena.borrow()[edge.1].parent().unwrap();

            let size_e1_siblings = e1_siblings.len();
            for _ in e1_siblings {
                // are the siblings map to the same subtree with the parent being the same
                let mut check_siblings_mapped_parent = 0;
                // Check if siblings exists in move hashset
                // loop through all the from node's siblings
                // The main goal is to check if the move operation for its siblings exists
                // 1. loop though all the move leaves operations
                //  1.1) Checks
                //     1.1.1) if the parent the same as the current edge (From nodes)
                //     1.1.2) if the parent the same as the current edge (to nodes)
                //    By checking all the leaves and its parents.
                for _ in all_leaves_move.clone() {
                    if store.check_subtree(e1_parent, e1_to_node_parent) {
                        check_siblings_mapped_parent += 1;
                    }
                }
                if check_siblings_mapped_parent > 0 {
                    // The sibling do map to the same move node
                    if check_siblings_mapped_parent >= size_e1_siblings {
                        all_leaves_parent.insert((e1_parent,
                                                 e1_to_node_parent,
                                                 edge.0,
                                                 edge.1));
                    }
                } else {
                    break;
                }
            }
        }
        // Loop through parents and check if they are in move hash set
        let mut end_copy_tree = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        for edge in all_leaves_parent.clone() {
            let mut edge_copy = edge;
            let mut edge_change = (edge_copy.0, edge_copy.1);
            let mut vector_input: Vec<(NodeId<FromNodeId>, NodeId<ToNodeId>)> =
                Vec::new();
            loop {
                if all_edges_move.contains(&(edge_change.0,
                                           edge_change.1,
                                           EdgeType::MOVE))
                {
                    vector_input.push((edge_change.0, edge_change.1));
                    let parent_m = store.from_arena.borrow()[edge_change.0].parent().unwrap();
                    let parent_n = store.to_arena.borrow()[edge_change.1].parent().unwrap();
                    if !parent_m.is_root(&store.from_arena.borrow())
                       && !parent_n.is_root(&store.to_arena.borrow())
                    {
                        edge_change = (parent_m, parent_n);
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            if !vector_input.is_empty() {
                end_copy_tree.insert(vector_input[vector_input.len() - 1]);
            }
        }
        // Add Nodes Copy Tree
        for nodes in end_copy_tree {
            if store.check_subtree(nodes.0, nodes.1) {
                // - If insertion in sub-tree from tree 2
                // - If deletion in sub-tree from tree 1
                //  -> Then it means the glue operations has occurred.
                // If node.0 in tree1 Exists in tree 2 then its copy
                // Otherwise its glue
                if !store.check_tree_1_exists_in_tree2(nodes.0) {
                    store.push_edge(nodes.0, nodes.1, 1, EdgeType::GLUE);
                } else {
                    store.push_edge(nodes.0, nodes.1, 1, EdgeType::COPY);
                }
            }
        }
        store
    }

    /// Describe this matcher for the user.
    fn describe(&self) -> String {
        String::from("This matcher performs matching operation from the paper Chawathe et al. (1998).")
    }
    /// Prune Edges
    fn prune_edges(&self, input: &mut MappingStoreGraph<T>) -> (Vec<Edge>, Vec<Edge>) {
        // Loop Though all the edges
        // TODO: Fix Cyclomatic complexity (31)
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
            // E2 = (m,n1) &&   E3 = (m1,n)
            let upper_bound_e2_and_e3 = input.clone()
                                             .upper_bound_edge(&edge.clone(),
                                                               &all_edges.clone());
            let mut lower_bound_e1 = 0;
            lower_bound_e1 = input.clone()
                                  .lower_bound_edge(&edge.clone(), all_edges.clone());
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
            if e1_root.from_node.is_root(&input.from_arena.borrow())
               || e1_root.to_node.is_root(&input.to_arena.borrow())
            {
                // There should be nothing moving the root of a tree.
                // This is described in the chawathe 98 paper.
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

                    if edge_glue.edge_type == EdgeType::GLUE
                       && edge_move.edge_type == EdgeType::MOVE
                       && m_move == m_glue && n_move == n_glue
                    {
                        // We have found a copy so the "check" variable is true
                        // so we have found (m,n) which is move and copy
                        check = true;
                        // We add copy edge to the remaining edges
                        remaining_edges.push(edge_glue.clone());
                    }
                }
            } else if edge_move.edge_type != EdgeType::GLUE {
                remaining_edges.push(edge_move.clone());
            }
            if !check && edge_move.edge_type == EdgeType::MOVE {
                remaining_edges.push(edge_move.clone());
            }
        }
        edges_still_left = remaining_edges;
        // Questions.
        // 1) Do we need to consider where when there multiple (m,n) edge ?
        // 2) What if there are ok edges :
        //    - If there are OK Edges remove the rest of the other edge type except COPY
        let mut filtered_remaining_edges: Vec<Edge> = Vec::new();
        input.list_edges = RefCell::new(edges_still_left.clone());

        let mut list_t_f: HashMap<NodeId<ToNodeId>,
                                  (HashSet<(NodeId<FromNodeId>,
                                           EdgeType,
                                           usize)>,
                                  usize)> = HashMap::new();
        for edge1 in input.list_edges.borrow().clone() {
            let mut set: HashSet<(NodeId<FromNodeId>, EdgeType, usize)> = HashSet::new();
            let mut size: usize = 1;
            for edge2 in input.list_edges.borrow().clone() {
                if !(edge1.from_node == edge2.from_node && edge1.to_node == edge2.to_node
                     && edge1.edge_type == edge2.edge_type)
                {
                    if edge2.to_node == edge1.to_node {
                        let temp: NodeId<FromNodeId> = edge2.from_node;
                        set.insert((temp, edge2.edge_type, edge2.value));
                        size += 1;
                    }
                } else {
                    let temp: NodeId<FromNodeId> = edge2.from_node;
                    set.insert((temp, edge2.edge_type, edge2.value));
                }
            }
            let temp_to_node: NodeId<ToNodeId> = edge1.to_node;
            list_t_f.insert(temp_to_node, (set, size));
        }

        let helper_to_mapping_from_nodes = list_t_f;
        // BFS Through the To Tree, Nodes,
        // Check if they have OK or copy if they do then keep them and remove the rest
        let root_to = input.to_arena.borrow().root().unwrap(); // To Tree Root
        let reference_matcher = &input.to_arena.borrow().clone();
        let bfs_root_to_tree = root_to.breadth_first_traversal(reference_matcher);
        // List Of Edges To Keep
        let mut list_edges_to_keep: HashSet<(NodeId<FromNodeId>,
                                            NodeId<ToNodeId>,
                                            EdgeType,
                                            usize)> = HashSet::new();
        for to_node in bfs_root_to_tree {
            let get_edge = (helper_to_mapping_from_nodes).get(&to_node);
            if get_edge.is_some() {
                // check if there is an OK edge.
                let mut checker_bool = false;
                for &(from, edge_type, cost) in &get_edge.unwrap().0 {
                    if edge_type == EdgeType::OK {
                        checker_bool = true;
                    }
                }
                if checker_bool {
                    for &(from, edge_type, cost) in &get_edge.unwrap().0 {
                        if edge_type == EdgeType::OK || edge_type == EdgeType::COPY {
                            list_edges_to_keep.insert((from, to_node, edge_type, cost));
                        }
                    }
                } else {
                    for &(from, edge_type, cost) in &get_edge.unwrap().0 {
                        list_edges_to_keep.insert((from, to_node, edge_type, cost));
                    }
                }
            }
        }
        let mut edges_still_left_final: Vec<Edge> = Vec::new();
        for edge in edges_still_left.clone() {
            if list_edges_to_keep.contains(&(edge.from_node,
                                           edge.to_node,
                                           edge.edge_type,
                                           edge.value))
            {
                edges_still_left_final.push(edge);
            }
        }
        input.list_edges = RefCell::new(edges_still_left_final.clone());
        (edges_still_left_final, edges_to_be_pruned)
    }
}

/// Match two trees and return a store of mappings between them.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait MatchingTreesScriptor<T: Clone + Debug> {
    /// Match two trees and return a store of mappings between them.
    fn match_trees(&self,
                   base: Arena<T, FromNodeId>,
                   diff: Arena<T, ToNodeId>)
                   -> MappingStoreGraph<T>;

    /// Describe the matcher for the user.
    ///
    /// This is the string that is printed when the user passes in the --list
    /// CLI option.
    fn describe(&self) -> String;

    /// This shows the trait for the pruning for the induced edges.
    /// The result should logically reduce the number of edges produced in induced edges.
    fn prune_edges(&self, input: &mut MappingStoreGraph<T>) -> (Vec<Edge>, Vec<Edge>);
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
        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, &EdgeType::OK, &mut checker_edges_induced));
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
        let matching_config = Config2::new();
        let matcher = matching_config.match_trees(arena.clone(), tree2.clone());
        matcher.print();
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(6,
                                      5,
                                      &EdgeType::DELETE,
                                      &mut checker_edges_induced));
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
        let matching_config = Config2::new();
        let matcher = matching_config.match_trees(tree1.clone(), arena.clone());
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 5, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5,
                                      6,
                                      &EdgeType::INSERT,
                                      &mut checker_edges_induced));
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
        //      n1
        //     /
        //   n2
        //   /
        // n3

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
        //     n4
        //    /  \
        // n5    n6
        //         \
        //         n7
        //           \
        //            n8
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

        //                n9
        //               /  \
        //             n10   n12
        //            /        \
        //         n11         n13
        // End of Subtree 3

        arena.new_node(String::from("DELETE"),
                       String::from("DELETE"),
                       None,
                       None,
                       None,
                       None);
        // Tree 1
        let mut tree1 = Arena::new();

        let root = tree1.new_node(String::from("Expr"),
                                  String::from("a"),
                                  None,
                                  None,
                                  None,
                                  None);

        // Subtree 2
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
        //            n4
        //           /  \
        //         n5    n6
        //                 \
        //                 n7
        //                   \
        //                   n8
        // End of Subtree 2

        // Subtree 3
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

        //                n9
        //               /  \
        //             n10   n12
        //            /        \
        //         n11         n13

        // End of Subtree 3
        tree1.new_node(String::from("INSERT"),
                       String::from("INSERT"),
                       None,
                       None,
                       None,
                       None);
        let matching_config = Config2::new();
        let matcher = matching_config.match_trees(arena.clone(), tree1.clone());

        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(0, 2, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1,
                                      1,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 3, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2,
                                      2,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3,
                                      3,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 5, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4,
                                      4,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 0, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 2, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5,
                                      5,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(6, 3, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(6,
                                      6,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(7, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(7,
                                      7,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(8, 5, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(8,
                                      8,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(9, 6, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(9,
                                      9,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(10,
                                      7,
                                      &EdgeType::MOVE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(10,
                                      10,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(11,
                                      8,
                                      &EdgeType::MOVE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(12,
                                      9,
                                      &EdgeType::MOVE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(13,
                                      10,
                                      &EdgeType::MOVE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(9, 6, &EdgeType::GLUE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, &EdgeType::GLUE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 3, &EdgeType::GLUE, &mut checker_edges_induced));
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

        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1,
                                      1,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 2, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3,
                                      2,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4,
                                      3,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 1, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2,
                                      4,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 2, &EdgeType::GLUE, &mut checker_edges_induced));
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

        let pruning = matching_config.prune_edges(&mut matcher.clone());
        // Pruning
        let mut new_matcher_pruning = matcher.clone();
        // Length Of Edges Induced
        let edges_pruned = pruning.0.clone();
        new_matcher_pruning.list_edges = RefCell::new(edges_pruned.clone());

        let mut checker_edges_pruned = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(0, 5, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1,
                                      1,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 2, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 7, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2,
                                      2,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 3, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 8, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(3,
                                      3,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(3, 4, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 0, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4,
                                      4,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 5, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(5, 1, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(5,
                                      5,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(5, 6, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6,
                                      6,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6, 10, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(7,
                                      7,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(7, 11, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8,
                                      8,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8, 12, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(9,
                                      9,
                                      &EdgeType::UPDATE,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(9, 13, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(10,
                                      9,
                                      &EdgeType::INSERT,
                                      &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 7, &EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 2, &EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8, 12, &EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6, 10, &EdgeType::GLUE, &mut checker_edges_pruned));

        // Test for Test 6

        // - Do Re-match trees.
        // - Perform pruning.
        // - Then check if all have OK matching.

        let matcher_copy = edgecover_solver(new_matcher_pruning);

        // Lets Do Rematch Test
        //  - This checks whether the mapping applied to from arena is the same as the to arena.
        let matching_config_last_test = Config2::new();
        let from_node_new = matcher_copy.0.from_arena.into_inner();
        let to_node_new = matcher_copy.0.to_arena.into_inner();
        let mut create_mapping_store_graph =
            matching_config_last_test.match_trees(from_node_new, to_node_new);
        let pruning_new_edge_mapping =
            matching_config_last_test.prune_edges(&mut create_mapping_store_graph);
        // Counter
        let mut count_ok_edges = 0;
        for edge in create_mapping_store_graph.list_edges.borrow().iter() {
            if edge.edge_type == EdgeType::OK {
                count_ok_edges += 1;
            }
        }

        assert_eq!(create_mapping_store_graph.list_edges.borrow().len(),
                   count_ok_edges);
    }
}
