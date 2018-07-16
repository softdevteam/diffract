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
use action::{ApplyAction, Copy, Delete, EditScript, Glue, Insert, Move, Update};
use ast::{Arena, DstNodeId, NodeId, SrcNodeId};
use emitters::RenderJson;
use matchers::{has_same_type, has_same_type_and_label, EditScriptResult};

use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::BuildHasher;

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
    OK
}

/// The initial cost of each edge
#[derive(Debug, Clone)]
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
    pub ok: usize
}

/// This shows the Cost Edge Struct where the user can define the cost of different edge type.
impl CostEdge {
    /// Create a new cost edge
    /// This shows the cost for each edge defined by the user
    pub fn new(update: usize,
               insert: usize,
               delete: usize,
               move_: usize,
               copy: usize,
               glue: usize,
               null: usize,
               ok: usize)
               -> CostEdge {
        CostEdge { update,
                   insert,
                   delete,
                   move_,
                   copy,
                   glue,
                   null,
                   ok }
    }
}

/// Edge construct which shows the edge between two nodes and their value
#[derive(Debug, Clone, Copy)]
pub struct Edge {
    /// src node shows the node id from the src-tree
    pub src_node: NodeId<SrcNodeId>,
    /// dst node shows the node id from dst-tree
    pub dst_node: NodeId<DstNodeId>,
    /// The value of the edge
    pub value: usize,
    /// The edge type
    pub edge_type: EdgeType
}

impl Edge {
    /// Create a new edge
    pub fn new(src_node: NodeId<SrcNodeId>,
               dst_node: NodeId<DstNodeId>,
               value: usize,
               edge_type: EdgeType)
               -> Edge {
        Edge { src_node,
               dst_node,
               value,
               edge_type }
    }
}

type NodeAndEdgeType<T, U> = HashMap<NodeId<T>, (NodeId<U>, EdgeType)>;

/// A store of mappings between nodes in different arenas.
/// Direction is important.
#[derive(Debug, Clone)]
pub struct MappingStoreGraph<T: Clone + Debug> {
    /// List Of edges
    pub list_edges: RefCell<Vec<Edge>>,
    /// Source arena (treat as immutable).
    pub src_arena: RefCell<Arena<T, SrcNodeId>>,
    /// Destination arena (treat as immutable).
    pub dst_arena: RefCell<Arena<T, DstNodeId>>,
    /// List of nodes to be inserted
    pub list_node_inserted: RefCell<Vec<NodeId<DstNodeId>>>,
    /// List of nodes to be deleted
    pub list_node_deleted: RefCell<Vec<NodeId<SrcNodeId>>>,
    /// Find the Cost of all edges
    pub all_edge_cost: CostEdge,
    /// Final mappings for nodes when the pruning are done in particular min-edge cover
    pub src: RefCell<NodeAndEdgeType<SrcNodeId, DstNodeId>>,
    /// Mappings from the destination tree to the source.
    ///
    /// Should contain the same information as `src_map`.
    pub dst: RefCell<NodeAndEdgeType<DstNodeId, SrcNodeId>>
}

impl<T: Clone + Debug + Eq + 'static> MappingStoreGraph<T> {
    /// Implementation of new in mapping store the second version
    pub fn new(src: Arena<T, SrcNodeId>, dst: Arena<T, DstNodeId>) -> MappingStoreGraph<T> {
        MappingStoreGraph { src_arena: RefCell::new(src),
                            dst_arena: RefCell::new(dst),
                            list_edges: RefCell::new(Vec::new()),
                            list_node_inserted: RefCell::new(Vec::new()),
                            list_node_deleted: RefCell::new(Vec::new()),
                            // The cost could change in the future but,
                            // we can make the default cost for every edge
                            // which is cost the value of 1.
                            all_edge_cost: CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1),
                            src: RefCell::new(HashMap::new()),
                            dst: RefCell::new(HashMap::new()) }
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
    pub fn push(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>, ty: EdgeType) {
        self.src.borrow_mut().insert(src, (dst, ty));
        self.dst.borrow_mut().insert(dst, (src, ty));
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
        self.dst.borrow().get(&dst).map(|x| x.0)
    }

    /// Get the `NodeId` that `src` is mapped to.
    pub fn get_dst(&self, src: NodeId<SrcNodeId>) -> Option<NodeId<DstNodeId>> {
        self.src.borrow().get(&src).map(|x| x.0)
    }

    /// Test whether `src` is mapped to `dst` in this store.
    pub fn is_mapped(&self, src: NodeId<SrcNodeId>, dst: NodeId<DstNodeId>) -> bool {
        if !self.contains_src(src) {
            return false;
        }
        self.get_dst(src).map_or(false, |x| x == dst)
    }

    /// Push New Edge
    pub fn push_edge(&mut self,
                     new_src_node: NodeId<SrcNodeId>,
                     new_dst_node: NodeId<DstNodeId>,
                     new_value: usize,
                     new_edge_type: EdgeType) {
        let new_edge = Edge::new(new_src_node, new_dst_node, new_value, new_edge_type);
        self.list_edges.borrow_mut().push(new_edge);
    }
    /// Remove Edge
    pub fn remove_edge(&mut self, index: usize) {
        self.list_edges.borrow_mut().remove(index);
    }
    /// Check contains the src_node
    pub fn contains_edge_src(&self, src: NodeId<SrcNodeId>) -> bool {
        let list_edges = self.list_edges.borrow();
        for edge in list_edges.iter() {
            if edge.src_node == src {
                return true;
            }
        }
        false
    }

    /// Check if the edges contains
    ///     1) src node id &
    ///     2) To node id
    ///     3) Edge Type
    pub fn contains_edge(&self,
                         src: usize,
                         dst: usize,
                         edge_type: EdgeType,
                         list_edges: &[Edge])
                         -> bool {
        for edge in list_edges {
            if edge.src_node.id() == src && edge.dst_node.id() == dst && edge.edge_type == edge_type
            {
                return true;
            }
        }
        false
    }

    /// Check if the edge contains the dst_node
    pub fn contains_edge_dst(&self, dst: NodeId<DstNodeId>) -> bool {
        let list_edges = self.list_edges.borrow();
        for edge in list_edges.iter() {
            if edge.dst_node == dst {
                return true;
            }
        }
        false
    }

    /// Print for all the pre order traversal tree2: <Vec<NodeId<DstNodeId>>>
    pub fn print_preorder_traversal(&self,
                                    tree1: Vec<NodeId<SrcNodeId>>,
                                    tree2: Vec<NodeId<DstNodeId>>)
                                    -> () {
        println!();
        println!("Tree 1");
        print!("|");
        for node in tree1 {
            print!("{:?},{:?}|", node.id(), self.src_arena.borrow()[node].label);
        }
        println!();
        println!("Tree 2");
        print!("|");
        for node in tree2 {
            print!("{:?},{:?}|", node.id(), self.dst_arena.borrow()[node].label);
        }
        println!();
    }
    /// Checks if the subtree in node id in From_node is the same same in dst_node
    /// This overall checks the subtrees of two trees and whether they are the same.
    /// The check is done by checking if the nodes are equal and as well as their descendants.
    pub fn check_subtree(&self, src_node: NodeId<SrcNodeId>, dst_node: NodeId<DstNodeId>) -> bool {
        let descendants_src_node = src_node.descendants(&self.src_arena.borrow())
                                           .collect::<Vec<NodeId<SrcNodeId>>>();
        let descendants_dst_node = dst_node.descendants(&self.dst_arena.borrow())
                                           .collect::<Vec<NodeId<DstNodeId>>>();
        if has_same_type_and_label(src_node,
                                   &self.src_arena.borrow(),
                                   dst_node,
                                   &self.dst_arena.borrow())
        {
            if descendants_dst_node.len() != descendants_src_node.len() {
                return false;
            }
            for i in 0..descendants_dst_node.len() {
                if !has_same_type_and_label(descendants_src_node[i],
                                            &self.src_arena.borrow(),
                                            descendants_dst_node[i],
                                            &self.dst_arena.borrow())
                {
                    // This means the descendants are !perfect by label and expression
                    return false;
                }
            }
        } else {
            return false;
        }
        true
    }
    /// Check if the subtree is deleted from src_node against dst_node.
    /// This checks whether the the descendants are in the deletion edges.
    pub fn check_tree_deletion(&self,
                               src_node_ancestor: NodeId<SrcNodeId>,
                               delete_tree2: NodeId<DstNodeId>,
                               hash_set_deletion: &HashSet<(NodeId<SrcNodeId>,
                                        NodeId<DstNodeId>)>)
                               -> bool {
        let mut output = true;
        let descendants_src_node = src_node_ancestor.descendants(&self.src_arena.borrow())
                                                    .collect::<Vec<NodeId<SrcNodeId>>>();
        for descendant in descendants_src_node {
            if !hash_set_deletion.contains(&(descendant, delete_tree2)) {
                output = false;
            }
        }
        output
    }

    /// check if subtree 1 exists in tree 2.
    /// This checks src node exists in the same position in tree 2 (To Node Id)
    /// If it does then true otherwise false.
    pub fn check_tree_1_exists_in_tree2(&self, src_node_ancestor: NodeId<SrcNodeId>) -> bool {
        let node_id_dst_node = src_node_ancestor.id();
        if node_id_dst_node >= self.dst_arena.borrow().size() {
            return false;
        }
        let dst_node = NodeId::<DstNodeId>::new(node_id_dst_node);
        self.check_subtree(src_node_ancestor, dst_node)
    }
    /// Check if the descendants of dst_node are in the insertion edges.
    /// From node would be the insertion node.
    pub fn check_tree_insertion(&self,
                                insert_tree1: NodeId<SrcNodeId>,
                                dst_node: NodeId<DstNodeId>,
                                hash_set_insertion: &HashSet<(NodeId<SrcNodeId>,
                                         NodeId<DstNodeId>)>)
                                -> bool {
        let mut output = true;
        let descendants_src_node = dst_node.descendants(&self.dst_arena.borrow())
                                           .collect::<Vec<NodeId<DstNodeId>>>();
        for descendant in descendants_src_node {
            if !hash_set_insertion.contains(&(insert_tree1, descendant)) {
                output = false;
            }
        }
        output
    }

    /// Converts vector(To_node_id) to hash set
    pub fn vector_to_hash_set_to_node_id(&self,
                                         tree2: &[NodeId<DstNodeId>])
                                         -> HashSet<NodeId<DstNodeId>> {
        let mut hashset = HashSet::<NodeId<DstNodeId>>::new();
        for node in tree2.to_owned() {
            hashset.insert(node);
        }
        hashset
    }

    /// Converts vector(From_node_id) to hash set
    pub fn vector_to_hash_set_src_node_id(&self,
                                          tree2: &[NodeId<SrcNodeId>])
                                          -> HashSet<NodeId<SrcNodeId>> {
        let mut hashset = HashSet::<NodeId<SrcNodeId>>::new();
        for node in tree2.to_owned() {
            hashset.insert(node);
        }
        hashset
    }
    /// Generate all lower bound values for all the edges.
    pub fn make_all_lower_bound_edges(&self,
                                      hash_edges: &HashSet<(NodeId<SrcNodeId>,
                                               NodeId<DstNodeId>)>)
                                      -> HashMap<(NodeId<SrcNodeId>, NodeId<DstNodeId>), usize>
    {
        // May be returner. Of all the lower bound for the edges.
        let mut hash_map_lower_bound: HashMap<(NodeId<SrcNodeId>, NodeId<DstNodeId>),
                                              usize> = HashMap::new();
        let cost_of_move = self.all_edge_cost.move_;
        let cost_of_update = self.all_edge_cost.update;
        for edge in self.list_edges.borrow().iter() {
            let m = edge.src_node;
            let n = edge.dst_node;

            // Cost of forced move calculate
            // Get all the children of m.
            let m_children =
                m.children(&self.src_arena.borrow()).collect::<Vec<NodeId<SrcNodeId>>>();
            let n_children =
                n.children(&self.dst_arena.borrow()).collect::<Vec<NodeId<DstNodeId>>>();

            // Get sum of forced move m' (Children of m) to n
            let mut sum_cost_forced_move_m_children_n = 0;
            let mut sum_cost_forced_move_n_children_m = 0;
            // Loop through all the children of m (m_1) and see if there is a mapping of n
            // if there is edge exist that m_1 -> n, forced move cost = 0.
            // otherwise forced move cost is the cost of move operation.

            for m_1 in m_children {
                if !hash_edges.contains(&(m_1, n)) {
                    //                    let m1_cost = m_1.children(&self.src_arena.borrow()).collect::<Vec<NodeId<SrcNodeId>>>().len();
                    sum_cost_forced_move_m_children_n += cost_of_move;
                }
            }

            for n_1 in n_children {
                if !hash_edges.contains(&(m, n_1)) {
                    //                    let n1_cost = n_1.children(&self.dst_arena.borrow()).collect::<Vec<NodeId<DstNodeId>>>().len();
                    sum_cost_forced_move_n_children_m += cost_of_move;
                }
            }

            // Analogously n_1 to m

            hash_map_lower_bound.insert((m, n),
                                        cost_of_update
                                        + (cmp::min(sum_cost_forced_move_m_children_n,
                                                    sum_cost_forced_move_n_children_m)
                                           / 2));
        }
        hash_map_lower_bound
    }

    /// Generate all the upper bound values for all the edges.
    pub fn make_all_upper_bound_edges(&self,
                                      hash_edges: &HashSet<(NodeId<SrcNodeId>,
                                               NodeId<DstNodeId>)>)
                                      -> HashMap<(NodeId<SrcNodeId>, NodeId<DstNodeId>), usize>
    {
        // May be returner. Of all the upper bound for the edges.
        let mut hash_map_upper_bound: HashMap<(NodeId<SrcNodeId>, NodeId<DstNodeId>),
                                              usize> = HashMap::new();
        let cost_of_move = self.all_edge_cost.move_;
        let cost_of_update = self.all_edge_cost.update;
        let cost_of_copy = self.all_edge_cost.copy;
        let cost_of_glue = self.all_edge_cost.glue;
        for edge in self.list_edges.borrow().iter() {
            let m = edge.src_node;
            let n = edge.dst_node;

            // Cost of forced move calculate
            // Get all the children of m.
            let m_children =
                m.children(&self.src_arena.borrow()).collect::<Vec<NodeId<SrcNodeId>>>();
            let n_children =
                n.children(&self.dst_arena.borrow()).collect::<Vec<NodeId<DstNodeId>>>();

            // Get sum of forced move m' (Children of m) to n
            let mut sum_cost_conditional_move_m_children_n = 0;
            let mut sum_cost_conditional_move_n_children_m = 0;
            // Loop through all the children of m (m_1) and see if there is a mapping of n
            // if there is edge exist that m_1 -> n, forced move cost = 0.
            // otherwise forced move cost is the cost of move operation.

            for m_1 in m_children {
                if !hash_edges.contains(&(m_1, n)) {
                    sum_cost_conditional_move_m_children_n += cost_of_copy + cost_of_move;
                }
            }
            // Analogously n_1 to m
            for n_1 in n_children {
                if !hash_edges.contains(&(m, n_1)) {
                    sum_cost_conditional_move_n_children_m += cost_of_glue + cost_of_move;
                }
            }
            let mut cost_cw =
                if edge.edge_type == EdgeType::INSERT || edge.edge_type == EdgeType::DELETE {
                    0
                } else {
                    cost_of_update
                };

            hash_map_upper_bound.insert((m, n),
                                        cost_cw
                                        + (sum_cost_conditional_move_m_children_n
                                           + sum_cost_conditional_move_n_children_m)
                                          / 2);
        }
        hash_map_upper_bound
    }

    /// add dst_nodes to vector to be inserted
    pub fn insert_node(&mut self, node_to_insert: NodeId<DstNodeId>) {
        self.list_node_inserted.borrow_mut().push(node_to_insert);
    }

    /// add src_node to vector to be deleted
    pub fn delete_node(&mut self, node_to_delete: NodeId<SrcNodeId>) {
        self.list_node_deleted.borrow_mut().push(node_to_delete);
    }

    /// Print
    pub fn print(&self) {
        let size_src_node = NodeId::new(self.src_arena.borrow().size() - 1);
        println!("{:?}", size_src_node);
        let size_dst_node = NodeId::<DstNodeId>::new(self.dst_arena.borrow().size() - 1);
        let last_node_src_node = &self.src_arena.borrow()[size_src_node];
        println!("The Last node From_NODE is --> {:?}", last_node_src_node);
        let last_node_dst_node = &self.dst_arena.borrow()[size_dst_node];
        println!("The Last node From_NODE is --> {:?}", last_node_dst_node);
        println!("List Of edges already mapped to");
        for (index, edge) in self.list_edges.borrow().iter().enumerate() {
            println!(" -- {:?} -- ", index);
            println!("TY        :   src node    -> {:?} || dst node -> {:?}",
                     self.src_arena.borrow()[edge.src_node].ty,
                     self.dst_arena.borrow()[edge.dst_node].ty);
            println!("Value     :   src node -> {:?} || dst node -> {:?}",
                     self.src_arena.borrow()[edge.src_node].label,
                     self.dst_arena.borrow()[edge.dst_node].label);
            println!("Src node -> {:?} || dst node -> {:?} || Edge type -> {:?}",
                     edge.src_node, edge.dst_node, edge.edge_type);
            println!("--");
        }
    }
}

/// A set of node edges of a particular type and with a cost.
type NodeEdgeSet<T, U> = HashSet<(NodeId<T>, NodeId<U>, EdgeType, usize)>;

/// A set of nodes with edge types and costs.
///
/// Used where one node can be mapped to many destination nodes.
type NodeVertexSet<T> = HashSet<(NodeId<T>, EdgeType, usize)>;

type OneToManyCostMapping<T, U> = HashMap<NodeId<T>, (NodeVertexSet<U>, usize)>;

/// This is the stage 3 for algorithm
/// This performs minimum edge cover solver problem
pub fn edgecover_solver(
    mut new_matcher_pruning: MappingStoreGraph<String>)
    -> (MappingStoreGraph<String>,
       OneToManyCostMapping<DstNodeId, SrcNodeId>,
       OneToManyCostMapping<SrcNodeId, DstNodeId>) {
    // This implements greedy solution to get only one edge mapping incidented to -- "To nodes"
    // Lets get rid of move operations where they have glue or copy as a parent
    let helper_mapping = edgecover_helper(&new_matcher_pruning);
    let root_dst = new_matcher_pruning.dst_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.dst_arena.borrow().clone();
    let bfs_root_dst_tree = root_dst.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search

    // Hash Set for different types of edges
    let mut copy_edges: NodeEdgeSet<SrcNodeId, DstNodeId> = HashSet::new();
    let mut glue_edges: NodeEdgeSet<SrcNodeId, DstNodeId> = HashSet::new();
    let mut move_edges: NodeEdgeSet<SrcNodeId, DstNodeId> = HashSet::new();
    let mut edge_keep: NodeEdgeSet<SrcNodeId, DstNodeId> = HashSet::new();

    // Find minimum cost edge
    // By looking at all edges associated with the nodes in the dst_Node

    for dst_node in bfs_root_dst_tree {
        let node_mapping = helper_mapping.0.get(&dst_node);
        let mut edge_keep_temp: NodeEdgeSet<SrcNodeId, DstNodeId> = HashSet::new();
        // Sorting by cost Keep the lowest cost
        if node_mapping.is_some() {
            let lowest_cost = node_mapping.unwrap().0.iter().map(|x| x.2).min().unwrap();
            for &(src_node, edge, value) in &node_mapping.unwrap().0 {
                // Add edges which are copy or glue
                if edge == EdgeType::COPY {
                    copy_edges.insert((src_node, dst_node, edge, value));
                } else if edge == EdgeType::GLUE {
                    glue_edges.insert((src_node, dst_node, edge, value));
                } else if edge == EdgeType::MOVE {
                    move_edges.insert((src_node, dst_node, edge, value));
                }
                if lowest_cost == value {
                    edge_keep_temp.insert((src_node, dst_node, edge, value));
                }
            }

            // Case 1 edge keep temp  could be of 1 - means perfect mapping
            // Case 2 there could be many edges of the same cost
            // In case 2 we would pick according to their the edge
            if edge_keep_temp.len() > 1 {
                let mut vec_order = edge_keep_temp.clone().into_iter().collect::<Vec<_>>();
                vec_order.sort_by_key(|k| k.0.id());
                let mut result_edge: Option<(NodeId<SrcNodeId>,
                                            NodeId<DstNodeId>,
                                            EdgeType,
                                            usize)> = None;
                let mut check_src_2 = false; // Edge type is Copy, Glue, Move

                for (src_node, dst_node, edge1, value) in vec_order {
                    let edge = edge1;
                    match result_edge {
                        None => {
                            if !(edge1 == EdgeType::OK
                                 || edge1 == EdgeType::INSERT
                                 || edge1 == EdgeType::DELETE)
                            {
                                check_src_2 = true;
                            }
                            result_edge = Some((src_node, dst_node, edge, value));
                            continue;
                        }
                        Some(_) => {}
                    }

                    if check_src_2
                       && (edge == EdgeType::INSERT
                           || edge == EdgeType::DELETE
                           || edge == EdgeType::OK)
                    {
                        // our current chosen edges either MOVE, COPY, GLUE
                        // current edge in the for loop is Insert or delete or ok
                        // So choose the current edge in the for loop;
                        check_src_2 = false;
                        result_edge = Some((src_node, dst_node, edge, value));
                    }
                    // We try to get the first insert or delete or ok edges
                    // If they are not available then we get the first
                    // Glue or copy or move edge.
                }
                // At the we should have an edge
                if result_edge.is_some() {
                    edge_keep.insert((result_edge.unwrap().0,
                                     result_edge.unwrap().1,
                                     result_edge.unwrap().2,
                                     result_edge.unwrap().3));
                }
            }
            // We found the lowest cost in the first go.
            else if edge_keep_temp.len() == 1 {
                for (src_node, dst_node, edge, value) in edge_keep_temp {
                    edge_keep.insert((src_node, dst_node, edge, value));
                }
            }
        }
    }
    // We would check through the edge keep and only retain edges in edge keep;
    let mut edge_filtered: Vec<Edge> = Vec::new();
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if edge_keep.contains(&(edge.src_node, edge.dst_node, edge.edge_type, edge.value))
           || edge.edge_type == EdgeType::DELETE
        {
            edge_filtered.push(Edge::new(edge.src_node, edge.dst_node, edge.value, edge.edge_type));
        }
    }
    // Half way breaking point

    new_matcher_pruning.list_edges = RefCell::new(edge_filtered);

    // Below will implement greedy solution to get only edge going into "src_node"
    let helper_mapping = edgecover_helper(&new_matcher_pruning);
    let root_src = new_matcher_pruning.src_arena.borrow().root().unwrap(); // From Tree Root
    let reference_matcher = &new_matcher_pruning.src_arena.borrow().clone();
    let bfs_root_src_tree = root_src.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search

    // Start the minimum edge cost finding
    // But looking at all nodes in the src tree.
    edge_keep.clear();
    let mut edge_filtered: Vec<Edge> = Vec::new();

    for src_node in bfs_root_src_tree {
        let node_mapping = helper_mapping.1.get(&src_node);
        let mut edge_keep_temp: HashSet<(NodeId<SrcNodeId>,
                                        NodeId<DstNodeId>,
                                        EdgeType,
                                        usize)> = HashSet::new();
        // Sorting by cost Keep the lowest cost
        if node_mapping.is_some() {
            let mut lowest_cost = node_mapping.unwrap().0.iter().map(|x| x.2).min().unwrap();

            for &(dst_node, edge, value) in &node_mapping.unwrap().0 {
                if lowest_cost == value {
                    edge_keep_temp.insert((src_node, dst_node, edge, value));
                }
            }

            // Case 1 edge keep temp  could be of 1 - means perfect mapping
            // Case 2 there could be many edges of the same cost
            // In case 2 we would pick according to their the edge
            if edge_keep_temp.len() > 1 {
                let mut vec_order = edge_keep_temp.clone().into_iter().collect::<Vec<_>>();
                vec_order.sort_by_key(|k| k.1.id());
                let mut result_edge: Option<(NodeId<SrcNodeId>,
                                            NodeId<DstNodeId>,
                                            EdgeType,
                                            usize)> = None;

                let mut check_src_2 = false; // Edge type is Copy, Glue, Move

                for (src_node, dst_node, edge1, value) in vec_order {
                    let edge = edge1;
                    match result_edge {
                        None => {
                            if !(edge1 == EdgeType::OK
                                 || edge1 == EdgeType::INSERT
                                 || edge1 == EdgeType::DELETE)
                            {
                                check_src_2 = true;
                            }
                            result_edge = Some((src_node, dst_node, edge, value));
                            continue;
                        }
                        Some(_) => {}
                    }

                    if check_src_2
                       && (edge == EdgeType::INSERT
                           || edge == EdgeType::DELETE
                           || edge == EdgeType::OK)
                    {
                        // our current chosen edges either MOVE, COPY, GLUE
                        // current edge in the for loop is Insert or delete or ok
                        // So choose the current edge in the for loop;
                        check_src_2 = false;
                        result_edge = Some((src_node, dst_node, edge, value));
                    }
                    // We try to get the first insert or delete or ok edges
                    // If they are not available then we get the first
                    // Glue or copy or move edge.
                }
                // At the we should have an edge
                if result_edge.is_some() {
                    edge_keep.insert((result_edge.unwrap().0,
                                     result_edge.unwrap().1,
                                     result_edge.unwrap().2,
                                     result_edge.unwrap().3));
                }
            }
            // We found the lowest cost in the first go.
            else if edge_keep_temp.len() == 1 {
                for (src_node, dst_node, edge, value) in edge_keep_temp {
                    edge_keep.insert((src_node, dst_node, edge, value));
                }
            }
        }
    }

    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if edge_keep.contains(&(edge.src_node, edge.dst_node, edge.edge_type, edge.value))
           || edge.edge_type == EdgeType::INSERT
        {
            edge_filtered.push(Edge::new(edge.src_node, edge.dst_node, edge.value, edge.edge_type));
        }
    }

    new_matcher_pruning.list_edges = RefCell::new(edge_filtered);

    // Get rid of edges where parent's do not have any edges associated with them.
    remove_edges_where_parent_not_edges(&mut new_matcher_pruning);

    let helper = edgecover_helper(&new_matcher_pruning);

    let mut edge_to_be_added = new_matcher_pruning.list_edges.borrow_mut().to_vec();

    let mut edges_hash: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    for e in edge_to_be_added.clone() {
        edges_hash.insert((e.src_node, e.dst_node));
    }

    // At this stage there should be one to one mapping and many edges which are not mapped.
    // Look though all the edges which are copy or glue
    // Case Copy
    // Check if the edges for the src and dst exist
    // lets call copy edge (F,T)
    // If there are no mapping F -> Nothing and T -> Nothing -> Easily add the edges
    // If there are mapping but the mapping are F1 and T1 which are the same as F = F1 and T = T1
    // But the edge type is different. Then check if the edge is move. If it move then make it COPY
    // Otherwise -> No Idea

    let mut visited_dst_nodes: HashSet<NodeId<DstNodeId>> = HashSet::new();
    for &(src, dst, edge_type, value) in &copy_edges {
        // Case when the src node does not have any edges but the dst_node has a edge
        // Vice Versa
        // Then easily add the edge. (Type Copy)
        if !visited_dst_nodes.contains(&dst)
           && !edges_hash.contains(&(src, dst))
           && (helper.0.get(&dst).is_none())
        {
            // check to the coping node
            edge_to_be_added.push(Edge::new(src, dst, value, edge_type));
            visited_dst_nodes.insert(dst);
            edges_hash.insert((src, dst));
        }
    }

    new_matcher_pruning.list_edges = RefCell::new(edge_to_be_added);

    // Remove any edges which have edges ancestor of glue or copy
    new_matcher_pruning = edgecover_annotate_helper_remove_move(new_matcher_pruning);

    // Add move operations that have been removed
    add_edges_move(&mut new_matcher_pruning, &move_edges);

    // Remove any move operations that have ancestor which is glue or move
    new_matcher_pruning = edgecover_annotate_helper_remove_move(new_matcher_pruning);

    // At this stage we may have unmapped edges
    // If there are nodes in src_arena that have not been mapped then
    // we should create new edges (Delete edges) and add them to our edge collection
    // If there are nodes in to_arena that have not been mapped then
    // we should create new edges (Insert edges) and add them to our edge collection
    check_dst_arena_mapping_which_are_empty(&mut new_matcher_pruning); // Add dst nodes which are not mapped as insert edges
    check_src_arena_mapping_which_are_empty(&mut new_matcher_pruning); // Add src nodes which are not mapped as delete edges

    // Add any remaining move operations. Where parent's are insert.
    add_edges_where_parents_are_insert(&mut new_matcher_pruning, &move_edges);

    // check remaining edges Remove edges with no relations.
    // Remove edges where parent nodes are delete.
    remove_edges_where_delete_is_parent(&mut new_matcher_pruning);

    check_dst_arena_mapping_which_are_empty(&mut new_matcher_pruning); // Add dst nodes which are not mapped as insert edges
    check_src_arena_mapping_which_are_empty(&mut new_matcher_pruning); // Add src nodes which are not mapped as delete edges

    let helper = edgecover_helper(&new_matcher_pruning);

    (new_matcher_pruning, helper.0, helper.1)
}

/// Remove edges with parent who are delete edges.
fn remove_edges_where_delete_is_parent(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    let mut edge_to_remove: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>, EdgeType)> =
        HashSet::new();

    let mut only_delete = new_matcher_pruning.list_edges.borrow().clone();

    only_delete.retain(|&x| x.edge_type == EdgeType::DELETE);

    let mut final_edges = new_matcher_pruning.list_edges.borrow().clone();

    for edge in &only_delete {
        let edge_desc = edge.src_node
                            .descendants(&new_matcher_pruning.src_arena.borrow())
                            .collect::<HashSet<NodeId<SrcNodeId>>>();
        let mut get_all_edges_which_have_descendants =
            new_matcher_pruning.list_edges.borrow().clone();
        get_all_edges_which_have_descendants.retain(|&x| {
                                                        edge_desc.contains(&x.src_node)
                                                        && x.edge_type != EdgeType::DELETE
                                                        && edge.dst_node.id() >= x.dst_node.id()
                                                    });
        for edge_descendants_of_delete in get_all_edges_which_have_descendants {
            edge_to_remove.insert((edge_descendants_of_delete.src_node,
                                  edge_descendants_of_delete.dst_node,
                                  edge_descendants_of_delete.edge_type));
        }
    }

    final_edges.retain(|&x| !edge_to_remove.contains(&(x.src_node, x.dst_node, x.edge_type)));

    new_matcher_pruning.list_edges = RefCell::new(final_edges);
}

/// Remove edges where parents are not edges
fn remove_edges_where_parent_not_edges(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    // Lets remove edges who do not have parents in the list of edges
    let helper = edgecover_helper(new_matcher_pruning);

    // Loop through the dst nodes and check if their nodes parent exists
    let mut hash_set_edges: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>, EdgeType, usize)> =
        HashSet::new();

    // Look through all the edges and check whether their parents are have edges if they do not then
    // Case 1 : From node
    // If the src node parent is not there it means that it will be removed latter on so don't keep the edge
    // Case 2: To Node

    // Ignoring Cases for now just removing if the parent edge doesn't exist
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if !edge.src_node
                .is_root(&new_matcher_pruning.src_arena.borrow())
           && !edge.dst_node
                   .is_root(&new_matcher_pruning.dst_arena.borrow())
        {
            // Check if they have parent
            let parent_src = new_matcher_pruning.src_arena.borrow()[edge.src_node].parent();
            let parent_dst = new_matcher_pruning.dst_arena.borrow()[edge.dst_node].parent();

            if parent_src.is_some() && helper.1.get(&parent_src.unwrap()).is_none() {
                // If the src node has a parent and they have some sort of mapping.
                hash_set_edges.insert((edge.src_node, edge.dst_node, edge.edge_type, edge.value));
            }
            if parent_dst.is_some() && helper.0.get(&parent_dst.unwrap()).is_none() {
                hash_set_edges.insert((edge.src_node, edge.dst_node, edge.edge_type, edge.value));
            }
        }
    }

    // If it does not have a parent it means it will be deleted later on.
    // So it is better to remove the edge.

    let mut keep_edges: Vec<Edge> = Vec::new();
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if !hash_set_edges.contains(&(edge.src_node, edge.dst_node, edge.edge_type, edge.value)) {
            keep_edges.push(*edge);
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(keep_edges);
}

/// Only add move operations where their parents are insert operations.
fn add_edges_where_parents_are_insert(new_matcher_pruning: &mut MappingStoreGraph<String>,
                                      hash_move_edges: &HashSet<(NodeId<SrcNodeId>,
                                               NodeId<DstNodeId>,
                                               EdgeType,
                                               usize)>) {
    // Helper
    let helper = edgecover_helper(new_matcher_pruning);
    // List of edges
    // all edges
    let mut hash_edges: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    let all_edges = new_matcher_pruning.list_edges.borrow().clone();
    for edge in all_edges {
        hash_edges.insert((edge.src_node, edge.dst_node));
    }
    // ok edges
    let mut hash_ok_edges: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    let mut ok_edges = new_matcher_pruning.list_edges.borrow().clone();
    ok_edges.retain(|&x| x.edge_type == EdgeType::OK);
    for edge in ok_edges {
        hash_ok_edges.insert((edge.src_node, edge.dst_node));
    }
    // insert edges
    let mut hash_insert_edges: HashSet<NodeId<DstNodeId>> = HashSet::new();
    let mut insert_edges = new_matcher_pruning.list_edges.borrow().clone();
    insert_edges.retain(|&x| x.edge_type == EdgeType::INSERT);
    for edge in insert_edges {
        hash_insert_edges.insert(edge.dst_node);
    }
    // Delete edges
    let mut hash_delete_edges: HashSet<NodeId<SrcNodeId>> = HashSet::new();
    let mut delete_edges = new_matcher_pruning.list_edges.borrow().clone();
    delete_edges.retain(|&x| x.edge_type == EdgeType::DELETE);
    for edge in delete_edges {
        hash_delete_edges.insert(edge.src_node);
    }

    // leaf moves
    let mut filter_move_only_leaves = hash_move_edges.clone();
    // Only retain moves that are both leafs in the src and dst arena
    filter_move_only_leaves.retain(|&x| {
                                       x.0.is_leaf(&new_matcher_pruning.src_arena.borrow())
                                       && x.1.is_leaf(&new_matcher_pruning.dst_arena.borrow())
                                   });
    // Only get move operations that are "not" in the list of edges
    filter_move_only_leaves.retain(|&x| !hash_edges.contains(&(x.0, x.1)));
    // Only get edges where parents are insert or ok edges
    filter_move_only_leaves.retain(|&x| {
        (new_matcher_pruning.dst_arena.borrow()[x.1].parent()
                                                    .is_some()
         && hash_insert_edges.contains(&new_matcher_pruning.dst_arena.borrow()[x.1].parent()
                                                                                   .unwrap()))
        || (new_matcher_pruning.dst_arena.borrow()[x.1].parent()
                                                       .is_some()
            && new_matcher_pruning.src_arena.borrow()[x.0].parent()
                                                          .is_some()
            && hash_ok_edges.contains(&(new_matcher_pruning.src_arena.borrow()[x.0].parent()
                                                                                   .unwrap(),
                                      new_matcher_pruning.dst_arena.borrow()[x.1].parent()
                                                                                   .unwrap())))
    });
    filter_move_only_leaves.retain(|&x| {
                                       !helper.0.contains_key(&x.1) && !helper.1.contains_key(&x.0)
                                   });

    let delete_node: NodeId<DstNodeId> =
        NodeId::new(new_matcher_pruning.dst_arena.borrow().size() - 1);
    let insert_node: NodeId<SrcNodeId> =
        NodeId::new(new_matcher_pruning.src_arena.borrow().size() - 1);
    let mut edges_to_remove_hash: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    let mut src_node_moves_added: HashSet<NodeId<SrcNodeId>> = HashSet::new();
    let mut dst_node_moves_added: HashSet<NodeId<DstNodeId>> = HashSet::new();

    let mut final_edges = new_matcher_pruning.list_edges.borrow().clone();
    for &(src, dst, edge, size) in &filter_move_only_leaves {
        if hash_delete_edges.contains(&src)
           && hash_insert_edges.contains(&dst)
           && !src_node_moves_added.contains(&src)
           && !dst_node_moves_added.contains(&dst)
        {
            final_edges.push(Edge::new(src, dst, size, edge));
            edges_to_remove_hash.insert((src, delete_node));
            edges_to_remove_hash.insert((insert_node, dst));
            // Add visited node
            src_node_moves_added.insert(src);
            dst_node_moves_added.insert(dst);
        }
    }
    final_edges.retain(|&x| !edges_to_remove_hash.contains(&(x.src_node, x.dst_node)));
    new_matcher_pruning.list_edges = RefCell::new(final_edges);
}

/// Add Move operations if they do not exist already.
fn add_edges_move(new_matcher_pruning: &mut MappingStoreGraph<String>,
                  hash_move_edges: &HashSet<(NodeId<SrcNodeId>,
                           NodeId<DstNodeId>,
                           EdgeType,
                           usize)>) {
    // Helper
    let helper = edgecover_helper(new_matcher_pruning);
    // List of edges

    // All edges mapping move
    let mut hash_map_f_t_move: HashMap<(NodeId<SrcNodeId>, NodeId<DstNodeId>),
                                       (EdgeType, usize)> = HashMap::new();
    for edge in hash_move_edges.clone() {
        hash_map_f_t_move.insert((edge.0, edge.1), (edge.2, edge.3));
    }

    // all edges
    let mut hash_edges: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    let all_edges = new_matcher_pruning.list_edges.borrow().clone();
    for edge in all_edges {
        hash_edges.insert((edge.src_node, edge.dst_node));
    }
    // Move edges
    let mut hash_move_exist: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    for edge in hash_move_edges.clone() {
        hash_move_exist.insert((edge.0, edge.1));
    }
    // ok edges
    let mut hash_ok_edges: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    let mut ok_edges = new_matcher_pruning.list_edges.borrow().clone();
    ok_edges.retain(|&x| x.edge_type == EdgeType::OK);
    for edge in ok_edges {
        hash_ok_edges.insert((edge.src_node, edge.dst_node));
    }
    // insert edges
    let mut hash_insert_edges: HashSet<NodeId<DstNodeId>> = HashSet::new();
    let mut insert_edges = new_matcher_pruning.list_edges.borrow().clone();
    insert_edges.retain(|&x| x.edge_type == EdgeType::INSERT);
    for edge in insert_edges {
        hash_insert_edges.insert(edge.dst_node);
    }
    // Delete edges
    let mut hash_delete_edges: HashSet<NodeId<SrcNodeId>> = HashSet::new();
    let mut delete_edges = new_matcher_pruning.list_edges.borrow().clone();
    delete_edges.retain(|&x| x.edge_type == EdgeType::DELETE);
    for edge in delete_edges {
        hash_delete_edges.insert(edge.src_node);
    }

    // Glue edges
    let mut glue_edges = new_matcher_pruning.list_edges.borrow().clone();
    glue_edges.retain(|&x| x.edge_type == EdgeType::GLUE || x.edge_type == EdgeType::MOVE);
    let mut glue_src_descendants: HashSet<NodeId<SrcNodeId>> = HashSet::new();
    let mut glue_dst_descendants: HashSet<NodeId<DstNodeId>> = HashSet::new();

    for edges in glue_edges {
        let desc_src = edges.src_node
                            .descendants(&new_matcher_pruning.src_arena.borrow())
                            .collect::<Vec<NodeId<SrcNodeId>>>();
        let desc_dst = edges.dst_node
                            .descendants(&new_matcher_pruning.dst_arena.borrow())
                            .collect::<Vec<NodeId<DstNodeId>>>();

        for src in desc_src {
            glue_src_descendants.insert(src);
        }

        for dst in desc_dst {
            glue_dst_descendants.insert(dst);
        }
    }

    // leaf moves
    let mut filter_move_only_leaves = hash_move_edges.clone();
    // Only retain moves that are  leafs in the src arena
    filter_move_only_leaves.retain(|&x| x.0.is_leaf(&new_matcher_pruning.src_arena.borrow()));
    // Only get move operations that are "not" in the list of edges
    filter_move_only_leaves.retain(|&x| !hash_edges.contains(&(x.0, x.1)));
    // Remove and descendants of move or glue
    filter_move_only_leaves.retain(|&x| {
                                       !glue_dst_descendants.contains(&(x.1))
                                       && !glue_src_descendants.contains(&(x.0))
                                   });
    // Check if move operation does not already exist.
    filter_move_only_leaves.retain(|&x| {
                                       !helper.0.contains_key(&x.1) && !helper.1.contains_key(&x.0)
                                   });

    // Add move where parent are the same
    // This only goes up one level
    let mut parent_moves_level: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    for edge_move in filter_move_only_leaves.clone() {
        // Get parent and check if they are in insert
        let parent_src = new_matcher_pruning.src_arena.borrow()[edge_move.0].parent();
        let parent_dst = new_matcher_pruning.dst_arena.borrow()[edge_move.1].parent();
        if parent_src.is_some() && parent_dst.is_some()
            && hash_move_exist.contains(&(parent_src.unwrap(),parent_dst.unwrap()))
            // Check if there are no mapping
            && !helper.0.contains_key(&parent_dst.unwrap()) &&
                                      !helper.1.contains_key(&parent_src.unwrap())
            // Check if it is not a descendant of glue or move
            && !glue_dst_descendants.contains(&(parent_dst.unwrap()))
           && !glue_src_descendants.contains(&(parent_src.unwrap()))
        {
            let get_move_insert =
                hash_map_f_t_move.get(&(parent_src.unwrap(), parent_dst.unwrap()));
            if get_move_insert.is_some() {
                parent_moves_level.insert((parent_src.unwrap(), parent_dst.unwrap()));
            }
        }
    }
    let insert_node = NodeId::<SrcNodeId>::new(new_matcher_pruning.src_arena.borrow().size() - 1);
    let mut edges_dst_insert_hash: HashSet<(NodeId<SrcNodeId>,
                                           NodeId<DstNodeId>,
                                           EdgeType,
                                           usize)> = HashSet::new();
    for edge in parent_moves_level {
        // Check whether there descendants are equal
        // To Arena more then add them to insert
        // From Arena has more descendants then add to delete
        let src_desc = edge.0
                           .descendants(&new_matcher_pruning.src_arena.borrow())
                           .collect::<Vec<NodeId<SrcNodeId>>>();
        let dst_desc = edge.1
                           .descendants(&new_matcher_pruning.dst_arena.borrow())
                           .collect::<Vec<NodeId<DstNodeId>>>();
        if src_desc.len() < dst_desc.len() {
            // Add dst desc to insert
            let mut checker = true;
            for i in 0..src_desc.len() {
                if !has_same_type_and_label(src_desc[i],
                                            &new_matcher_pruning.src_arena.borrow(),
                                            dst_desc[i],
                                            &new_matcher_pruning.dst_arena.borrow())
                {
                    checker = false;
                }
            }
            if checker {
                let get_move_insert = hash_map_f_t_move.get(&(edge.0, edge.1));
                if get_move_insert.is_some() {
                    let mapping_value = *get_move_insert.unwrap();
                    filter_move_only_leaves.insert((edge.0,
                                                   edge.1,
                                                   EdgeType::GLUE,
                                                   mapping_value.1));
                    for item in dst_desc.iter().skip(src_desc.len()) {
                        if !hash_insert_edges.contains(item) && helper.0.get(item).is_none() {
                            edges_dst_insert_hash.insert((insert_node, *item, EdgeType::INSERT, 1));
                        }
                    }
                }
            }
        }
    }

    let mut src_node_moves_added: HashSet<NodeId<SrcNodeId>> = HashSet::new();
    let mut dst_node_moves_added: HashSet<NodeId<DstNodeId>> = HashSet::new();

    let mut final_edges = new_matcher_pruning.list_edges.borrow().clone();
    for &(src, dst, edge, size) in &filter_move_only_leaves {
        if !src_node_moves_added.contains(&src) && !dst_node_moves_added.contains(&dst) {
            final_edges.push(Edge::new(src, dst, size, edge));
            // Add visited node
            src_node_moves_added.insert(src);
            dst_node_moves_added.insert(dst);
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(final_edges);

    let helper = edgecover_helper(new_matcher_pruning);
    // all edges
    let mut hash_edges: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();
    let all_edges = new_matcher_pruning.list_edges.borrow().clone();
    for edge in all_edges {
        hash_edges.insert((edge.src_node, edge.dst_node));
    }
    // insert edges
    let mut hash_insert_edges: HashSet<NodeId<DstNodeId>> = HashSet::new();
    let mut insert_edges = new_matcher_pruning.list_edges.borrow().clone();
    insert_edges.retain(|&x| x.edge_type == EdgeType::INSERT);
    for edge in insert_edges {
        hash_insert_edges.insert(edge.dst_node);
    }

    // Inserted nodes visited
    let mut hash_dst_nodes_visited: HashSet<NodeId<DstNodeId>> = HashSet::new();
    let mut final_all_edges = new_matcher_pruning.list_edges.borrow().clone();
    for &(src, dst, edge, size) in &edges_dst_insert_hash {
        // Insert edge
        // Last node in src _arena (MAPS) ANY Node in To_arena
        if helper.0.get(&dst).is_none()
           && !hash_dst_nodes_visited.contains(&dst)
           && !glue_dst_descendants.contains(&dst)
        {
            hash_dst_nodes_visited.insert(dst);
            final_all_edges.push(Edge::new(src, dst, size, edge));
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(final_all_edges);
}

/// The Config2 matcher needs no configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct Config2 {}

/// Uses the edges and makes hash map that used for min edge cover solver.
/// 1 - To Node << Mapping dst >> src Nodes, Edge Type, Cost.
/// 2 - From Node << Mapping dst >> dst Nodes, Edge Type, Cost.
pub fn edgecover_helper(
    new_matcher_pruning: &MappingStoreGraph<String>)
    -> (OneToManyCostMapping<DstNodeId, SrcNodeId>, OneToManyCostMapping<SrcNodeId, DstNodeId>) {
    // To Node -> From Node
    let mut list_t_f: HashMap<NodeId<DstNodeId>, (NodeVertexSet<SrcNodeId>, usize)> =
        HashMap::new();
    for edge1 in new_matcher_pruning.list_edges.borrow().clone() {
        let mut set: NodeVertexSet<SrcNodeId> = HashSet::new();
        let mut size: usize = 1;
        for edge2 in new_matcher_pruning.list_edges.borrow().clone() {
            if !(edge1.src_node == edge2.src_node
                 && edge1.dst_node == edge2.dst_node
                 && edge1.edge_type == edge2.edge_type)
            {
                if edge2.dst_node == edge1.dst_node {
                    let temp: NodeId<SrcNodeId> = edge2.src_node;
                    set.insert((temp, edge2.edge_type, edge2.value));
                    size += 1;
                }
            } else {
                let temp: NodeId<SrcNodeId> = edge2.src_node;
                set.insert((temp, edge2.edge_type, edge2.value));
            }
        }
        let temp_dst_node: NodeId<DstNodeId> = edge1.dst_node;
        list_t_f.insert(temp_dst_node, (set, size));
    }

    // From_ Node -> To Node
    let mut list_f_t: HashMap<NodeId<SrcNodeId>, (NodeVertexSet<DstNodeId>, usize)> =
        HashMap::new();
    for edge1 in new_matcher_pruning.list_edges.borrow().clone() {
        let mut set: NodeVertexSet<DstNodeId> = HashSet::new();
        let mut size: usize = 1;
        for edge2 in new_matcher_pruning.list_edges.borrow().clone() {
            if !(edge1.src_node == edge2.src_node
                 && edge1.dst_node == edge2.dst_node
                 && edge1.edge_type == edge2.edge_type)
            {
                if edge2.src_node == edge1.src_node {
                    let temp: NodeId<DstNodeId> = edge2.dst_node;
                    set.insert((temp, edge2.edge_type, edge2.value));
                    size += 1;
                }
            } else {
                let temp: NodeId<DstNodeId> = edge2.dst_node;
                set.insert((temp, edge2.edge_type, edge2.value));
            }
        }
        let temp_dst_node: NodeId<SrcNodeId> = edge1.src_node;
        list_f_t.insert(temp_dst_node, (set, size));
    }
    (list_t_f, list_f_t)
}

/// Add Edges of type Insert for all dst_node that does not have any edge
fn check_dst_arena_mapping_which_are_empty(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    // Add all nodes not mapped to Insert
    // If all dst nodes are mapped then Good
    // But if nodes in the src_arena are not mapped that means add them to the remove section
    let mut new_edges_final = new_matcher_pruning.list_edges.borrow_mut().clone();
    let helper_mapping = edgecover_helper(new_matcher_pruning);
    let root_dst = new_matcher_pruning.dst_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.dst_arena.borrow().clone();
    let bfs_root_dst_tree = root_dst.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search
    let bfs_root_dst_tree_1 = root_dst.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search

    // checking if all dst nodes are mapped.
    let mut bool_check_if_dst_nodes_are_mapped = true;
    for dst_node in bfs_root_dst_tree {
        if !helper_mapping.0.contains_key(&dst_node) {
            bool_check_if_dst_nodes_are_mapped = false;
            break;
        }
    }

    // Insert Node
    let last_node_src_node =
        NodeId::<SrcNodeId>::new(new_matcher_pruning.src_arena.borrow().size() - 1);
    // There are some nodes not mapped
    if !bool_check_if_dst_nodes_are_mapped {
        let mut get_descendants_copy_glue: HashSet<NodeId<DstNodeId>> = HashSet::new();
        // Loop through all the edges and get the ones who have copy or glue
        for edge_traverse in &new_edges_final {
            if edge_traverse.edge_type == EdgeType::GLUE
               || edge_traverse.edge_type == EdgeType::COPY
            {
                let children_copy_glue = edge_traverse.dst_node
                                                      .descendants(&new_matcher_pruning.dst_arena
                                                                                       .borrow())
                                                      .collect::<Vec<NodeId<DstNodeId>>>();
                for children in children_copy_glue {
                    get_descendants_copy_glue.insert(children);
                }
            }
        }

        for dst_node in bfs_root_dst_tree_1 {
            if !helper_mapping.0.contains_key(&dst_node) {
                let edge_created = Edge::new(last_node_src_node, dst_node, 1, EdgeType::INSERT);
                if !get_descendants_copy_glue.contains(&dst_node) {
                    // Check if its a descendant of glue of copy thats is why there is no edge.
                    new_edges_final.push(edge_created);
                }
            }
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(new_edges_final.to_vec());
}

/// Add Edges of type Delete for all src_node that does not have any edge
fn check_src_arena_mapping_which_are_empty(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    // Add all nodes not mapped to delete
    // If all dst nodes are mapped then Good
    // But if nodes in the src_arena are not mapped that means add them to the remove section

    let mut new_edges_final = new_matcher_pruning.list_edges.borrow_mut().clone();
    let helper_mapping = edgecover_helper(new_matcher_pruning);
    let root_src = new_matcher_pruning.src_arena.borrow().root().unwrap(); // From Tree Root
    let reference_matcher = &new_matcher_pruning.src_arena.borrow().clone();
    let bfs_root_src_tree = root_src.breadth_first_traversal(reference_matcher); // Root From Traversal BFS Search
    let bfs_root_src_tree_1 = root_src.breadth_first_traversal(reference_matcher); // Root From Traversal BFS Search

    // checking if all dst nodes are mapped.
    let mut bool_check_if_src_nodes_are_mapped = true;
    for src_node in bfs_root_src_tree {
        if !helper_mapping.1.contains_key(&src_node) {
            bool_check_if_src_nodes_are_mapped = false;
            break;
        }
    }

    // Delete Node
    let last_node_dst_node =
        NodeId::<DstNodeId>::new(new_matcher_pruning.dst_arena.borrow().size() - 1);
    // There are some nodes not mapped
    if !bool_check_if_src_nodes_are_mapped {
        let mut get_descendants_copy_glue: HashSet<NodeId<SrcNodeId>> = HashSet::new();
        // Loop through all the edges and get the ones who have copy or glue
        for edge_traverse in &new_edges_final {
            if edge_traverse.edge_type == EdgeType::GLUE
               || edge_traverse.edge_type == EdgeType::COPY
            {
                let children_copy_glue = edge_traverse.src_node
                                                      .descendants(&new_matcher_pruning.src_arena
                                                                                       .borrow())
                                                      .collect::<Vec<NodeId<SrcNodeId>>>();
                for children in children_copy_glue {
                    get_descendants_copy_glue.insert(children);
                }
            }
        }

        for src_node in bfs_root_src_tree_1 {
            if !helper_mapping.1.contains_key(&src_node) {
                let edge_created = Edge::new(src_node, last_node_dst_node, 1, EdgeType::DELETE);
                if !get_descendants_copy_glue.contains(&src_node) {
                    // Check if its a descendant of glue of copy thats is why there is no edge.
                    new_edges_final.push(edge_created);
                }
            }
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(new_edges_final.to_vec());
}

/// Apply edit script to the src_arena.
pub fn edgecover_apply_edit_script(new_matcher_pruning: &mut MappingStoreGraph<String>)
                                   -> (EditScript<String>) {
    // Final Edit script
    let mut script: EditScript<String> = EditScript::new();
    let mut src_arena = new_matcher_pruning.src_arena.borrow_mut();
    let dst_arena = new_matcher_pruning.dst_arena.borrow();

    // Parent Mapping hash map.
    let mut mapping_t_f: HashMap<NodeId<DstNodeId>, NodeId<SrcNodeId>> = HashMap::new();
    let mut check_mapper = edgecover_helper(new_matcher_pruning); // Mappings (1) To -> From    (2) From -> To

    let root_dst = new_matcher_pruning.dst_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.dst_arena.borrow().clone();

    let bfs_root_dst_tree = root_dst.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search
    let bfs_root_dst_tree_node = root_dst.breadth_first_traversal(reference_matcher); // Root to Traversal BFS Search Copy

    for dst_node in bfs_root_dst_tree_node {
        let get_edge = (check_mapper.0).get(&dst_node);
        if get_edge.is_some() {
            for &(src, _, _) in &get_edge.unwrap().0 {
                mapping_t_f.insert(dst_node, src);
            }
        }
    }

    let mut indexes_t_f: HashMap<usize, usize> = HashMap::new();
    let mut hash_src_ids_inserted: HashSet<NodeId<SrcNodeId>> = HashSet::new();
    // Apply all the edit script operation.
    for dst_node in bfs_root_dst_tree {
        let get_edge = (check_mapper.0).get(&dst_node);
        if get_edge.is_some() {
            for &(src, edge_type, _) in &get_edge.unwrap().0 {
                if edge_type == EdgeType::MOVE {
                    let parent_index_1 = dst_arena[dst_node].parent().unwrap().id();
                    // Check if the src node parent exists
                    let size_src_arena: usize = src_arena.size();
                    let index_parent_src_index =
                        *get_index_src_node(&indexes_t_f.clone(), parent_index_1);
                    if index_parent_src_index < size_src_arena {
                        let get_position = dst_node.get_child_position(&(dst_arena));
                        let parent_src_node = NodeId::<SrcNodeId>::new(index_parent_src_index);
                        // if the parent exists then do the operation
                        // if the parent are the same and the position are the same then we dont need to perform the operation
                        let parent_src_node_checker = src_arena[src].parent().unwrap();
                        if !(src_arena[parent_src_node_checker].ty == src_arena[parent_src_node].ty
                             && src_arena[parent_src_node_checker].label
                                == src_arena[parent_src_node].label
                             && src.get_child_position(&src_arena).unwrap()
                                == (get_position.unwrap())
                             && parent_src_node_checker == parent_src_node)
                        {
                            let mut move_action =
                                Move::new(src, parent_src_node, get_position.unwrap() as u16);
                            move_action.apply(&mut src_arena)
                                       .expect("Cannot apply MOVE action to AST.");
                            script.push(move_action);
                            hash_src_ids_inserted.insert(src);
                        }
                        // The if above checks if they were in the same position if they are not then move otherwise "Perform the move operation"
                        indexes_t_f.insert(dst_node.id(), src.id());
                    }
                } else if edge_type == EdgeType::GLUE {
                    // Get Parent
                    let parent_index_1 = dst_arena[dst_node].parent().unwrap().id();
                    let get_position = dst_node.get_child_position(&(dst_arena));
                    // if the parent exists then do the operation
                    let size_src_arena: usize = src_arena.size();
                    let index_parent_src_index =
                        *get_index_src_node(&indexes_t_f.clone(), parent_index_1);
                    if index_parent_src_index < size_src_arena {
                        let src_node_parent: NodeId<SrcNodeId> =
                            NodeId::new(index_parent_src_index);
                        let mut glue_action =
                            Glue::new(src, src_node_parent, get_position.unwrap() as u16);
                        glue_action.apply(&mut src_arena)
                                   .expect("Cannot apply GLUE action to AST.");
                        script.push(glue_action);
                    }
                    indexes_t_f.insert(dst_node.id(), src.id());
                } else if edge_type == EdgeType::COPY {
                    let parent_index_1 = dst_arena[dst_node].parent().unwrap().id();

                    let mut get_position = dst_node.get_child_position(&(dst_arena));

                    // if the parent exists then do the operation
                    let size_src_arena: usize = src_arena.size();

                    let index_parent_src_index =
                        *get_index_src_node(&indexes_t_f.clone(), parent_index_1);

                    if index_parent_src_index < size_src_arena {
                        let src_node_parent: NodeId<SrcNodeId> =
                            NodeId::new(index_parent_src_index);

                        let mut copy_action =
                            Copy::new(src_node_parent, src, get_position.unwrap() as u16);
                        copy_action.apply(&mut src_arena)
                                   .expect("Cannot apply COPY action to AST.");
                        script.push(copy_action);
                    }
                    indexes_t_f.insert(dst_node.id(), src.id());
                } else if edge_type == EdgeType::INSERT {
                    let mut get_position = dst_node.get_child_position(&(dst_arena));
                    // Get Parent
                    let parent_index_1 = dst_arena[dst_node].parent().unwrap().id();
                    let size_src_arena: usize = src_arena.size();

                    let index_parent_src_index = if indexes_t_f.contains_key(&parent_index_1) {
                        *get_index_src_node(&indexes_t_f.clone(), parent_index_1)
                    } else {
                        parent_index_1
                    };
                    if index_parent_src_index < size_src_arena {
                        let src_node_parent: NodeId<SrcNodeId> =
                            NodeId::new(index_parent_src_index);
                        let mut new_node: NodeId<SrcNodeId> =
                            src_arena.new_node(dst_arena[dst_node].ty.clone(),
                                               dst_arena[dst_node].label.clone(),
                                               dst_arena[dst_node].col_no,
                                               dst_arena[dst_node].line_no,
                                               None,
                                               None);

                        // Check if the parent node are the same
                        // Check if the new inserted node doesn't exist
                        // Get Parents children
                        let inserting_parent_children =
                            src_node_parent.children(&src_arena)
                                           .collect::<Vec<NodeId<SrcNodeId>>>();
                        let size_of_children = inserting_parent_children.len();
                        if get_position.unwrap() > size_of_children {
                            get_position = Some(size_of_children);
                        }

                        let mut insert_action = Insert::new(new_node,
                                                            Some(src_node_parent),
                                                            get_position.unwrap() as u16);
                        insert_action.apply(&mut src_arena)
                                     .expect("Cannot apply INSERT action to AST.");
                        script.push(insert_action);

                        indexes_t_f.insert(dst_node.id(), new_node.id());
                    }
                } else if edge_type == EdgeType::UPDATE {
                    // Check if the src node parent exists
                    let size_src_arena: usize = src_arena.size();
                    let parent_index_1 = dst_arena[dst_node].parent().unwrap().id();
                    let index_parent_src_index = if indexes_t_f.contains_key(&parent_index_1) {
                        *get_index_src_node(&indexes_t_f.clone(), parent_index_1)
                    } else {
                        parent_index_1
                    };
                    if index_parent_src_index < size_src_arena {
                        // if the parent exists then do the operation
                        let mut update_action = Update::new(src,
                                                            dst_arena[dst_node].ty.clone(),
                                                            dst_arena[dst_node].label.clone());
                        update_action.apply(&mut src_arena)
                                     .expect("Cannot apply UPDATE action to AST.");
                        script.push(update_action);
                    }
                    indexes_t_f.insert(dst_node.id(), src.id());
                } else if edge_type == EdgeType::OK {
                    indexes_t_f.insert(dst_node.id(), src.id());
                }
            }
        }
    }

    // Do Delete Edit operation first.
    check_mapper = edgecover_helper(new_matcher_pruning);

    let root_src = src_arena.root().unwrap(); // From Tree Root
    let reference_matcher_src = &src_arena.clone();
    let post_order_src_tree = root_src.post_order_traversal(reference_matcher_src); // Traversal Post Order To Delete Nodes
    let mut vec_delete_actions: Vec<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = Vec::new();
    for src_node in post_order_src_tree {
        // Previously
        let get_edge = (check_mapper.1).get(&src_node);
        if get_edge.is_some() {
            for &(dst, edge_type, _) in &get_edge.unwrap().0 {
                if edge_type == EdgeType::DELETE {
                    let mut delete_action = Delete::new(src_node);
                    script.push(delete_action.clone());
                    delete_action.apply(&mut src_arena)
                                 .expect("Cannot apply DELETE action to AST.");
                    vec_delete_actions.push((src_node, dst));
                }
            }
        }
    }
    (script)
}

/// Remove all edges of copy and glue that have move as their descendants
fn edgecover_annotate_helper_remove_move(mut new_matcher_pruning: MappingStoreGraph<String>)
                                         -> MappingStoreGraph<String> {
    let all_edges = new_matcher_pruning.list_edges.borrow_mut().clone(); // Vec<Edge> -- type
    let all_edge_run = all_edges.clone();
    let mut remove_index: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>, EdgeType, usize)> =
        HashSet::new();
    // All edges glue or move
    let mut all_edges_move_glue = all_edge_run.clone();
    all_edges_move_glue.retain(|&x| x.edge_type == EdgeType::MOVE || x.edge_type == EdgeType::GLUE);

    // get list of moves and remove them.
    for edge in &all_edge_run {
        if edge.edge_type == EdgeType::COPY || edge.edge_type == EdgeType::GLUE {
            // Look at their descendants
            let all_src_descendants = edge.src_node
                                          .descendants(&new_matcher_pruning.src_arena.borrow())
                                          .collect::<HashSet<NodeId<SrcNodeId>>>();
            // counter
            for edge_move in all_edge_run.clone() {
                // Look for edge move
                if edge_move.edge_type == EdgeType::MOVE
                   || edge_move.edge_type == EdgeType::GLUE
                      && all_src_descendants.contains(&edge_move.src_node)
                      && !remove_index.contains(&(edge_move.src_node,
                                                edge_move.dst_node,
                                                edge_move.edge_type,
                                                edge_move.value))
                {
                    remove_index.insert((edge_move.src_node,
                                        edge_move.dst_node,
                                        edge_move.edge_type,
                                        edge_move.value));
                }
            }
        }
    }

    let mut all_edges_output: Vec<Edge> = Vec::new();
    for edge_iter in all_edges.clone() {
        if !remove_index.contains(&(edge_iter.src_node,
                                  edge_iter.dst_node,
                                  edge_iter.edge_type,
                                  edge_iter.value))
        {
            all_edges_output.push(edge_iter);
        }
    }

    new_matcher_pruning.list_edges = RefCell::new(all_edges_output.to_vec()); // Make reassign the list edges

    // Remove any copy operations that have the same dst node ancestor node

    let mut all_edge_copy = all_edge_run.clone();
    all_edge_copy.retain(|&x| x.edge_type == EdgeType::COPY);

    let mut hash_dst_node: HashSet<NodeId<DstNodeId>> = HashSet::new();
    for e in all_edge_copy.clone() {
        hash_dst_node.insert(e.dst_node);
    }

    let root_dst = new_matcher_pruning.dst_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.dst_arena.borrow().clone();
    let bfs_root_dst_tree = root_dst.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search

    // Remove copy within copy
    let mut hash_set_copy_visited: HashSet<NodeId<DstNodeId>> = HashSet::new();
    let mut hash_set_copy_visited_good: HashSet<NodeId<DstNodeId>> = HashSet::new();
    for dst_node in bfs_root_dst_tree {
        // The dst node is an copy edge
        if hash_dst_node.contains(&dst_node) && !hash_set_copy_visited.contains(&dst_node) {
            let dst_node_desc = dst_node.descendants(&new_matcher_pruning.dst_arena.borrow())
                                        .collect::<HashSet<NodeId<DstNodeId>>>();

            for t_node_desc in dst_node_desc {
                hash_set_copy_visited.insert(t_node_desc);
            }
            hash_set_copy_visited_good.insert(dst_node);
        }
    }

    let mut edges_dst_keep = new_matcher_pruning.list_edges.borrow_mut().clone();
    edges_dst_keep.retain(|&x| x.edge_type != EdgeType::COPY);
    for edge_copy in all_edge_copy.clone() {
        if hash_set_copy_visited_good.contains(&edge_copy.dst_node) {
            edges_dst_keep.push(edge_copy);
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(edges_dst_keep.to_vec()); // Make reassign the list edges

    new_matcher_pruning
}

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
pub fn get_index_src_node<T: BuildHasher>(map: &HashMap<usize, usize, T>,
                                          dst_node: usize)
                                          -> &usize {
    map.get(&dst_node).expect("Node not found.")
}

impl<T: Clone + Debug + Eq + 'static> MatchingTreesScriptor<T> for Config2 {
    /// Perform matches
    fn match_trees(&self,
                   src: Arena<T, SrcNodeId>,
                   dst: Arena<T, DstNodeId>,
                   cost_all: CostEdge)
                   -> MappingStoreGraph<T> {
        let mut store = MappingStoreGraph::new(src.clone(), dst.clone());
        if store.src_arena.borrow().is_empty() || store.dst_arena.borrow().is_empty() {
            return store;
        }
        store.all_edge_cost = cost_all;
        let cost_store_ok = store.all_edge_cost.ok;
        let cost_store_move = store.all_edge_cost.move_;
        let cost_store_update = store.all_edge_cost.update;
        let cost_store_copy = store.all_edge_cost.copy;
        let cost_store_glue = store.all_edge_cost.glue;
        let cost_store_insert = store.all_edge_cost.insert;
        let cost_store_delete = store.all_edge_cost.delete;

        let last_node_src_node = NodeId::<SrcNodeId>::new(src.size() - 1);
        let last_node_dst_node = NodeId::<DstNodeId>::new(dst.clone().size() - 1);
        let src_pre = store.src_arena
                           .borrow()
                           .root()
                           .unwrap()
                           .pre_order_traversal(&store.src_arena.borrow())
                           .collect::<Vec<NodeId<SrcNodeId>>>();
        let dst_pre = store.dst_arena
                           .borrow()
                           .root()
                           .unwrap()
                           .pre_order_traversal(&store.dst_arena.borrow())
                           .collect::<Vec<NodeId<DstNodeId>>>();
        // Hash Set See if it contains insertion and deletion
        let mut set_contains_insertion = HashSet::<(NodeId<SrcNodeId>, NodeId<DstNodeId>)>::new();
        let mut set_contains_deletion = HashSet::<(NodeId<SrcNodeId>, NodeId<DstNodeId>)>::new();
        for (src_node_id, src_node) in src_pre.iter().enumerate() {
            let mut bool_check = false;
            for (dst_node_id, dst_node) in dst_pre.iter().enumerate() {
                if has_same_type_and_label(*src_node,
                                           &store.src_arena.borrow(),
                                           *dst_node,
                                           &store.dst_arena.borrow())
                {
                    if src_node_id == dst_node_id {
                        let parent_src_node =
                            store.src_arena.borrow()[src_pre[src_node_id]].parent();
                        let parent_dst_node =
                            store.dst_arena.borrow()[dst_pre[dst_node_id]].parent();
                        // Check if their parents are the same
                        // Then OK should be good to go
                        if (src_pre[src_node_id].is_root(&store.src_arena.borrow())
                            && dst_pre[dst_node_id].is_root(&store.dst_arena.borrow()))
                           || (parent_src_node.is_some() && parent_dst_node.is_some()
                               && has_same_type_and_label(parent_src_node.unwrap(),
                                                          &store.src_arena.borrow(),
                                                          parent_dst_node.unwrap(),
                                                          &store.dst_arena.borrow()))
                        {
                            // If the parent are equal if so then OK edge
                            store.push_edge(src_pre[src_node_id],
                                            dst_pre[dst_node_id],
                                            cost_store_ok,
                                            EdgeType::OK);
                        }
                    } else {
                        let descendants_src =
                            src_pre[src_node_id].descendants(&store.src_arena.borrow())
                                                .collect::<HashSet<NodeId<SrcNodeId>>>();
                        let descendants_dst =
                            dst_pre[dst_node_id].descendants(&store.dst_arena.borrow())
                                                .collect::<HashSet<NodeId<DstNodeId>>>();
                        if descendants_src.len() == descendants_dst.len() {
                            store.push_edge(src_pre[src_node_id],
                                            dst_pre[dst_node_id],
                                            cost_store_move,
                                            EdgeType::MOVE);
                        }
                    }
                    bool_check = true;
                } else if has_same_type(src_pre[src_node_id],
                                        &store.src_arena.borrow(),
                                        dst_pre[dst_node_id],
                                        &store.dst_arena.borrow())
                          && src_node_id == dst_node_id
                {
                    // the same label and value but different position
                    store.push_edge(src_pre[src_node_id],
                                    dst_pre[dst_node_id],
                                    cost_store_update,
                                    EdgeType::UPDATE);
                }

                // We have reached the last node in the dst tree
                if !bool_check && dst_node_id == dst_pre.len() - 1 {
                    store.delete_node(src_pre[src_node_id]);
                    store.push_edge(src_pre[src_node_id],
                                    last_node_dst_node,
                                    cost_store_delete,
                                    EdgeType::DELETE);
                    set_contains_deletion.insert((src_pre[src_node_id], last_node_dst_node));
                }
            }
        }
        // for dst_node_id in 0..dst_pre.len() {
        for dst_node in &dst_pre {
            let mut bool_check = false;
            for (src_node_id, src_node) in src_pre.iter().enumerate() {
                if has_same_type_and_label(*src_node,
                                           &store.src_arena.borrow(),
                                           *dst_node,
                                           &store.dst_arena.borrow())
                {
                    bool_check = true;
                }
                if !bool_check && src_node_id == src_pre.len() - 1 {
                    store.push_edge(last_node_src_node,
                                    *dst_node,
                                    cost_store_insert,
                                    EdgeType::INSERT);
                    set_contains_insertion.insert((last_node_src_node, *dst_node));
                }
            }
        }
        let get_all_edges = store.list_edges.borrow().clone();
        // Mappings mean
        let mut all_move_mapping_t_f: HashMap<NodeId<DstNodeId>,
                                              HashSet<NodeId<SrcNodeId>>> = HashMap::new();
        let mut all_move_mapping_f_t: HashMap<NodeId<SrcNodeId>,
                                              HashSet<NodeId<DstNodeId>>> = HashMap::new();

        let mut all_move_mapping_t_f_leaves: HashMap<NodeId<DstNodeId>,
                                                     HashSet<NodeId<SrcNodeId>>> = HashMap::new();
        let mut all_move_mapping_f_t_leaves: HashMap<NodeId<SrcNodeId>,
                                                     HashSet<NodeId<DstNodeId>>> = HashMap::new();

        let mut all_edges_move = HashSet::<(NodeId<SrcNodeId>, NodeId<DstNodeId>, EdgeType)>::new();
        let mut all_leaves_move =
            HashSet::<(NodeId<SrcNodeId>, NodeId<DstNodeId>, EdgeType)>::new();
        for edge in get_all_edges.clone() {
            if edge.edge_type == EdgeType::MOVE {
                // start dst mappings
                all_move_mapping_t_f.entry(edge.dst_node)
                                    .or_insert_with(HashSet::new)
                                    .insert(edge.src_node);
                all_move_mapping_f_t.entry(edge.src_node)
                                    .or_insert_with(HashSet::new)
                                    .insert(edge.dst_node);
                // end of mappings

                let edge_copy = edge;
                all_edges_move.insert((edge.src_node, edge.dst_node, edge.edge_type));
                let m_check = edge.src_node.is_leaf(&store.src_arena.borrow());
                let n_check = edge.dst_node.is_leaf(&store.dst_arena.borrow());
                if m_check && n_check {
                    all_move_mapping_t_f_leaves.entry(edge.dst_node)
                                               .or_insert_with(HashSet::new)
                                               .insert(edge.src_node);
                    all_move_mapping_f_t_leaves.entry(edge.src_node)
                                               .or_insert_with(HashSet::new)
                                               .insert(edge.dst_node);

                    all_leaves_move.insert((edge_copy.src_node,
                                           edge_copy.dst_node,
                                           edge_copy.edge_type));
                }
            }
        }
        let mut all_leaves_parent = HashSet::<(NodeId<SrcNodeId>,
                                            NodeId<DstNodeId>,
                                            NodeId<SrcNodeId>,
                                            NodeId<DstNodeId>)>::new();
        // Check if its nodes are in the leaf move operation
        for edge in all_leaves_move.clone() {
            let e1_parent = store.src_arena.borrow()[edge.0].parent().unwrap();
            let e1_dst_node_parent = store.dst_arena.borrow()[edge.1].parent().unwrap();

            // first check parent nodes are have move operation
            // Then check thier children also have move operation
            // The children nodes of both src_node parent and dst_node_parent
            // should have edges to one another.
            if all_edges_move.contains(&(e1_parent, e1_dst_node_parent, EdgeType::MOVE)) {
                let e1_siblings = e1_parent.children(&store.src_arena.borrow())
                                           .collect::<Vec<NodeId<SrcNodeId>>>();
                let e1_dst_node_siblings = e1_dst_node_parent.children(&store.dst_arena.borrow())
                                                             .collect::<Vec<NodeId<DstNodeId>>>();

                // First check
                // See if the parent in the dst_nodes have the same number of
                // children as the number of siblings.
                if e1_dst_node_siblings.len() == e1_siblings.len() {
                    let mut counter_of_children_mapping = 0;
                    for i in 0..e1_siblings.len() {
                        if all_leaves_move.contains(&(e1_siblings[i],
                                                    e1_dst_node_siblings[i],
                                                    EdgeType::MOVE))
                        {
                            counter_of_children_mapping += 1;
                        }
                    }
                    if counter_of_children_mapping == e1_dst_node_siblings.len() {
                        all_leaves_parent.insert((e1_parent, e1_dst_node_parent, edge.0, edge.1));
                    }
                }
            }
        }
        // Loop through parents and check if they are in move hash set
        let mut end_copy_tree = HashSet::<(NodeId<SrcNodeId>, NodeId<DstNodeId>)>::new();
        for edge in all_leaves_parent.clone() {
            let mut edge_copy = edge;
            let mut edge_change = (edge_copy.0, edge_copy.1);
            let mut vector_input: Vec<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = Vec::new();
            vector_input.push((edge_change.0, edge_change.1));
            loop {
                let parent_m_option = store.src_arena.borrow()[edge_change.0].parent();
                let parent_n_option = store.dst_arena.borrow()[edge_change.1].parent();
                if parent_n_option.is_some() && parent_m_option.is_some() {
                    let parent_m = parent_m_option.unwrap();
                    let parent_n = parent_n_option.unwrap();

                    if all_edges_move.contains(&(parent_m, parent_n, EdgeType::MOVE))
                       && !parent_m.is_root(&store.src_arena.borrow())
                       && !parent_n.is_root(&store.dst_arena.borrow())
                    {
                        edge_change = (parent_m, parent_n);
                        vector_input.push((edge_change.0, edge_change.1));
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
        let mut hash_set_copy_glue_edges: HashSet<(NodeId<SrcNodeId>,
                                                  NodeId<DstNodeId>,
                                                  EdgeType)> = HashSet::new();
        for nodes in end_copy_tree {
            if store.check_subtree(nodes.0, nodes.1) {
                // - If insertion in sub-tree src tree 2
                // - If deletion in sub-tree src tree 1
                //  -> Then it means the glue operations has occurred.
                // If node.0 in tree1 Exists in tree 2 then its copy
                // Otherwise its glue
                if !store.check_tree_1_exists_in_tree2(nodes.0) {
                    store.push_edge(nodes.0, nodes.1, cost_store_glue, EdgeType::GLUE);
                    hash_set_copy_glue_edges.insert((nodes.0, nodes.1, EdgeType::GLUE));
                } else {
                    store.push_edge(nodes.0, nodes.1, cost_store_copy, EdgeType::COPY);
                    hash_set_copy_glue_edges.insert((nodes.0, nodes.1, EdgeType::COPY));
                }
            }
        }
        store
    }

    /// Describe this matcher for the user.
    fn describe(&self) -> String {
        String::from(
            "This matcher performs matching operation from the paper Chawathe et al. (1998).",
        )
    }
    /// Prune Edges
    fn prune_edges(&self, input: &mut MappingStoreGraph<T>) -> (Vec<Edge>, Vec<Edge>) {
        // Loop Though all the edges
        let ct1 = cmp::max(input.all_edge_cost.move_, input.all_edge_cost.copy);
        // ct the max value of move, copy and glue.
        let ct = cmp::max(ct1, input.all_edge_cost.glue);
        let all_edges = input.list_edges.borrow().clone();
        let mut edges_to_be_pruned: Vec<Edge> = Vec::new();
        let mut edges_still_left: Vec<Edge> = Vec::new();

        // Loop through all the edges and get their src,and dst and type
        let mut hash_edges: HashSet<(NodeId<SrcNodeId>, NodeId<DstNodeId>)> = HashSet::new();

        for edge in &all_edges {
            if edge.edge_type == EdgeType::MOVE {
                hash_edges.insert((edge.src_node, edge.dst_node));
            }
        }
        // Get lower bounds for all edges.
        let all_lower_bound_edges = input.make_all_lower_bound_edges(&hash_edges);
        let all_upper_bound_edges = input.make_all_upper_bound_edges(&hash_edges);
        let mut list_t_f: HashMap<NodeId<DstNodeId>, (NodeVertexSet<SrcNodeId>, usize)> =
            HashMap::new();
        for edge1 in input.list_edges.borrow().clone() {
            let mut set: NodeVertexSet<SrcNodeId> = HashSet::new();
            let mut size: usize = 1;
            for edge2 in input.list_edges.borrow().clone() {
                if !(edge1.src_node == edge2.src_node
                     && edge1.dst_node == edge2.dst_node
                     && edge1.edge_type == edge2.edge_type)
                {
                    if edge2.dst_node == edge1.dst_node {
                        let temp: NodeId<SrcNodeId> = edge2.src_node;
                        set.insert((temp, edge2.edge_type, edge2.value));
                        size += 1;
                    }
                } else {
                    let temp: NodeId<SrcNodeId> = edge2.src_node;
                    set.insert((temp, edge2.edge_type, edge2.value));
                }
            }
            let temp_dst_node: NodeId<DstNodeId> = edge1.dst_node;
            list_t_f.insert(temp_dst_node, (set, size));
        }

        // From_ Node -> To Node
        let mut list_f_t: HashMap<NodeId<SrcNodeId>, (NodeVertexSet<DstNodeId>, usize)> =
            HashMap::new();
        for edge1 in input.list_edges.borrow().clone() {
            let mut set: HashSet<(NodeId<DstNodeId>, EdgeType, usize)> = HashSet::new();
            let mut size: usize = 1;
            for edge2 in input.list_edges.borrow().clone() {
                if !(edge1.src_node == edge2.src_node
                     && edge1.dst_node == edge2.dst_node
                     && edge1.edge_type == edge2.edge_type)
                {
                    if edge2.src_node == edge1.src_node {
                        let temp: NodeId<DstNodeId> = edge2.dst_node;
                        set.insert((temp, edge2.edge_type, edge2.value));
                        size += 1;
                    }
                } else {
                    let temp: NodeId<DstNodeId> = edge2.dst_node;
                    set.insert((temp, edge2.edge_type, edge2.value));
                }
            }
            let temp_dst_node: NodeId<SrcNodeId> = edge1.src_node;
            list_f_t.insert(temp_dst_node, (set, size));
        }
        let helper_function = (list_t_f, list_f_t);

        for edge in &all_edges {
            // edge variable will be E1 --> (m,n)
            // Get All The edges for E2 --> where (m,n1)
            // Where the src_node_id is the same but dst_id is different
            // Get All The edges for E3 ---> where (m1,n)
            // Where the dst_node_id is the same but dst src_id is different
            // E2 = (m,n1) &&   E3 = (m1,n)

            let m = edge.src_node;
            let n = edge.dst_node;

            // Lets get edge E2
            let e2_dst_node_checker = helper_function.1.get(&m);
            let e2_dst_node = e2_dst_node_checker.map(|x| x.0.iter().nth(0)).unwrap();

            let e3_src_node_checker = helper_function.0.get(&n);
            let e3_src_node = e3_src_node_checker.map(|x| x.0.iter().nth(0)).unwrap();
            // Get the upper bound of e2
            let upper_bound_e2 = *all_upper_bound_edges.get(&(m, e2_dst_node.unwrap().0))
                                                       .unwrap_or_else(|| &1);
            let upper_bound_e3 = *all_upper_bound_edges.get(&(e3_src_node.unwrap().0, n))
                                                       .unwrap_or_else(|| &1);

            let lower_bound_e1 = *all_lower_bound_edges.get(&(m, n)).unwrap_or_else(|| &1);
            let descendants_m_delete =
                m.descendants(&input.src_arena.borrow()).collect::<Vec<NodeId<SrcNodeId>>>();
            let descendants_n_insert =
                n.descendants(&input.dst_arena.borrow()).collect::<Vec<NodeId<DstNodeId>>>();

            if (lower_bound_e1 >= upper_bound_e2 + upper_bound_e3 + 2 * ct)
               || (lower_bound_e1
                   >= input.all_edge_cost.insert * (descendants_n_insert.len() + 1)
                      + (input.all_edge_cost.delete * descendants_m_delete.len() + 1))
            {
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
                let m = e1_update.src_node
                                 .descendants(&input.src_arena.borrow())
                                 .collect::<Vec<NodeId<SrcNodeId>>>()
                                 .len();
                let n = e1_update.dst_node
                                 .descendants(&input.dst_arena.borrow())
                                 .collect::<Vec<NodeId<DstNodeId>>>()
                                 .len();
                // if the number of descendants are the same then ok to update
                // Otherwise you may need to insert nodes and delete etc so
                // update won't make any sense.
                if m == n {
                    edges_after_pruning_update.push(e1_update);
                }
            } else {
                edges_after_pruning_update.push(e1_update);
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

            if e1_root.src_node.is_root(&input.src_arena.borrow())
               || e1_root.dst_node.is_root(&input.dst_arena.borrow())
            {
                // There should be nothing moving the root of a tree.
                // This is described in the chawathe 98 paper.
                // Same logic as the if statement above
                if e1_root.edge_type != EdgeType::MOVE {
                    remaining_edges_remove_root_move.push(e1_root);
                }
            } else {
                remaining_edges_remove_root_move.push(e1_root);
            }
        }
        edges_still_left = remaining_edges_remove_root_move;
        // Remove weird move operation and copy operation
        // If move and copy exist exist together take the one which has the same descendants
        // And Both Map to the same node

        let all_edges_remaining = edges_still_left.clone();
        let mut remaining_edges: Vec<Edge> = Vec::new();
        for edge_move in all_edges_remaining.clone() {
            let m_move = edge_move.src_node;
            let n_move = edge_move.dst_node;
            let mut check = false;
            if edge_move.edge_type == EdgeType::MOVE {
                for edge_glue in all_edges_remaining.clone() {
                    let m_glue = edge_glue.src_node;
                    let n_glue = edge_glue.dst_node;

                    if edge_glue.edge_type == EdgeType::GLUE
                       && edge_move.edge_type == EdgeType::MOVE
                       && m_move == m_glue
                       && n_move == n_glue
                    {
                        // We have found a copy so the "check" variable is true
                        // so we have found (m,n) which is move and copy
                        check = true;
                        // We add copy edge to the remaining edges
                        remaining_edges.push(edge_glue);
                    }
                }
            } else if edge_move.edge_type != EdgeType::GLUE {
                remaining_edges.push(edge_move);
            }
            if !check && edge_move.edge_type == EdgeType::MOVE {
                remaining_edges.push(edge_move);
            }
        }
        edges_still_left = remaining_edges;

        input.list_edges = RefCell::new(edges_still_left.clone());
        (edges_still_left, edges_to_be_pruned)
    }
}

/// Match two trees and return a store of mappings between them.
///
/// This trait should usually be implemented on configuration objects that
/// define thresholds and weights for a given algorithm.
pub trait MatchingTreesScriptor<T: Clone + Debug> {
    /// Match two trees and return a store of mappings between them.
    fn match_trees(&self,
                   src: Arena<T, SrcNodeId>,
                   dst: Arena<T, DstNodeId>,
                   costs_edges: CostEdge)
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

/// Test for actual files
pub fn chawathe_matching_actual(mut src_arena: Arena<String, SrcNodeId>,
                                mut dst_arena: Arena<String, DstNodeId>,
                                user_defined_cost: CostEdge)
                                -> EditScriptResult<String> {
    src_arena.new_node(String::from("INSERT"),
                       String::from("INSERT"),
                       None,
                       None,
                       None,
                       None);
    dst_arena.new_node(String::from("DELETE"),
                       String::from("DELETE"),
                       None,
                       None,
                       None,
                       None);

    let matching_config = Config2::new();

    // Define cost from use input
    let cost_edges = user_defined_cost;
    let mut matcher = matching_config.match_trees(src_arena.clone(), dst_arena.clone(), cost_edges);

    // Prune Edges
    matching_config.prune_edges(&mut matcher);

    // Start of Min Edge Cover
    let edge_min = edgecover_solver(matcher);
    let mut new_matcher_pruning = edge_min.0;

    // Apply edit script operations
    let edit_script = edgecover_apply_edit_script(&mut new_matcher_pruning);

    let src_root = new_matcher_pruning.src_arena.borrow().root().unwrap();
    let dst_root = new_matcher_pruning.dst_arena.borrow().root().unwrap();
    let traverse_src = src_root.breadth_first_traversal(&new_matcher_pruning.src_arena.borrow())
                               .collect::<Vec<NodeId<SrcNodeId>>>();
    let traverse_dst = dst_root.breadth_first_traversal(&new_matcher_pruning.dst_arena.borrow())
                               .collect::<Vec<NodeId<DstNodeId>>>();

    let mut values_same = true;
    // Check whether the size of arena's are the same
    assert_eq!(traverse_src.len(), traverse_dst.len());
    if traverse_src.len() == traverse_dst.len() {
        for i in 0..traverse_src.len() {
            if !has_same_type_and_label(traverse_src[i],
                                        &new_matcher_pruning.src_arena.borrow(),
                                        traverse_dst[i],
                                        &new_matcher_pruning.dst_arena.borrow())
            {
                values_same = false;
            }
        }
        assert_eq!(values_same, true);
    }
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        new_matcher_pruning.push(edge.src_node, edge.dst_node, edge.edge_type);
    }
    // Print the edit script
    let string_output = edit_script.render_json(0);

    // Print the edit script
    for line in string_output.split('\n') {
        println!("{}", line);
    }

    let mut list_edges = new_matcher_pruning.list_edges.borrow().clone();
    list_edges.retain(|x| x.edge_type != EdgeType::OK);

    Ok(edit_script)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Creates a simple tree for tests which is SrcNodeId
    pub fn create_arena() -> Arena<String, SrcNodeId> {
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

    /// Creates a simple tree for tests which is DstNodeId
    pub fn create_arena_2() -> Arena<String, DstNodeId> {
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
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);
        let matcher = matching_config.match_trees(tree1.clone(), tree2.clone(), cost_edges);
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, EdgeType::OK, &mut checker_edges_induced));
    }

    #[test]
    // Tree 1 has more nodes and tree 2 stays the same as create arena
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
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);

        let matcher = matching_config.match_trees(arena.clone(), tree2.clone(), cost_edges);
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
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);

        let matcher = matching_config.match_trees(tree1.clone(), arena.clone(), cost_edges);
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
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);

        let matcher = matching_config.match_trees(arena.clone(), tree1.clone(), cost_edges);

        let mut checker_edges_induced = matcher.list_edges.borrow().clone();

        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 3, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 4, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 5, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, EdgeType::UPDATE, &mut checker_edges_induced));
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
        let mut arena: Arena<String, SrcNodeId> = Arena::new();
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
        let mut tree1: Arena<String, DstNodeId> = Arena::new();
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
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);

        let matcher = matching_config.match_trees(arena.clone(), tree1.clone(), cost_edges);
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 2, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 2, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 3, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 4, EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 2, EdgeType::GLUE, &mut checker_edges_induced));
    }

    #[test]
    pub fn test_chawathe_matching_6() -> () {
        // Tree 1
        let mut arena: Arena<String, SrcNodeId> = Arena::new();
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
        let mut tree1: Arena<String, DstNodeId> = Arena::new();
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
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);
        let mut matcher = matching_config.match_trees(arena.clone(), tree1.clone(), cost_edges);
        matcher.update_cost(1, 1, 1, 1, 1, 1, 1, 1);

        let pruning = matching_config.prune_edges(&mut matcher.clone());
        // Pruning
        let mut new_matcher_pruning = matcher.clone();
        // Length Of Edges Induced
        let edges_pruned = pruning.0.clone();
        new_matcher_pruning.list_edges = RefCell::new(edges_pruned.clone());

        let mut checker_edges_pruned = matcher.list_edges.borrow().clone();

        assert!(matcher.contains_edge(0, 0, EdgeType::OK, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 1, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 2, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 7, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 2, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 3, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 8, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(3, 3, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 4, EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 5, EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(5, 5, EdgeType::UPDATE, &mut checker_edges_pruned));
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

        // Test for Test 6

        // - Do Re-match trees.
        // - Perform pruning.
        // - Then check if all have OK matching.

        let matcher_copy = edgecover_solver(new_matcher_pruning);
        new_matcher_pruning = matcher_copy.0;
        edgecover_apply_edit_script(&mut new_matcher_pruning);
        let src_root = new_matcher_pruning.src_arena.borrow().root().unwrap();
        let dst_root = new_matcher_pruning.dst_arena.borrow().root().unwrap();
        let traverse_src = src_root.breadth_first_traversal(&new_matcher_pruning.src_arena
                                                                                .borrow())
                                   .collect::<Vec<NodeId<SrcNodeId>>>();
        let traverse_dst = dst_root.breadth_first_traversal(&new_matcher_pruning.dst_arena
                                                                                .borrow())
                                   .collect::<Vec<NodeId<DstNodeId>>>();

        assert_eq!(traverse_src.len(), traverse_dst.len());

        let mut values_same = true;
        if traverse_src.len() == traverse_dst.len() {
            for i in 0..traverse_src.len() {
                if !has_same_type_and_label(traverse_src[i],
                                            &new_matcher_pruning.src_arena.borrow(),
                                            traverse_dst[i],
                                            &new_matcher_pruning.dst_arena.borrow())
                {
                    values_same = false;
                }
            }
        }
        assert_eq!(values_same, true);
    }
}
