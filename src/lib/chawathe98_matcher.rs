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

use action::{ApplyAction, Copy, Delete, EditScript, Glue, Insert, Move, Update};
use emitters::RenderJson;
use matchers::EditScriptResult;
use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::collections::HashSet;

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
    pub fn new(
        cost_update_1: usize,
        cost_insert_1: usize,
        cost_delete_1: usize,
        cost_move_1: usize,
        cost_copy_1: usize,
        cost_glue_1: usize,
        cost_null_1: usize,
        cost_ok_1: usize,
    ) -> CostEdge {
        CostEdge {
            update: cost_update_1,
            insert: cost_insert_1,
            delete: cost_delete_1,
            move_: cost_move_1,
            copy: cost_copy_1,
            glue: cost_glue_1,
            null: cost_null_1,
            ok: cost_ok_1,
        }
    }
}

/// Edge construct which shows the edge between two nodes and their value
#[derive(Debug, Clone, Copy)]
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
    pub fn new(
        new_from_node: NodeId<FromNodeId>,
        new_to_node: NodeId<ToNodeId>,
        new_value: usize,
        new_edge_type: EdgeType,
    ) -> Edge {
        Edge {
            from_node: new_from_node,
            to_node: new_to_node,
            value: new_value,
            edge_type: new_edge_type,
        }
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
    pub fn new(base: Arena<T, FromNodeId>, diff: Arena<T, ToNodeId>) -> MappingStoreGraph<T> {
        MappingStoreGraph {
            from_arena: RefCell::new(base),
            to_arena: RefCell::new(diff),
            list_edges: RefCell::new(Vec::new()),
            list_node_inserted: RefCell::new(Vec::new()),
            list_node_deleted: RefCell::new(Vec::new()),
            // The cost could change in the future but,
            // we can make the default cost for every edge
            // which is cost the value of 1.
            all_edge_cost: CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1),
            from: RefCell::new(HashMap::new()),
            to: RefCell::new(HashMap::new()),
        }
    }

    /// Implementation of the update of the cost edge.
    /// This will renew the cost value for each edge type.
    pub fn update_cost(
        &mut self,
        cost_update: usize,
        cost_insert: usize,
        cost_delete: usize,
        cost_move: usize,
        cost_copy: usize,
        cost_glue: usize,
        cost_null: usize,
        cost_ok: usize,
    ) {
        self.all_edge_cost = CostEdge::new(
            cost_update,
            cost_insert,
            cost_delete,
            cost_move,
            cost_copy,
            cost_glue,
            cost_null,
            cost_ok,
        );
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
    pub fn push_edge(
        &mut self,
        new_from_node: NodeId<FromNodeId>,
        new_to_node: NodeId<ToNodeId>,
        new_value: usize,
        new_edge_type: EdgeType,
    ) {
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
    pub fn contains_edge(
        &self,
        from: usize,
        to: usize,
        edge_type: &EdgeType,
        list_edges: &[Edge],
    ) -> bool {
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
    pub fn print_preorder_traversal(
        &self,
        tree1: Vec<NodeId<FromNodeId>>,
        tree2: Vec<NodeId<ToNodeId>>,
    ) -> () {
        println!();
        println!("Tree 1");
        print!("|");
        for node in tree1 {
            print!(
                "{:?},{:?}|",
                node.id(),
                self.from_arena.borrow()[node].label
            );
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
        let descendants_from_node = from_node
            .descendants(&self.from_arena.borrow())
            .collect::<Vec<NodeId<FromNodeId>>>();
        let descendants_to_node = to_node
            .descendants(&self.to_arena.borrow())
            .collect::<Vec<NodeId<ToNodeId>>>();
        if has_same_type_and_label(
            &from_node,
            &self.from_arena.borrow(),
            &to_node,
            &self.to_arena.borrow(),
        ) {
            if descendants_to_node.len() != descendants_from_node.len() {
                return false;
            }
            for i in 0..descendants_to_node.len() {
                if !has_same_type_and_label(
                    &descendants_from_node[i],
                    &self.from_arena.borrow(),
                    &descendants_to_node[i],
                    &self.to_arena.borrow(),
                ) {
                    // This means the descendants are !perfect by label and expression
                    return false;
                }
            }
        } else {
            return false;
        }
        true
    }
    /// Check if the subtree is deleted from from_node against to_node.
    /// This checks whether the the descendants are in the deletion edges.
    pub fn check_tree_deletion(
        &self,
        from_node_ancestor: NodeId<FromNodeId>,
        delete_tree2: NodeId<ToNodeId>,
        hash_set_deletion: &HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)>,
    ) -> bool {
        let mut output = true;
        let descendants_from_node = from_node_ancestor
            .descendants(&self.from_arena.borrow())
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
    pub fn check_tree_1_exists_in_tree2(&self, from_node_ancestor: NodeId<FromNodeId>) -> bool {
        let node_id_to_node = from_node_ancestor.id();
        if node_id_to_node >= self.to_arena.borrow().size() {
            return false;
        }
        let to_node = NodeId::<ToNodeId>::new(node_id_to_node);
        self.check_subtree(from_node_ancestor, to_node)
    }
    /// Check if the descendants of to_node are in the insertion edges.
    /// From node would be the insertion node.
    pub fn check_tree_insertion(
        &self,
        insert_tree1: NodeId<FromNodeId>,
        to_node: NodeId<ToNodeId>,
        hash_set_insertion: &HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)>,
    ) -> bool {
        let mut output = true;
        let descendants_from_node = to_node
            .descendants(&self.to_arena.borrow())
            .collect::<Vec<NodeId<ToNodeId>>>();
        for descendant in descendants_from_node {
            if !hash_set_insertion.contains(&(insert_tree1, descendant)) {
                output = false;
            }
        }
        output
    }

    /// Converts vector(To_node_id) to hash set
    pub fn vector_to_hash_set_to_node_id(
        &self,
        tree2: &[NodeId<ToNodeId>],
    ) -> HashSet<NodeId<ToNodeId>> {
        let mut hashset = HashSet::<NodeId<ToNodeId>>::new();
        for node in tree2.to_owned() {
            hashset.insert(node);
        }
        hashset
    }

    /// Converts vector(From_node_id) to hash set
    pub fn vector_to_hash_set_from_node_id(
        &self,
        tree2: &[NodeId<FromNodeId>],
    ) -> HashSet<NodeId<FromNodeId>> {
        let mut hashset = HashSet::<NodeId<FromNodeId>>::new();
        for node in tree2.to_owned() {
            hashset.insert(node);
        }
        hashset
    }
    /// Generate all lower bound values for all the edges.
    pub fn make_all_lower_bound_edges(
        &self,
        hash_edges: &HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)>,
    ) -> HashMap<(NodeId<FromNodeId>, NodeId<ToNodeId>), usize> {
        // May be returner. Of all the lower bound for the edges.
        let mut hash_map_lower_bound: HashMap<
            (NodeId<FromNodeId>, NodeId<ToNodeId>),
            usize,
        > = HashMap::new();
        let cost_of_move = self.all_edge_cost.move_;
        let cost_of_update = self.all_edge_cost.update;
        for edge in self.list_edges.borrow().iter() {
            let m = edge.from_node;
            let n = edge.to_node;

            // Cost of forced move calculate
            // Get all the children of m.
            let m_children = m.children(&self.from_arena.borrow())
                .collect::<Vec<NodeId<FromNodeId>>>();
            let n_children = n.children(&self.to_arena.borrow())
                .collect::<Vec<NodeId<ToNodeId>>>();

            // Get sum of forced move m' (Children of m) to n
            let mut sum_cost_forced_move_m_children_n = 0;
            let mut sum_cost_forced_move_n_children_m = 0;
            // Loop through all the children of m (m_1) and see if there is a mapping of n
            // if there is edge exist that m_1 -> n, forced move cost = 0.
            // otherwise forced move cost is the cost of move operation.

            for m_1 in m_children {
                if !hash_edges.contains(&(m_1, n)) {
                    //                    let m1_cost = m_1.children(&self.from_arena.borrow()).collect::<Vec<NodeId<FromNodeId>>>().len();
                    sum_cost_forced_move_m_children_n += cost_of_move;
                }
            }

            for n_1 in n_children {
                if !hash_edges.contains(&(m, n_1)) {
                    //                    let n1_cost = n_1.children(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>().len();
                    sum_cost_forced_move_n_children_m += cost_of_move;
                }
            }

            // Analogously n_1 to m

            hash_map_lower_bound.insert(
                (m, n),
                cost_of_update
                    + (cmp::min(
                        sum_cost_forced_move_m_children_n,
                        sum_cost_forced_move_n_children_m,
                    ) / 2),
            );
        }
        hash_map_lower_bound
    }

    /// Generate all the upper bound values for all the edges.
    pub fn make_all_upper_bound_edges(
        &self,
        hash_edges: &HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)>,
    ) -> HashMap<(NodeId<FromNodeId>, NodeId<ToNodeId>), usize> {
        // May be returner. Of all the upper bound for the edges.
        let mut hash_map_upper_bound: HashMap<
            (NodeId<FromNodeId>, NodeId<ToNodeId>),
            usize,
        > = HashMap::new();
        let cost_of_move = self.all_edge_cost.move_;
        let cost_of_update = self.all_edge_cost.update;
        let cost_of_copy = self.all_edge_cost.copy;
        let cost_of_glue = self.all_edge_cost.glue;
        for edge in self.list_edges.borrow().iter() {
            let m = edge.from_node;
            let n = edge.to_node;

            // Cost of forced move calculate
            // Get all the children of m.
            let m_children = m.children(&self.from_arena.borrow())
                .collect::<Vec<NodeId<FromNodeId>>>();
            let n_children = n.children(&self.to_arena.borrow())
                .collect::<Vec<NodeId<ToNodeId>>>();

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

            hash_map_upper_bound.insert(
                (m, n),
                cost_cw
                    + (sum_cost_conditional_move_m_children_n
                        + sum_cost_conditional_move_n_children_m) / 2,
            );
        }
        hash_map_upper_bound
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
            println!(
                "TY        :   From node    -> {:?} || to node -> {:?}",
                self.from_arena.borrow()[edge.from_node].ty,
                self.to_arena.borrow()[edge.to_node].ty
            );
            println!(
                "Value     :   From node -> {:?} || to node -> {:?}",
                self.from_arena.borrow()[edge.from_node].label,
                self.to_arena.borrow()[edge.to_node].label
            );
            println!(
                "From node -> {:?} || to node -> {:?} || Edge type -> {:?}",
                edge.from_node, edge.to_node, edge.edge_type
            );
            println!("--");
        }
    }
}

/// This is the stage 3 for algorithm
/// This performs minimum edge cover solver problem
pub fn edgecover_solver(
    mut new_matcher_pruning: MappingStoreGraph<String>,
) -> (
    MappingStoreGraph<String>,
    HashMap<NodeId<ToNodeId>, (HashSet<(NodeId<FromNodeId>, EdgeType, usize)>, usize)>,
    HashMap<NodeId<FromNodeId>, (HashSet<(NodeId<ToNodeId>, EdgeType, usize)>, usize)>,
) {
    // This implements greedy solution to get only one edge mapping incidented to -- "To nodes"
    // Lets get rid of move operations where they have glue or copy as a parent
    let helper_mapping = edgecover_helper(&new_matcher_pruning);
    let root_to = new_matcher_pruning.to_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.to_arena.borrow().clone();
    let bfs_root_to_tree = root_to.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search

    // Hash Set for different types of edges
    let mut copy_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType, usize)> =
        HashSet::new();
    let mut glue_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType, usize)> =
        HashSet::new();
    let mut move_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType, usize)> =
        HashSet::new();
    let mut edge_keep: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType, usize)> =
        HashSet::new();

    // Find minimum cost edge
    // By looking at all edges associated with the nodes in the To_Node

    for to_node in bfs_root_to_tree {
        let node_mapping = helper_mapping.0.get(&to_node);
        let mut edge_keep_temp: HashSet<(
            NodeId<FromNodeId>,
            NodeId<ToNodeId>,
            EdgeType,
            usize,
        )> = HashSet::new();
        // Sorting by cost Keep the lowest cost
        if node_mapping.is_some() {
            let lowest_cost = node_mapping.unwrap().0.iter().map(|x| x.2).min().unwrap();
            for &(from_node, edge, value) in &node_mapping.unwrap().0 {
                // Add edges which are copy or glue
                if edge == EdgeType::COPY {
                    copy_edges.insert((from_node, to_node, edge, value));
                } else if edge == EdgeType::GLUE {
                    glue_edges.insert((from_node, to_node, edge, value));
                } else if edge == EdgeType::MOVE {
                    move_edges.insert((from_node, to_node, edge, value));
                }
                if lowest_cost == value {
                    edge_keep_temp.insert((from_node, to_node, edge, value));
                }
            }

            // Case 1 edge keep temp  could be of 1 - means perfect mapping
            // Case 2 there could be many edges of the same cost
            // In case 2 we would pick according to their the edge
            if edge_keep_temp.len() > 1 {
                let mut vec_order = edge_keep_temp.clone().into_iter().collect::<Vec<_>>();
                vec_order.sort_by_key(|k| k.0.id());
                let mut result_edge: Option<(
                    NodeId<FromNodeId>,
                    NodeId<ToNodeId>,
                    EdgeType,
                    usize,
                )> = None;
                let mut check_base_2 = false; // Edge type is Copy, Glue, Move

                for (from_node, to_node, edge1, value) in vec_order {
                    let edge = edge1;
                    match result_edge {
                        None => {
                            if !(edge1 == EdgeType::OK || edge1 == EdgeType::INSERT
                                || edge1 == EdgeType::DELETE)
                            {
                                check_base_2 = true;
                            }
                            result_edge = Some((from_node, to_node, edge, value));
                            continue;
                        }
                        Some(_) => {}
                    }

                    if check_base_2
                        && (edge == EdgeType::INSERT || edge == EdgeType::DELETE
                            || edge == EdgeType::OK)
                    {
                        // our current chosen edges either MOVE, COPY, GLUE
                        // current edge in the for loop is Insert or delete or ok
                        // So choose the current edge in the for loop;
                        check_base_2 = false;
                        result_edge = Some((from_node, to_node, edge, value));
                    }
                    // We try to get the first insert or delete or ok edges
                    // If they are not available then we get the first
                    // Glue or copy or move edge.
                }
                // At the we should have an edge
                if result_edge.is_some() {
                    edge_keep.insert((
                        result_edge.unwrap().0,
                        result_edge.unwrap().1,
                        result_edge.unwrap().2,
                        result_edge.unwrap().3,
                    ));
                }
            }
            // We found the lowest cost in the first go.
            else if edge_keep_temp.len() == 1 {
                for (from_node, to_node, edge, value) in edge_keep_temp {
                    edge_keep.insert((from_node, to_node, edge, value));
                }
            }
        }
    }
    // We would check through the edge keep and only retain edges in edge keep;
    let mut edge_filtered: Vec<Edge> = Vec::new();
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if edge_keep.contains(&(edge.from_node, edge.to_node, edge.edge_type, edge.value)) {
            edge_filtered.push(Edge::new(
                edge.from_node,
                edge.to_node,
                edge.value,
                edge.edge_type,
            ));
        } else if edge.edge_type == EdgeType::DELETE {
            edge_filtered.push(Edge::new(
                edge.from_node,
                edge.to_node,
                edge.value,
                edge.edge_type,
            ));
        }
    }
    // Half way breaking point

    new_matcher_pruning.list_edges = RefCell::new(edge_filtered);

    // Below will implement greedy solution to get only edge going into "from_node"
    let helper_mapping = edgecover_helper(&new_matcher_pruning);
    let root_from = new_matcher_pruning.from_arena.borrow().root().unwrap(); // From Tree Root
    let reference_matcher = &new_matcher_pruning.from_arena.borrow().clone();
    let bfs_root_from_tree = root_from.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search

    // Start the minimum edge cost finding
    // But looking at all nodes in the from tree.
    edge_keep.clear();
    let mut edge_filtered: Vec<Edge> = Vec::new();

    for from_node in bfs_root_from_tree {
        let node_mapping = helper_mapping.1.get(&from_node);
        let mut edge_keep_temp: HashSet<(
            NodeId<FromNodeId>,
            NodeId<ToNodeId>,
            EdgeType,
            usize,
        )> = HashSet::new();
        // Sorting by cost Keep the lowest cost
        if node_mapping.is_some() {
            let mut lowest_cost = node_mapping.unwrap().0.iter().map(|x| x.2).min().unwrap();

            for &(to_node, edge, value) in &node_mapping.unwrap().0 {
                if lowest_cost == value {
                    edge_keep_temp.insert((from_node, to_node, edge, value));
                }
            }

            // Case 1 edge keep temp  could be of 1 - means perfect mapping
            // Case 2 there could be many edges of the same cost
            // In case 2 we would pick according to their the edge
            if edge_keep_temp.len() > 1 {
                let mut vec_order = edge_keep_temp.clone().into_iter().collect::<Vec<_>>();
                vec_order.sort_by_key(|k| k.1.id());
                let mut result_edge: Option<(
                    NodeId<FromNodeId>,
                    NodeId<ToNodeId>,
                    EdgeType,
                    usize,
                )> = None;

                let mut check_base_2 = false; // Edge type is Copy, Glue, Move

                for (from_node, to_node, edge1, value) in vec_order {
                    let edge = edge1;
                    match result_edge {
                        None => {
                            if !(edge1 == EdgeType::OK || edge1 == EdgeType::INSERT
                                || edge1 == EdgeType::DELETE)
                            {
                                check_base_2 = true;
                            }
                            result_edge = Some((from_node, to_node, edge, value));
                            continue;
                        }
                        Some(_) => {}
                    }

                    if check_base_2
                        && (edge == EdgeType::INSERT || edge == EdgeType::DELETE
                            || edge == EdgeType::OK)
                    {
                        // our current chosen edges either MOVE, COPY, GLUE
                        // current edge in the for loop is Insert or delete or ok
                        // So choose the current edge in the for loop;
                        check_base_2 = false;
                        result_edge = Some((from_node, to_node, edge, value));
                    }
                    // We try to get the first insert or delete or ok edges
                    // If they are not available then we get the first
                    // Glue or copy or move edge.
                }
                // At the we should have an edge
                if result_edge.is_some() {
                    edge_keep.insert((
                        result_edge.unwrap().0,
                        result_edge.unwrap().1,
                        result_edge.unwrap().2,
                        result_edge.unwrap().3,
                    ));
                }
            }
            // We found the lowest cost in the first go.
            else if edge_keep_temp.len() == 1 {
                for (from_node, to_node, edge, value) in edge_keep_temp {
                    edge_keep.insert((from_node, to_node, edge, value));
                }
            }
        }
    }

    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if edge_keep.contains(&(edge.from_node, edge.to_node, edge.edge_type, edge.value)) {
            edge_filtered.push(Edge::new(
                edge.from_node,
                edge.to_node,
                edge.value,
                edge.edge_type,
            ));
        } else if edge.edge_type == EdgeType::INSERT {
            edge_filtered.push(Edge::new(
                edge.from_node,
                edge.to_node,
                edge.value,
                edge.edge_type,
            ));
        }
    }

    new_matcher_pruning.list_edges = RefCell::new(edge_filtered);

    // Get rid of edges where parent's do not have any edges associated with them.
    remove_edges_where_parent_not_edges(&mut new_matcher_pruning);

    let helper = edgecover_helper(&new_matcher_pruning);

    let mut edge_to_be_added = new_matcher_pruning.list_edges.borrow_mut().to_vec();

    let mut edges_hash: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    for e in edge_to_be_added.clone() {
        edges_hash.insert((e.from_node, e.to_node));
    }

    // At this stage there should be one to one mapping and many edges which are not mapped.
    // Look though all the edges which are copy or glue
    // Case Copy
    // Check if the edges for the from and to exist
    // lets call copy edge (F,T)
    // If there are no mapping F -> Nothing and T -> Nothing -> Easily add the edges
    // If there are mapping but the mapping are F1 and T1 which are the same as F = F1 and T = T1
    // But the edge type is different. Then check if the edge is move. If it move then make it COPY
    // Otherwise -> No Idea

    let mut visited_to_nodes: HashSet<NodeId<ToNodeId>> = HashSet::new();
    for &(from, to, edge_type, value) in &copy_edges {
        // Case when the from node does not have any edges but the to_node has a edge
        // Vice Versa
        // Then easily add the edge. (Type Copy)
        if !visited_to_nodes.contains(&to) && !edges_hash.contains(&(from, to))
            && (helper.0.get(&to).is_none())
        {
            // check to the coping node
            edge_to_be_added.push(Edge::new(from, to, value, edge_type));
            visited_to_nodes.insert(to);
            edges_hash.insert((from, to));
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
    // If there are nodes in from_arena that have not been mapped then
    // we should create new edges (Delete edges) and add them to our edge collection
    // If there are nodes in to_arena that have not been mapped then
    // we should create new edges (Insert edges) and add them to our edge collection
    check_to_arena_mapping_which_are_empty(&mut new_matcher_pruning); // Add to nodes which are not mapped as insert edges
    check_from_arena_mapping_which_are_empty(&mut new_matcher_pruning); // Add from nodes which are not mapped as delete edges

    // Add any remaining move operations. Where parent's are insert.
    add_edges_where_parents_are_insert(&mut new_matcher_pruning, &move_edges);

    // check remaining edges Remove edges with no relations.
    // Remove edges where parent nodes are delete.
    remove_edges_where_delete_is_parent(&mut new_matcher_pruning);

    check_to_arena_mapping_which_are_empty(&mut new_matcher_pruning); // Add to nodes which are not mapped as insert edges
    check_from_arena_mapping_which_are_empty(&mut new_matcher_pruning); // Add from nodes which are not mapped as delete edges

    let helper = edgecover_helper(&new_matcher_pruning);

    return (new_matcher_pruning, helper.0, helper.1);
}

/// Remove edges with parent who are delete edges.
fn remove_edges_where_delete_is_parent(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    let mut edge_to_remove: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType)> =
        HashSet::new();

    let mut only_delete = new_matcher_pruning.list_edges.borrow().clone();

    only_delete.retain(|&x| x.edge_type == EdgeType::DELETE);

    let mut final_edges = new_matcher_pruning.list_edges.borrow().clone();

    for edge in &only_delete {
        let edge_desc = edge.from_node
            .descendants(&new_matcher_pruning.from_arena.borrow())
            .collect::<HashSet<NodeId<FromNodeId>>>();
        let mut get_all_edges_which_have_descendants =
            new_matcher_pruning.list_edges.borrow().clone();
        get_all_edges_which_have_descendants.retain(|&x| {
            edge_desc.contains(&x.from_node) && x.edge_type != EdgeType::DELETE
                && edge.to_node.id() >= x.to_node.id()
        });
        for edge_descendants_of_delete in get_all_edges_which_have_descendants {
            edge_to_remove.insert((
                edge_descendants_of_delete.from_node,
                edge_descendants_of_delete.to_node,
                edge_descendants_of_delete.edge_type,
            ));
        }
    }

    final_edges.retain(|&x| !edge_to_remove.contains(&(x.from_node, x.to_node, x.edge_type)));

    new_matcher_pruning.list_edges = RefCell::new(final_edges);
}

/// Remove edges where parents are not edges
fn remove_edges_where_parent_not_edges(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    // Lets remove edges who do not have parents in the list of edges
    let helper = edgecover_helper(&new_matcher_pruning);

    // Loop through the to nodes and check if their nodes parent exists
    let mut hash_set_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType, usize)> =
        HashSet::new();

    // Look through all the edges and check whether their parents are have edges if they do not then
    // Case 1 : From node
    // If the from node parent is not there it means that it will be removed latter on so don't keep the edge
    // Case 2: To Node

    // Ignoring Cases for now just removing if the parent edge doesn't exist
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if !edge.from_node
            .is_root(&new_matcher_pruning.from_arena.borrow())
            && !edge.to_node.is_root(&new_matcher_pruning.to_arena.borrow())
        {
            // Check if they have parent
            let parent_from = new_matcher_pruning.from_arena.borrow()[edge.from_node].parent();
            let parent_to = new_matcher_pruning.to_arena.borrow()[edge.to_node].parent();

            if parent_from.is_some() {
                // If the from node has a parent and they have some sort of mapping.
                if helper.1.get(&parent_from.unwrap()).is_none() {
                    hash_set_edges.insert((
                        edge.from_node,
                        edge.to_node,
                        edge.edge_type,
                        edge.value,
                    ));
                }
            }
            if parent_to.is_some() {
                if helper.0.get(&parent_to.unwrap()).is_none() {
                    hash_set_edges.insert((
                        edge.from_node,
                        edge.to_node,
                        edge.edge_type,
                        edge.value,
                    ));
                }
            }
        }
    }

    // If it does not have a parent it means it will be deleted later on.
    // So it is better to remove the edge.

    let mut keep_edges: Vec<Edge> = Vec::new();
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        if !hash_set_edges.contains(&(edge.from_node, edge.to_node, edge.edge_type, edge.value)) {
            keep_edges.push(*edge);
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(keep_edges);
}

/// Only add move operations where their parents are insert operations.
fn add_edges_where_parents_are_insert(
    new_matcher_pruning: &mut MappingStoreGraph<String>,
    hash_move_edges: &HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType, usize)>,
) {
    // Helper
    let helper = edgecover_helper(&new_matcher_pruning);
    // List of edges
    // all edges
    let mut hash_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    let all_edges = new_matcher_pruning.list_edges.borrow().clone();
    for edge in all_edges {
        hash_edges.insert((edge.from_node, edge.to_node));
    }
    // ok edges
    let mut hash_ok_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    let mut ok_edges = new_matcher_pruning.list_edges.borrow().clone();
    ok_edges.retain(|&x| x.edge_type == EdgeType::OK);
    for edge in ok_edges {
        hash_ok_edges.insert((edge.from_node, edge.to_node));
    }
    // insert edges
    let mut hash_insert_edges: HashSet<NodeId<ToNodeId>> = HashSet::new();
    let mut insert_edges = new_matcher_pruning.list_edges.borrow().clone();
    insert_edges.retain(|&x| x.edge_type == EdgeType::INSERT);
    for edge in insert_edges {
        hash_insert_edges.insert(edge.to_node);
    }
    // Delete edges
    let mut hash_delete_edges: HashSet<NodeId<FromNodeId>> = HashSet::new();
    let mut delete_edges = new_matcher_pruning.list_edges.borrow().clone();
    delete_edges.retain(|&x| x.edge_type == EdgeType::DELETE);
    for edge in delete_edges {
        hash_delete_edges.insert(edge.from_node);
    }

    // leaf moves
    let mut filter_move_only_leaves = hash_move_edges.clone();
    // Only retain moves that are both leafs in the from and to arena
    filter_move_only_leaves.retain(|&x| {
        x.0.is_leaf(&new_matcher_pruning.from_arena.borrow())
            && x.1.is_leaf(&new_matcher_pruning.to_arena.borrow())
    });
    // Only get move operations that are "not" in the list of edges
    filter_move_only_leaves.retain(|&x| !hash_edges.contains(&(x.0, x.1)));
    // Only get edges where parents are insert or ok edges
    filter_move_only_leaves.retain(|&x| {
        (new_matcher_pruning.to_arena.borrow()[x.1]
            .parent()
            .is_some()
            && hash_insert_edges
                .contains(&new_matcher_pruning.to_arena.borrow()[x.1].parent().unwrap()))
            || (new_matcher_pruning.to_arena.borrow()[x.1]
                .parent()
                .is_some()
                && new_matcher_pruning.from_arena.borrow()[x.0]
                    .parent()
                    .is_some()
                && hash_ok_edges.contains(&(
                    new_matcher_pruning.from_arena.borrow()[x.0]
                        .parent()
                        .unwrap(),
                    new_matcher_pruning.to_arena.borrow()[x.1].parent().unwrap(),
                )))
    });
    filter_move_only_leaves
        .retain(|&x| !helper.0.contains_key(&x.1) && !helper.1.contains_key(&x.0));

    let delete_node: NodeId<ToNodeId> =
        NodeId::new(new_matcher_pruning.to_arena.borrow().size() - 1);
    let insert_node: NodeId<FromNodeId> =
        NodeId::new(new_matcher_pruning.from_arena.borrow().size() - 1);
    let mut edges_to_remove_hash: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    let mut from_node_moves_added: HashSet<NodeId<FromNodeId>> = HashSet::new();
    let mut to_node_moves_added: HashSet<NodeId<ToNodeId>> = HashSet::new();

    let mut final_edges = new_matcher_pruning.list_edges.borrow().clone();
    for &(from, to, edge, size) in &filter_move_only_leaves {
        if hash_delete_edges.contains(&from) && hash_insert_edges.contains(&to)
            && !from_node_moves_added.contains(&from)
            && !to_node_moves_added.contains(&to)
        {
            final_edges.push(Edge::new(from, to, size, edge));
            edges_to_remove_hash.insert((from, delete_node));
            edges_to_remove_hash.insert((insert_node, to));
            // Add visited node
            from_node_moves_added.insert(from);
            to_node_moves_added.insert(to);
        }
    }
    final_edges.retain(|&x| !edges_to_remove_hash.contains(&(x.from_node, x.to_node)));
    new_matcher_pruning.list_edges = RefCell::new(final_edges);
}

/// Add Move operations if they do not exist already.
fn add_edges_move(
    new_matcher_pruning: &mut MappingStoreGraph<String>,
    hash_move_edges: &HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType, usize)>,
) {
    // Helper
    let helper = edgecover_helper(&new_matcher_pruning);
    // List of edges

    // All edges mapping move
    let mut hash_map_f_t_move: HashMap<
        (NodeId<FromNodeId>, NodeId<ToNodeId>),
        (EdgeType, usize),
    > = HashMap::new();
    for edge in hash_move_edges.clone() {
        hash_map_f_t_move.insert((edge.0, edge.1), (edge.2, edge.3));
    }

    // all edges
    let mut hash_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    let all_edges = new_matcher_pruning.list_edges.borrow().clone();
    for edge in all_edges {
        hash_edges.insert((edge.from_node, edge.to_node));
    }
    // Move edges
    let mut hash_move_exist: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    for edge in hash_move_edges.clone() {
        hash_move_exist.insert((edge.0, edge.1));
    }
    // ok edges
    let mut hash_ok_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    let mut ok_edges = new_matcher_pruning.list_edges.borrow().clone();
    ok_edges.retain(|&x| x.edge_type == EdgeType::OK);
    for edge in ok_edges {
        hash_ok_edges.insert((edge.from_node, edge.to_node));
    }
    // insert edges
    let mut hash_insert_edges: HashSet<NodeId<ToNodeId>> = HashSet::new();
    let mut insert_edges = new_matcher_pruning.list_edges.borrow().clone();
    insert_edges.retain(|&x| x.edge_type == EdgeType::INSERT);
    for edge in insert_edges {
        hash_insert_edges.insert(edge.to_node);
    }
    // Delete edges
    let mut hash_delete_edges: HashSet<NodeId<FromNodeId>> = HashSet::new();
    let mut delete_edges = new_matcher_pruning.list_edges.borrow().clone();
    delete_edges.retain(|&x| x.edge_type == EdgeType::DELETE);
    for edge in delete_edges {
        hash_delete_edges.insert(edge.from_node);
    }

    // Glue edges
    let mut glue_edges = new_matcher_pruning.list_edges.borrow().clone();
    glue_edges.retain(|&x| x.edge_type == EdgeType::GLUE || x.edge_type == EdgeType::MOVE);
    let mut glue_from_descendants: HashSet<NodeId<FromNodeId>> = HashSet::new();
    let mut glue_to_descendants: HashSet<NodeId<ToNodeId>> = HashSet::new();

    for edges in glue_edges {
        let desc_from = edges
            .from_node
            .descendants(&new_matcher_pruning.from_arena.borrow())
            .collect::<Vec<NodeId<FromNodeId>>>();
        let desc_to = edges
            .to_node
            .descendants(&new_matcher_pruning.to_arena.borrow())
            .collect::<Vec<NodeId<ToNodeId>>>();

        for from in desc_from {
            glue_from_descendants.insert(from);
        }

        for to in desc_to {
            glue_to_descendants.insert(to);
        }
    }

    // leaf moves
    let mut filter_move_only_leaves = hash_move_edges.clone();
    // Only retain moves that are  leafs in the from arena
    filter_move_only_leaves.retain(|&x| x.0.is_leaf(&new_matcher_pruning.from_arena.borrow()));
    // Only get move operations that are "not" in the list of edges
    filter_move_only_leaves.retain(|&x| !hash_edges.contains(&(x.0, x.1)));
    // Remove and descendants of move or glue
    filter_move_only_leaves.retain(|&x| {
        !glue_to_descendants.contains(&(x.1)) && !glue_from_descendants.contains(&(x.0))
    });
    // Check if move operation does not already exist.
    filter_move_only_leaves
        .retain(|&x| !helper.0.contains_key(&x.1) && !helper.1.contains_key(&x.0));

    // Add move where parent are the same
    // This only goes up one level
    let mut parent_moves_level: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    for edge_move in filter_move_only_leaves.clone() {
        // Get parent and check if they are in insert
        let parent_from = new_matcher_pruning.from_arena.borrow()[edge_move.0].parent();
        let parent_to = new_matcher_pruning.to_arena.borrow()[edge_move.1].parent();
        if parent_from.is_some() && parent_to.is_some() && hash_move_exist.contains(&(parent_from.unwrap(),parent_to.unwrap()))
            // Check if there are no mapping
            && !helper.0.contains_key(&parent_to.unwrap()) && !helper.1.contains_key(&parent_from.unwrap())
            // Check if it is not a descendant of glue or move
            && !glue_to_descendants.contains(&(parent_to.unwrap()))
            && !glue_from_descendants.contains(&(parent_from.unwrap()))
        {
            let get_move_insert =
                hash_map_f_t_move.get(&(parent_from.unwrap(), parent_to.unwrap()));
            if get_move_insert.is_some() {
                parent_moves_level.insert((parent_from.unwrap(), parent_to.unwrap()));
            }
        }
    }
    let insert_node = NodeId::<FromNodeId>::new(new_matcher_pruning.from_arena.borrow().size() - 1);
    let mut edges_to_insert_hash: HashSet<(
        NodeId<FromNodeId>,
        NodeId<ToNodeId>,
        EdgeType,
        usize,
    )> = HashSet::new();
    for edge in parent_moves_level {
        // Check whether there descendants are equal
        // To Arena more then add them to insert
        // From Arena has more descendants then add to delete
        let from_desc = edge.0
            .descendants(&new_matcher_pruning.from_arena.borrow())
            .collect::<Vec<NodeId<FromNodeId>>>();
        let to_desc = edge.1
            .descendants(&new_matcher_pruning.to_arena.borrow())
            .collect::<Vec<NodeId<ToNodeId>>>();
        if from_desc.len() < to_desc.len() {
            // Add to desc to insert
            let mut checker = true;
            for i in 0..from_desc.len() {
                if !has_same_type_and_label(
                    &from_desc[i],
                    &new_matcher_pruning.from_arena.borrow(),
                    &to_desc[i],
                    &new_matcher_pruning.to_arena.borrow(),
                ) {
                    checker = false;
                }
            }
            if checker {
                let get_move_insert = hash_map_f_t_move.get(&(edge.0, edge.1));
                if get_move_insert.is_some() {
                    let mapping_value = *get_move_insert.unwrap();
                    filter_move_only_leaves.insert((
                        edge.0,
                        edge.1,
                        EdgeType::GLUE,
                        mapping_value.1,
                    ));
                    for i in from_desc.len()..to_desc.len() {
                        if !hash_insert_edges.contains(&to_desc[i])
                            && helper.0.get(&to_desc[i]).is_none()
                        {
                            edges_to_insert_hash.insert((
                                insert_node,
                                to_desc[i],
                                EdgeType::INSERT,
                                1,
                            ));
                        }
                    }
                }
            }
        }
    }

    let mut from_node_moves_added: HashSet<NodeId<FromNodeId>> = HashSet::new();
    let mut to_node_moves_added: HashSet<NodeId<ToNodeId>> = HashSet::new();

    let mut final_edges = new_matcher_pruning.list_edges.borrow().clone();
    for &(from, to, edge, size) in &filter_move_only_leaves {
        if !from_node_moves_added.contains(&from) && !to_node_moves_added.contains(&to) {
            final_edges.push(Edge::new(from, to, size, edge));
            // Add visited node
            from_node_moves_added.insert(from);
            to_node_moves_added.insert(to);
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(final_edges);

    let helper = edgecover_helper(&new_matcher_pruning);
    // all edges
    let mut hash_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();
    let all_edges = new_matcher_pruning.list_edges.borrow().clone();
    for edge in all_edges {
        hash_edges.insert((edge.from_node, edge.to_node));
    }
    // insert edges
    let mut hash_insert_edges: HashSet<NodeId<ToNodeId>> = HashSet::new();
    let mut insert_edges = new_matcher_pruning.list_edges.borrow().clone();
    insert_edges.retain(|&x| x.edge_type == EdgeType::INSERT);
    for edge in insert_edges {
        hash_insert_edges.insert(edge.to_node);
    }

    // Inserted nodes visited
    let mut hash_to_nodes_visited: HashSet<NodeId<ToNodeId>> = HashSet::new();
    let mut final_all_edges = new_matcher_pruning.list_edges.borrow().clone();
    for &(from, to, edge, size) in &edges_to_insert_hash {
        // Insert edge
        // Last node in from _arena (MAPS) ANY Node in To_arena
        if helper.0.get(&to).is_none() && !hash_to_nodes_visited.contains(&to)
            && !glue_to_descendants.contains(&to)
        {
            hash_to_nodes_visited.insert(to);
            final_all_edges.push(Edge::new(from, to, size, edge));
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(final_all_edges);
}

/// The Config2 matcher needs no configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct Config2 {}

/// Uses the edges and makes hash map that used for min edge cover solver.
/// 1 - To Node << Mapping to >> From Nodes, Edge Type, Cost.
/// 2 - From Node << Mapping to >> To Nodes, Edge Type, Cost.
pub fn edgecover_helper(
    new_matcher_pruning: &MappingStoreGraph<String>,
) -> (
    HashMap<NodeId<ToNodeId>, (HashSet<(NodeId<FromNodeId>, EdgeType, usize)>, usize)>,
    HashMap<NodeId<FromNodeId>, (HashSet<(NodeId<ToNodeId>, EdgeType, usize)>, usize)>,
) {
    // To Node -> From Node
    let mut list_t_f: HashMap<
        NodeId<ToNodeId>,
        (HashSet<(NodeId<FromNodeId>, EdgeType, usize)>, usize),
    > = HashMap::new();
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
    let mut list_f_t: HashMap<
        NodeId<FromNodeId>,
        (HashSet<(NodeId<ToNodeId>, EdgeType, usize)>, usize),
    > = HashMap::new();
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

/// Add Edges of type Insert for all to_node that does not have any edge
fn check_to_arena_mapping_which_are_empty(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    // Add all nodes not mapped to Insert
    // If all to nodes are mapped then Good
    // But if nodes in the from_arena are not mapped that means add them to the remove section
    let mut new_edges_final = new_matcher_pruning.list_edges.borrow_mut().clone();
    let helper_mapping = edgecover_helper(&new_matcher_pruning);
    let root_to = new_matcher_pruning.to_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.to_arena.borrow().clone();
    let bfs_root_to_tree = root_to.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search
    let bfs_root_to_tree_1 = root_to.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search

    // checking if all to nodes are mapped.
    let mut bool_check_if_to_nodes_are_mapped = true;
    for to_node in bfs_root_to_tree {
        if !helper_mapping.0.contains_key(&to_node) {
            bool_check_if_to_nodes_are_mapped = false;
            break;
        }
    }

    // Insert Node
    let last_node_from_node =
        NodeId::<FromNodeId>::new(new_matcher_pruning.from_arena.borrow().size() - 1);
    // There are some nodes not mapped
    if !bool_check_if_to_nodes_are_mapped {
        let mut get_descendants_copy_glue: HashSet<NodeId<ToNodeId>> = HashSet::new();
        // Loop through all the edges and get the ones who have copy or glue
        for edge_traverse in new_edges_final.iter() {
            if edge_traverse.edge_type == EdgeType::GLUE
                || edge_traverse.edge_type == EdgeType::COPY
            {
                let children_copy_glue = edge_traverse
                    .to_node
                    .descendants(&new_matcher_pruning.to_arena.borrow())
                    .collect::<Vec<NodeId<ToNodeId>>>();
                for children in children_copy_glue {
                    get_descendants_copy_glue.insert(children);
                }
            }
        }

        for to_node in bfs_root_to_tree_1 {
            if !helper_mapping.0.contains_key(&to_node) {
                let edge_created = Edge::new(last_node_from_node, to_node, 1, EdgeType::INSERT);
                if !get_descendants_copy_glue.contains(&to_node) {
                    // Check if its a descendant of glue of copy thats is why there is no edge.
                    new_edges_final.push(edge_created);
                }
            }
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(new_edges_final.to_vec());
}

/// Add Edges of type Delete for all from_node that does not have any edge
fn check_from_arena_mapping_which_are_empty(new_matcher_pruning: &mut MappingStoreGraph<String>) {
    // Add all nodes not mapped to delete
    // If all to nodes are mapped then Good
    // But if nodes in the from_arena are not mapped that means add them to the remove section

    let mut new_edges_final = new_matcher_pruning.list_edges.borrow_mut().clone();
    let helper_mapping = edgecover_helper(&new_matcher_pruning);
    let root_from = new_matcher_pruning.from_arena.borrow().root().unwrap(); // From Tree Root
    let reference_matcher = &new_matcher_pruning.from_arena.borrow().clone();
    let bfs_root_from_tree = root_from.breadth_first_traversal(reference_matcher); // Root From Traversal BFS Search
    let bfs_root_from_tree_1 = root_from.breadth_first_traversal(reference_matcher); // Root From Traversal BFS Search

    // checking if all to nodes are mapped.
    let mut bool_check_if_from_nodes_are_mapped = true;
    for from_node in bfs_root_from_tree {
        if !helper_mapping.1.contains_key(&from_node) {
            bool_check_if_from_nodes_are_mapped = false;
            break;
        }
    }

    // Delete Node
    let last_node_to_node =
        NodeId::<ToNodeId>::new(new_matcher_pruning.to_arena.borrow().size() - 1);
    // There are some nodes not mapped
    if !bool_check_if_from_nodes_are_mapped {
        let mut get_descendants_copy_glue: HashSet<NodeId<FromNodeId>> = HashSet::new();
        // Loop through all the edges and get the ones who have copy or glue
        for edge_traverse in new_edges_final.iter() {
            if edge_traverse.edge_type == EdgeType::GLUE
                || edge_traverse.edge_type == EdgeType::COPY
            {
                let children_copy_glue = edge_traverse
                    .from_node
                    .descendants(&new_matcher_pruning.from_arena.borrow())
                    .collect::<Vec<NodeId<FromNodeId>>>();
                for children in children_copy_glue {
                    get_descendants_copy_glue.insert(children);
                }
            }
        }

        for from_node in bfs_root_from_tree_1 {
            if !helper_mapping.1.contains_key(&from_node) {
                let edge_created = Edge::new(from_node, last_node_to_node, 1, EdgeType::DELETE);
                if !get_descendants_copy_glue.contains(&from_node) {
                    // Check if its a descendant of glue of copy thats is why there is no edge.
                    new_edges_final.push(edge_created);
                }
            }
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(new_edges_final.to_vec());
}

/// Apply edit script to the from_arena.
pub fn edgecover_apply_edit_script(
    new_matcher_pruning: &mut MappingStoreGraph<String>,
) -> (EditScript<String>) {
    // Final Edit script
    let mut script: EditScript<String> = EditScript::new();
    let mut from_arena = new_matcher_pruning.from_arena.borrow_mut();
    let to_arena = new_matcher_pruning.to_arena.borrow();

    // Parent Mapping hash map.
    let mut mapping_t_f: HashMap<NodeId<ToNodeId>, NodeId<FromNodeId>> = HashMap::new();
    let mut check_mapper = edgecover_helper(new_matcher_pruning); // Mappings (1) To -> From    (2) From -> To

    let root_to = new_matcher_pruning.to_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.to_arena.borrow().clone();

    let bfs_root_to_tree = root_to.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search
    let bfs_root_to_tree_node = root_to.breadth_first_traversal(reference_matcher); // Root to Traversal BFS Search Copy

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
                    let parent_index_1 = to_arena[to_node].parent().unwrap().id();
                    // Check if the from node parent exists
                    let size_from_arena: usize = from_arena.size();
                    let index_parent_from_index =
                        *get_index_from_node(&indexes_t_f.clone(), parent_index_1);
                    if index_parent_from_index < size_from_arena {
                        let get_position = to_node.get_child_position(&(to_arena));
                        let parent_from_node = NodeId::<FromNodeId>::new(index_parent_from_index);
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
                            let mut move_action =
                                Move::new(from, parent_from_node, get_position.unwrap() as u16);
                            move_action.apply(&mut from_arena);
                            script.push(move_action);
                            hash_from_ids_inserted.insert(from);
                        }
                        // The if above checks if they were in the same position if they are not then move otherwise "Perform the move operation"
                        indexes_t_f.insert(to_node.id(), from.id());
                    }
                } else if edge_type == EdgeType::GLUE {
                    // Get Parent
                    let parent_index_1 = to_arena[to_node].parent().unwrap().id();
                    let get_position = to_node.get_child_position(&(to_arena));
                    // if the parent exists then do the operation
                    let size_from_arena: usize = from_arena.size();
                    let index_parent_from_index =
                        *get_index_from_node(&indexes_t_f.clone(), parent_index_1);
                    if index_parent_from_index < size_from_arena {
                        let from_node_parent: NodeId<FromNodeId> =
                            NodeId::new(index_parent_from_index);
                        let mut glue_action =
                            Glue::new(from, from_node_parent, get_position.unwrap() as u16);
                        glue_action.apply(&mut from_arena);
                        script.push(glue_action);
                    }
                    indexes_t_f.insert(to_node.id(), from.id());
                } else if edge_type == EdgeType::COPY {
                    let parent_index_1 = to_arena[to_node].parent().unwrap().id();

                    let mut get_position = to_node.get_child_position(&(to_arena));

                    // if the parent exists then do the operation
                    let size_from_arena: usize = from_arena.size();

                    let index_parent_from_index =
                        *get_index_from_node(&indexes_t_f.clone(), parent_index_1);

                    if index_parent_from_index < size_from_arena {
                        let from_node_parent: NodeId<FromNodeId> =
                            NodeId::new(index_parent_from_index);

                        let mut copy_action =
                            Copy::new(from_node_parent, from, get_position.unwrap() as u16);
                        copy_action.apply(&mut from_arena);
                        script.push(copy_action);
                    }
                    indexes_t_f.insert(to_node.id(), from.id());
                } else if edge_type == EdgeType::INSERT {
                    let mut get_position = to_node.get_child_position(&(to_arena));
                    // Get Parent
                    let parent_index_1 = to_arena[to_node].parent().unwrap().id();
                    let size_from_arena: usize = from_arena.size();
                    //                    let parent_insert :NodeId<FromNodeId> = NodeId::new(parent_index_1);
                    let mut index_parent_from_index = parent_index_1;
                    if indexes_t_f.contains_key(&parent_index_1) {
                        index_parent_from_index =
                            *get_index_from_node(&indexes_t_f.clone(), parent_index_1);
                    }
                    if index_parent_from_index < size_from_arena {
                        let from_node_parent: NodeId<FromNodeId> =
                            NodeId::new(index_parent_from_index);
                        let mut new_node: NodeId<FromNodeId> = from_arena.new_node(
                            to_arena[to_node].ty.clone(),
                            to_arena[to_node].label.clone(),
                            to_arena[to_node].col_no,
                            to_arena[to_node].line_no,
                            None,
                            None,
                        );

                        // Check if the parent node are the same
                        // Check if the new inserted node doesn't exist
                        // Get Parents children
                        let inserting_parent_children = from_node_parent
                            .children(&from_arena)
                            .collect::<Vec<NodeId<FromNodeId>>>();
                        let size_of_children = inserting_parent_children.len();
                        if get_position.unwrap() != 0 && size_of_children > get_position.unwrap() {
                            if get_position.unwrap() > size_of_children {
                                get_position = Some(size_of_children);
                            }

                            let mut insert_action = Insert::new(
                                new_node,
                                Some(from_node_parent),
                                get_position.unwrap() as u16,
                            );
                            // Parent exists
                            insert_action.apply(&mut from_arena);
                            script.push(insert_action);

                            indexes_t_f.insert(to_node.id(), new_node.id());
                        } else {
                            if get_position.unwrap() > size_of_children {
                                get_position = Some(size_of_children);
                            }

                            let mut insert_action = Insert::new(
                                new_node,
                                Some(from_node_parent),
                                get_position.unwrap() as u16,
                            );
                            // Parent exists
                            insert_action.apply(&mut from_arena);
                            script.push(insert_action);

                            indexes_t_f.insert(to_node.id(), new_node.id());
                        }
                    }
                } else if edge_type == EdgeType::UPDATE {
                    // Check if the from node parent exists
                    let size_from_arena: usize = from_arena.size();
                    let parent_index_1 = to_arena[to_node].parent().unwrap().id();
                    let mut index_parent_from_index = parent_index_1;
                    if indexes_t_f.contains_key(&parent_index_1) {
                        index_parent_from_index =
                            *get_index_from_node(&indexes_t_f.clone(), parent_index_1);
                    }

                    if index_parent_from_index < size_from_arena {
                        // if the parent exists then do the operation
                        let mut update_action = Update::new(
                            from,
                            to_arena[to_node].ty.clone(),
                            to_arena[to_node].label.clone(),
                        );
                        update_action.apply(&mut from_arena);
                        script.push(update_action);
                    }
                    indexes_t_f.insert(to_node.id(), from.id());
                } else if edge_type == EdgeType::OK {
                    indexes_t_f.insert(to_node.id(), from.id());
                }
            }
        }
    }

    // Do Delete Edit operation first.
    check_mapper = edgecover_helper(&new_matcher_pruning);

    let root_from = from_arena.root().unwrap(); // From Tree Root
    let reference_matcher_from = &from_arena.clone();
    let post_order_from_tree = root_from.post_order_traversal(reference_matcher_from); // Traversal Post Order To Delete Nodes
    let mut vec_delete_actions: Vec<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = Vec::new();
    for from_node in post_order_from_tree {
        // Previously
        let get_edge = (check_mapper.1).get(&from_node);
        if get_edge.is_some() {
            for &(to, edge_type, _) in &get_edge.unwrap().0 {
                if edge_type == EdgeType::DELETE {
                    let mut delete_action = Delete::new(from_node);
                    script.push(delete_action.clone());
                    delete_action.apply(&mut from_arena);
                    vec_delete_actions.push((from_node, to));
                }
            }
        }
    }
    (script)
}

/// Remove all edges of copy and glue that have move as their descendants
fn edgecover_annotate_helper_remove_move(
    mut new_matcher_pruning: MappingStoreGraph<String>,
) -> MappingStoreGraph<String> {
    let all_edges = new_matcher_pruning.list_edges.borrow_mut().clone(); // Vec<Edge> -- type
    let all_edge_run = all_edges.clone();
    let mut remove_index: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType, usize)> =
        HashSet::new();
    // All edges glue or move
    let mut all_edges_move_glue = all_edge_run.clone();
    all_edges_move_glue.retain(|&x| x.edge_type == EdgeType::MOVE || x.edge_type == EdgeType::GLUE);

    // get list of moves and remove them.
    for edge in &all_edge_run {
        if edge.edge_type == EdgeType::COPY || edge.edge_type == EdgeType::GLUE {
            // Look at their descendants
            let all_from_descendants = edge.from_node
                .descendants(&new_matcher_pruning.from_arena.borrow())
                .collect::<HashSet<NodeId<FromNodeId>>>();
            // counter
            for edge_move in all_edge_run.clone() {
                // Look for edge move
                if edge_move.edge_type == EdgeType::MOVE
                    || edge_move.edge_type == EdgeType::GLUE
                        && all_from_descendants.contains(&edge_move.from_node)
                {
                    if !remove_index.contains(&(
                        edge_move.from_node,
                        edge_move.to_node,
                        edge_move.edge_type,
                        edge_move.value,
                    )) {
                        remove_index.insert((
                            edge_move.from_node,
                            edge_move.to_node,
                            edge_move.edge_type,
                            edge_move.value,
                        ));
                    }
                }
            }
        }
    }

    let mut all_edges_output: Vec<Edge> = Vec::new();
    for edge_iter in all_edges.clone() {
        if !remove_index.contains(&(
            edge_iter.from_node,
            edge_iter.to_node,
            edge_iter.edge_type,
            edge_iter.value,
        )) {
            all_edges_output.push(edge_iter);
        }
    }

    new_matcher_pruning.list_edges = RefCell::new(all_edges_output.to_vec()); // Make reassign the list edges

    // Remove any copy operations that have the same to node ancestor node

    let mut all_edge_copy = all_edge_run.clone();
    all_edge_copy.retain(|&x| x.edge_type == EdgeType::COPY);

    let mut hash_to_node: HashSet<NodeId<ToNodeId>> = HashSet::new();
    for e in all_edge_copy.clone() {
        hash_to_node.insert(e.to_node);
    }

    let root_to = new_matcher_pruning.to_arena.borrow().root().unwrap(); // To Tree Root
    let reference_matcher = &new_matcher_pruning.to_arena.borrow().clone();
    let bfs_root_to_tree = root_to.breadth_first_traversal(reference_matcher); // Root To Traversal BFS Search

    // Remove copy within copy
    let mut hash_set_copy_visited: HashSet<NodeId<ToNodeId>> = HashSet::new();
    let mut hash_set_copy_visited_good: HashSet<NodeId<ToNodeId>> = HashSet::new();
    for to_node in bfs_root_to_tree {
        // The to node is an copy edge
        if hash_to_node.contains(&to_node) && !hash_set_copy_visited.contains(&to_node) {
            let to_node_desc = to_node
                .descendants(&new_matcher_pruning.to_arena.borrow())
                .collect::<HashSet<NodeId<ToNodeId>>>();

            for t_node_desc in to_node_desc {
                hash_set_copy_visited.insert(t_node_desc);
            }
            hash_set_copy_visited_good.insert(to_node);
        }
    }

    let mut edges_to_keep = new_matcher_pruning.list_edges.borrow_mut().clone();
    edges_to_keep.retain(|&x| x.edge_type != EdgeType::COPY);
    for edge_copy in all_edge_copy.clone() {
        if hash_set_copy_visited_good.contains(&edge_copy.to_node) {
            edges_to_keep.push(edge_copy);
        }
    }
    new_matcher_pruning.list_edges = RefCell::new(edges_to_keep.to_vec()); // Make reassign the list edges

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
pub fn get_index_from_node(map: &HashMap<usize, usize>, to_node: usize) -> &usize {
    map.get(&to_node).expect("Node not found.")
}

impl<T: Clone + Debug + Eq + 'static> MatchingTreesScriptor<T> for Config2 {
    /// Perform matches
    fn match_trees(
        &self,
        base: Arena<T, FromNodeId>,
        diff: Arena<T, ToNodeId>,
        cost_all: CostEdge,
    ) -> MappingStoreGraph<T> {
        let mut store = MappingStoreGraph::new(base.clone(), diff.clone());
        if store.from_arena.borrow().is_empty() || store.to_arena.borrow().is_empty() {
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

        let last_node_from_node = NodeId::<FromNodeId>::new(base.size() - 1);
        let last_node_to_node = NodeId::<ToNodeId>::new(diff.clone().size() - 1);
        let base_pre = store
            .from_arena
            .borrow()
            .root()
            .unwrap()
            .pre_order_traversal(&store.from_arena.borrow())
            .collect::<Vec<NodeId<FromNodeId>>>();
        let diff_pre = store
            .to_arena
            .borrow()
            .root()
            .unwrap()
            .pre_order_traversal(&store.to_arena.borrow())
            .collect::<Vec<NodeId<ToNodeId>>>();
        // Hash Set See if it contains insertion and deletion
        let mut set_contains_insertion = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        let mut set_contains_deletion = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        for (from_node_id, from_node) in base_pre.iter().enumerate() {
            let mut bool_check = false;
            for (to_node_id, to_node) in diff_pre.iter().enumerate() {
                if has_same_type_and_label(
                    from_node,
                    &store.from_arena.borrow(),
                    to_node,
                    &store.to_arena.borrow(),
                ) {
                    if from_node_id == to_node_id {
                        let parent_from_node =
                            store.from_arena.borrow()[base_pre[from_node_id]].parent();
                        let parent_to_node = store.to_arena.borrow()[diff_pre[to_node_id]].parent();
                        // Check if their parents are the same
                        // Then OK should be good to go
                        if base_pre[from_node_id].is_root(&store.from_arena.borrow())
                            && diff_pre[to_node_id].is_root(&store.to_arena.borrow())
                        {
                            store.push_edge(
                                base_pre[from_node_id],
                                diff_pre[to_node_id],
                                cost_store_ok + 0,
                                EdgeType::OK,
                            );
                        } else {
                            if parent_from_node.is_some() && parent_to_node.is_some()
                                && has_same_type_and_label(
                                    &parent_from_node.unwrap(),
                                    &store.from_arena.borrow(),
                                    &parent_to_node.unwrap(),
                                    &store.to_arena.borrow(),
                                ) {
                                // If the parent are equal if so then OK edge
                                store.push_edge(
                                    base_pre[from_node_id],
                                    diff_pre[to_node_id],
                                    cost_store_ok,
                                    EdgeType::OK,
                                );
                            }
                        }
                    } else {
                        let descendants_from = base_pre[from_node_id]
                            .descendants(&store.from_arena.borrow())
                            .collect::<HashSet<NodeId<FromNodeId>>>();
                        let descendants_to = diff_pre[to_node_id]
                            .descendants(&store.to_arena.borrow())
                            .collect::<HashSet<NodeId<ToNodeId>>>();
                        if descendants_from.len() == descendants_to.len() {
                            store.push_edge(
                                base_pre[from_node_id],
                                diff_pre[to_node_id],
                                cost_store_move,
                                EdgeType::MOVE,
                            );
                        }
                    }
                    bool_check = true;
                } else if has_same_type(
                    &base_pre[from_node_id],
                    &store.from_arena.borrow(),
                    &diff_pre[to_node_id],
                    &store.to_arena.borrow(),
                ) {
                    if from_node_id == to_node_id {
                        // the same label and value but different position
                        store.push_edge(
                            base_pre[from_node_id],
                            diff_pre[to_node_id],
                            cost_store_update,
                            EdgeType::UPDATE,
                        );
                    }
                }

                // We have reached the last node in the diff tree
                if !bool_check && to_node_id == diff_pre.len() - 1 {
                    store.delete_node(base_pre[from_node_id]);
                    store.push_edge(
                        base_pre[from_node_id],
                        last_node_to_node,
                        cost_store_delete,
                        EdgeType::DELETE,
                    );
                    set_contains_deletion.insert((base_pre[from_node_id], last_node_to_node));
                }
            }
        }
        for to_node_id in 0..diff_pre.len() {
            let mut bool_check = false;
            for (from_node_id, from_node) in base_pre.iter().enumerate() {
                if has_same_type_and_label(
                    from_node,
                    &store.from_arena.borrow(),
                    &diff_pre[to_node_id],
                    &store.to_arena.borrow(),
                ) {
                    bool_check = true;
                }
                if !bool_check && from_node_id == base_pre.len() - 1 {
                    store.push_edge(
                        last_node_from_node,
                        diff_pre[to_node_id],
                        cost_store_insert,
                        EdgeType::INSERT,
                    );
                    set_contains_insertion.insert((last_node_from_node, diff_pre[to_node_id]));
                }
            }
        }
        let get_all_edges = store.list_edges.borrow().clone();
        // Mappings mean
        let mut all_move_mapping_t_f: HashMap<
            NodeId<ToNodeId>,
            HashSet<NodeId<FromNodeId>>,
        > = HashMap::new();
        let mut all_move_mapping_f_t: HashMap<
            NodeId<FromNodeId>,
            HashSet<NodeId<ToNodeId>>,
        > = HashMap::new();

        let mut all_move_mapping_t_f_leaves: HashMap<
            NodeId<ToNodeId>,
            HashSet<NodeId<FromNodeId>>,
        > = HashMap::new();
        let mut all_move_mapping_f_t_leaves: HashMap<
            NodeId<FromNodeId>,
            HashSet<NodeId<ToNodeId>>,
        > = HashMap::new();

        let mut all_edges_move = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType)>::new();
        let mut all_leaves_move =
            HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>, EdgeType)>::new();
        for edge in get_all_edges.clone() {
            if edge.edge_type == EdgeType::MOVE {
                // start to mappings
                all_move_mapping_t_f
                    .entry(edge.to_node)
                    .or_insert(HashSet::new())
                    .insert(edge.from_node);
                all_move_mapping_f_t
                    .entry(edge.from_node)
                    .or_insert(HashSet::new())
                    .insert(edge.to_node);
                // end of mappings

                let edge_copy = edge.clone();
                all_edges_move.insert((edge.from_node, edge.to_node, edge.edge_type));
                let m_check = edge.from_node.is_leaf(&store.from_arena.borrow());
                let n_check = edge.to_node.is_leaf(&store.to_arena.borrow());
                if m_check && n_check {
                    all_move_mapping_t_f_leaves
                        .entry(edge.to_node)
                        .or_insert(HashSet::new())
                        .insert(edge.from_node);
                    all_move_mapping_f_t_leaves
                        .entry(edge.from_node)
                        .or_insert(HashSet::new())
                        .insert(edge.to_node);

                    all_leaves_move.insert((
                        edge_copy.from_node,
                        edge_copy.to_node,
                        edge_copy.edge_type,
                    ));
                }
            }
        }
        let mut all_leaves_parent = HashSet::<(
            NodeId<FromNodeId>,
            NodeId<ToNodeId>,
            NodeId<FromNodeId>,
            NodeId<ToNodeId>,
        )>::new();
        // Check if its nodes are in the leaf move operation
        for edge in all_leaves_move.clone() {
            let e1_parent = store.from_arena.borrow()[edge.0].parent().unwrap(); // edge.0.parent;
            let e1_to_node_parent = store.to_arena.borrow()[edge.1].parent().unwrap(); // edge to node parent

            // first check parent nodes are have move operation
            // Then check thier children also have move operation
            // The children nodes of both from_node parent and to_node_parent
            // should have edges to one another.
            if all_edges_move.contains(&(e1_parent, e1_to_node_parent, EdgeType::MOVE)) {
                let e1_siblings = e1_parent
                    .children(&store.from_arena.borrow())
                    .collect::<Vec<NodeId<FromNodeId>>>();
                let e1_to_node_siblings = e1_to_node_parent
                    .children(&store.to_arena.borrow())
                    .collect::<Vec<NodeId<ToNodeId>>>();

                // First check
                // See if the parent in the to_nodes have the same number of children as the number of sibilings
                if e1_to_node_siblings.len() == e1_siblings.len() {
                    let mut counter_of_children_mapping = 0;
                    for i in 0..e1_siblings.len() {
                        if all_leaves_move.contains(&(
                            e1_siblings[i],
                            e1_to_node_siblings[i],
                            EdgeType::MOVE,
                        )) {
                            counter_of_children_mapping += 1;
                        }
                    }
                    if counter_of_children_mapping == e1_to_node_siblings.len() {
                        all_leaves_parent.insert((e1_parent, e1_to_node_parent, edge.0, edge.1));
                    }
                }
            }
        }
        // Loop through parents and check if they are in move hash set
        let mut end_copy_tree = HashSet::<(NodeId<FromNodeId>, NodeId<ToNodeId>)>::new();
        for edge in all_leaves_parent.clone() {
            let mut edge_copy = edge;
            let mut edge_change = (edge_copy.0, edge_copy.1);
            let mut vector_input: Vec<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = Vec::new();
            vector_input.push((edge_change.0, edge_change.1));
            loop {
                let parent_m_option = store.from_arena.borrow()[edge_change.0].parent();
                let parent_n_option = store.to_arena.borrow()[edge_change.1].parent();
                if parent_n_option.is_some() && parent_m_option.is_some() {
                    let parent_m = parent_m_option.unwrap();
                    let parent_n = parent_n_option.unwrap();

                    if all_edges_move.contains(&(parent_m, parent_n, EdgeType::MOVE))
                        && !parent_m.is_root(&store.from_arena.borrow())
                        && !parent_n.is_root(&store.to_arena.borrow())
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
        let mut hash_set_copy_glue_edges: HashSet<(
            NodeId<FromNodeId>,
            NodeId<ToNodeId>,
            EdgeType,
        )> = HashSet::new();
        for nodes in end_copy_tree {
            if store.check_subtree(nodes.0, nodes.1) {
                // - If insertion in sub-tree from tree 2
                // - If deletion in sub-tree from tree 1
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
        let ct = cmp::max(ct1, input.all_edge_cost.glue); // ct the max value of Move , copy and glue
        let all_edges = input.list_edges.borrow().clone();
        let mut edges_to_be_pruned: Vec<Edge> = Vec::new();
        let mut edges_still_left: Vec<Edge> = Vec::new();

        // Loop through all the edges and get their from,and to and type
        let mut hash_edges: HashSet<(NodeId<FromNodeId>, NodeId<ToNodeId>)> = HashSet::new();

        for edge in &all_edges {
            if edge.edge_type == EdgeType::MOVE {
                hash_edges.insert((edge.from_node, edge.to_node));
            }
        }
        // Get lower bounds for all edges.

        let all_lower_bound_edges = input.make_all_lower_bound_edges(&hash_edges);
        let all_upper_bound_edges = input.make_all_upper_bound_edges(&hash_edges);
        let mut list_t_f: HashMap<
            NodeId<ToNodeId>,
            (HashSet<(NodeId<FromNodeId>, EdgeType, usize)>, usize),
        > = HashMap::new();
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

        // From_ Node -> To Node
        let mut list_f_t: HashMap<
            NodeId<FromNodeId>,
            (HashSet<(NodeId<ToNodeId>, EdgeType, usize)>, usize),
        > = HashMap::new();
        for edge1 in input.list_edges.borrow().clone() {
            let mut set: HashSet<(NodeId<ToNodeId>, EdgeType, usize)> = HashSet::new();
            let mut size: usize = 1;
            for edge2 in input.list_edges.borrow().clone() {
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
        let helper_function = (list_t_f, list_f_t);

        for edge in &all_edges {
            // edge variable will be E1 --> (m,n)
            // Get All The edges for E2 --> where (m,n1)
            // Where the from_node_id is the same but to_id is different
            // Get All The edges for E3 ---> where (m1,n)
            // Where the to_node_id is the same but to from_id is different
            // E2 = (m,n1) &&   E3 = (m1,n)

            let m = edge.from_node;
            let n = edge.to_node;

            // Lets get edge E2
            let e2_to_node_checker = helper_function.1.get(&m);
            let mut e2_to_node = None;
            if e2_to_node_checker.is_some() {
                e2_to_node = e2_to_node_checker.unwrap().0.iter().nth(0);
            }

            let e3_from_node_checker = helper_function.0.get(&n);
            let mut e3_from_node = None;
            if e3_from_node_checker.is_some() {
                e3_from_node = e3_from_node_checker.unwrap().0.iter().nth(0);
            }
            // Get the upper bound of e2
            let upper_bound_e2 = *all_upper_bound_edges
                .get(&(m, e2_to_node.unwrap().0))
                .unwrap_or_else(|| &1);
            let upper_bound_e3 = *all_upper_bound_edges
                .get(&(e3_from_node.unwrap().0, n))
                .unwrap_or_else(|| &1);

            let lower_bound_e1 = *all_lower_bound_edges.get(&(m, n)).unwrap_or_else(|| &1);
            let descendants_m_delete = m.descendants(&input.from_arena.borrow())
                .collect::<Vec<NodeId<FromNodeId>>>();
            let descendants_n_insert = n.descendants(&input.to_arena.borrow())
                .collect::<Vec<NodeId<ToNodeId>>>();

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
                let m = e1_update
                    .from_node
                    .descendants(&input.from_arena.borrow())
                    .collect::<Vec<NodeId<FromNodeId>>>()
                    .len();
                let n = e1_update
                    .to_node
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
    fn match_trees(
        &self,
        base: Arena<T, FromNodeId>,
        diff: Arena<T, ToNodeId>,
        costs_edges: CostEdge,
    ) -> MappingStoreGraph<T>;

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
pub fn chawathe_matching_actual(
    mut from_arena: Arena<String, FromNodeId>,
    mut to_arena: Arena<String, ToNodeId>,
    user_defined_cost: CostEdge,
) -> EditScriptResult<String> {
    from_arena.new_node(
        String::from("INSERT"),
        String::from("INSERT"),
        None,
        None,
        None,
        None,
    );
    to_arena.new_node(
        String::from("DELETE"),
        String::from("DELETE"),
        None,
        None,
        None,
        None,
    );

    let matching_config = Config2::new();

    // Define cost from use input
    let cost_edges = user_defined_cost;
    let mut matcher = matching_config.match_trees(from_arena.clone(), to_arena.clone(), cost_edges);

    // Prune Edges
    matching_config.prune_edges(&mut matcher);

    // Start of Min Edge Cover
    let edge_min = edgecover_solver(matcher);
    let mut new_matcher_pruning = edge_min.0;

    // Apply edit script operations
    let edit_script = edgecover_apply_edit_script(&mut new_matcher_pruning);

    let from_root = new_matcher_pruning.from_arena.borrow().root().unwrap();
    let to_root = new_matcher_pruning.to_arena.borrow().root().unwrap();
    let traverse_from = from_root
        .breadth_first_traversal(&new_matcher_pruning.from_arena.borrow())
        .collect::<Vec<NodeId<FromNodeId>>>();
    let traverse_to = to_root
        .breadth_first_traversal(&new_matcher_pruning.to_arena.borrow())
        .collect::<Vec<NodeId<ToNodeId>>>();

    let mut values_same = true;
    // Check whether the size of arena's are the same
    assert_eq!(traverse_from.len(), traverse_to.len());
    if traverse_from.len() == traverse_to.len() {
        for i in 0..traverse_from.len() {
            if !has_same_type_and_label(
                &traverse_from[i],
                &new_matcher_pruning.from_arena.borrow(),
                &traverse_to[i],
                &new_matcher_pruning.to_arena.borrow(),
            ) {
                values_same = false;
            }
        }
        assert_eq!(values_same, true);
    }
    for edge in new_matcher_pruning.list_edges.borrow().iter() {
        new_matcher_pruning.push(edge.from_node, edge.to_node, &edge.edge_type);
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

    /// Creates a simple tree for tests which is FromNodeId
    pub fn create_arena() -> Arena<String, FromNodeId> {
        let mut arena = Arena::new();
        let root = arena.new_node(
            String::from("Expr"),
            String::from("+"),
            None,
            None,
            None,
            None,
        );

        let n2 = arena.new_node(
            String::from("Expr"),
            String::from("*"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(
            String::from("INT"),
            String::from("3"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node(
            String::from("INT"),
            String::from("4"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(n2, &mut arena).unwrap();

        let n1 = arena.new_node(
            String::from("INT"),
            String::from("1"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(root, &mut arena).unwrap();
        arena
    }

    /// Creates a simple tree for tests which is ToNodeId
    pub fn create_arena_2() -> Arena<String, ToNodeId> {
        let mut arena = Arena::new();
        let root = arena.new_node(
            String::from("Expr"),
            String::from("+"),
            None,
            None,
            None,
            None,
        );

        let n2 = arena.new_node(
            String::from("Expr"),
            String::from("*"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(
            String::from("INT"),
            String::from("3"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node(
            String::from("INT"),
            String::from("4"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(n2, &mut arena).unwrap();

        let n1 = arena.new_node(
            String::from("INT"),
            String::from("1"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(root, &mut arena).unwrap();

        arena
    }

    #[test]
    /// Test for perfect Matching
    pub fn test_chawathe_matching_1() -> () {
        let mut tree1 = create_arena();
        tree1.new_node(
            String::from("INSERT"),
            String::from("INSERT"),
            None,
            None,
            None,
            None,
        );

        let mut tree2 = create_arena_2();
        tree2.new_node(
            String::from("DELETE"),
            String::from("DELETE"),
            None,
            None,
            None,
            None,
        );

        let matching_config = Config2::new();
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);
        let matcher = matching_config.match_trees(tree1.clone(), tree2.clone(), cost_edges);
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
        let root = arena.new_node(
            String::from("Expr"),
            String::from("+"),
            None,
            None,
            None,
            None,
        );

        let n4 = arena.new_node(
            String::from("Expr"),
            String::from("*"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(root, &mut arena).unwrap();
        let n5 = arena.new_node(
            String::from("INT"),
            String::from("3"),
            None,
            None,
            None,
            None,
        );
        n5.make_child_of(n4, &mut arena).unwrap();
        let n6 = arena.new_node(
            String::from("INT"),
            String::from("4"),
            None,
            None,
            None,
            None,
        );
        n6.make_child_of(n4, &mut arena).unwrap();

        let n1 = arena.new_node(
            String::from("Expr"),
            String::from("*"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(
            String::from("INT"),
            String::from("1"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(n1, &mut arena).unwrap();
        let n3 = arena.new_node(
            String::from("INT"),
            String::from("2"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(n1, &mut arena).unwrap();

        arena.new_node(
            String::from("INSERT"),
            String::from("INSERT"),
            None,
            None,
            None,
            None,
        );
        // Tree 2
        let mut tree2 = create_arena_2();
        tree2.new_node(
            String::from("DELETE"),
            String::from("DELETE"),
            None,
            None,
            None,
            None,
        );
        let matching_config = Config2::new();
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);

        let matcher = matching_config.match_trees(arena.clone(), tree2.clone(), cost_edges);
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(6, 5, &EdgeType::DELETE, &mut checker_edges_induced));
    }

    #[test]
    // Tree 2 has more nodes and tree 2 stays the same as create arena
    pub fn test_chawathe_matching_3() -> () {
        // Tree 2
        let mut arena = Arena::new();
        let root = arena.new_node(
            String::from("Expr"),
            String::from("+"),
            None,
            None,
            None,
            None,
        );

        let n4 = arena.new_node(
            String::from("Expr"),
            String::from("*"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(root, &mut arena).unwrap();
        let n5 = arena.new_node(
            String::from("INT"),
            String::from("3"),
            None,
            None,
            None,
            None,
        );
        n5.make_child_of(n4, &mut arena).unwrap();
        let n6 = arena.new_node(
            String::from("INT"),
            String::from("4"),
            None,
            None,
            None,
            None,
        );
        n6.make_child_of(n4, &mut arena).unwrap();

        let n1 = arena.new_node(
            String::from("Expr"),
            String::from("*"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(
            String::from("INT"),
            String::from("1"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(n1, &mut arena).unwrap();
        let n3 = arena.new_node(
            String::from("INT"),
            String::from("2"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(n1, &mut arena).unwrap();

        arena.new_node(
            String::from("DELETE"),
            String::from("DELETE"),
            None,
            None,
            None,
            None,
        );
        // Tree 1
        let mut tree1 = create_arena();
        tree1.new_node(
            String::from("INSERT"),
            String::from("INSERT"),
            None,
            None,
            None,
            None,
        );
        let matching_config = Config2::new();
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);

        let matcher = matching_config.match_trees(tree1.clone(), arena.clone(), cost_edges);
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 5, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 6, &EdgeType::INSERT, &mut checker_edges_induced));
    }

    #[test]
    pub fn test_chawathe_matching_4() -> () {
        // Tree 2
        let mut arena = Arena::new();
        let root = arena.new_node(
            String::from("Expr"),
            String::from("a"),
            None,
            None,
            None,
            None,
        );

        // subtree 1
        let n1 = arena.new_node(
            String::from("Expr"),
            String::from("f"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(
            String::from("Expr"),
            String::from("b"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(n1, &mut arena).unwrap();
        let n3 = arena.new_node(
            String::from("Expr"),
            String::from("d"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(n2, &mut arena).unwrap();
        //      n1
        //     /
        //   n2
        //   /
        // n3

        // End subtree 1
        // Subtree 2
        let n4 = arena.new_node(
            String::from("Expr"),
            String::from("e"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(root, &mut arena).unwrap();
        let n5 = arena.new_node(
            String::from("Expr"),
            String::from("a"),
            None,
            None,
            None,
            None,
        );
        n5.make_child_of(n4, &mut arena).unwrap();
        let n6 = arena.new_node(
            String::from("Expr"),
            String::from("f"),
            None,
            None,
            None,
            None,
        );
        n6.make_child_of(n4, &mut arena).unwrap();
        let n7 = arena.new_node(
            String::from("Expr"),
            String::from("b"),
            None,
            None,
            None,
            None,
        );
        n7.make_child_of(n6, &mut arena).unwrap();
        let n8 = arena.new_node(
            String::from("Expr"),
            String::from("d"),
            None,
            None,
            None,
            None,
        );
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
        let n9 = arena.new_node(
            String::from("Expr"),
            String::from("g"),
            None,
            None,
            None,
            None,
        );
        n9.make_child_of(root, &mut arena).unwrap();
        let n10 = arena.new_node(
            String::from("Expr"),
            String::from("cc"),
            None,
            None,
            None,
            None,
        );
        n10.make_child_of(n9, &mut arena).unwrap();
        let n11 = arena.new_node(
            String::from("Expr"),
            String::from("ac"),
            None,
            None,
            None,
            None,
        );
        n11.make_child_of(n10, &mut arena).unwrap();
        let n12 = arena.new_node(
            String::from("Expr"),
            String::from("cd"),
            None,
            None,
            None,
            None,
        );
        n12.make_child_of(n9, &mut arena).unwrap();
        let n13 = arena.new_node(
            String::from("Expr"),
            String::from("ad"),
            None,
            None,
            None,
            None,
        );
        n13.make_child_of(n12, &mut arena).unwrap();

        //                n9
        //               /  \
        //             n10   n12
        //            /        \
        //         n11         n13
        // End of Subtree 3

        arena.new_node(
            String::from("DELETE"),
            String::from("DELETE"),
            None,
            None,
            None,
            None,
        );
        // Tree 1
        let mut tree1 = Arena::new();

        let root = tree1.new_node(
            String::from("Expr"),
            String::from("a"),
            None,
            None,
            None,
            None,
        );

        // Subtree 2
        let n4 = tree1.new_node(
            String::from("Expr"),
            String::from("e"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(root, &mut tree1).unwrap();
        let n5 = tree1.new_node(
            String::from("Expr"),
            String::from("a"),
            None,
            None,
            None,
            None,
        );
        n5.make_child_of(n4, &mut tree1).unwrap();
        let n6 = tree1.new_node(
            String::from("Expr"),
            String::from("f"),
            None,
            None,
            None,
            None,
        );
        n6.make_child_of(n4, &mut tree1).unwrap();
        let n7 = tree1.new_node(
            String::from("Expr"),
            String::from("b"),
            None,
            None,
            None,
            None,
        );
        n7.make_child_of(n6, &mut tree1).unwrap();
        let n8 = tree1.new_node(
            String::from("Expr"),
            String::from("d"),
            None,
            None,
            None,
            None,
        );
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
        let n9 = tree1.new_node(
            String::from("Expr"),
            String::from("g"),
            None,
            None,
            None,
            None,
        );
        n9.make_child_of(root, &mut tree1).unwrap();
        let n10 = tree1.new_node(
            String::from("Expr"),
            String::from("cc"),
            None,
            None,
            None,
            None,
        );
        n10.make_child_of(n9, &mut tree1).unwrap();
        let n11 = tree1.new_node(
            String::from("Expr"),
            String::from("ac"),
            None,
            None,
            None,
            None,
        );
        n11.make_child_of(n10, &mut tree1).unwrap();
        let n12 = tree1.new_node(
            String::from("Expr"),
            String::from("cd"),
            None,
            None,
            None,
            None,
        );
        n12.make_child_of(n9, &mut tree1).unwrap();
        let n13 = tree1.new_node(
            String::from("Expr"),
            String::from("ad"),
            None,
            None,
            None,
            None,
        );
        n13.make_child_of(n12, &mut tree1).unwrap();

        //                n9
        //               /  \
        //             n10   n12
        //            /        \
        //         n11         n13

        // End of Subtree 3
        tree1.new_node(
            String::from("INSERT"),
            String::from("INSERT"),
            None,
            None,
            None,
            None,
        );
        let matching_config = Config2::new();
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);

        let matcher = matching_config.match_trees(arena.clone(), tree1.clone(), cost_edges);

        let mut checker_edges_induced = matcher.list_edges.borrow().clone();

        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 3, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 2, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 5, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 2, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(5, 5, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(6, 3, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(6, 6, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(7, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(7, 7, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(8, 5, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(8, 8, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(9, 6, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(9, 9, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(10, 7, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(10, 10, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(11, 8, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(12, 9, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(13, 10, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(9, 6, &EdgeType::GLUE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 1, &EdgeType::GLUE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 3, &EdgeType::GLUE, &mut checker_edges_induced));
    }

    #[test]
    pub fn test_chawathe_matching_5() -> () {
        // Tree 2
        let mut arena: Arena<String, FromNodeId> = Arena::new();
        let root = arena.new_node(
            String::from("NUMB"),
            String::from("1"),
            None,
            None,
            None,
            None,
        );
        // subtree 1
        let n1 = arena.new_node(
            String::from("NUMB"),
            String::from("2"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(
            String::from("NUMB"),
            String::from("3"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(
            String::from("NUMB"),
            String::from("4"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(n1, &mut arena).unwrap();
        let n4 = arena.new_node(
            String::from("NUMB"),
            String::from("5"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(n1, &mut arena).unwrap();
        arena.new_node(
            String::from("INSERT"),
            String::from("INSERT"),
            None,
            None,
            None,
            None,
        );
        // Tree 1
        let mut tree1: Arena<String, ToNodeId> = Arena::new();
        let root = tree1.new_node(
            String::from("NUMB"),
            String::from("1"),
            None,
            None,
            None,
            None,
        );
        let n2 = tree1.new_node(
            String::from("NUMB"),
            String::from("3"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(root, &mut tree1).unwrap();
        let n1 = tree1.new_node(
            String::from("NUMB"),
            String::from("2"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(n2, &mut tree1).unwrap();
        let n3 = tree1.new_node(
            String::from("NUMB"),
            String::from("4"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(n1, &mut tree1).unwrap();
        let n4 = tree1.new_node(
            String::from("NUMB"),
            String::from("5"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(n1, &mut tree1).unwrap();

        tree1.new_node(
            String::from("DELETE"),
            String::from("DELETE"),
            None,
            None,
            None,
            None,
        );

        let matching_config = Config2::new();
        let cost_edges = CostEdge::new(1, 1, 1, 1, 1, 1, 1, 1);

        let matcher = matching_config.match_trees(arena.clone(), tree1.clone(), cost_edges);
        let mut checker_edges_induced = matcher.list_edges.borrow().clone();
        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 1, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 2, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 2, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(3, 3, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 3, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(4, 4, &EdgeType::MOVE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(2, 4, &EdgeType::UPDATE, &mut checker_edges_induced));
        assert!(matcher.contains_edge(1, 2, &EdgeType::GLUE, &mut checker_edges_induced));
    }

    #[test]
    pub fn test_chawathe_matching_6() -> () {
        // Tree 1
        let mut arena: Arena<String, FromNodeId> = Arena::new();
        let root = arena.new_node(
            String::from("Exp"),
            String::from("a"),
            None,
            None,
            None,
            None,
        );
        // subtree 1
        let n1 = arena.new_node(
            String::from("Exp"),
            String::from("b"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(root, &mut arena).unwrap();
        let n1_1 = arena.new_node(
            String::from("Exp"),
            String::from("d"),
            None,
            None,
            None,
            None,
        );
        n1_1.make_child_of(n1, &mut arena).unwrap();

        // Subtree 2
        let n2 = arena.new_node(
            String::from("Exp"),
            String::from("e"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(root, &mut arena).unwrap();
        let n2_1 = arena.new_node(
            String::from("Exp"),
            String::from("a"),
            None,
            None,
            None,
            None,
        );
        n2_1.make_child_of(n2, &mut arena).unwrap();
        let n2_2 = arena.new_node(
            String::from("Exp"),
            String::from("f"),
            None,
            None,
            None,
            None,
        );
        n2_2.make_child_of(n2, &mut arena).unwrap();
        // Subtree 3
        let n3 = arena.new_node(
            String::from("Exp"),
            String::from("cc"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(root, &mut arena).unwrap();
        let n3_1 = arena.new_node(
            String::from("Exp"),
            String::from("ac"),
            None,
            None,
            None,
            None,
        );
        n3_1.make_child_of(n3, &mut arena).unwrap();
        // Subtree 4
        let n4 = arena.new_node(
            String::from("Exp"),
            String::from("cd"),
            None,
            None,
            None,
            None,
        );
        n4.make_child_of(root, &mut arena).unwrap();
        let n4_1 = arena.new_node(
            String::from("Exp"),
            String::from("ad"),
            None,
            None,
            None,
            None,
        );
        n4_1.make_child_of(n4, &mut arena).unwrap();
        arena.new_node(
            String::from("INSERT"),
            String::from("INSERT"),
            None,
            None,
            None,
            None,
        );
        // Tree 2
        let mut tree1: Arena<String, ToNodeId> = Arena::new();
        let root = tree1.new_node(
            String::from("Exp"),
            String::from("a"),
            None,
            None,
            None,
            None,
        );
        // Subtree 1
        let n1 = tree1.new_node(
            String::from("Exp"),
            String::from("f"),
            None,
            None,
            None,
            None,
        );
        n1.make_child_of(root, &mut tree1).unwrap();
        let n1_1 = tree1.new_node(
            String::from("Exp"),
            String::from("b"),
            None,
            None,
            None,
            None,
        );
        n1_1.make_child_of(n1, &mut tree1).unwrap();
        let n1_2 = tree1.new_node(
            String::from("Exp"),
            String::from("d"),
            None,
            None,
            None,
            None,
        );
        n1_2.make_child_of(n1_1, &mut tree1).unwrap();

        // Subtree 2
        let n2 = tree1.new_node(
            String::from("Exp"),
            String::from("e"),
            None,
            None,
            None,
            None,
        );
        n2.make_child_of(root, &mut tree1).unwrap();
        let n2_1 = tree1.new_node(
            String::from("Exp"),
            String::from("a"),
            None,
            None,
            None,
            None,
        );
        n2_1.make_child_of(n2, &mut tree1).unwrap();
        let n2_2 = tree1.new_node(
            String::from("Exp"),
            String::from("f"),
            None,
            None,
            None,
            None,
        );
        n2_2.make_child_of(n2, &mut tree1).unwrap();
        let n2_3 = tree1.new_node(
            String::from("Exp"),
            String::from("b"),
            None,
            None,
            None,
            None,
        );
        n2_3.make_child_of(n2_2, &mut tree1).unwrap();
        let n2_4 = tree1.new_node(
            String::from("Exp"),
            String::from("d"),
            None,
            None,
            None,
            None,
        );
        n2_4.make_child_of(n2_3, &mut tree1).unwrap();

        // Subtree 3

        let n3 = tree1.new_node(
            String::from("Exp"),
            String::from("g"),
            None,
            None,
            None,
            None,
        );
        n3.make_child_of(root, &mut tree1).unwrap();
        let n3_1 = tree1.new_node(
            String::from("Exp"),
            String::from("cc"),
            None,
            None,
            None,
            None,
        );
        n3_1.make_child_of(n3, &mut tree1).unwrap();
        let n3_2 = tree1.new_node(
            String::from("Exp"),
            String::from("ac"),
            None,
            None,
            None,
            None,
        );
        n3_2.make_child_of(n3_1, &mut tree1).unwrap();
        let n3_3 = tree1.new_node(
            String::from("Exp"),
            String::from("cd"),
            None,
            None,
            None,
            None,
        );
        n3_3.make_child_of(n3, &mut tree1).unwrap();
        let n3_4 = tree1.new_node(
            String::from("Exp"),
            String::from("ad"),
            None,
            None,
            None,
            None,
        );
        n3_4.make_child_of(n3_3, &mut tree1).unwrap();

        tree1.new_node(
            String::from("DELETE"),
            String::from("DELETE"),
            None,
            None,
            None,
            None,
        );

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

        assert!(matcher.contains_edge(0, 0, &EdgeType::OK, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 1, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 2, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 7, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 2, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 3, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(2, 8, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(3, 3, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 4, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(4, 5, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(5, 5, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6, 6, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6, 10, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(7, 7, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(7, 11, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8, 8, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8, 12, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(9, 9, &EdgeType::UPDATE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(9, 13, &EdgeType::MOVE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(10, 9, &EdgeType::INSERT, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 7, &EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(1, 2, &EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(8, 12, &EdgeType::GLUE, &mut checker_edges_pruned));
        assert!(matcher.contains_edge(6, 10, &EdgeType::GLUE, &mut checker_edges_pruned));

        // Test for Test 6

        // - Do Re-match trees.
        // - Perform pruning.
        // - Then check if all have OK matching.

        let matcher_copy = edgecover_solver(new_matcher_pruning);
        new_matcher_pruning = matcher_copy.0;
        let mut edit_script = edgecover_apply_edit_script(&mut new_matcher_pruning);
        let from_root = new_matcher_pruning.from_arena.borrow().root().unwrap();
        let to_root = new_matcher_pruning.to_arena.borrow().root().unwrap();
        let traverse_from = from_root
            .breadth_first_traversal(&new_matcher_pruning.from_arena.borrow())
            .collect::<Vec<NodeId<FromNodeId>>>();
        let traverse_to = to_root
            .breadth_first_traversal(&new_matcher_pruning.to_arena.borrow())
            .collect::<Vec<NodeId<ToNodeId>>>();

        assert_eq!(traverse_from.len(), traverse_to.len());

        let mut values_same = true;
        if traverse_from.len() == traverse_to.len() {
            for i in 0..traverse_from.len() {
                if !has_same_type_and_label(
                    &traverse_from[i],
                    &new_matcher_pruning.from_arena.borrow(),
                    &traverse_to[i],
                    &new_matcher_pruning.to_arena.borrow(),
                ) {
                    values_same = false;
                }
            }
        }
        assert_eq!(values_same, true);
    }
}
