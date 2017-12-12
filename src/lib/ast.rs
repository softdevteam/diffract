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

use std::collections::VecDeque;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fs::File;
use std::marker::PhantomData;
use std::io::Read;
use std::ops::{Index, IndexMut};
use std::path::Path;

use cfgrammar::TIdx;
use cfgrammar::yacc::{yacc_grm, YaccGrammar, YaccKind};
use lrlex::{build_lex, Lexer};
use lrpar::parser;
use lrtable::{Minimiser, from_yacc};

use hqueue::HeightQueue;

/// Errors raised when parsing a source file.
#[derive(Debug)]
pub enum ParseError {
    /// Io error returned from standard library routines.
    Io(String),
    /// File not found.
    FileNotFound(String),
    /// Lexer could not be built by `lrlex`.
    BrokenLexer,
    /// Parser could not be built by `lrpar`.
    BrokenParser,
    /// File contained lexical error and could not be lexed.
    LexicalError,
    /// File contained syntax error and could not be parsed.
    SyntaxError,
}

/// Errors raised by arenas.
#[derive(Debug)]
pub enum ArenaError {
    /// Arena is unexpectedly empty.
    EmtpyArena,
    /// Node ID could not be found in this arena.
    NodeIdNotFound,
    /// Node was expected to have more than `N` children.
    NodeHasTooFewChildren(u16),
    /// Two node ids were unexpectedly identical.
    NodeIdsAreIdentical,
}

/// Result type returned by AST operations.
pub type ArenaResult = Result<(), ArenaError>;

/// A node identifier for a 'from' `Arena`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct FromNodeId;

/// A node identifier for a 'to' `Arena`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct ToNodeId;

/// A node identifier used for indexing nodes within a particular `Arena`.
///
/// A `NodeId` should be immutable, in the sense that no action on a node or
/// arena should change a given `NodeId`.
#[derive(Clone, Copy, Eq, Hash, PartialEq, PartialOrd)]
pub struct NodeId<T: PartialEq + Copy> {
    index: usize,
    phantom: PhantomData<T>,
}

impl<T: PartialEq + Copy> fmt::Display for NodeId<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.index)
    }
}

/// A type to represent edges between nodes within an `Arena`.
pub type EdgeId<T> = (NodeId<T>, NodeId<T>);

/// An arena in which to place `NodeId`-indexed AST nodes.
///
/// `T` type is given to internal values stored within each node.
/// These values contain information that the `lrpar` crate will provide, and
/// represent the names of grammar productions.
#[derive(Clone)]
pub struct Arena<T: Clone, U: PartialEq + Copy> {
    nodes: Vec<Node<T, U>>,
    root: Option<NodeId<U>>,
}

impl<T: Clone, U: PartialEq + Copy> Default for Arena<T, U> {
    fn default() -> Arena<T, U> {
        Arena {
            nodes: Vec::new(),
            root: None,
        }
    }
}

impl<T: Clone, U: PartialEq + Copy> Index<NodeId<U>> for Arena<T, U> {
    type Output = Node<T, U>;

    fn index(&self, node: NodeId<U>) -> &Node<T, U> {
        &self.nodes[node.index]
    }
}

impl<T: Clone, U: PartialEq + Copy> IndexMut<NodeId<U>> for Arena<T, U> {
    fn index_mut(&mut self, node: NodeId<U>) -> &mut Node<T, U> {
        &mut self.nodes[node.index]
    }
}

#[cfg(test)]
impl<T: Clone> From<Arena<T, ToNodeId>> for Arena<T, FromNodeId> {
    fn from(arena: Arena<T, ToNodeId>) -> Self {
        let coerced = arena.nodes
                           .iter()
                           .cloned()
                           .map(|node| Node::<T, FromNodeId>::from(node))
                           .collect();
        Arena {
            nodes: coerced,
            root: arena.root().map(|id| NodeId::<FromNodeId>::from(id)),
        }
    }
}

#[cfg(test)]
impl<T: Clone> From<Arena<T, FromNodeId>> for Arena<T, ToNodeId> {
    fn from(arena: Arena<T, FromNodeId>) -> Self {
        let coerced = arena.nodes
                           .iter()
                           .cloned()
                           .map(|node| Node::<T, ToNodeId>::from(node))
                           .collect();
        Arena {
            nodes: coerced,
            root: arena.root().map(|id| NodeId::<ToNodeId>::from(id)),
        }
    }
}

impl<T: Clone, U: PartialEq + Copy> Arena<T, U> {
    /// Create an empty `Arena`.
    pub fn new() -> Arena<T, U> {
        Default::default()
    }

    /// Create a new node from its data.
    pub fn new_node(&mut self,
                    ty: T,
                    label: String,
                    col_no: Option<usize>,
                    line_no: Option<usize>,
                    char_no: Option<usize>,
                    token_len: Option<usize>)
                    -> NodeId<U> {
        let next_id = NodeId::new(self.nodes.len());
        let mut node = Node::new(ty, label, col_no, line_no, char_no, token_len);
        node.index = Some(next_id);
        if next_id.index == 0 {
            self.root = node.index;
        }
        self.nodes.push(node);
        next_id
    }

    /// Create a new root node from its data.
    /// The new root has the previous root as its single child.
    pub fn new_root(&mut self, new_root: NodeId<U>) -> ArenaResult {
        if self.root.is_none() {
            self.root = Some(new_root);
            return Ok(());
        }
        let old_root = self.root.unwrap();
        self.root = Some(new_root);
        old_root.make_child_of(new_root, self)
    }

    /// Return `true` if the arena is empty, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Return root node.
    pub fn root(&self) -> Option<NodeId<U>> {
        self.root
    }

    /// Return the size of this arena (i.e. how many nodes are held within it).
    ///
    /// Note that if some nodes have been detached (or never appended to other
    /// nodes as children), then some nodes may be unreachable.
    pub fn size(&self) -> usize {
        self.nodes.len()
    }

    /// Return `true` if index is in arena, `false` otherwise.
    pub fn contains(&self, index: NodeId<U>) -> bool {
        index.index < self.nodes.len()
    }

    /// Return a queue of `NodeId`s sorted by height.
    pub fn get_priority_queue(&self) -> HeightQueue<U> {
        let mut queue = HeightQueue::<U>::new();
        for id in 0..self.size() {
            queue.push(NodeId::new(id), self);
        }
        queue
    }

    /// Return a vector of edges between nodes in this arena.
    ///
    /// Each edge pair `(n1, n2)` denotes an edge between nodes `n1` and `n2`,
    /// both being `NodeId` indices within this arena. An edge within an `Arena`
    /// denotes a parent / child relationship between nodes.
    pub fn get_edges(&self) -> Vec<EdgeId<U>> {
        let mut edges: Vec<EdgeId<U>> = vec![];
        for index in 0..self.nodes.len() {
            let node = &self.nodes[index];
            match node.parent {
                None => (),
                Some(par) => edges.push((par, NodeId::<U>::new(index))),
            }
        }
        edges
    }
}

// Should have similar output as lrpar::parser::Node<TokId>::pretty_print().
impl<T: fmt::Debug + Clone, U: PartialEq + Copy> fmt::Debug for Arena<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.nodes.is_empty() {
            // Case where self.root is None.
            return write!(f, "");
        }
        let mut formatted = String::new();
        // Stack of (indent level, `NodeId`) pairs.
        let mut stack = vec![(0, self.root.unwrap())];
        while !stack.is_empty() {
            let (indent, id) = stack.pop().unwrap();
            let node = &self.nodes[id.index];
            for _ in 0..indent {
                formatted.push_str("  ");
            }
            formatted.push_str(&format!("{:?}\n", node));
            if !id.is_leaf(self) {
                // Non-terminal node.
                let first_child = node.first_child.unwrap();
                let mut current_child = node.last_child.unwrap();
                loop {
                    stack.push((indent + 1, current_child));
                    if current_child == first_child {
                        break;
                    }
                    match self.nodes[current_child.index].previous_sibling {
                        None => break,
                        Some(id) => current_child = id,
                    };
                }
            }
        }
        write!(f, "{}", formatted)
    }
}

/// An AST node within an `Arena`.
#[derive(Clone, PartialEq, Eq)]
pub struct Node<T: Clone, U: PartialEq + Copy> {
    parent: Option<NodeId<U>>,
    previous_sibling: Option<NodeId<U>>,
    next_sibling: Option<NodeId<U>>,
    first_child: Option<NodeId<U>>,
    last_child: Option<NodeId<U>>,
    /// Index of this node in the arena.
    ///
    /// Note that nodes with a non-`None` index are unique.
    index: Option<NodeId<U>>,
    /// The `label` of a node is the lexeme it corresponds to in the code.
    ///
    /// For example, a numerical or string literal, bracket, semicolon, etc.
    pub label: String,
    /// The `ty` (type) of a node is its name of its production in the grammar.
    ///
    /// For example, `Expr`, `Factor`, `Term`.
    pub ty: T,
    /// The character number at which the token this node represents starts in
    /// the original file.
    ///
    /// `None` if this node represents some non-terminal node in the grammar.
    pub char_no: Option<usize>,
    /// The column number at which the token this node represents starts in the
    /// original file.
    ///
    /// `None` if this node represents some non-terminal node in the grammar.
    pub col_no: Option<usize>,
    /// The line number of the token this node represents in the original file.
    ///
    /// `None` if this node represents some non-terminal node in the grammar.
    pub line_no: Option<usize>,
    /// The length of the token this node represents in the original file.
    ///
    /// `None` if this node represents some non-terminal node in the grammar.
    pub token_len: Option<usize>,
}

#[cfg(test)]
impl<T: Clone> From<Node<T, ToNodeId>> for Node<T, FromNodeId> {
    fn from(node: Node<T, ToNodeId>) -> Self {
        Node::<T, FromNodeId> {
            parent: node.parent().map(|id| NodeId::<FromNodeId>::from(id)),
            previous_sibling: node.previous_sibling()
                                  .map(|id| NodeId::<FromNodeId>::from(id)),
            next_sibling: node.next_sibling().map(|id| NodeId::<FromNodeId>::from(id)),
            first_child: node.first_child().map(|id| NodeId::<FromNodeId>::from(id)),
            last_child: node.last_child().map(|id| NodeId::<FromNodeId>::from(id)),
            index: node.index.map(|id| NodeId::<FromNodeId>::from(id)),
            ty: node.ty,
            label: node.label,
            col_no: node.col_no,
            line_no: node.line_no,
            char_no: node.char_no,
            token_len: node.token_len,
        }
    }
}

#[cfg(test)]
impl<T: Clone> From<Node<T, FromNodeId>> for Node<T, ToNodeId> {
    fn from(node: Node<T, FromNodeId>) -> Self {
        Node::<T, ToNodeId> {
            parent: node.parent().map(|id| NodeId::<ToNodeId>::from(id)),
            previous_sibling: node.previous_sibling()
                                  .map(|id| NodeId::<ToNodeId>::from(id)),
            next_sibling: node.next_sibling().map(|id| NodeId::<ToNodeId>::from(id)),
            first_child: node.first_child().map(|id| NodeId::<ToNodeId>::from(id)),
            last_child: node.last_child().map(|id| NodeId::<ToNodeId>::from(id)),
            index: node.index.map(|id| NodeId::<ToNodeId>::from(id)),
            ty: node.ty,
            label: node.label,
            col_no: node.col_no,
            line_no: node.line_no,
            char_no: node.char_no,
            token_len: node.token_len,
        }
    }
}

impl<T: Clone, U: PartialEq + Copy> Node<T, U> {
    /// Create a new node, with data, but without a parent or children.
    pub fn new(ty: T,
               label: String,
               col_no: Option<usize>,
               line_no: Option<usize>,
               char_no: Option<usize>,
               token_len: Option<usize>)
               -> Node<T, U> {
        Node {
            parent: None,
            previous_sibling: None,
            next_sibling: None,
            first_child: None,
            last_child: None,
            ty: ty,
            label: label,
            index: None,
            col_no: col_no,
            line_no: line_no,
            char_no: char_no,
            token_len: token_len,
        }
    }

    /// Return the Id of the parent node, if there is one.
    pub fn parent(&self) -> Option<NodeId<U>> {
        self.parent
    }

    /// Return the Id of the first child of this node, if there is one.
    pub fn first_child(&self) -> Option<NodeId<U>> {
        self.first_child
    }

    /// Return the Id of the last child of this node, if there is one.
    pub fn last_child(&self) -> Option<NodeId<U>> {
        self.last_child
    }

    /// Return the Id of the previous sibling of this node, if there is one.
    pub fn previous_sibling(&self) -> Option<NodeId<U>> {
        self.previous_sibling
    }

    /// Return the Id of the previous sibling of this node, if there is one.
    pub fn next_sibling(&self) -> Option<NodeId<U>> {
        self.next_sibling
    }
}

impl<T: fmt::Debug + Clone, U: PartialEq + Copy> fmt::Debug for Node<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let label_s = format!(" {}", self.label);
        // Avoid empty space in output.
        if label_s == " " {
            return write!(f, "{:?}", self.ty);
        }
        write!(f, "{:?}{}", self.ty, label_s)
    }
}

impl<T: PartialEq + Copy> fmt::Debug for NodeId<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NodeId {{ index: {} }}", self.index)
    }
}

#[cfg(test)]
impl From<NodeId<FromNodeId>> for NodeId<ToNodeId> {
    fn from(id: NodeId<FromNodeId>) -> Self {
        NodeId::<ToNodeId>::new(id.index)
    }
}

#[cfg(test)]
impl From<NodeId<ToNodeId>> for NodeId<FromNodeId> {
    fn from(id: NodeId<ToNodeId>) -> Self {
        NodeId::<FromNodeId>::new(id.index)
    }
}

impl<U: PartialEq + Copy> NodeId<U> {
    /// Create a new NodeId with a given index.
    pub fn new(index: usize) -> NodeId<U> {
        NodeId {
            index: index,
            phantom: PhantomData,
        }
    }

    /// Return the index of this Node Id.
    pub fn id(&self) -> usize {
        self.index
    }

    /// `true` if this node has no children.
    pub fn is_leaf<T: Clone>(&self, arena: &Arena<T, U>) -> bool {
        arena[*self].first_child.is_none()
    }

    /// `true` if this node has no parent.
    ///
    /// If a node has been added to the arena without being given a parent, or
    /// a node has been detached from its parent, this method will return `true`.
    pub fn is_root<T: Clone>(&self, arena: &Arena<T, U>) -> bool {
        arena[*self].parent.is_none()
    }

    /// Get the height of this node.
    ///
    /// The height of a leaf node is 1, the height of a branch node is 1 +
    /// the height of its tallest child node.
    pub fn height<T: Clone>(&self, arena: &Arena<T, U>) -> u32 {
        if self.is_leaf(arena) {
            return 1;
        }
        self.children(arena)
            .map(|child| child.height(arena))
            .max()
            .unwrap() + 1
    }

    /// Detach this node, leaving its children unaffected.
    ///
    /// Detaching a node makes it inaccessible to other nodes. The node is not
    /// actually removed from the arena, because `NodeId`s should be immutable.
    /// This means that if you detach the root of the tree, any iterator will
    /// still be able to reach the root (but not any of the other nodes).
    pub fn detach<T: Clone>(&self, arena: &mut Arena<T, U>) -> ArenaResult {
        if !arena.contains(*self) {
            return Err(ArenaError::NodeIdNotFound);
        }
        let (parent, previous_sibling, next_sibling) = {
            let node = &mut arena[*self];
            (node.parent.take(), node.previous_sibling.take(), node.next_sibling.take())
        };
        if let Some(next_sibling) = next_sibling {
            arena[next_sibling].previous_sibling = previous_sibling;
        } else if let Some(parent) = parent {
            arena[parent].last_child = previous_sibling;
        }
        if let Some(previous_sibling) = previous_sibling {
            arena[previous_sibling].next_sibling = next_sibling;
        } else if let Some(parent) = parent {
            arena[parent].first_child = next_sibling;
        }
        Ok(())
    }

    /// Detach this node and its children.
    ///
    /// Detaching a node makes it inaccessible to other nodes. The node is not
    /// actually removed from the arena, because `NodeId`s should be immutable.
    /// This means that if you detach the root of the tree, any iterator will
    /// still be able to reach the root (but not any of the other nodes).
    pub fn detach_with_children<T: Clone>(&self, arena: &mut Arena<T, U>) -> ArenaResult {
        self.detach(arena)?;
        let ids = self.children(arena).collect::<Vec<NodeId<U>>>();
        for id in ids {
            id.detach_with_children(arena)?;
        }
        Ok(())
    }

    /// Make self the next (i.e. last) child of another.
    pub fn make_child_of<T: Clone>(&self,
                                   parent: NodeId<U>,
                                   arena: &mut Arena<T, U>)
                                   -> ArenaResult {
        if self.index >= arena.size() || parent.index >= arena.size() {
            return Err(ArenaError::NodeIdNotFound);
        }
        self.detach(arena)?;
        arena[*self].parent = Some(parent);
        if arena[parent].first_child.is_none() {
            arena[parent].first_child = Some(*self);
        }
        match arena[parent].last_child {
            None => arena[parent].last_child = Some(*self),
            Some(id) => {
                arena[parent].last_child = Some(*self);
                arena[id].next_sibling = Some(*self);
                arena[*self].previous_sibling = Some(id);
            }
        };
        Ok(())
    }

    /// Make one node the nth child of another.
    ///
    /// Children are numbered from zero, so `nth == 0` makes `self` the *first*
    /// child of `parent`.
    pub fn make_nth_child_of<T: Clone>(&mut self,
                                       parent: NodeId<U>,
                                       nth: u16,
                                       arena: &mut Arena<T, U>)
                                       -> ArenaResult {
        if !arena.contains(*self) {
            return Err(ArenaError::NodeIdNotFound);
        }
        if !arena.contains(parent) {
            return Err(ArenaError::NodeIdNotFound);
        }
        if nth == 0 && arena[parent].first_child == None {
            // Make self the *first* child of parent.
            return self.make_child_of(parent, arena);
        }
        let children = parent.children(arena).collect::<Vec<NodeId<U>>>();
        let n_children = children.len() as u16;
        if nth == children.len() as u16 {
            // Make self the *last* child of parent.
            return self.make_child_of(parent, arena);
        } else if n_children < nth {
            return Err(ArenaError::NodeHasTooFewChildren(n_children));
        }
        self.detach(arena)?; // Leave current children attached.
        arena[*self].parent = Some(parent);
        if nth == 0 {
            // first child cannot be None here.
            arena[parent].first_child = Some(*self);
        }
        if nth > 0 {
            let new_prev_sibling = children[nth as usize - 1];
            arena[*self].previous_sibling = Some(new_prev_sibling);
            arena[new_prev_sibling].next_sibling = Some(*self);
        }
        if nth < n_children {
            let new_next_sibling = children[nth as usize];
            arena[*self].next_sibling = Some(new_next_sibling);
            arena[new_next_sibling].previous_sibling = Some(*self);
        }
        if nth == n_children {
            arena[parent].last_child = Some(*self);
        }
        Ok(())
    }

    /// If `self` is the *nth* child of its parent, return *n*.
    pub fn get_child_position<T: Clone>(self, arena: &Arena<T, U>) -> Option<usize> {
        if arena[self].parent.is_none() {
            return None;
        }
        arena[self]
            .parent
            .unwrap()
            .children(arena)
            .position(|val| val == self)
    }

    /// Return an iterator of references to this node’s children.
    pub fn children<T: Clone>(self, arena: &Arena<T, U>) -> Children<T, U> {
        Children {
            arena: arena,
            node: arena[self].first_child,
        }
    }

    /// Return an iterator of references to this node’s children, in reverse order.
    pub fn reverse_children<T: Clone>(self, arena: &Arena<T, U>) -> ReverseChildren<T, U> {
        ReverseChildren {
            arena: arena,
            node: arena[self].last_child,
        }
    }

    /// Return a breadth-first iterator of references to this node's descendants.
    pub fn breadth_first_traversal<T: Clone>(self,
                                             arena: &Arena<T, U>)
                                             -> BreadthFirstTraversal<T, U> {
        let mut queue = VecDeque::new();
        queue.push_back(self);
        BreadthFirstTraversal {
            arena: arena,
            queue: queue,
        }
    }

    /// Return a post-order iterator of references to this node's descendants.
    pub fn post_order_traversal<T: Clone>(self, arena: &Arena<T, U>) -> PostOrderTraversal<T, U> {
        let mut stack1 = VecDeque::new();
        stack1.push_front(self);
        PostOrderTraversal {
            arena: arena,
            stack1: stack1,
            stack2: VecDeque::new(),
        }
    }

    /// Return a pre-order iterator of references to this node's descendants.
    pub fn pre_order_traversal<T: Clone>(self, arena: &Arena<T, U>) -> PreOrderTraversal<T, U> {
        let mut stack = VecDeque::new();
        stack.push_front(self);
        PreOrderTraversal {
            arena: arena,
            stack: stack,
        }
    }

    /// Get the descendants of this node, without including the node itself.
    pub fn descendants<T: Clone>(self, arena: &Arena<T, U>) -> PreOrderTraversal<T, U> {
        let mut stack = VecDeque::new();
        stack.push_front(self);
        let mut iterator = PreOrderTraversal {
            arena: arena,
            stack: stack,
        };
        iterator.next(); // Consume self.
        iterator
    }
}

macro_rules! impl_node_iterator {
    ($name: ident, $next: expr) => {
        impl<'a, T: Clone, U: PartialEq + Copy> Iterator for $name<'a, T, U> {
            type Item = NodeId<U>;
            fn next(&mut self) -> Option<NodeId<U>> {
                match self.node.take() {
                    Some(node) => {
                        self.node = $next(&self.arena[node]);
                        Some(node)
                    }
                    None => None
                }
            }
        }
    }
}

/// An iterator of references to the children of a given node.
pub struct Children<'a, T: Clone + 'a, U: PartialEq + Copy + 'a> {
    arena: &'a Arena<T, U>,
    node: Option<NodeId<U>>,
}
impl_node_iterator!(Children, |node: &Node<T, U>| node.next_sibling);

/// An iterator of references to the children of a given node.
pub struct ReverseChildren<'a, T: Clone + 'a, U: PartialEq + Copy + 'a> {
    arena: &'a Arena<T, U>,
    node: Option<NodeId<U>>,
}
impl_node_iterator!(ReverseChildren, |node: &Node<T, U>| node.previous_sibling);

/// A breadth-first iterator of references to the descendants of a given node.
pub struct BreadthFirstTraversal<'a, T: Clone + 'a, U: PartialEq + Copy + 'a> {
    arena: &'a Arena<T, U>,
    queue: VecDeque<NodeId<U>>,
}

impl<'a, T: Clone, U: PartialEq + Copy> Iterator for BreadthFirstTraversal<'a, T, U> {
    type Item = NodeId<U>;
    fn next(&mut self) -> Option<NodeId<U>> {
        match self.queue.pop_front() {
            Some(node) => {
                for child in node.children(self.arena) {
                    self.queue.push_back(child);
                }
                Some(node)
            }
            None => None,
        }
    }
}

/// A post-order iterator of references to the descendants of a given node.
pub struct PostOrderTraversal<'a, T: Clone + 'a, U: PartialEq + Copy + 'a> {
    arena: &'a Arena<T, U>,
    stack1: VecDeque<NodeId<U>>,
    stack2: VecDeque<NodeId<U>>,
}

impl<'a, T: Clone, U: PartialEq + Copy> Iterator for PostOrderTraversal<'a, T, U> {
    type Item = NodeId<U>;
    fn next(&mut self) -> Option<NodeId<U>> {
        let mut current: NodeId<U>;
        while !self.stack1.is_empty() {
            current = self.stack1.pop_front().unwrap();
            self.stack2.push_front(current);
            for child in current.children(self.arena) {
                self.stack1.push_front(child);
            }
        }
        self.stack2.pop_front()
    }
}

/// A pre-order iterator of references to the descendants of a given node.
pub struct PreOrderTraversal<'a, T: Clone + 'a, U: PartialEq + Copy + 'a> {
    arena: &'a Arena<T, U>,
    stack: VecDeque<NodeId<U>>,
}

impl<'a, T: Clone, U: PartialEq + Copy> Iterator for PreOrderTraversal<'a, T, U> {
    type Item = NodeId<U>;
    fn next(&mut self) -> Option<NodeId<U>> {
        if self.stack.is_empty() {
            return None;
        }
        let current = self.stack.pop_front().unwrap();
        for child in current.reverse_children(self.arena) {
            self.stack.push_front(child);
        }
        Some(current)
    }
}

// Turn a grammar, parser and input string into an AST arena.
fn parse_into_ast<T: PartialEq + Copy>(pt: &parser::Node<u16>,
                                       lexer: &Lexer<u16>,
                                       grm: &YaccGrammar,
                                       input: &str)
                                       -> Arena<String, T> {
    let mut arena = Arena::new();
    let mut st = vec![pt]; // Stack of nodes.
    // Stack of `Option<NodeId>`s which are parents of nodes on the `st` stack.
    // The stack should never be empty, a `None` should be at the bottom of the stack.
    let mut parent = vec![None];
    let mut child_node: NodeId<T>;
    while !st.is_empty() {
        let e = st.pop().unwrap();
        match *e {
            parser::Node::Term { lexeme } => {
                let token_id: usize = lexeme.tok_id().try_into().ok().unwrap();
                let term_name = grm.term_name(TIdx::from(token_id)).unwrap();
                let lexeme_string = &input[lexeme.start()..lexeme.start() + lexeme.len()];
                let (line_no, col_no) = lexer.line_and_col(&lexeme).unwrap();
                child_node = arena.new_node(term_name.to_string(),
                                            lexeme_string.to_string(),
                                            Some(col_no),
                                            Some(line_no),
                                            Some(lexeme.start()),
                                            Some(lexeme.len()));
                match parent.pop().unwrap() {
                    None => parent.push(None),
                    Some(id) => {
                        child_node.make_child_of(id, &mut arena).ok();
                    }
                };
            }
            parser::Node::Nonterm {
                nonterm_idx,
                ref nodes,
            } => {
                // A non-terminal has no label of its own, but has a node type.
                child_node = arena.new_node(grm.nonterm_name(nonterm_idx).to_string(),
                                            "".to_string(),
                                            None,
                                            None,
                                            None,
                                            None);
                match parent.pop().unwrap() {
                    None => parent.push(None),
                    Some(id) => {
                        child_node.make_child_of(id, &mut arena).ok();
                    }
                };
                // Push children of current non-terminal onto stacks.
                for x in nodes.iter().rev() {
                    st.push(x);
                    parent.push(Some(child_node));
                }
            }
        }
    }
    arena
}

// Read file and return its contents or `ParseError`.
fn read_file(path: &Path) -> Result<String, ParseError> {
    if !Path::new(path).exists() {
        return Err(ParseError::FileNotFound(path.to_str().unwrap().into()));
    }
    let mut f = File::open(path).map_err(|e| ParseError::Io(e.to_string()))?;
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    Ok(s)
}

/// Parse an individual input file, and return an `Arena` or `ParseError`.
pub fn parse_file<T: PartialEq + Copy>(input_path: &str,
                                       lex_path: &Path,
                                       yacc_path: &Path)
                                       -> Result<Arena<String, T>, ParseError> {
    // Determine lexer and yacc files by extension. For example if the input
    // file is named Foo.java, the lexer should be grammars/java.l.
    // TODO: create a HashMap of file extensions -> lex/yacc files.
    // Get input files.
    let lexs = read_file(lex_path)?;
    let grms = read_file(yacc_path)?;
    let input = read_file(Path::new(input_path))?;

    let mut lexerdef = build_lex::<u16>(&lexs)
        .map_err(|_| ParseError::BrokenLexer)?;
    let grm = yacc_grm(YaccKind::Eco, &grms)
        .map_err(|_| ParseError::BrokenParser)?;
    let (sgraph, stable) = from_yacc(&grm, Minimiser::Pager)
        .map_err(|_| ParseError::BrokenParser)?;

    // Sync up the IDs of terminals in the lexer and parser.
    let rule_ids = grm.terms_map()
                      .iter()
                      .map(|(&n, &i)| (n, u16::try_from(usize::from(i)).unwrap()))
                      .collect();
    lexerdef.set_rule_ids(&rule_ids);

    // Lex input file.
    let lexer = lexerdef.lexer(&input);
    let lexemes = lexer.lexemes().map_err(|_| ParseError::LexicalError)?;

    // Return parse tree.
    let pt = parser::parse::<u16>(&grm, &sgraph, &stable, &lexemes)
        .map_err(|_| ParseError::SyntaxError)?;
    Ok(parse_into_ast::<T>(&pt, &lexer, &grm, &input))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_trait_node_ids() {
        let from = NodeId::<FromNodeId>::new(100);
        let to = NodeId::<ToNodeId>::new(200);
        assert_eq!(NodeId::<FromNodeId>::from(to).index, to.index);
        assert_eq!(NodeId::<ToNodeId>::from(from).index, from.index);
    }

    #[test]
    fn from_trait_nodes() {
        let mut arena_from = Arena::<&str, FromNodeId>::new();
        let _root_from =
            arena_from.new_node("CLASS", String::from("class"), None, None, None, None);
        let nodeid_from =
            arena_from.new_node("MODIFIER", String::from("private"), None, None, None, None);
        let mut arena_to = Arena::<&str, ToNodeId>::new();
        let _root_to = arena_to.new_node("EXPR", String::from(""), None, None, None, None);
        let nodeid_to = arena_to.new_node("MULT", String::from("*"), None, None, None, None);
        let coerced_from = Node::<&str, FromNodeId>::from(arena_to[nodeid_to].clone());
        assert_eq!(arena_from[nodeid_from].index, coerced_from.index);
        assert_eq!(arena_from[nodeid_from].parent(), coerced_from.parent());
        assert_eq!(arena_from[nodeid_from].first_child(),
                   coerced_from.first_child());
        assert_eq!(arena_from[nodeid_from].last_child(),
                   coerced_from.last_child());
        assert_eq!(arena_from[nodeid_from].previous_sibling(),
                   coerced_from.previous_sibling());
        assert_eq!(arena_from[nodeid_from].next_sibling(),
                   coerced_from.next_sibling());
        let coerced_to = Node::<&str, ToNodeId>::from(arena_from[nodeid_from].clone());
        assert_eq!(arena_to[nodeid_to].index, coerced_to.index);
        assert_eq!(arena_to[nodeid_to].parent(), coerced_to.parent());
        assert_eq!(arena_to[nodeid_to].first_child(), coerced_to.first_child());
        assert_eq!(arena_to[nodeid_to].last_child(), coerced_to.last_child());
        assert_eq!(arena_to[nodeid_to].previous_sibling(),
                   coerced_to.previous_sibling());
        assert_eq!(arena_to[nodeid_to].next_sibling(),
                   coerced_to.next_sibling());
    }

    fn create_arena() -> Arena<&'static str, FromNodeId> {
        let mut arena = Arena::<&str, FromNodeId>::new();
        let root = arena.new_node("Expr",
                                  String::from("+"),
                                  None,
                                  None,
                                  None,
                                  None);
        let n1 = arena.new_node("INT",
                                String::from("1"),
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("Expr",
                                String::from("*"),
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node("INT",
                                String::from("3"),
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node("INT",
                                String::from("4"),
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(n2, &mut arena).unwrap();
        arena
    }

    #[test]
    fn from_trait_arenas() {
        let from_arena = create_arena();
        let coerced_to_arena = Arena::<&str, ToNodeId>::from(from_arena.clone());
        let coerced_from_arena = Arena::<&str, FromNodeId>::from(coerced_to_arena.clone());
        let format = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format, format!("{:?}", from_arena));
        assert_eq!(format, format!("{:?}", coerced_to_arena));
        assert_eq!(format, format!("{:?}", coerced_from_arena));
        assert!(from_arena.root().is_some());
        assert_eq!(from_arena.root(),
                   coerced_to_arena.root()
                                   .map(|id| NodeId::<FromNodeId>::from(id)));
        assert_eq!(from_arena.root(), coerced_from_arena.root());
    }

    #[test]
    fn new_ast_node() {
        let arena = &mut Arena::<&str, FromNodeId>::new();
        assert!(arena.is_empty());
        let n0 = arena.new_node("INT", String::from("100"), None, None, None, None);
        assert!(arena[n0].index != None);
        assert_eq!(n0, arena[n0].index.unwrap());
        assert!(!arena.is_empty());
        let n1 = arena.new_node("STR", String::from("foobar"), None, None, None, None);
        assert!(arena[n1].index != None);
        assert_eq!(n1, arena[n1].index.unwrap());
        assert!(!arena.is_empty());
        // Check node ids, data, types and indent levels.
        assert_eq!(0, n0.index);
        assert_eq!(String::from("100"), arena[n0].label);
        assert_eq!("INT", arena[n0].ty);
        assert_eq!(1, n1.index);
        assert_eq!(String::from("foobar"), arena[n1].label);
        assert_eq!("STR", arena[n1].ty);
    }

    #[test]
    fn make_child_of_1() {
        let arena = create_arena();
        let root = NodeId::new(0);
        let n1 = NodeId::new(1);
        let n2 = NodeId::new(2);
        let n3 = NodeId::new(3);
        let n4 = NodeId::new(4);
        // Check indices.
        assert_eq!(root, arena[root].index.unwrap());
        assert_eq!(n1, arena[n1].index.unwrap());
        assert_eq!(n2, arena[n2].index.unwrap());
        assert_eq!(n3, arena[n3].index.unwrap());
        assert_eq!(n4, arena[n4].index.unwrap());
        // Check roots and leaves.
        assert!(root.is_root(&arena));
        assert!(!n1.is_root(&arena));
        assert!(!n2.is_root(&arena));
        assert!(!n3.is_root(&arena));
        assert!(!n4.is_root(&arena));
        assert!(!root.is_leaf(&arena));
        assert!(n1.is_leaf(&arena));
        assert!(!n2.is_leaf(&arena));
        assert!(n3.is_leaf(&arena));
        assert!(n4.is_leaf(&arena));
    }

    #[test]
    fn make_child_of_2() {
        let mut arena = Arena::<&str, FromNodeId>::new();
        assert!(arena.is_empty());
        let root = arena.new_node("Expr", String::from("+"), None, None, None, None);
        assert!(!arena.is_empty());
        let n1 = arena.new_node("INT", String::from("1"), None, None, None, None);
        assert!(!arena.is_empty());
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("INT", String::from("2"), None, None, None, None);
        assert!(!arena.is_empty());
        n2.make_child_of(root, &mut arena).unwrap();
        // Check indices.
        assert_eq!(root, arena[root].index.unwrap());
        assert_eq!(n1, arena[n1].index.unwrap());
        assert_eq!(n2, arena[n2].index.unwrap());
        // Check parents correctly set.
        assert_eq!(None, arena[root].parent);
        assert_eq!(root, arena[n1].parent.unwrap());
        assert_eq!(root, arena[n2].parent.unwrap());
        // Check children correctly set.
        assert_eq!(n1, arena[root].first_child.unwrap());
        assert_eq!(n2, arena[root].last_child.unwrap());
        assert_eq!(None, arena[n1].first_child);
        assert_eq!(None, arena[n1].last_child);
        assert_eq!(None, arena[n2].first_child);
        assert_eq!(None, arena[n2].last_child);
        // Check siblings correctly set.
        assert_eq!(n1, arena[n2].previous_sibling.unwrap());
        assert_eq!(None, arena[n2].next_sibling);
        assert_eq!(None, arena[n1].previous_sibling);
        assert_eq!(n2, arena[n1].next_sibling.unwrap());
        assert_eq!(None, arena[root].previous_sibling);
        assert_eq!(None, arena[root].next_sibling);
    }

    #[test]
    fn make_nth_child_of_1() {
        let mut arena = Arena::<&str, FromNodeId>::new();
        let root = arena.new_node("Expr", String::from("+"), None, None, None, None);
        assert!(!arena.is_empty());
        let n1 = arena.new_node("INT", String::from("1"), None, None, None, None);
        assert!(!arena.is_empty());
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("INT", String::from("2"), None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"INT\" 2
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut n3 = arena.new_node("INT", String::from("100"), None, None, None, None);
        // Make first child.
        n3.make_nth_child_of(n2, 0, &mut arena).unwrap();
        let format2 = "\"Expr\" +
  \"INT\" 1
  \"INT\" 2
    \"INT\" 100
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn make_nth_child_of_2() {
        let mut arena = Arena::<&str, FromNodeId>::new();
        let root = arena.new_node("Expr", String::from("+"), None, None, None, None);
        assert!(!arena.is_empty());
        let n1 = arena.new_node("INT", String::from("1"), None, None, None, None);
        assert!(!arena.is_empty());
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("INT", String::from("2"), None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"INT\" 2
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut n3 = arena.new_node("INT", String::from("100"), None, None, None, None);
        // Make last child.
        n3.make_nth_child_of(root, 2, &mut arena).unwrap();
        let format2 = "\"Expr\" +
  \"INT\" 1
  \"INT\" 2
  \"INT\" 100
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn make_nth_child_of_3() {
        let mut arena = Arena::<&str, FromNodeId>::new();
        let root = arena.new_node("Expr", String::from("+"), None, None, None, None);
        assert!(!arena.is_empty());
        let n1 = arena.new_node("INT", String::from("1"), None, None, None, None);
        assert!(!arena.is_empty());
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("INT", String::from("2"), None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"INT\" 2
";
        assert_eq!(format1, format!("{:?}", arena));
        let mut n3 = arena.new_node("INT", String::from("100"), None, None, None, None);
        // Make second child.
        n3.make_nth_child_of(root, 1, &mut arena).unwrap();
        let format2 = "\"Expr\" +
  \"INT\" 1
  \"INT\" 100
  \"INT\" 2
";
        assert_eq!(format2, format!("{:?}", arena));
    }

    #[test]
    fn node_fmt() {
        let n1 = Node::<&str, NodeId<FromNodeId>>::new("MODIFIER",
                                                       String::from("private"),
                                                       None,
                                                       None,
                                                       None,
                                                       None);
        let expected1 = "\"MODIFIER\" private";
        assert_eq!(expected1, format!("{:?}", n1));
        let n2 =
            Node::<&str, NodeId<FromNodeId>>::new("Expr", String::from(""), None, None, None, None);
        let expected2 = "\"Expr\"";
        assert_eq!(expected2, format!("{:?}", n2));
    }

    #[test]
    fn arena_fmt_debug() {
        let mut arena = Arena::<&str, NodeId<FromNodeId>>::new();
        let root = arena.new_node("Expr", String::from("+"), None, None, None, None);
        let n1 = arena.new_node("INT", String::from("1"), None, None, None, None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("INT", String::from("2"), None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let expected = "\"Expr\" +
  \"INT\" 1
  \"INT\" 2
";
        assert_eq!(expected, format!("{:?}", arena));
    }

    #[test]
    fn get_edges_1() {
        let arena: Arena<&str, FromNodeId> = Arena::new();
        assert!(arena.is_empty());
        let edges: Vec<EdgeId<FromNodeId>> = vec![];
        assert_eq!(edges, arena.get_edges());
    }

    #[test]
    fn get_edges_2() {
        let arena = &mut Arena::<&str, FromNodeId>::new();
        arena.new_node("INT", String::from("1"), None, None, None, None);
        let edges: Vec<EdgeId<FromNodeId>> = vec![];
        assert_eq!(edges, arena.get_edges());
    }

    #[test]
    fn get_edges_3() {
        let arena = create_arena();
        let edges: Vec<EdgeId<FromNodeId>> = vec![(NodeId::new(0), NodeId::new(1)),
                                                  (NodeId::new(0), NodeId::new(2)),
                                                  (NodeId::new(2), NodeId::new(3)),
                                                  (NodeId::new(2), NodeId::new(4))];
        assert_eq!(edges, arena.get_edges());
    }


    #[test]
    fn detach_1() {
        let mut arena = create_arena();
        let root = NodeId::new(0);
        let first_format = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(first_format, format!("{:?}", arena));
        root.detach_with_children(&mut arena).unwrap();
        let second_format = "\"Expr\" +\n";
        assert_eq!(second_format, format!("{:?}", arena));
    }

    #[test]
    fn detach_2() {
        let mut arena = create_arena();
        let n2 = NodeId::new(2);
        let first_format = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(first_format, format!("{:?}", arena));
        n2.detach_with_children(&mut arena).unwrap();
        let second_format = "\"Expr\" +
  \"INT\" 1
";
        assert_eq!(second_format, format!("{:?}", arena));
    }

    #[test]
    fn children_iterator() {
        let arena = create_arena();
        let root = NodeId::new(0);
        let n2 = NodeId::new(2);
        // Children of root.
        let expected1: Vec<NodeId<FromNodeId>> = vec![NodeId {
                                                          index: 1,
                                                          phantom: PhantomData,
                                                      },
                                                      NodeId {
                                                          index: 2,
                                                          phantom: PhantomData,
                                                      }];
        let root_child_ids = root.children(&arena).collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected1.len(), root_child_ids.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], root_child_ids[index]);
        }
        // Children of n2.
        let expected2: Vec<NodeId<FromNodeId>> = vec![NodeId {
                                                          index: 3,
                                                          phantom: PhantomData,
                                                      },
                                                      NodeId {
                                                          index: 4,
                                                          phantom: PhantomData,
                                                      }];
        let n2_child_ids = n2.children(&arena).collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected2.len(), n2_child_ids.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], n2_child_ids[index]);
        }
    }

    #[test]
    fn reverse_children_iterator() {
        let arena = create_arena();
        let root = NodeId::new(0);
        let n2 = NodeId::new(2);
        // Children of root.
        let expected1: Vec<NodeId<FromNodeId>> = vec![NodeId {
                                                          index: 2,
                                                          phantom: PhantomData,
                                                      },
                                                      NodeId {
                                                          index: 1,
                                                          phantom: PhantomData,
                                                      }];
        let root_child_ids = root.reverse_children(&arena)
                                 .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected1.len(), root_child_ids.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], root_child_ids[index]);
        }
        // Children of n2.
        let expected2: Vec<NodeId<FromNodeId>> = vec![NodeId {
                                                          index: 4,
                                                          phantom: PhantomData,
                                                      },
                                                      NodeId {
                                                          index: 3,
                                                          phantom: PhantomData,
                                                      }];
        let n2_child_ids = n2.reverse_children(&arena)
                             .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected2.len(), n2_child_ids.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], n2_child_ids[index]);
        }
    }

    #[test]
    fn breadth_first_traversal() {
        let arena = create_arena();
        // Descendants  of root.
        let expected1: Vec<NodeId<FromNodeId>> = vec![NodeId::new(0),
                                                      NodeId::new(1),
                                                      NodeId::new(2),
                                                      NodeId::new(3),
                                                      NodeId::new(4)];
        let descendants1 = NodeId::new(0)
            .breadth_first_traversal(&arena)
            .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected1.len(), descendants1.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], descendants1[index]);
        }
        // Descendants of n2.
        let expected2: Vec<NodeId<FromNodeId>> =
            vec![NodeId::new(2), NodeId::new(3), NodeId::new(4)];
        let descendants2 = NodeId::new(2)
            .breadth_first_traversal(&arena)
            .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected2.len(), descendants2.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], descendants2[index]);
        }
    }

    #[test]
    fn post_order_traversal() {
        let arena = create_arena();
        // Descendants  of root.
        let expected1: Vec<NodeId<FromNodeId>> = vec![NodeId::new(1),
                                                      NodeId::new(3),
                                                      NodeId::new(4),
                                                      NodeId::new(2),
                                                      NodeId::new(0)];
        let descendants1 = NodeId::new(0)
            .post_order_traversal(&arena)
            .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected1.len(), descendants1.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], descendants1[index]);
        }
        // Descendants of n2.
        let expected2: Vec<NodeId<FromNodeId>> =
            vec![NodeId::new(3), NodeId::new(4), NodeId::new(2)];
        let descendants2 = NodeId::new(2)
            .post_order_traversal(&arena)
            .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected2.len(), descendants2.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], descendants2[index]);
        }
    }

    #[test]
    fn pre_order_traversal() {
        let arena = create_arena();
        // Descendants  of root.
        let expected1: Vec<NodeId<FromNodeId>> = vec![NodeId::new(0),
                                                      NodeId::new(1),
                                                      NodeId::new(2),
                                                      NodeId::new(3),
                                                      NodeId::new(4)];
        let descendants1 = NodeId::new(0)
            .pre_order_traversal(&arena)
            .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected1.len(), descendants1.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], descendants1[index]);
        }
        // Descendants of n2.
        let expected2: Vec<NodeId<FromNodeId>> =
            vec![NodeId::new(2), NodeId::new(3), NodeId::new(4)];
        let descendants2 = NodeId::new(2)
            .pre_order_traversal(&arena)
            .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected2.len(), descendants2.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], descendants2[index]);
        }
    }

    #[test]
    fn descendants() {
        let arena = create_arena();
        // Descendants  of root.
        let expected1: Vec<NodeId<FromNodeId>> = vec![NodeId::new(1),
                                                      NodeId::new(2),
                                                      NodeId::new(3),
                                                      NodeId::new(4)];
        let descendants1 = NodeId::new(0)
            .descendants(&arena)
            .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected1.len(), descendants1.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], descendants1[index]);
        }
        // Descendants of n2.
        let expected2: Vec<NodeId<FromNodeId>> = vec![NodeId::new(3), NodeId::new(4)];
        let descendants2 = NodeId::new(2)
            .descendants(&arena)
            .collect::<Vec<NodeId<FromNodeId>>>();
        assert_eq!(expected2.len(), descendants2.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], descendants2[index]);
        }
    }

    #[test]
    fn contains() {
        let arena = &mut Arena::<&str, FromNodeId>::new();
        let n1 = arena.new_node("1", String::from("INT"), None, None, None, None);
        assert!(arena.contains(n1));
        assert!(!arena.contains(NodeId {
                                    index: 1,
                                    phantom: PhantomData,
                                }));
    }

    #[test]
    fn size() {
        let arena = &mut Arena::<&str, FromNodeId>::new();
        assert_eq!(0, arena.size());
        let _ = arena.new_node("+", String::from("Expr"), None, None, None, None);
        assert_eq!(1, arena.size());
        let _ = arena.new_node("1", String::from("INT"), None, None, None, None);
        assert_eq!(2, arena.size());
        let _ = arena.new_node("*", String::from("Expr"), None, None, None, None);
        assert_eq!(3, arena.size());
        let _ = arena.new_node("3", String::from("INT"), None, None, None, None);
        assert_eq!(4, arena.size());
        let _ = arena.new_node("4", String::from("INT"), None, None, None, None);
        assert_eq!(5, arena.size());
    }

    #[test]
    fn height() {
        let arena = create_arena();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        assert_eq!(3, NodeId::new(0).height(&arena));
        assert_eq!(1, NodeId::new(1).height(&arena));
        assert_eq!(2, NodeId::new(2).height(&arena));
        assert_eq!(1, NodeId::new(3).height(&arena));
        assert_eq!(1, NodeId::new(4).height(&arena));
    }

    #[test]
    fn get_child_position() {
        let arena = create_arena();
        assert_eq!(None, NodeId::new(0).get_child_position(&arena));
        assert_eq!(Some(0), NodeId::new(1).get_child_position(&arena));
        assert_eq!(Some(1), NodeId::new(2).get_child_position(&arena));
        assert_eq!(Some(0), NodeId::new(3).get_child_position(&arena));
        assert_eq!(Some(1), NodeId::new(4).get_child_position(&arena));
    }
}
