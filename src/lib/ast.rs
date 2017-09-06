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

use std::collections::{BTreeMap, VecDeque};
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::ops::{Index, IndexMut};
use std::path::Path;

use cfgrammar::TIdx;
use cfgrammar::yacc::{yacc_grm, YaccGrammar, YaccKind};
use lrlex::{build_lex, Lexer};
use lrpar::parser;
use lrtable::{Minimiser, from_yacc};

use hqueue::HeightQueue;

#[derive(Debug)]
/// Errors raised when parsing a source file.
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

#[derive(Debug)]
/// Errors raised by arenas.
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
/// A node identifier used for indexing nodes within a particular `Arena`.
///
/// A `NodeId` should be immutable, in the sense that no action on a node or
/// arena should change a given `NodeId`.
pub struct NodeId {
    index: usize,
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.index)
    }
}

/// A type to represent edges between nodes within an `Arena`.
pub type EdgeId = (NodeId, NodeId);

#[derive(Clone)]
/// An arena in which to place `NodeId`-indexed AST nodes.
///
/// `T` type is given to internal values stored within each node.
/// These values contain information that the `lrpar` crate will provide, and
/// represent any lexical information (e.g. literals) from the original text.
pub struct Arena<T: Clone> {
    nodes: Vec<Node<T>>,
}

impl<T: Clone> Index<NodeId> for Arena<T> {
    type Output = Node<T>;

    fn index(&self, node: NodeId) -> &Node<T> {
        &self.nodes[node.index]
    }
}

impl<T: Clone> IndexMut<NodeId> for Arena<T> {
    fn index_mut(&mut self, node: NodeId) -> &mut Node<T> {
        &mut self.nodes[node.index]
    }
}

impl<T: Clone> Arena<T> {
    /// Create an empty `Arena`.
    pub fn new() -> Arena<T> {
        Arena { nodes: Vec::new() }
    }

    /// Create a new node from its data.
    pub fn new_node(&mut self,
                    value: T,
                    label: String,
                    indent: u32,
                    col_no: Option<usize>,
                    line_no: Option<usize>,
                    char_no: Option<usize>,
                    token_len: Option<usize>)
                    -> NodeId {
        let next_index = self.nodes.len();
        let mut node = Node::new(value, label, indent, col_no, line_no, char_no, token_len);
        node.index = Some(NodeId { index: next_index });
        self.nodes.push(node);
        NodeId { index: next_index }
    }

    /// Return `true` if the arena is empty, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Return the size of this arena (i.e. how many nodes are held within it).
    ///
    /// Note that if some nodes have been detached (or never appended to other
    /// nodes as children), then some nodes may be unreachable.
    pub fn size(&self) -> usize {
        self.nodes.len()
    }

    /// Return `true` if index is in arena, `false` otherwise.
    pub fn contains(&self, index: NodeId) -> bool {
        index.index < self.nodes.len()
    }

    /// Return a queue of `NodeId`s sorted by height.
    pub fn get_priority_queue(&self) -> HeightQueue {
        let mut queue = HeightQueue::new();
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
    pub fn get_edges(&self) -> Vec<EdgeId> {
        let mut edges: Vec<EdgeId> = vec![];
        for index in 0..self.nodes.len() {
            let node = &self.nodes[index];
            match node.parent {
                None => (),
                Some(par) => edges.push((par, NodeId { index })),
            }
        }
        edges
    }

    /// Return a map of `NodeId`s relating to each line of the original input file.
    ///
    /// Note: vectors may not be sorted by column number.
    pub fn get_lines(&self) -> BTreeMap<usize, Vec<NodeId>> {
        let mut terminals = vec![];
        for i in 0..self.nodes.len() {
            let id = NodeId::new(i);
            // Ignore nodes that have been detached.
            if (self[id].parent.is_some() || self[id].first_child.is_some()) &&
               id.is_terminal(self) {
                terminals.push(id);
            }
        }
        let mut lines = BTreeMap::new();
        for leaf in terminals {
            let line = self[leaf].line_no.unwrap();
            if !lines.contains_key(&line) {
                lines.insert(line, vec![]);
            }
            // Note: may not be sorted.
            if let Some(nodes) = lines.get_mut(&line) {
                nodes.push(leaf);
            }
        }
        lines
    }
}

// Should have same output as lrpar::parser::Node<TokId>::pretty_print().
impl<T: fmt::Display + Clone> fmt::Display for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.nodes.is_empty() {
            return write!(f, "");
        }
        let mut formatted = String::new();
        let mut stack: Vec<NodeId> = vec![NodeId { index: 0 }];
        while !stack.is_empty() {
            let id = stack.pop().unwrap();
            let node = &self.nodes[id.index];
            if id.is_leaf(self) {
                // Terminal node.
                formatted.push_str(&format!("{}\n", node));
            } else {
                // Non-terminal node.
                formatted.push_str(&format!("{}\n", node));
                let first_child = node.first_child.unwrap();
                let mut current_child = node.last_child.unwrap();
                loop {
                    stack.push(current_child);
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

#[derive(Clone, Debug, PartialEq, Eq)]
/// An AST node within an `Arena`.
pub struct Node<T: Clone> {
    parent: Option<NodeId>,
    previous_sibling: Option<NodeId>,
    next_sibling: Option<NodeId>,
    first_child: Option<NodeId>,
    last_child: Option<NodeId>,
    /// Index of this node in the arena.
    ///
    /// Note that nodes with a non-`None` index are unique.
    index: Option<NodeId>,
    /// The `value` of a node is the lexeme it corresponds to in the code.
    ///
    /// For example, a numerical or string literal, bracket, semicolon, etc.
    pub value: T,
    /// The `label` of a node is its name of its production in the grammar.
    ///
    /// For example, `Expr`, `Factor`, `Term`.
    pub label: String,
    /// Level of indentation, useful for pretty printing, formatting, etc.
    ///
    /// This node should be indented by `indent` spaces by a pretty-printer.
    pub indent: u32,
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

impl<T: Clone> Node<T> {
    /// Create a new node, with data, but without a parent or children.
    pub fn new(value: T,
               label: String,
               indent: u32,
               col_no: Option<usize>,
               line_no: Option<usize>,
               char_no: Option<usize>,
               token_len: Option<usize>)
               -> Node<T> {
        Node {
            parent: None,
            previous_sibling: None,
            next_sibling: None,
            first_child: None,
            last_child: None,
            value: value,
            label: label,
            indent: indent,
            index: None,
            col_no: col_no,
            line_no: line_no,
            char_no: char_no,
            token_len: token_len,
        }
    }

    /// Return the Id of the parent node, if there is one.
    pub fn parent(&self) -> Option<NodeId> {
        self.parent
    }

    /// Return the Id of the first child of this node, if there is one.
    pub fn first_child(&self) -> Option<NodeId> {
        self.first_child
    }

    /// Return the Id of the last child of this node, if there is one.
    pub fn last_child(&self) -> Option<NodeId> {
        self.last_child
    }

    /// Return the Id of the previous sibling of this node, if there is one.
    pub fn previous_sibling(&self) -> Option<NodeId> {
        self.previous_sibling
    }

    /// Return the Id of the previous sibling of this node, if there is one.
    pub fn next_sibling(&self) -> Option<NodeId> {
        self.next_sibling
    }
}

impl<T: fmt::Display + Clone> fmt::Display for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = " ".repeat(self.indent as usize);
        write!(f, "{}", s).unwrap(); // Indentation.
        let data_s = format!(" {}", self.value);
        if data_s.len() == 1 {
            return write!(f, "{}", self.label);
        }
        write!(f, "{}", self.label).unwrap();
        write!(f, "{}", data_s)
    }
}

impl NodeId {
    /// Create a new NodeId with a given index.
    pub fn new(index: usize) -> NodeId {
        NodeId { index: index }
    }

    /// Return the index of this Node Id.
    pub fn id(&self) -> usize {
        self.index
    }

    /// `true` if this node has no children.
    pub fn is_leaf<T: Clone>(&self, arena: &Arena<T>) -> bool {
        arena[*self].first_child.is_none() && arena[*self].last_child.is_none()
    }

    /// `true` if this node represents a terminal node in the AST.
    pub fn is_terminal<T: Clone>(&self, arena: &Arena<T>) -> bool {
        arena[*self].line_no.is_some() && arena[*self].col_no.is_some()
    }

    /// `true` if this node has no parent.
    ///
    /// If a node has been added to the arena without being given a parent, or
    /// a node has been detached from its parent, this method will return `true`.
    pub fn is_root<T: Clone>(&self, arena: &Arena<T>) -> bool {
        arena[*self].parent.is_none()
    }

    /// Get the height of this node.
    ///
    /// The height of a leaf node is 1, the height of a branch node is 1 +
    /// the height of its tallest child node.
    pub fn height<T: Clone>(&self, arena: &Arena<T>) -> u32 {
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
    pub fn detach<T: Clone>(&self, arena: &mut Arena<T>) -> ArenaResult {
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
    pub fn detach_with_children<T: Clone>(&self, arena: &mut Arena<T>) -> ArenaResult {
        self.detach(arena)?;
        let ids = self.children(arena).collect::<Vec<NodeId>>();
        for id in ids {
            id.detach_with_children(arena)?;
        }
        Ok(())
    }

    /// Make self the next (i.e. last) child of another.
    pub fn make_child_of<T: Clone>(&self, parent: NodeId, arena: &mut Arena<T>) -> ArenaResult {
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
                                       parent: NodeId,
                                       nth: u16,
                                       arena: &mut Arena<T>)
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
        let children = parent.children(arena).collect::<Vec<NodeId>>();
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
    pub fn get_child_position<T: Clone>(self, arena: &Arena<T>) -> Option<usize> {
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
    pub fn children<T: Clone>(self, arena: &Arena<T>) -> Children<T> {
        Children {
            arena: arena,
            node: arena[self].first_child,
        }
    }

    /// Return an iterator of references to this node’s children, in reverse order.
    pub fn reverse_children<T: Clone>(self, arena: &Arena<T>) -> ReverseChildren<T> {
        ReverseChildren {
            arena: arena,
            node: arena[self].last_child,
        }
    }

    /// Return a breadth-first iterator of references to this node's descendants.
    pub fn breadth_first_traversal<T: Clone>(self, arena: &Arena<T>) -> BreadthFirstTraversal<T> {
        let mut queue = VecDeque::new();
        queue.push_back(self);
        BreadthFirstTraversal {
            arena: arena,
            queue: queue,
        }
    }

    /// Return a post-order iterator of references to this node's descendants.
    pub fn post_order_traversal<T: Clone>(self, arena: &Arena<T>) -> PostOrderTraversal<T> {
        let mut stack1 = VecDeque::new();
        stack1.push_front(self);
        PostOrderTraversal {
            arena: arena,
            stack1: stack1,
            stack2: VecDeque::new(),
        }
    }

    /// Return a pre-order iterator of references to this node's descendants.
    pub fn pre_order_traversal<T: Clone>(self, arena: &Arena<T>) -> PreOrderTraversal<T> {
        let mut stack = VecDeque::new();
        stack.push_front(self);
        PreOrderTraversal {
            arena: arena,
            stack: stack,
        }
    }

    /// Get the descendants of this node, without including the node itself.
    pub fn descendants<T: Clone>(self, arena: &Arena<T>) -> PreOrderTraversal<T> {
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
        impl<'a, T: Clone> Iterator for $name<'a, T> {
            type Item = NodeId;
            fn next(&mut self) -> Option<NodeId> {
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
pub struct Children<'a, T: Clone + 'a> {
    arena: &'a Arena<T>,
    node: Option<NodeId>,
}
impl_node_iterator!(Children, |node: &Node<T>| node.next_sibling);

/// An iterator of references to the children of a given node.
pub struct ReverseChildren<'a, T: Clone + 'a> {
    arena: &'a Arena<T>,
    node: Option<NodeId>,
}
impl_node_iterator!(ReverseChildren, |node: &Node<T>| node.previous_sibling);

/// A breadth-first iterator of references to the descendants of a given node.
pub struct BreadthFirstTraversal<'a, T: Clone + 'a> {
    arena: &'a Arena<T>,
    queue: VecDeque<NodeId>,
}

impl<'a, T: Clone> Iterator for BreadthFirstTraversal<'a, T> {
    type Item = NodeId;
    fn next(&mut self) -> Option<NodeId> {
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
pub struct PostOrderTraversal<'a, T: Clone + 'a> {
    arena: &'a Arena<T>,
    stack1: VecDeque<NodeId>,
    stack2: VecDeque<NodeId>,
}

impl<'a, T: Clone> Iterator for PostOrderTraversal<'a, T> {
    type Item = NodeId;
    fn next(&mut self) -> Option<NodeId> {
        let mut current: NodeId;
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
pub struct PreOrderTraversal<'a, T: Clone + 'a> {
    arena: &'a Arena<T>,
    stack: VecDeque<NodeId>,
}

impl<'a, T: Clone> Iterator for PreOrderTraversal<'a, T> {
    type Item = NodeId;
    fn next(&mut self) -> Option<NodeId> {
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
fn parse_into_ast(pt: &parser::Node<u16>,
                  lexer: &Lexer<u16>,
                  grm: &YaccGrammar,
                  input: &str)
                  -> Arena<String> {
    let mut arena = Arena::new();
    let mut st = vec![(0, pt)]; // Stack of (indent level, node) pairs
    // Stack of `Option<NodeId>`s which are parents of nodes on the `st` stack.
    // The stack should never be empty, a `None` should be at the bottom of the stack.
    let mut parent = vec![None];
    let mut child_node: NodeId;
    while !st.is_empty() {
        let (indent, e) = st.pop().unwrap();
        match *e {
            parser::Node::Term { lexeme } => {
                let token_id: usize = lexeme.tok_id().try_into().ok().unwrap();
                let term_name = grm.term_name(TIdx::from(token_id)).unwrap();
                let lexeme_string = &input[lexeme.start()..lexeme.start() + lexeme.len()];
                let (line_no, col_no) = lexer.line_and_col(&lexeme).unwrap();
                child_node = arena.new_node(lexeme_string.to_string(),
                                            term_name.to_string(),
                                            indent,
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
                // A non-terminal has no value of its own, but has a node type.
                child_node = arena.new_node("".to_string(),
                                            grm.nonterm_name(nonterm_idx).unwrap().to_string(),
                                            indent,
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
                    st.push((indent + 1, x));
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
pub fn parse_file(input_path: &str,
                  lex_path: &Path,
                  yacc_path: &Path)
                  -> Result<Arena<String>, ParseError> {
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
    let (_, stable) = from_yacc(&grm, Minimiser::Pager)
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
    let pt = parser::parse::<u16>(&grm, &stable, &lexemes)
        .map_err(|_| ParseError::SyntaxError)?;
    Ok(parse_into_ast(&pt, &lexer, &grm, &input))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_arena() -> Arena<String> {
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
                                4,
                                None,
                                None,
                                None,
                                None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node(String::from("*"),
                                String::from("Expr"),
                                4,
                                None,
                                None,
                                None,
                                None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node(String::from("3"),
                                String::from("INT"),
                                8,
                                None,
                                None,
                                None,
                                None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node(String::from("4"),
                                String::from("INT"),
                                8,
                                None,
                                None,
                                None,
                                None);
        n4.make_child_of(n2, &mut arena).unwrap();
        arena
    }

    #[test]
    fn new_ast_node() {
        let arena = &mut Arena::new();
        assert!(arena.is_empty());
        let n0 = arena.new_node("100", String::from("INT"), 8, None, None, None, None);
        assert!(arena[n0].index != None);
        assert_eq!(n0, arena[n0].index.unwrap());
        assert!(!arena.is_empty());
        let n1 = arena.new_node("foobar", String::from("STR"), 12, None, None, None, None);
        assert!(arena[n1].index != None);
        assert_eq!(n1, arena[n1].index.unwrap());
        assert!(!arena.is_empty());
        // Check node ids, data, types and indent levels.
        assert_eq!(0, n0.index);
        assert_eq!("100", arena[n0].value);
        assert_eq!(String::from("INT"), arena[n0].label);
        assert_eq!(8, arena[n0].indent);
        assert_eq!(1, n1.index);
        assert_eq!("foobar", arena[n1].value);
        assert_eq!(String::from("STR"), arena[n1].label);
        assert_eq!(12, arena[n1].indent);
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
        let mut arena = Arena::new();
        assert!(arena.is_empty());
        let root = arena.new_node("+", String::from("Expr"), 4, None, None, None, None);
        assert!(!arena.is_empty());
        let n1 = arena.new_node("1", String::from("INT"), 0, None, None, None, None);
        assert!(!arena.is_empty());
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("2", String::from("INT"), 0, None, None, None, None);
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
        let mut arena = Arena::new();
        let root = arena.new_node("+", String::from("Expr"), 0, None, None, None, None);
        assert!(!arena.is_empty());
        let n1 = arena.new_node("1", String::from("INT"), 4, None, None, None, None);
        assert!(!arena.is_empty());
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("2", String::from("INT"), 4, None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let format1 = "Expr +
    INT 1
    INT 2
";
        assert_eq!(format1, format!("{:}", arena));
        let mut n3 = arena.new_node("100", String::from("INT"), 8, None, None, None, None);
        // Make first child.
        n3.make_nth_child_of(n2, 0, &mut arena).unwrap();
        let format2 = "Expr +
    INT 1
    INT 2
        INT 100
";
        assert_eq!(format2, format!("{:}", arena));
    }

    #[test]
    fn make_nth_child_of_2() {
        let mut arena = Arena::new();
        let root = arena.new_node("+", String::from("Expr"), 0, None, None, None, None);
        assert!(!arena.is_empty());
        let n1 = arena.new_node("1", String::from("INT"), 4, None, None, None, None);
        assert!(!arena.is_empty());
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("2", String::from("INT"), 4, None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let format1 = "Expr +
    INT 1
    INT 2
";
        assert_eq!(format1, format!("{:}", arena));
        let mut n3 = arena.new_node("100", String::from("INT"), 4, None, None, None, None);
        // Make last child.
        n3.make_nth_child_of(root, 2, &mut arena).unwrap();
        let format2 = "Expr +
    INT 1
    INT 2
    INT 100
";
        assert_eq!(format2, format!("{:}", arena));
    }

    #[test]
    fn make_nth_child_of_3() {
        let mut arena = Arena::new();
        let root = arena.new_node("+", String::from("Expr"), 0, None, None, None, None);
        assert!(!arena.is_empty());
        let n1 = arena.new_node("1", String::from("INT"), 4, None, None, None, None);
        assert!(!arena.is_empty());
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("2", String::from("INT"), 4, None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let format1 = "Expr +
    INT 1
    INT 2
";
        assert_eq!(format1, format!("{:}", arena));
        let mut n3 = arena.new_node("100", String::from("INT"), 4, None, None, None, None);
        // Make second child.
        n3.make_nth_child_of(root, 1, &mut arena).unwrap();
        let format2 = "Expr +
    INT 1
    INT 100
    INT 2
";
        assert_eq!(format2, format!("{:}", arena));
    }

    #[test]
    fn node_fmt() {
        let node = Node::new("private",
                             String::from("MODIFIER"),
                             4,
                             None,
                             None,
                             None,
                             None);
        let expected = "    MODIFIER private";
        assert_eq!(expected, format!("{:}", node));
    }

    #[test]
    fn arena_fmt() {
        let mut arena = Arena::new();
        let root = arena.new_node("+", String::from("Expr"), 4, None, None, None, None);
        let n1 = arena.new_node("1", String::from("INT"), 0, None, None, None, None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("2", String::from("INT"), 0, None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let expected = "    Expr +
INT 1
INT 2
";
        assert_eq!(expected, format!("{:}", arena));
    }

    #[test]
    fn get_edges_1() {
        let arena: Arena<String> = Arena::new();
        assert!(arena.is_empty());
        let edges: Vec<EdgeId> = vec![];
        assert_eq!(edges, arena.get_edges());
    }

    #[test]
    fn get_edges_2() {
        let arena = &mut Arena::new();
        arena.new_node("1", String::from("INT"), 0, None, None, None, None);
        let edges: Vec<EdgeId> = vec![];
        assert_eq!(edges, arena.get_edges());
    }

    #[test]
    fn get_edges_3() {
        let arena = create_arena();
        let edges: Vec<EdgeId> = vec![(NodeId::new(0), NodeId::new(1)),
                                      (NodeId::new(0), NodeId::new(2)),
                                      (NodeId::new(2), NodeId::new(3)),
                                      (NodeId::new(2), NodeId::new(4))];
        assert_eq!(edges, arena.get_edges());
    }


    #[test]
    fn detach_1() {
        let mut arena = create_arena();
        let root = NodeId::new(0);
        let first_format = "Expr +
    INT 1
    Expr *
        INT 3
        INT 4
";
        assert_eq!(first_format, format!("{:}", arena));
        root.detach_with_children(&mut arena).unwrap();
        let second_format = "Expr +\n";
        assert_eq!(second_format, format!("{:}", arena));
    }

    #[test]
    fn detach_2() {
        let mut arena = create_arena();
        let n2 = NodeId::new(2);
        let first_format = "Expr +
    INT 1
    Expr *
        INT 3
        INT 4
";
        assert_eq!(first_format, format!("{:}", arena));
        n2.detach_with_children(&mut arena).unwrap();
        let second_format = "Expr +
    INT 1
";
        assert_eq!(second_format, format!("{:}", arena));
    }

    #[test]
    fn children_iterator() {
        let arena = create_arena();
        let root = NodeId::new(0);
        let n2 = NodeId::new(2);
        // Children of root.
        let expected1: Vec<NodeId> = vec![NodeId { index: 1 }, NodeId { index: 2 }];
        let root_child_ids = root.children(&arena).collect::<Vec<NodeId>>();
        assert_eq!(expected1.len(), root_child_ids.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], root_child_ids[index]);
        }
        // Children of n2.
        let expected2: Vec<NodeId> = vec![NodeId { index: 3 }, NodeId { index: 4 }];
        let n2_child_ids = n2.children(&arena).collect::<Vec<NodeId>>();
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
        let expected1: Vec<NodeId> = vec![NodeId { index: 2 }, NodeId { index: 1 }];
        let root_child_ids = root.reverse_children(&arena).collect::<Vec<NodeId>>();
        assert_eq!(expected1.len(), root_child_ids.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], root_child_ids[index]);
        }
        // Children of n2.
        let expected2: Vec<NodeId> = vec![NodeId { index: 4 }, NodeId { index: 3 }];
        let n2_child_ids = n2.reverse_children(&arena).collect::<Vec<NodeId>>();
        assert_eq!(expected2.len(), n2_child_ids.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], n2_child_ids[index]);
        }
    }

    #[test]
    fn breadth_first_traversal() {
        let arena = create_arena();
        // Descendants  of root.
        let expected1: Vec<NodeId> = vec![NodeId::new(0),
                                          NodeId::new(1),
                                          NodeId::new(2),
                                          NodeId::new(3),
                                          NodeId::new(4)];
        let descendants1 = NodeId::new(0)
            .breadth_first_traversal(&arena)
            .collect::<Vec<NodeId>>();
        assert_eq!(expected1.len(), descendants1.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], descendants1[index]);
        }
        // Descendants of n2.
        let expected2: Vec<NodeId> = vec![NodeId::new(2), NodeId::new(3), NodeId::new(4)];
        let descendants2 = NodeId::new(2)
            .breadth_first_traversal(&arena)
            .collect::<Vec<NodeId>>();
        assert_eq!(expected2.len(), descendants2.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], descendants2[index]);
        }
    }

    #[test]
    fn post_order_traversal() {
        let arena = create_arena();
        // Descendants  of root.
        let expected1: Vec<NodeId> = vec![NodeId::new(1),
                                          NodeId::new(3),
                                          NodeId::new(4),
                                          NodeId::new(2),
                                          NodeId::new(0)];
        let descendants1 = NodeId::new(0)
            .post_order_traversal(&arena)
            .collect::<Vec<NodeId>>();
        assert_eq!(expected1.len(), descendants1.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], descendants1[index]);
        }
        // Descendants of n2.
        let expected2: Vec<NodeId> = vec![NodeId::new(3), NodeId::new(4), NodeId::new(2)];
        let descendants2 = NodeId::new(2)
            .post_order_traversal(&arena)
            .collect::<Vec<NodeId>>();
        assert_eq!(expected2.len(), descendants2.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], descendants2[index]);
        }
    }

    #[test]
    fn pre_order_traversal() {
        let arena = create_arena();
        // Descendants  of root.
        let expected1: Vec<NodeId> = vec![NodeId::new(0),
                                          NodeId::new(1),
                                          NodeId::new(2),
                                          NodeId::new(3),
                                          NodeId::new(4)];
        let descendants1 = NodeId::new(0)
            .pre_order_traversal(&arena)
            .collect::<Vec<NodeId>>();
        assert_eq!(expected1.len(), descendants1.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], descendants1[index]);
        }
        // Descendants of n2.
        let expected2: Vec<NodeId> = vec![NodeId::new(2), NodeId::new(3), NodeId::new(4)];
        let descendants2 = NodeId::new(2)
            .pre_order_traversal(&arena)
            .collect::<Vec<NodeId>>();
        assert_eq!(expected2.len(), descendants2.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], descendants2[index]);
        }
    }

    #[test]
    fn descendants() {
        let arena = create_arena();
        // Descendants  of root.
        let expected1: Vec<NodeId> = vec![NodeId::new(1),
                                          NodeId::new(2),
                                          NodeId::new(3),
                                          NodeId::new(4)];
        let descendants1 = NodeId::new(0).descendants(&arena).collect::<Vec<NodeId>>();
        assert_eq!(expected1.len(), descendants1.len());
        for index in 0..expected1.len() {
            assert_eq!(expected1[index], descendants1[index]);
        }
        // Descendants of n2.
        let expected2: Vec<NodeId> = vec![NodeId::new(3), NodeId::new(4)];
        let descendants2 = NodeId::new(2).descendants(&arena).collect::<Vec<NodeId>>();
        assert_eq!(expected2.len(), descendants2.len());
        for index in 0..expected2.len() {
            assert_eq!(expected2[index], descendants2[index]);
        }
    }

    #[test]
    fn contains() {
        let arena = &mut Arena::new();
        let n1 = arena.new_node("1", String::from("INT"), 4, None, None, None, None);
        assert!(arena.contains(n1));
        assert!(!arena.contains(NodeId { index: 1 }));
    }

    #[test]
    fn size() {
        let arena = &mut Arena::new();
        assert_eq!(0, arena.size());
        let _ = arena.new_node("+", String::from("Expr"), 0, None, None, None, None);
        assert_eq!(1, arena.size());
        let _ = arena.new_node("1", String::from("INT"), 4, None, None, None, None);
        assert_eq!(2, arena.size());
        let _ = arena.new_node("*", String::from("Expr"), 4, None, None, None, None);
        assert_eq!(3, arena.size());
        let _ = arena.new_node("3", String::from("INT"), 8, None, None, None, None);
        assert_eq!(4, arena.size());
        let _ = arena.new_node("4", String::from("INT"), 8, None, None, None, None);
        assert_eq!(5, arena.size());
    }

    #[test]
    fn height() {
        let arena = create_arena();
        let format1 = "Expr +
    INT 1
    Expr *
        INT 3
        INT 4
";
        assert_eq!(format1, format!("{}", arena));
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
