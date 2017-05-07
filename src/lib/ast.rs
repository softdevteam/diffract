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
// (b) any piece of software and/or hardware listed in the lrgrwrks.txt file //
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

use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::ops::{Index, IndexMut};
use std::path::Path;

use dot;
use lrlex::{build_lex, Lexeme};
use lrpar::parser;
use lrtable::{Grammar, Minimiser, TIdx, yacc_to_statetable};

#[derive(Debug)]
/// Errors raised when parsing a source file.
pub enum ParseError {
    /// Io error returned from standard library routine.
    Io(io::Error),
    /// File not found.
    FileNotFound,
    /// File name had no extension (used to determine which lexer/parser to use).
    NoFileExtension,
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
/// Errors raised by arenas..
pub enum ArenaError {
    /// Arena is unexpectedly empty.
    EmtpyArena,
    /// Node ID could not be found in this arena.
    NodeIdNotFound,
}

/// A node identifier for nodes within a particular `Arena`.
type NodeId = usize;

/// A type to represent edges between nodes within an `Arena`.
type EdgeId = (usize, usize);

/// An arena in which to place `NodeID`-indexed AST nodes.
///
/// `T` and `U` are types given to internal values stored within each node.
/// These values contain information that the `lrpar` crate will provide, and
/// represent the type (e.g. `expr`, `factor`, `term`) of each node and any
/// information (e.g. literals) stored within the node. In most uses, `T` and
/// `U` will both be `String` types.
pub struct Arena<T, U> {
    nodes: Vec<Node<T, U>>,
}

impl<T, U> Index<NodeId> for Arena<T, U> {
    type Output = Node<T, U>;

    fn index(&self, node: NodeId) -> &Node<T, U> {
        &self.nodes[node]
    }
}

impl<T, U> IndexMut<NodeId> for Arena<T, U> {
    fn index_mut(&mut self, node: NodeId) -> &mut Node<T, U> {
        &mut self.nodes[node]
    }
}

impl<T, U> Arena<T, U> {
    /// Create an empty `Arena`.
    pub fn new() -> Arena<T, U> {
        Arena { nodes: Vec::new() }
    }

    /// Create a new node from its data.
    pub fn new_node(&mut self, data: T, ty: U, indent: u32) -> NodeId {
        let next_index = self.nodes.len();
        let node = Node::new(data, ty, indent);
        self.nodes.push(node);
        next_index
    }

    /// Return `true` if the arena is empty, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Make one node the child of another.
    pub fn make_child_of(&mut self, child: NodeId, parent: NodeId) -> Result<(), ArenaError> {
        if child >= self.nodes.len() {
            return Err(ArenaError::NodeIdNotFound);
        }
        if parent >= self.nodes.len() {
            return Err(ArenaError::NodeIdNotFound);
        }
        self[child].parent = Some(parent);
        match self[parent].first_child {
            None => self[parent].first_child = Some(child),
            Some(_) => (),
        };
        match self[parent].last_child {
            None => self[parent].last_child = Some(child),
            Some(id) => {
                self[parent].last_child = Some(child);
                self[id].next_sibling = Some(child);
                self[child].previous_sibling = Some(id);
            }
        };
        Ok(())
    }

    // Return a vector of edges between nodes in this arena.
    // Each edge pair `(n1, n2)` denotes an edge between nodes `n1` and `n2`,
    // both numbers being `NodeId` indices within this arena.
    fn get_edges(&self) -> Vec<EdgeId> {
        let mut edges: Vec<EdgeId> = vec![];
        for index in 0..self.nodes.len() {
            let node = &self.nodes[index];
            match node.parent {
                None => (),
                Some(par) => edges.push((par, index)),
            }
        }
        edges
    }
}

impl Arena<String, String> {
    /// Create a graphviz representation of this arena and write to buffer.
    pub fn render_dotgraph<W: Write>(&self, buffer: &mut W) -> Result<(), io::Error> {
        dot::render(self, buffer)
    }
}

impl<'a> dot::Labeller<'a, NodeId, EdgeId> for Arena<String, String> {
    fn graph_id(&self) -> dot::Id {
        dot::Id::new("AST").unwrap()
    }

    fn node_id(&self, id: &NodeId) -> dot::Id {
        // Node ids must be unique dot identifiers, be non-empty strings made up
        // of alphanumeric or underscore characters, not beginning with a digit
        // (i.e. the regular expression [a-zA-Z_][a-zA-Z_0-9]*).
        dot::Id::new(format!("N{}", id)).unwrap()
    }

    fn node_label(&self, id: &NodeId) -> dot::LabelText {
        let mut label = String::new();
        label.push_str(self.nodes[*id].ty.as_str());
        label.push_str(&" ");
        label.push_str(&self.nodes[*id].data.as_str());
        dot::LabelText::LabelStr(label.into())
    }
}

impl<'a, T, U> dot::GraphWalk<'a, NodeId, EdgeId> for Arena<T, U> {
    fn nodes(&self) -> dot::Nodes<'a, NodeId> {
        let mut nodeids: Vec<NodeId> = vec![];
        for index in 0..self.nodes.len() {
            nodeids.push(index);
        }
        Cow::Owned(nodeids)
    }

    fn edges(&'a self) -> dot::Edges<'a, EdgeId> {
        Cow::Owned(self.get_edges())
    }

    fn source(&self, e: &EdgeId) -> NodeId {
        let &(s, _) = e;
        s
    }

    fn target(&self, e: &EdgeId) -> NodeId {
        let &(_, t) = e;
        t
    }
}

// Should have same output as lrpar::parser::Node<TokId>::pretty_print().
impl<T, U> fmt::Display for Arena<T, U>
    where T: fmt::Display,
          U: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.nodes.is_empty() {
            return write!(f, "");
        }
        let mut formatted = String::new();
        let mut stack: Vec<NodeId> = vec![0];
        while !stack.is_empty() {
            let id = stack.pop().unwrap();
            let node = &self.nodes[id];
            if node.is_leaf() {
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
                    match self.nodes[current_child].previous_sibling {
                        None => break,
                        Some(id) => current_child = id,
                    };
                }
            }
        }
        write!(f, "{}", formatted)
    }
}

// #[derive(Clone, Debug)]
/// An AST node within an `Arena`.
pub struct Node<T, U> {
    parent: Option<NodeId>,
    previous_sibling: Option<NodeId>,
    next_sibling: Option<NodeId>,
    first_child: Option<NodeId>,
    last_child: Option<NodeId>,
    /// The actual data which will be stored within the tree.
    pub data: T,
    /// Type of this AST node, derived from grammar e.g. `expr`, `factor`, `term`.
    pub ty: U,
    /// Level of indentation, useful for pretty printing, formatting, etc.
    pub indent: u32,
}

impl<T, U> Node<T, U> {
    /// Create a new node, with data, but without a parent or children.
    pub fn new(data: T, ty: U, indent: u32) -> Node<T, U> {
        Node {
            parent: None,
            previous_sibling: None,
            next_sibling: None,
            first_child: None,
            last_child: None,
            data: data,
            ty: ty,
            indent: indent,
        }
    }

    /// `true` if this node has no children.
    pub fn is_leaf(&self) -> bool {
        match self.first_child {
            Some(_) => false,
            None => true,
        }
    }

    /// `true` if this node has no parent.
    pub fn is_root(&self) -> bool {
        match self.parent {
            Some(_) => false,
            None => true,
        }
    }

    /// Return the ID of the parent node, if there is one.
    pub fn parent(&self) -> Option<NodeId> {
        self.parent
    }

    /// Return the ID of the first child of this node, if there is one.
    pub fn first_child(&self) -> Option<NodeId> {
        self.first_child
    }

    /// Return the ID of the last child of this node, if there is one.
    pub fn last_child(&self) -> Option<NodeId> {
        self.last_child
    }

    /// Return the ID of the previous sibling of this node, if there is one.
    pub fn previous_sibling(&self) -> Option<NodeId> {
        self.previous_sibling
    }

    /// Return the ID of the previous sibling of this node, if there is one.
    pub fn next_sibling(&self) -> Option<NodeId> {
        self.next_sibling
    }
}

impl<T, U> fmt::Display for Node<T, U>
    where T: fmt::Display,
          U: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        for _ in 0..self.indent {
            s.push_str(" ");
        }
        write!(f, "{}", s).unwrap(); // Indentation.
        let data_s = format!(" {}", self.data);
        if data_s.len() == 1 {
            return write!(f, "{}", self.ty);
        }
        write!(f, "{}", self.ty).unwrap();
        write!(f, "{}", data_s)
    }
}

// Turn a grammar, parser and input string into an AST arena.
fn parse_into_ast(pt: &parser::Node<u16>, grm: &Grammar, input: &str) -> Arena<String, String> {
    let mut arena = Arena::new();
    let mut st = vec![(0, pt)]; // Stack of (indent level, node) pairs
    // Stack of Option(NodeId)s which are parents of nodes on the `st` stack.
    // The stack should never be empty, a `None` should be at the bottom of the stack.
    let mut parent = vec![None];
    let mut child_node: NodeId;
    while !st.is_empty() {
        let (indent, e) = st.pop().unwrap();
        match e {
            &parser::Node::Terminal { lexeme } => {
                let token_id: usize = lexeme.tok_id().try_into().ok().unwrap();
                let term_name = grm.term_name(TIdx::from(token_id)).unwrap();
                let lexeme_string = &input[lexeme.start()..lexeme.start() + lexeme.len()];
                child_node =
                    arena.new_node(lexeme_string.to_string(), term_name.to_string(), indent);
                match parent.pop().unwrap() {
                    None => parent.push(None),
                    Some(id) => {
                        arena.make_child_of(child_node, id).ok();
                    }
                };
            }
            &parser::Node::Nonterminal {
                 nonterm_idx,
                 ref nodes,
             } => {
                // A non-terminal has no value of its own, but has a node type.
                child_node = arena.new_node("".to_string(),
                                            grm.nonterm_name(nonterm_idx).unwrap().to_string(),
                                            indent);
                match parent.pop().unwrap() {
                    None => parent.push(None),
                    Some(id) => {
                        arena.make_child_of(child_node, id).ok();
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
fn read_file(path: &str) -> Result<String, ParseError> {
    let mut f = match File::open(path) {
        Ok(r) => r,
        Err(e) => return Err(ParseError::Io(e)),
    };
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    Ok(s)
}

/// Parse an individual input file, and return an `Arena` or `ParseError`.
pub fn parse_file(input_path: &str) -> Result<Arena<String, String>, ParseError> {
    // Determine lexer and yacc files by extension. For example if the input
    // file is named Foo.java, the lexer should be grammars/java.l.
    // TODO: create a HashMap of file extensions -> lex/yacc files.
    let extension = match Path::new(&input_path).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => return Err(ParseError::NoFileExtension),
    };
    let lex_l_path = format!("grammars/{}.l", extension);
    let yacc_y_path = format!("grammars/{}.y", extension);
    info!("Using lexer: {} and parser: {} for input: {}",
          &lex_l_path,
          &yacc_y_path,
          input_path);
    if !Path::new(&lex_l_path).exists() || !Path::new(&yacc_y_path).exists() {
        return Err(ParseError::FileNotFound);
    }

    // Get input files.
    let lexs = match read_file(&lex_l_path) {
        Ok(string) => string,
        Err(err) => return Err(err),
    };
    let grms = match read_file(&yacc_y_path) {
        Ok(string) => string,
        Err(err) => return Err(err),
    };
    let input = match read_file(input_path) {
        Ok(string) => string,
        Err(err) => return Err(err),
    };

    // Create lexer.
    let mut lexer = match build_lex::<u16>(&lexs) {
        Ok(ast) => ast,
        Err(_) => return Err(ParseError::BrokenLexer),
    };

    // Create parser.
    let (grm, stable) = match yacc_to_statetable(&grms, Minimiser::Pager) {
        Ok(x) => x,
        Err(_) => return Err(ParseError::BrokenParser),
    };

    // Sync up the IDs of terminals in the lexer and parser.
    let mut rule_ids = HashMap::<&str, u16>::new();
    for term_idx in grm.iter_term_idxs() {
        rule_ids.insert(grm.term_name(term_idx).unwrap(),
                        u16::try_from(usize::from(term_idx)).unwrap());
    }
    lexer.set_rule_ids(&rule_ids);

    // Lex input file.
    let mut lexemes = match lexer.lex(&input) {
        Ok(tokens) => tokens,
        Err(_) => return Err(ParseError::LexicalError),
    };
    lexemes.push(Lexeme::new(u16::try_from(usize::from(grm.end_term)).unwrap(),
                             input.len(),
                             0));

    // Return parse tree.
    let pt = match parser::parse::<u16>(&grm, &stable, &lexemes) {
        Ok(tree) => tree,
        Err(_) => return Err(ParseError::SyntaxError),
    };
    Ok(parse_into_ast(&pt, &grm, &input))
}

#[cfg(test)]
#[test]
fn new_ast_node() {
    let arena = &mut Arena::new();
    assert!(arena.is_empty());
    let n0 = arena.new_node("100", "INT", 8);
    assert!(!arena.is_empty());
    let n1 = arena.new_node("foobar", "STR", 12);
    assert!(!arena.is_empty());
    // Check node ids, data, types and indent levels.
    assert_eq!(0, n0);
    assert_eq!("100", arena[n0].data);
    assert_eq!("INT", arena[n0].ty);
    assert_eq!(8, arena[n0].indent);
    assert_eq!(1, n1);
    assert_eq!("foobar", arena[n1].data);
    assert_eq!("STR", arena[n1].ty);
    assert_eq!(12, arena[n1].indent);
}

#[test]
fn make_child_of_1() {
    let arena = &mut Arena::new();
    assert!(arena.is_empty());
    let root = arena.new_node("+", "Expr", 0);
    let n1 = arena.new_node("1", "INT", 4);
    arena.make_child_of(n1, root).unwrap();
    let n2 = arena.new_node("*", "Expr", 4);
    arena.make_child_of(n2, root).unwrap();
    let n3 = arena.new_node("3", "INT", 8);
    arena.make_child_of(n3, n2).unwrap();
    let n4 = arena.new_node("4", "INT", 8);
    arena.make_child_of(n4, n2).unwrap();
    assert!(!arena.is_empty());
    // Check roots and leaves.
    assert!(arena[root].is_root());
    assert!(!arena[n1].is_root());
    assert!(!arena[n2].is_root());
    assert!(!arena[n3].is_root());
    assert!(!arena[n4].is_root());
    assert!(!arena[root].is_leaf());
    assert!(arena[n1].is_leaf());
    assert!(!arena[n2].is_leaf());
    assert!(arena[n3].is_leaf());
    assert!(arena[n4].is_leaf());
}

#[test]
fn make_child_of_2() {
    let arena = &mut Arena::new();
    assert!(arena.is_empty());
    let root = arena.new_node("+", "Expr", 4);
    assert!(!arena.is_empty());
    let n1 = arena.new_node("1", "INT", 0);
    assert!(!arena.is_empty());
    arena.make_child_of(n1, root).unwrap();
    let n2 = arena.new_node("2", "INT", 0);
    assert!(!arena.is_empty());
    arena.make_child_of(n2, root).unwrap();
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
fn node_fmt() {
    let node = Node::new("private", "MODIFIER", 4);
    let expected = "    MODIFIER private";
    assert_eq!(expected, format!("{:}", node));
}

#[test]
fn arena_fmt() {
    let arena = &mut Arena::new();
    let root = arena.new_node("+", "Expr", 4);
    let n1 = arena.new_node("1", "INT", 0);
    arena.make_child_of(n1, root).unwrap();
    let n2 = arena.new_node("2", "INT", 0);
    arena.make_child_of(n2, root).unwrap();
    let expected = "    Expr +
INT 1
INT 2
";
    assert_eq!(expected, format!("{:}", arena));
}

#[test]
fn get_edges_1() {
    let arena: Arena<String, String> = Arena::new();
    assert!(arena.is_empty());
    let edges: Vec<EdgeId> = vec![];
    assert_eq!(edges, arena.get_edges());
}

#[test]
fn get_edges_2() {
    let arena = &mut Arena::new();
    arena.new_node("1", "INT", 0);
    let edges: Vec<EdgeId> = vec![];
    assert_eq!(edges, arena.get_edges());
}

#[test]
fn get_edges_3() {
    let arena = &mut Arena::new();
    let root = arena.new_node("+", "Expr", 0);
    let n1 = arena.new_node("1", "INT", 4);
    arena.make_child_of(n1, root).unwrap();
    let n2 = arena.new_node("*", "Expr", 4);
    arena.make_child_of(n2, root).unwrap();
    let n3 = arena.new_node("3", "INT", 8);
    arena.make_child_of(n3, n2).unwrap();
    let n4 = arena.new_node("4", "INT", 8);
    arena.make_child_of(n4, n2).unwrap();
    let edges: Vec<EdgeId> = vec![(0, 1), (0, 2), (2, 3), (2, 4)];
    assert_eq!(edges, arena.get_edges());
}
