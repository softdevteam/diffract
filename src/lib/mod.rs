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

#![feature(test)]
#![feature(try_from)]

extern crate cfgrammar;
extern crate crypto;
extern crate dot;
#[macro_use]
extern crate log;
extern crate lrlex;
extern crate lrpar;
extern crate lrtable;
extern crate multiset;
extern crate term;
extern crate test;

#[cfg(test)]
extern crate serde;
#[cfg(test)]
#[macro_use]
extern crate serde_derive;
#[cfg(test)]
extern crate serde_xml_rs;

/// Actions are operations that transform abstract syntax trees.
pub mod action;

/// AST defines the abstract syntax tree types that the differ works on.
///
/// Routines are provided to create and iterate over ASTs, and to parse a file
/// into an AST.
pub mod ast;

/// Algorithms to generate edit scripts, based on an existing AST matching.
pub mod edit_script;

/// Emitters generate output for the user in a variety of formats (e.g. JSON, Graphviz).
pub mod emitters;

/// Matchers create mappings between abstract syntax trees.
pub mod matchers;

/// GT matching algorithm.
pub mod gt_matcher;

/// Longest common subsequence matching algorithm.
///
/// Described in Myers (1986).
pub mod myers_matcher;

/// Zhang-Shasha matching algorithm.
///
/// Described in Zhang & Shasha (1989).
pub mod zs_matcher;

/// The null matcher produces no matches and is only used for testing.
pub mod null_matcher;

/// A queue of `NodeId`s sorted on the height of their respective nodes.
pub mod hqueue;

/// Parse strings and files into `ast::Arena` types.
pub mod parser;

/// A patch represents a diff on a single AST node.
///
/// Also deals with turning nearby patches into hunks.
pub mod patch;

/// QGram distance algorithm.
pub mod qgram;

/// Algorithms which act on sequences of values.
pub mod sequence;

/// Compute the similarity of two subtrees in a `MappingStore`.
pub mod similarity;

/// Algorithms which were described in Myers(1996).
///
/// Induced, pruning algorithms currently implemented.
pub mod chawathe98_matcher;

/// Fingerprinting algorithms for tree isomorphism tests.
pub mod fingerprint;

#[cfg(test)]
pub mod test_common {
    use serde::{Deserialize, Deserializer};
    use serde_xml_rs::deserialize;

    use ast::{Arena, FromNodeId, NodeId};

    fn to_string<'de, D>(deserializer: D) -> Result<String, D::Error>
        where D: Deserializer<'de>
    {
        Ok(Deserialize::deserialize(deserializer)?)
    }

    #[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
    struct Tree {
        label: String,
        #[serde(deserialize_with = "to_string")]
        ty: String,
        #[serde(rename = "Tree", default)]
        children: Vec<Tree>
    }

    /// Load an AST from an XML string.
    /// Intended to be used for small trees needed by unit tests.
    ///
    /// To use the result as a destination parse tree, use the `From` trait:
    ///  ```
    ///     let xmltree = load_xml_tree(...);
    ///     ... Arena::<&'static str, ToNodeId>::from(xmltree) ...
    /// ```
    pub fn load_xml_ast(xml: &str) -> Arena<String, FromNodeId> {
        tree_to_arena(load_xml_tree(xml))
    }

    fn load_xml_tree(xml: &str) -> Tree {
        deserialize(xml.as_bytes()).unwrap()
    }

    fn tree_to_arena(tree: Tree) -> Arena<String, FromNodeId> {
        let mut arena = Arena::new();
        let mut next_root = vec![tree.clone()];
        let mut root = arena.new_node(tree.ty.clone(), tree.label.clone(), None, None, None, None);
        let mut next_node: Vec<NodeId<FromNodeId>> = vec![root];
        while !next_root.is_empty() {
            let next_tree = next_root.pop().unwrap();
            root = next_node.pop().unwrap();
            for child in next_tree.children.clone() {
                let node = arena.new_node(child.ty.clone(),
                                          child.label.clone(),
                                          None,
                                          None,
                                          None,
                                          None);
                node.make_child_of(root, &mut arena).ok();
                next_root.push(child);
                next_node.push(node);
            }
        }
        arena
    }

    /// Create an arena of type `Arena<&'static str, FromNodeId>` for testing.
    /// To use this as a destination parse tree, use the `From` trait:
    ///  ```
    ///     let mult = create_mult_arena();
    ///     ... Arena::<&'static str, ToNodeId>::from(mult) ...
    /// ```
    pub fn create_mult_arena() -> Arena<String, FromNodeId> {
        let xml = "<Tree ty=\"Expr\" label=\"+\">
        <Tree ty=\"INT\" label=\"1\"/>
        <Tree ty=\"Expr\" label=\"*\">
            <Tree ty=\"INT\" label=\"3\"/>
            <Tree ty=\"INT\" label=\"4\"/>
        </Tree>
    </Tree>
    ";
        let arena = load_xml_ast(xml);
        let expected_format = "000: \"Expr\" +
001:   \"INT\" 1
002:   \"Expr\" *
003:     \"INT\" 3
004:     \"INT\" 4
";
        assert_eq!(expected_format, format!("{:?}", arena));
        arena
    }

    /// Create an arena of type `Arena<&'static str, FromNodeId>` for testing.
    /// To use this as a destination parse tree, use the `From` trait:
    ///  ```
    ///     let plus = create_plus_arena();
    ///     ... Arena::<&'static str, ToNodeId>::from(plus) ...
    /// ```
    pub fn create_plus_arena() -> Arena<String, FromNodeId> {
        let xml = "<Tree ty=\"Expr\" label=\"+\">
        <Tree ty=\"INT\" label=\"3\"/>
        <Tree ty=\"INT\" label=\"4\"/>
    </Tree>
    ";
        let arena = load_xml_ast(xml);
        let expected_format = "000: \"Expr\" +
001:   \"INT\" 3
002:   \"INT\" 4
";
        assert_eq!(expected_format, format!("{:?}", arena));
        arena
    }

    #[test]
    fn load_xml0() {
        let xml = "<Tree ty=\"0\" label=\"a\"></Tree>";
        let expected = Tree { ty: String::from("0"),
                              label: String::from("a"),
                              children: vec![] };
        assert_eq!(expected, load_xml_tree(xml));
    }

    #[test]
    fn load_xml1() {
        let xml = "<Tree ty=\"0\" label=\"a\">
        <Tree ty=\"0\" label=\"b\"/></Tree>
    </Tree>
    ";
        let expected = Tree { ty: "0".to_string(),
                              label: "a".to_string(),
                              children: vec![Tree { ty: "0".to_string(),
                                                    label: "b".to_string(),
                                                    children: vec![] }] };
        assert_eq!(expected, load_xml_tree(xml));
    }

    #[test]
    fn load_xml2() {
        let xml = "<Tree ty=\"0\" label=\"a\">
        <Tree ty=\"0\" label=\"b\"/>
        <Tree ty=\"0\" label=\"c\">
            <Tree ty=\"0\" label=\"d\"/>
            <Tree ty=\"0\" label=\"e\"/>
            <Tree ty=\"0\" label=\"f\"/>
        </Tree>
    </Tree>
    ";
        let expected =
            Tree { ty: "0".to_string(),
                   label: "a".to_string(),
                   children: vec![Tree { ty: "0".to_string(),
                                         label: "b".to_string(),
                                         children: vec![] },
                                  Tree { ty: "0".to_string(),
                                         label: "c".to_string(),
                                         children: vec![Tree { ty: "0".to_string(),
                                                               label: "d".to_string(),
                                                               children: vec![] },
                                                        Tree { ty: "0".to_string(),
                                                               label: "e".to_string(),
                                                               children: vec![] },
                                                        Tree { ty: "0".to_string(),
                                                               label: "f".to_string(),
                                                               children: vec![] }] }] };
        assert_eq!(expected, load_xml_tree(xml));
    }

    #[test]
    fn convert_arena0() {
        let xml = "<Tree ty=\"0\" label=\"a\"></Tree>";
        let tree = load_xml_tree(xml);
        let mut arena: Arena<String, FromNodeId> = Arena::new();
        arena.new_node("0".to_string(), "a".to_string(), None, None, None, None);
        assert_eq!(format!("{:?}", arena), format!("{:?}", tree_to_arena(tree)));
    }

    #[test]
    fn convert_arena1() {
        let xml = "<Tree ty=\"0\" label=\"a\">
        <Tree ty=\"0\" label=\"b\"/></Tree>
    </Tree>
    ";
        let tree = load_xml_tree(xml);
        let mut arena: Arena<String, FromNodeId> = Arena::new();
        let root = arena.new_node("0".to_string(), "a".to_string(), None, None, None, None);
        let b = arena.new_node("0".to_string(), "b".to_string(), None, None, None, None);
        b.make_child_of(root, &mut arena).unwrap();
        assert_eq!(format!("{:?}", arena), format!("{:?}", tree_to_arena(tree)));
    }

    #[test]
    fn convert_arena2() {
        let xml = "<Tree ty=\"0\" label=\"a\">
        <Tree ty=\"0\" label=\"b\"/>
        <Tree ty=\"0\" label=\"c\">
            <Tree ty=\"0\" label=\"d\"/>
            <Tree ty=\"0\" label=\"e\"/>
            <Tree ty=\"0\" label=\"f\"/>
        </Tree>
    </Tree>
    ";
        let tree = load_xml_tree(xml);
        let mut arena: Arena<String, FromNodeId> = Arena::new();
        let a = arena.new_node("0".to_string(), "a".to_string(), None, None, None, None);
        let b = arena.new_node("0".to_string(), "b".to_string(), None, None, None, None);
        b.make_child_of(a, &mut arena).unwrap();
        let c = arena.new_node("0".to_string(), "c".to_string(), None, None, None, None);
        c.make_child_of(a, &mut arena).unwrap();
        let d = arena.new_node("0".to_string(), "d".to_string(), None, None, None, None);
        d.make_child_of(c, &mut arena).unwrap();
        let e = arena.new_node("0".to_string(), "e".to_string(), None, None, None, None);
        e.make_child_of(c, &mut arena).unwrap();
        let f = arena.new_node("0".to_string(), "f".to_string(), None, None, None, None);
        f.make_child_of(c, &mut arena).unwrap();
        assert_eq!(format!("{:?}", arena), format!("{:?}", tree_to_arena(tree)));
    }
}
