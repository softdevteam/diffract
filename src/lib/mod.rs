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
#![cfg_attr(feature = "cargo-clippy", allow(clippy::cyclomatic_complexity))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::new_without_default))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::new_without_default_derive))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::range_plus_one))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::too_many_arguments))]
#![cfg_attr(feature = "cargo-clippy", allow(clippy::type_complexity))]

extern crate cfgrammar;
extern crate crypto;
extern crate dot;
#[macro_use]
extern crate log;
extern crate lrlex;
extern crate lrpar;
extern crate lrtable;
extern crate multiset;
#[macro_use]
extern crate quick_error;
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

/// Algorithms which were described in Myers(1996).
pub mod chawathe98_matcher;

/// Algorithms to generate edit scripts, based on an existing AST matching.
pub mod edit_script;

/// Emitters generate output for the user in a variety of formats (e.g. JSON, Graphviz).
pub mod emitters;

/// Fingerprinting algorithms for tree isomorphism tests.
pub mod fingerprint;

/// GT matching algorithm.
pub mod gt_matcher;

/// A queue of `NodeId`s sorted on the height of their respective nodes.
pub mod hqueue;

/// Store AST information relevant to RTED in vectors.
pub mod info_tree;

/// Store AST labels as small, unsigned integers.
pub mod label_maps;

/// Matchers create mappings between two abstract syntax trees.
pub mod matchers;

/// A multi-mapping store holds non-unique mappings between two abstract syntax trees.
pub mod multi_mappings;

/// Longest common subsequence matching algorithm.
///
/// Described in Myers (1986).
pub mod myers_matcher;

/// The null matcher produces no matches and is only used for testing.
pub mod null_matcher;

/// Parse strings and files into `ast::Arena` types.
pub mod parser;

/// A patch represents a diff on a single AST node.
///
/// Also deals with turning nearby patches into hunks.
pub mod patch;

/// QGram distance algorithm.
pub mod qgram;

/// RTED matcher.
///
/// Described in Pawlik and Augsten (2011).
pub mod rted;

/// Algorithms which act on sequences of values.
pub mod sequence;

/// Compute the similarity of two subtrees in a `MappingStore`.
pub mod similarity;

/// Zhang-Shasha matching algorithm.
///
/// Described in Zhang & Shasha (1989).
pub mod zs_matcher;

use std::f64::{EPSILON, MAX};
/// Compare floating-point numbers for equality.
///
/// Since floating point numbers are not exact, in the general case we do not
/// wish to use a simple comparison. In general, we use a relative error, which
/// compares the difference between the two arguments to their magnitude. This
/// captures the case where the arguments are very large, but their difference
/// might be bigger than machine epsilon. However, when the inputs are close to
/// zero, a relative error can result in a division by zero or a NaN, and so in
/// this case we use an absolute difference. Note that this function will return
/// `false` if the inputs are both very small but differently signed, even if
/// they are both the smallest possible positive / negative f64s.
///
/// See also: http://floating-point-gui.de/errors/comparison/
pub fn f64_eq(val1: f64, val2: f64) -> bool {
    // Simple case (works with infinities).
    if val1 == val2 {
        return true;
    }
    // Absolute difference - suitable where the numbers are close to zero.
    let diff = (val1 - val2).abs();
    if val1 == 0.0 || val2 == 0.0 || diff <= EPSILON {
        return diff <= EPSILON;
    }
    // Use relative error.
    diff / f64::min(val1.abs() + val2.abs(), MAX) < EPSILON
}

#[cfg(test)]
pub mod tests {
    use super::f64_eq;
    use std::f64::{EPSILON, INFINITY, MAX, MIN, MIN_POSITIVE, NAN, NEG_INFINITY};

    #[test]
    fn test_f64_eq_same() {
        assert!(f64_eq(0.0, 0.0));
        assert!(f64_eq(0.0, -0.0));
        assert!(f64_eq(-0.0, 0.0));
        assert!(f64_eq(0.1, 0.1));
        assert!(f64_eq(1.0, 1.0));
    }

    #[test]
    fn test_f64_eq_infinity() {
        assert!(f64_eq(INFINITY, INFINITY));
        assert!(f64_eq(NEG_INFINITY, NEG_INFINITY));
    }

    #[test]
    fn test_f64_eq_min_max() {
        assert!(f64_eq(MAX, MAX));
        assert!(f64_eq(MIN, MIN));
        assert!(f64_eq(MIN_POSITIVE, MIN_POSITIVE));
    }

    #[test]
    fn test_f64_eq_nan() {
        assert!(!f64_eq(NAN, NAN));
        assert!(!f64_eq(NAN, 0.0));
        assert!(!f64_eq(0.0, NAN));
    }

    #[test]
    fn test_f64_eq_differ_by_epsilon() {
        assert!(f64_eq(0.0, 0.0 + EPSILON));
        assert!(f64_eq(1.0, 1.0 + EPSILON));
        assert!(f64_eq(MAX, MAX + EPSILON));
        assert!(f64_eq(MIN, MIN + EPSILON));
        assert!(f64_eq(MIN_POSITIVE, MIN_POSITIVE + EPSILON));
        assert!(f64_eq(INFINITY, INFINITY + EPSILON));
        assert!(f64_eq(NEG_INFINITY, NEG_INFINITY + EPSILON));
    }

    #[test]
    fn test_f64_eq_differ_by_more_than_epsilon() {
        let delta = 2.5 * EPSILON;
        assert!(!f64_eq(0.0, 0.0 + delta));
        assert!(!f64_eq(1.0, -1.0 + delta));
        assert!(!f64_eq(-1.0, 1.0 + delta));
        assert!(!f64_eq(MIN_POSITIVE, MIN_POSITIVE + delta));
        assert!(!f64_eq(0.6956521739130435, 0.6956521739130455));
        assert!(!f64_eq(-0.6956521739130435, -0.6956521739130456));
        assert!(!f64_eq(-0.6956521739130435, 0.6956521739130456));
        assert!(!f64_eq(0.6956521739130435, -0.6956521739130456));
    }
}

/// Helper functions to load data used by test cases across the crate.
#[cfg(test)]
pub mod test_common {
    use serde::{Deserialize, Deserializer};
    use serde_xml_rs::deserialize;

    use ast::{Arena, DstNodeId, NodeId, SrcNodeId};

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
    ///     ... Arena::<String, DstNodeId>::from(xmltree) ...
    /// ```
    pub fn load_xml_ast(xml: &str) -> Arena<String, SrcNodeId> {
        tree_to_arena(load_xml_tree(xml))
    }

    fn load_xml_tree(xml: &str) -> Tree {
        deserialize(xml.as_bytes()).unwrap()
    }

    fn tree_to_arena(tree: Tree) -> Arena<String, SrcNodeId> {
        let mut arena = Arena::new();
        let mut next_root = vec![tree.clone()];
        let mut root = arena.new_node(tree.ty.clone(), tree.label.clone(), None, None, None, None);
        let mut next_node: Vec<NodeId<SrcNodeId>> = vec![root];
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

    /// Create an arena of type `Arena<&'static str, SrcNodeId>` for testing.
    /// To use this as a destination parse tree, use the `From` trait:
    ///  ```
    ///     let mult = create_mult_arena();
    ///     ... Arena::<String, DstNodeId>::from(mult) ...
    /// ```
    pub fn create_mult_arena() -> Arena<String, SrcNodeId> {
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

    /// Create an arena of type `Arena<&'static str, SrcNodeId>` for testing.
    /// To use this as a destination parse tree, use the `From` trait:
    ///  ```
    ///     let plus = create_plus_arena();
    ///     ... Arena::<String, DstNodeId>::from(plus) ...
    /// ```
    pub fn create_plus_arena() -> Arena<String, SrcNodeId> {
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

    /// Example from Fig 4. of Zhang and Shasha (1989).
    pub fn create_zs_paper_src_arena() -> Arena<String, SrcNodeId> {
        load_xml_ast(
                     "<Tree ty=\"node\" label=\"f\">
    <Tree ty=\"node\" label=\"d\">
        <Tree ty=\"node\" label=\"a\"/>
        <Tree ty=\"node\" label=\"c\">
            <Tree ty=\"node\" label=\"b\"/>
        </Tree>
    </Tree>
    <Tree ty=\"node\" label=\"e\">
    </Tree>
</Tree>
"
        )
    }

    /// Example from Fig 4. of Zhang and Shasha (1989).
    pub fn create_zs_paper_dst_arena() -> Arena<String, DstNodeId> {
        let ast = load_xml_ast(
                               "<Tree ty=\"node\" label=\"f\">
    <Tree ty=\"node\" label=\"c\">
        <Tree ty=\"node\" label=\"d\">
            <Tree ty=\"node\" label=\"a\"/>
            <Tree ty=\"node\" label=\"b\"/>
        </Tree>
    </Tree>
    <Tree ty=\"node\" label=\"e\">
    </Tree>
</Tree>
"
        );
        Arena::<String, DstNodeId>::from(ast)
    }

    /// Example from GT test cases.
    pub fn create_zs_src_arena() -> Arena<String, SrcNodeId> {
        load_xml_ast(
                     "<Tree ty=\"0\" label=\"a\">
    <Tree ty=\"0\" label=\"b\"/>
    <Tree ty=\"0\" label=\"c\">
        <Tree ty=\"0\" label=\"d\"/>
        <Tree ty=\"0\" label=\"e\"/>
        <Tree ty=\"0\" label=\"f\"/>
    </Tree>
</Tree>
"
        )
    }

    /// Example from GT test cases.
    pub fn create_zs_dst_arena() -> Arena<String, DstNodeId> {
        let ast = load_xml_ast(
                               "<Tree ty=\"0\" label=\"z\">
    <Tree ty=\"0\" label=\"a\">
        <Tree ty=\"0\" label=\"b\" />
        <Tree ty=\"0\" label=\"c\">
            <Tree ty=\"0\" label=\"y\" />
            <Tree ty=\"1\" label=\"e\" />
            <Tree ty=\"0\" label=\"f\" />
        </Tree>
    </Tree>
</Tree>
"
        );
        Arena::<String, DstNodeId>::from(ast)
    }

    /// Example from GT test cases.
    pub fn create_slide_src_arena() -> Arena<String, SrcNodeId> {
        load_xml_ast(
                     "<Tree ty=\"0\" label=\"6\">
    <Tree ty=\"0\" label=\"5\">
        <Tree ty=\"0\" label=\"2\">
            <Tree ty=\"0\" label=\"1\" />
        </Tree>
        <Tree ty=\"0\" label=\"3\" />
        <Tree ty=\"0\" label=\"4\" />
    </Tree>
</Tree>
"
        )
    }

    /// Example from GT test cases.
    pub fn create_slide_dst_arena() -> Arena<String, DstNodeId> {
        let ast = load_xml_ast(
                               "<Tree ty=\"0\" label=\"6\">
    <Tree ty=\"0\" label=\"2\">
        <Tree ty=\"0\" label=\"1\" />
    </Tree>
    <Tree ty=\"0\" label=\"4\">
        <Tree ty=\"0\" label=\"3\" />
    </Tree>
    <Tree ty=\"0\" label=\"5\" />
</Tree>
"
        );
        Arena::<String, DstNodeId>::from(ast)
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
        let mut arena: Arena<String, SrcNodeId> = Arena::new();
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
        let mut arena: Arena<String, SrcNodeId> = Arena::new();
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
        let mut arena: Arena<String, SrcNodeId> = Arena::new();
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
