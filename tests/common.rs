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

//! Code common to several integration tests.
#![warn(missing_docs)]

extern crate diffract;

use diffract::edit_script::{Chawathe96Config, EditScriptGenerator, TMP_ROOT};
use diffract::matchers::MatchTrees;
use diffract::parser::{get_lexer, get_parser, parse_file};

/// Check that the Chawathe 1996 edit script generator correctly generates an
/// edit script for two files and a given matcher.
pub fn check_files(path1: &str, path2: &str, matcher: Box<MatchTrees<String>>) {
    let ast_from = parse_file(path1, &get_lexer(path1), &get_parser(path1)).unwrap();
    let ast_to = parse_file(path2, &get_lexer(path2), &get_parser(path2)).unwrap();
    // Generate mappings between ASTs.
    let store = matcher.match_trees(ast_from.clone(), ast_to.clone());
    // Generate an edit script.
    let gen_config: Box<EditScriptGenerator<String>> = Box::new(Chawathe96Config::new());
    let edit_script_wrapped = gen_config.generate_script(&store);
    assert!(edit_script_wrapped.is_ok(),
            "Edit script generator failed to complete.");
    // Get root node ids after the edit script has run (note that the root nodes
    // may be altered by the edit script).
    let root_from = store.from_arena.borrow().root().unwrap();
    let root_to = store.to_arena.borrow().root().unwrap();
    // Test 1: every reachable node in both ASTs should be mapped. The edit
    // script generator should have turned the partial matching provided by the
    // matcher into a total matching. N.B. deleted nodes in the to AST will be
    // in the arena but should not be reachable from the root node.
    let mut count_nodes = 0;
    let mut count_mapped = 0;
    for node in root_from.breadth_first_traversal(&store.from_arena.borrow()) {
        count_nodes += 1;
        assert!(store.contains_from(&node));
        count_mapped += 1;
    }
    assert_eq!(count_nodes, count_mapped);
    count_nodes = 0;
    count_mapped = 0;
    for node in root_to.breadth_first_traversal(&store.to_arena.borrow()) {
        count_nodes += 1;
        assert!(store.contains_to(&node));
        count_mapped += 1;
    }
    assert_eq!(count_nodes, count_mapped,
               "Final mapping not total for files: {} and {}.",
               path1, path2);
    // Test 2: final from and to ASTs should be isomorphic.
    // Isomorphism is defined strictly, as there being no differences in the two
    // trees, except in their node identifiers. As a belt-and-braces check, we
    // also check that pretty-printed versions of both trees are identical.
    assert!(store.is_isomorphic(root_from, root_to),
            "ASTs not isomorphic for files: {} and {}.",
            path1,
            path2);
    assert_eq!(format!("{:?}", store.from_arena.borrow()),
               format!("{:?}", store.to_arena.borrow()),
               "ASTs not isomorphic for files: {} and {}.",
               path1,
               path2);
    // Test 3: final from and to ASTs should be isomorphic.
    // In this test isomorphism is determined by examining the hashes of the
    // two trees.
    assert!(store.is_isomorphic_hash(root_from, root_to),
            "ASTs not isomorphic for files: {} and {}.",
            path1,
            path2);
    // Test 4: check that `TMP_ROOT` is not found in the tree. This text is
    // used when the `from` and `to` trees have roots with different types or
    // labels. It should be removed before the edit script generator completes.
    for id in root_from.breadth_first_traversal(&store.from_arena.borrow()) {
        if TMP_ROOT == store.from_arena.borrow()[id].label {
            assert!(false, "TMP_ROOT not removed from {:?}", path1);
        }
    }
    for id in root_to.breadth_first_traversal(&store.to_arena.borrow()) {
        if TMP_ROOT == store.to_arena.borrow()[id].label {
            assert!(false, "TMP_ROOT not removed from {:?}", path2);
        }
    }
}
