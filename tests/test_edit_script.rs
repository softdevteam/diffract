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

//! Integration tests for `edit_script` module.
//! All file paths are relative to the root of the repository.

extern crate diffract;

use std::path::Path;

use diffract::ast::{NodeId, parse_file};
use diffract::edit_script::{Chawathe96Config, EditScriptGenerator};
use diffract::matchers::MatchTrees;
use diffract::myers_matcher::MyersConfig;

fn check_trees(is_java: bool, filepath1: &str, filepath2: &str) {
    // Lex and parse the input files.
    let lex = if is_java {
        Path::new("grammars/java.l")
    } else {
        Path::new("grammars/calc.l")
    };
    let yacc = if is_java {
        Path::new("grammars/java.y")
    } else {
        Path::new("grammars/calc.y")
    };
    let ast_from = parse_file(filepath1, lex, yacc).unwrap();
    let ast_to = parse_file(filepath2, lex, yacc).unwrap();
    // Generate mappings between ASTs. For these tests we don't care what the
    // mapping is.
    let matcher_config = MyersConfig::new();
    let mut store = matcher_config.match_trees(ast_from, ast_to);
    // Generate an edit script.
    let gen_config: Box<EditScriptGenerator<String>> = Box::new(Chawathe96Config::new());
    let edit_script_wrapped = gen_config.generate_script(&mut store);
    assert!(edit_script_wrapped.is_ok(), "Edit script generator failed to complete.");
    // Get root nodes.
    let root_from = store.from_arena.root().unwrap_or_else(|| NodeId::new(0));
    let root_to = store.to_arena.root().unwrap_or_else(|| NodeId::new(0));
    // Every reachable node in both ASTs should be mapped. N.B. deleted
    // nodes in the to AST will be in the arena but should not be reachable
    // from the root node.
    let mut count_nodes = 0;
    let mut count_mapped = 0;
    for node in root_from.breadth_first_traversal(&store.from_arena) {
        count_nodes += 1;
        assert!(store.contains_from(&node));
        count_mapped += 1;
    }
    assert_eq!(count_nodes, count_mapped);
    count_nodes = 0;
    count_mapped = 0;
    for node in root_to.breadth_first_traversal(&store.to_arena) {
        count_nodes += 1;
        assert!(store.contains_to(&node));
        count_mapped += 1;
    }
    assert_eq!(count_nodes, count_mapped);
}

#[test]
fn test_empty_one_calc() {
    check_trees(false, "tests/empty.calc", "tests/one.calc")
}

#[test]
fn test_one_add_calc() {
    check_trees(false, "tests/one.calc", "tests/add.calc")
}

#[test]
fn test_add_mult_calc() {
    check_trees(false, "tests/add.calc", "tests/mult.calc")
}

#[test]
fn test_one_empty_calc() {
    check_trees(false, "tests/one.calc", "tests/empty.calc")
}

#[test]
fn test_add_one_calc() {
    check_trees(false, "tests/add.calc", "tests/one.calc",)
}

#[test]
fn test_mult_add_calc() {
    check_trees(false, "tests/mult.calc", "tests/add.calc")
}

#[test]
fn test_test0_test1_java() {
    check_trees(true, "tests/Test0.java", "tests/Test1.java")
}

#[test]
fn test_empty_hello_java() {
    check_trees(true, "tests/Empty.java", "tests/Hello.java")
}

#[test]
fn test_test1_test0_java() {
    check_trees(true, "tests/Test1.java", "tests/Test0.java")
}

#[test]
fn test_hello_empty_java() {
    check_trees(true, "tests/Hello.java", "tests/Empty.java")
}
