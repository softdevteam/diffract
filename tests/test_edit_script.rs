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

use diffract::ast::Arena;
use diffract::edit_script::{Chawathe96Config, EditScriptGenerator};
use diffract::matchers::MatchTrees;
use diffract::myers_matcher::MyersConfig;
use diffract::null_matcher::NullConfig;
use diffract::parser::{get_lexer, get_parser, parse_file};

fn check_files(path1: &str, path2: &str, matcher: Box<MatchTrees<String>>) {
    // Lex and parse the input files.
    let ast_from = parse_file(path1, &get_lexer(path1), &get_parser(path1)).unwrap();
    let ast_to = parse_file(path2, &get_lexer(path2), &get_parser(path2)).unwrap();
    // Generate mappings between ASTs.
    let store = matcher.match_trees(ast_from.clone(), ast_to.clone());
    // Generate an edit script.
    let gen_config: Box<EditScriptGenerator<String>> = Box::new(Chawathe96Config::new());
    let edit_script_wrapped = gen_config.generate_script(&store);
    assert!(edit_script_wrapped.is_ok(),
            "Edit script generator failed to complete.");
    // Get root nodes after the edit script has run (note that the root nodes
    // may be altered by the edit script.
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
}

#[test]
#[should_panic]
fn test_empty_arena() {
    // Lex and parse the input files.
    let ast_from = Arena::new(); // Empty Arena with no root.
    let ast_to = parse_file("tests/empty.calc", &get_lexer("grammars/calc.l"), &get_parser("grammars/calc.y")).unwrap();
    // Generate mappings between ASTs.
    let matcher_config = MyersConfig::new();
    let store = matcher_config.match_trees(ast_from, ast_to);
    assert!(store.from.borrow().is_empty());
    assert!(store.to.borrow().is_empty());
    // Generate an edit script.
    let gen_config: Box<EditScriptGenerator<String>> = Box::new(Chawathe96Config::new());
    let _edit_script_wrapped = gen_config.generate_script(&store); // Panic.
}

#[test]
#[ignore]
fn test_short_text_null() {
    check_files("tests/short1.txt",
                "tests/short2.txt",
                Box::new(NullConfig::new()));
}

#[test]
fn test_short_text_myers() {
    check_files("tests/short1.txt",
                "tests/short2.txt",
                Box::new(MyersConfig::new()));
}

#[test]
#[ignore]
fn test_plain_text_null() {
    check_files("tests/lorem1.txt",
                "tests/lorem2.txt",
                Box::new(NullConfig::new()));
}

#[test]
fn test_plain_text_myers() {
    check_files("tests/lorem1.txt",
                "tests/lorem2.txt",
                Box::new(MyersConfig::new()));
}

#[test]
#[ignore]
fn test_wiki_null() {
    // Example from wikipedia.
    check_files("tests/wiki1.txt",
                "tests/wiki2.txt",
                Box::new(NullConfig::new()));
}

#[test]
fn test_wiki_myers() {
    // Example from wikipedia.
    check_files("tests/wiki1.txt",
                "tests/wiki2.txt",
                Box::new(MyersConfig::new()));
}

#[test]
#[ignore]
fn test_empty_one_calc_null() {
    check_files("tests/empty.calc",
                "tests/one.calc",
                Box::new(NullConfig::new()))
}

#[test]
fn test_empty_one_calc_myers() {
    check_files("tests/empty.calc",
                "tests/one.calc",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_one_two_calc_null() {
    check_files("tests/one.calc",
                "tests/two.calc",
                Box::new(NullConfig::new()))
}

#[test]
fn test_one_two_calc_myers() {
    check_files("tests/one.calc",
                "tests/two.calc",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_one_add_calc_null() {
    check_files("tests/one.calc",
                "tests/add.calc",
                Box::new(NullConfig::new()))
}

#[test]
fn test_one_add_calc_myers() {
    check_files("tests/one.calc",
                "tests/add.calc",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_add_mult_calc_null() {
    check_files("tests/add.calc",
                "tests/mult.calc",
                Box::new(NullConfig::new()))
}

#[test]
fn test_add_mult_calc_myers() {
    check_files("tests/add.calc",
                "tests/mult.calc",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_one_empty_calc_null() {
    check_files("tests/one.calc",
                "tests/empty.calc",
                Box::new(NullConfig::new()))
}

#[test]
fn test_one_empty_calc_myers() {
    check_files("tests/one.calc",
                "tests/empty.calc",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_add_one_calc_null() {
    check_files("tests/add.calc",
                "tests/one.calc",
                Box::new(NullConfig::new()))
}

#[test]
fn test_add_one_calc_myers() {
    check_files("tests/add.calc",
                "tests/one.calc",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_mult_add_calc_null() {
    check_files("tests/mult.calc",
                "tests/add.calc",
                Box::new(NullConfig::new()))
}

#[test]
fn test_mult_add_calc_myers() {
    check_files("tests/mult.calc",
                "tests/add.calc",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_test0_test1_java_null() {
    check_files("tests/Test0.java",
                "tests/Test1.java",
                Box::new(NullConfig::new()))
}

#[test]
fn test_test0_test1_java_myers() {
    check_files("tests/Test0.java",
                "tests/Test1.java",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_empty_hello_java_null() {
    check_files("tests/Empty.java",
                "tests/Hello.java",
                Box::new(NullConfig::new()))
}

#[test]
fn test_empty_hello_java_myers() {
    check_files("tests/Empty.java",
                "tests/Hello.java",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_test1_test0_java_null() {
    check_files("tests/Test1.java",
                "tests/Test0.java",
                Box::new(NullConfig::new()))
}

#[test]
fn test_test1_test0_java_myers() {
    check_files("tests/Test1.java",
                "tests/Test0.java",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_hello_empty_java_null() {
    check_files("tests/Hello.java",
                "tests/Empty.java",
                Box::new(NullConfig::new()))
}

#[test]
fn test_hello_empty_java_myers() {
    check_files("tests/Hello.java",
                "tests/Empty.java",
                Box::new(MyersConfig::new()))
}

// Tests where AST root nodes are unmatched because the input files use
// different grammars.

#[test]
#[ignore]
fn test_empty_one_both_null() {
    check_files("tests/empty.calc",
                "tests/Empty.java",
                Box::new(NullConfig::new()))
}

#[test]
#[ignore]
fn test_add_hello_both_null() {
    check_files("tests/add.calc",
                "tests/Hello.java",
                Box::new(NullConfig::new()))
}
