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

mod common;
use common::check_files;

#[test]
fn test_both_arenas_empty() {
    let ast_from = Arena::new();
    let ast_to = Arena::new();
    // Generate mappings between ASTs.
    let matcher_config = MyersConfig::new();
    let store = matcher_config.match_trees(ast_from, ast_to);
    assert!(store.from.borrow().is_empty());
    assert!(store.to.borrow().is_empty());
    // Generate an edit script.
    let gen_config: Box<EditScriptGenerator<String>> = Box::new(Chawathe96Config::new());
    let _edit_script_wrapped = gen_config.generate_script(&store);
}

#[test]
#[should_panic]
fn test_from_arena_empty() {
    let ast_from = Arena::new();
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
#[should_panic]
fn test_to_arena_empty() {
    let ast_from = parse_file("tests/empty.calc", &get_lexer("grammars/calc.l"), &get_parser("grammars/calc.y")).unwrap();
    let ast_to = Arena::new();
    // Generate mappings between ASTs.
    let matcher_config = MyersConfig::new();
    let store = matcher_config.match_trees(ast_from, ast_to);
    assert!(store.from.borrow().is_empty());
    assert!(store.to.borrow().is_empty());
    // Generate an edit script.
    let gen_config: Box<EditScriptGenerator<String>> = Box::new(Chawathe96Config::new());
    gen_config.generate_script(&store).ok(); // Panic.
}

#[test]
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

// The following are regression tests. These combinations of files have
// previously triggered bugs found by running tests generated by build.rs.

#[test]
fn test_hello_wiki1_myers() {
    check_files("tests/Hello.txt",
                "tests/wiki1.txt",
                Box::new(MyersConfig::new()))
}

#[test]
fn test_hello_test1_myers() {
    check_files("tests/Hello.java",
                "tests/Test0.java",
                Box::new(MyersConfig::new()))
}

#[test]
#[ignore]
fn test_hello_test0_myers() {
    check_files("tests/Hello.java",
                "tests/Test0.txt",
                Box::new(MyersConfig::new()))
}

#[test]
fn test_comment_hello_myers() {
    check_files("tests/Comment.java",
                "tests/Hello.java",
                Box::new(MyersConfig::new()))
}

#[test]
fn test_comment_minimal_hello_minimal_myers() {
    check_files("tests/CommentMinimal.java",
                "tests/HelloMinimal.java",
                Box::new(MyersConfig::new()))
}

#[test]
fn test_hello_minimal_comment_minimal_myers() {
    check_files("tests/HelloMinimal.java",
                "tests/CommentMinimal.java",
                Box::new(MyersConfig::new()))
}

#[test]
fn test_short1_wiki2_myers() {
    check_files("tests/short1.txt",
                "tests/wiki2.txt",
                Box::new(MyersConfig::new()))
}
