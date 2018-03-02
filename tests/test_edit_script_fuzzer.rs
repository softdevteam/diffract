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

//! Fuzzed integration tests for `edit_script` module.
//! All file paths are relative to the root of the repository.

extern crate diffract;
#[macro_use]
extern crate proptest;

use std::path::Path;

use diffract::edit_script::{Chawathe96Config, EditScriptGenerator};
use diffract::matchers::MatchTrees;
use diffract::myers_matcher::MyersConfig;
use diffract::parser::parse_string;

fn check_calc_strings(from: String, to: String) {
    // Lex and parse the input strings. Because we are parsing strings and
    // not files we cannot use `parser::{get_lexer, get_parser}`. For now,
    // we only support strings from the `calc` grammar.
    let lex = Path::new("grammars/calc.l");
    let yacc = Path::new("grammars/calc.y");
    let ast_from = parse_string(&from, lex, yacc).unwrap();
    let ast_to = parse_string(&to, lex, yacc).unwrap();
    // Generate mappings between ASTs.
    let matcher = MyersConfig::new();
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
    assert_eq!(count_nodes, count_mapped);
    // Test 2: final `from` and `to` ASTs should be isomorphic.
    assert!(store.is_isomorphic(root_from, root_to));
}

proptest! {
    #[test]
    fn diff_numbers(ref from in "[0-9]+", ref to in "[0-9]+") {
        check_calc_strings(from.to_string(), to.to_string());
    }
}
