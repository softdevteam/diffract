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

//! Integration tests for matcher module, testing the edit script generator.
//! All file paths are relative to the root of the repository.

extern crate treediff;

use std::path::Path;

use treediff::ast::{NodeId, parse_file};
use treediff::myers_matcher::MyersConfig;
use treediff::matchers::MatchTrees;

enum Filetype {
    JAVA,
    TEXT,
    CALC,
}

fn compare_asts_post_edit_script(ty: &Filetype, base_file: &str, diff_file: &str) {
    let lex = match *ty {
        Filetype::JAVA => Path::new("grammars/java.l"),
        Filetype::TEXT => Path::new("grammars/txt.l"),
        Filetype::CALC => Path::new("grammars/calc.l"),
    };
    let yacc = match *ty {
        Filetype::JAVA => Path::new("grammars/java.y"),
        Filetype::TEXT => Path::new("grammars/txt.y"),
        Filetype::CALC => Path::new("grammars/calc.y"),
    };
    let ast_base = parse_file(base_file, lex, yacc).unwrap();
    let ast_diff = parse_file(diff_file, lex, yacc).unwrap();
    let size = ast_diff.size();
    // All tests use the Myers matcher.
    let config = MyersConfig::new();
    // Generate mappings between ASTs.
    let mut mapping = config.match_trees(ast_base, ast_diff);
    // Generate edit script. By convention, the parser will generate an AST
    // whose root is in the 0th element of its arena.
    let _ = mapping.generate_edit_script(NodeId::new(0));
    // Once the edit script has been generated, the mapping between the base
    // and diff ASTs should be a total mapping.
    assert_eq!(size, mapping.from.len());
    assert_eq!(size, mapping.to.len());
    for (key, val) in &mapping.from {
        assert!(mapping.to.contains_key(&val.0));
        assert_eq!(*key, mapping.get_from(&val.0).unwrap());
        assert_eq!(val.0, mapping.get_to(key).unwrap());
    }
}

#[test]
fn test_empty_calc() {
    compare_asts_post_edit_script(&Filetype::CALC, "tests/empty.calc", "tests/one.calc");
    compare_asts_post_edit_script(&Filetype::CALC, "tests/one.calc", "tests/empty.calc");
}


#[test]
fn test_gt_example() {
    // Example from the GumTree paper.
    compare_asts_post_edit_script(&Filetype::JAVA, "tests/Test0.java", "tests/Test1.java");
}

#[test]
fn test_plain_text() {
    compare_asts_post_edit_script(&Filetype::TEXT, "tests/lorem1.txt", "tests/lorem2.txt");
}

#[test]
fn test_wiki() {
    // Example from wikipedia.
    compare_asts_post_edit_script(&Filetype::TEXT, "tests/wiki1.txt", "tests/wiki2.txt");
}
