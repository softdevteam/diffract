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

//! Integration tests for ast module.
//! All file paths are relative to the root of the repository.

extern crate diffract;

use std::path::Path;

use diffract::ast::{Arena, FromNodeId};
use diffract::parser::{get_lexer, get_parser, parse_file, ParseError};

fn compare_ast_dump_to_lrpar_output(filepath: &str, expected: &str) {
    let arena: Arena<String, FromNodeId> =
        parse_file(filepath, &get_lexer(filepath), &get_parser(filepath)).unwrap();
    let arena_pretty_printed = format!("{:?}", arena);
    // Remove `\r` from pretty printed string, as the 'expected' values all
    // use UNIX line endings.
    let pp_unix = arena_pretty_printed.replace("\r", "");
    assert_eq!(expected, pp_unix);
}

#[test]
fn test_empty_calc() {
    compare_ast_dump_to_lrpar_output(
                                     "tests/empty.calc",
                                     "000: \"^~\"
001:   \"~\"
002:     \"WHITESPACE\"  \n
003:     \"~\"
004:   \"Expr\"
005:     \"Term\"
006:       \"Factor\"
"
    );
}

#[test]
fn test_one_calc() {
    compare_ast_dump_to_lrpar_output(
                                     "tests/one.calc",
                                     "000: \"^~\"
001:   \"~\"
002:   \"Expr\"
003:     \"Term\"
004:       \"Factor\"
005:         \"INT\" 1
006:         \"~\"
007:           \"WHITESPACE\" \n
008:           \"~\"
"
    );
}

#[test]
fn test_add_calc() {
    compare_ast_dump_to_lrpar_output(
                                     "tests/add.calc",
                                     "000: \"^~\"
001:   \"~\"
002:   \"Expr\"
003:     \"Term\"
004:       \"Factor\"
005:         \"INT\" 1
006:         \"~\"
007:     \"PLUS\" +
008:     \"~\"
009:     \"Expr\"
010:       \"Term\"
011:         \"Factor\"
012:           \"INT\" 2
013:           \"~\"
014:             \"WHITESPACE\" \n
015:             \"~\"
"
    );
}

#[test]
fn test_mult_calc() {
    compare_ast_dump_to_lrpar_output(
                                     "tests/mult.calc",
                                     "000: \"^~\"
001:   \"~\"
002:   \"Expr\"
003:     \"Term\"
004:       \"Factor\"
005:         \"INT\" 3
006:         \"~\"
007:       \"MULT\" *
008:       \"~\"
009:       \"Term\"
010:         \"Factor\"
011:           \"INT\" 1
012:           \"~\"
013:     \"PLUS\" +
014:     \"~\"
015:     \"Expr\"
016:       \"Term\"
017:         \"Factor\"
018:           \"INT\" 2
019:           \"~\"
020:             \"WHITESPACE\" \n
021:             \"~\"
"
    );
}

#[test]
fn test_nested_comment_java() {
    compare_ast_dump_to_lrpar_output(
                                     "tests/NestedComment.java",
                                     "000: \"^~\"
001:   \"~\"
002:     \"COMMENT\" /*\n * // Single line comment nested in multi-line comment.\n */
003:     \"~\"
004:       \"WHITESPACE\" \n
005:       \"~\"
006:   \"goal\"
007:     \"compilation_unit\"
008:       \"type_declarations_opt\"
009:         \"type_declarations\"
010:           \"type_declaration\"
011:             \"class_declaration\"
012:               \"modifiers_opt\"
013:                 \"modifiers\"
014:                   \"modifier\"
015:                     \"PUBLIC\" public
016:                     \"~\"
017:                       \"WHITESPACE\"  \n018:                       \"~\"
019:               \"CLASS\" class
020:               \"~\"
021:                 \"WHITESPACE\"  \n022:                 \"~\"
023:               \"IDENTIFIER\" NestedComment
024:               \"~\"
025:                 \"WHITESPACE\"  \n026:                 \"~\"
027:               \"type_parameters_opt\"
028:               \"super_opt\"
029:               \"interfaces_opt\"
030:               \"class_body\"
031:                 \"LBRACE\" {
032:                 \"~\"
033:                 \"class_body_declarations_opt\"
034:                 \"RBRACE\" }
035:                 \"~\"
036:                   \"WHITESPACE\" \n
037:                   \"~\"\n"
    );
}

#[test]
fn test_non_existant_input() {
    let filepath = "nosuchfileexists.calc";
    let ast_result =
        parse_file::<FromNodeId>(filepath, &get_lexer(filepath), &get_parser(filepath));
    match ast_result {
        Err(ParseError::FileNotFound(s)) => assert_eq!(s, String::from("nosuchfileexists.calc")),
        Err(e) => panic!("Expected FileNotFound error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST")
    }
}

#[test]
fn test_non_existant_lex() {
    let lex = Path::new("grammars/nosuchfileexists.l");
    let yacc = Path::new("grammars/calc.y");
    let ast_result = parse_file::<FromNodeId>("tests/one.calc", lex, yacc);
    match ast_result {
        Err(ParseError::FileNotFound(s)) => {
            assert_eq!(s, String::from("grammars/nosuchfileexists.l"))
        }
        Err(e) => panic!("Expected FileNotFound error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST")
    }
}

#[test]
fn test_non_existant_grm() {
    let lex = Path::new("grammars/calc.l");
    let yacc = Path::new("grammars/nosuchfileexists.y");
    let ast_result = parse_file::<FromNodeId>("tests/one.calc", lex, yacc);
    match ast_result {
        Err(ParseError::FileNotFound(s)) => {
            assert_eq!(s, String::from("grammars/nosuchfileexists.y"))
        }
        Err(e) => panic!("Expected FileNotFound error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST")
    }
}

#[test]
fn test_lexical_err() {
    let filepath = "tests/calc_lexical_err.calc";
    let ast_result =
        parse_file::<FromNodeId>(filepath, &get_lexer(filepath), &get_parser(filepath));
    match ast_result {
        Err(ParseError::LexicalError) => assert!(true),
        Err(e) => panic!("Expected LexicalError error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST")
    }
}

#[test]
fn test_syntax_err() {
    let filepath = "tests/calc_syntax_err.calc";
    let ast_result =
        parse_file::<FromNodeId>(filepath, &get_lexer(filepath), &get_parser(filepath));
    match ast_result {
        Err(ParseError::SyntaxError) => assert!(true),
        Err(e) => panic!("Expected SyntaxError error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST")
    }
}

#[test]
fn test_broken_lex() {
    let lex = Path::new("tests/grammars/broken.l");
    let yacc = Path::new("grammars/txt.y");
    let ast_result = parse_file::<FromNodeId>("tests/lorem1.txt", lex, yacc);
    match ast_result {
        Err(ParseError::BrokenLexer) => assert!(true),
        Err(e) => panic!("Expected BrokenLexer error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST")
    }
}

#[test]
fn test_broken_grm() {
    let lex = Path::new("grammars/txt.l");
    let yacc = Path::new("tests/grammars/broken.y");
    let ast_result = parse_file::<FromNodeId>("tests/lorem1.txt", lex, yacc);
    match ast_result {
        Err(ParseError::BrokenParser) => assert!(true),
        Err(e) => panic!("Expected BrokenParser error, got: {:?}", e),
        Ok(_) => panic!("Expected FileNotFound error, got an AST")
    }
}
