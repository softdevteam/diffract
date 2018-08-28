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

#![warn(missing_docs)]

use std::convert::TryFrom;
use std::fs::{canonicalize, File};
use std::io::Read;
use std::path::{Path, PathBuf};

use cfgrammar::yacc::{YaccGrammar, YaccKind};
use cfgrammar::TIdx;
use lrlex::{build_lex, Lexer};
use lrpar::parser;
use lrtable::{from_yacc, Minimiser};

use ast::{Arena, NodeId};

/// Needed for grmtools: must be big enough to index (separately) all nonterminals, productions,
/// symbols (in productions) and terminals.
type StorageT = u16;

quick_error! {
    /// Errors raised when parsing a source file.
    #[derive(Debug)]
    pub enum ParseError {
        Io(path: String) {
            description("Could not create a new file.")
            display(r#"File "{}" could not be created."#, path)
        }
        FileNotFound(path: String) {
            description("Could not find file.")
            display(r#"Could not find file: "{}"."#, path)
        }
        BrokenLexer {
            description("Lexer could not be built by lrlex.")
            display(r#"Lexer could not be built by lrlex."#)
        }
        BrokenParser {
            description("Parser could not be built by lrpar.")
            display(r#"Parser could not be built by lrpar."#)
        }
        LexicalError(path: String) {
            description("File contained lexical error.")
            display(r#"File "{}" contains a lexical error."#, path)
        }
        SyntaxError(path: String) {
            description("File contains a syntax error.")
            display(r#"File "{}" contains a syntax error."#, path)
        }
    }
}

/// Given a filename (with extension), return a suitable lex file.
pub fn get_lexer(path: &str) -> PathBuf {
    let ext = match Path::new(&path).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => ".txt"
    };
    match ext {
        "java" | "calc" => canonicalize(format!("grammars/{}.l", ext)).unwrap(),
        _ => canonicalize("grammars/txt.l".to_string()).unwrap()
    }
}

/// Given a filename (with extension), return a suitable yacc file.
pub fn get_parser(path: &str) -> PathBuf {
    let ext = match Path::new(&path).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => ".txt"
    };
    match ext {
        "java" | "calc" => canonicalize(format!("grammars/{}.y", ext)).unwrap(),
        _ => canonicalize("grammars/txt.y".to_string()).unwrap()
    }
}

// Read file and return its contents or `ParseError`.
fn read_file(path: &Path) -> Result<String, ParseError> {
    if !Path::new(path).exists() {
        return Err(ParseError::FileNotFound(path.to_str().unwrap().into()));
    }
    let mut f = File::open(path).map_err(|e| ParseError::Io(e.to_string()))?;
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    Ok(s)
}

/// Parse a string, and return an `Arena` or `ParseError`.
pub fn parse_string<T: PartialEq + Copy>(input: &str,
                                         input_path: &str,
                                         lex_path: &Path,
                                         yacc_path: &Path)
                                         -> Result<Arena<String, T>, ParseError> {
    // Determine lexer and yacc files by extension. For example if the input
    // file is named Foo.java, the lexer should be grammars/java.l.
    // TODO: create a HashMap of file extensions -> lex/yacc files.
    // Get input files.
    let lexs = read_file(lex_path)?;
    let grms = read_file(yacc_path)?;

    let mut lexerdef = build_lex::<StorageT>(&lexs).map_err(|_| ParseError::BrokenLexer)?;
    let grm = YaccGrammar::<StorageT>::new_with_storaget(YaccKind::Eco, &grms)
                                      .map_err(|_| ParseError::BrokenParser)?;
    let (sgraph, stable) = from_yacc(&grm, Minimiser::Pager).map_err(|_| ParseError::BrokenParser)?;

    // Sync up the IDs of terminals in the lexer and parser.
    let rule_ids = grm.terms_map()
                      .iter()
                      .map(|(&n, &i)| (n, StorageT::try_from(usize::from(i)).unwrap()))
                      .collect();
    lexerdef.set_rule_ids(&rule_ids);

    // Lex input file.
    let lexer = lexerdef.lexer(input);
    let lexemes = lexer.lexemes()
                       .map_err(|_| ParseError::LexicalError(input_path.to_string()))?;

    // Return parse tree.
    let pt =
        parser::parse(&grm, &sgraph, &stable, &lexemes).map_err(|_| {
                                                                            ParseError::SyntaxError(input_path.to_string())
                                                                        })?;
    Ok(parse_into_ast::<T>(&pt, &lexer, &grm, input))
}

/// Parse an individual input file, and return an `Arena` or `ParseError`.
pub fn parse_file<T: PartialEq + Copy>(input_path: &str,
                                       lex_path: &Path,
                                       yacc_path: &Path)
                                       -> Result<Arena<String, T>, ParseError> {
    let input = read_file(Path::new(input_path))?;
    parse_string(&input, &input_path, lex_path, yacc_path)
}

// Turn a grammar, parser and input string into an AST arena.
fn parse_into_ast<T: PartialEq + Copy>(pt: &parser::Node<StorageT>,
                                       lexer: &Lexer<StorageT>,
                                       grm: &YaccGrammar<StorageT>,
                                       input: &str)
                                       -> Arena<String, T> {
    let mut arena = Arena::new();
    let mut st = vec![pt]; // Stack of nodes.
                           // Stack of `Option<NodeId>`s which are parents of nodes on the `st` stack.
                           // The stack should never be empty, a `None` should be at the bottom of the stack.
    let mut parent = vec![None];
    let mut child_node: NodeId<T>;
    while !st.is_empty() {
        let e = st.pop().unwrap();
        match *e {
            parser::Node::Term { lexeme } => {
                let token_id = lexeme.tok_id();
                let term_name = grm.term_name(TIdx(token_id)).unwrap();
                let lexeme_string = &input[lexeme.start()..lexeme.start() + lexeme.len()];
                let (line_no, col_no) = lexer.line_and_col(&lexeme).unwrap();
                child_node = arena.new_node(term_name.to_string(),
                                            lexeme_string.to_string(),
                                            Some(col_no),
                                            Some(line_no),
                                            Some(lexeme.start()),
                                            Some(lexeme.len()));
                match parent.pop().unwrap() {
                    None => parent.push(None),
                    Some(id) => {
                        child_node.make_child_of(id, &mut arena).ok();
                    }
                };
            }
            parser::Node::Nonterm { nonterm_idx,
                                    ref nodes } => {
                // A non-terminal has no label of its own, but has a node type.
                child_node = arena.new_node(grm.nonterm_name(nonterm_idx).to_string(),
                                            "".to_string(),
                                            None,
                                            None,
                                            None,
                                            None);
                match parent.pop().unwrap() {
                    None => parent.push(None),
                    Some(id) => {
                        child_node.make_child_of(id, &mut arena).ok();
                    }
                };
                // Push children of current non-terminal onto stacks.
                for x in nodes.iter().rev() {
                    st.push(x);
                    parent.push(Some(child_node));
                }
            }
        }
    }
    arena
}
