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
// (b) any piece of software and/or hardware listed in the lrgrwrks.txt file //
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

use std::{env, process};
use std::io::{stdout, stderr, Write};
use std::path::Path;

#[macro_use]
extern crate log;
extern crate env_logger;

extern crate getopts;
use getopts::Options;

extern crate treediff;
use treediff::ast;

fn usage(prog: String, msg: &str, error: bool, options: Options) {
    let path = Path::new(prog.as_str());
    let program = match path.file_name() {
        Some(m) => m.to_str().unwrap(),
        None => "rstreediff",
    };
    let brief = format!("Usage: {} [OPTIONS] <base file> <diff file>", program);
    let mut output = String::new();
    if msg.len() > 0 {
        output.push_str(msg);
        output.push_str("\n");
    }
    output.push_str(options.usage(&brief).as_str());
    if error {
        writeln!(stderr(), "{}", output).ok();
    } else {
        writeln!(stdout(), "{}", output).ok();
    }
}

fn parse_file(filename: &str,
              lexer_path: &str,
              yacc_path: &str)
              -> treediff::ast::Arena<String, String> {
    match ast::parse_file(filename) {
        Ok(arena) => arena,
        Err(treediff::ast::ParseError::FileNotFound) => {
            writeln!(&mut stderr(),
                     "File not found. Check grammar and input files.")
                    .ok();
            process::exit(1);
        }
        Err(treediff::ast::ParseError::BrokenLexer) => {
            writeln!(&mut stderr(), "Could not build lexer {}.", lexer_path).ok();
            process::exit(1);
        }
        Err(treediff::ast::ParseError::BrokenParser) => {
            writeln!(&mut stderr(), "Could not build parser {}.", yacc_path).ok();
            process::exit(1);
        }
        Err(treediff::ast::ParseError::LexicalError) => {
            writeln!(&mut stderr(), "Lexical error in {}.", filename).ok();
            process::exit(1);
        }
        Err(treediff::ast::ParseError::SyntaxError) => {
            writeln!(&mut stderr(), "Syntax error in {}.", filename).ok();
            process::exit(1);
        }
        Err(_) => {
            writeln!(&mut stderr(), "Error parsing {}.", filename).ok();
            process::exit(1);
        }
    }
}

fn main() {
    env_logger::init().unwrap();

    let args: Vec<String> = env::args().collect();
    let prog = args[0].clone();
    let mut options = Options::new();
    options.optflag("a", "ast", "print AST of input files to STDOUT");
    options.optflag("h", "help", "print this help menu");
    let matches = match options.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            usage(prog, f.to_string().as_str(), true, options);
            process::exit(1);
        }
    };
    debug!("All arguments: {:?}", args);
    debug!("Free text arguments: {:?}", matches.free);
    if matches.opt_present("h") {
        usage(prog, "", false, options);
        process::exit(0);
    }
    let dump_ast = matches.opt_present("a");
    if matches.free.len() != 2 {
        usage(prog, "Please provide two input files.", true, options);
        process::exit(1);
    }

    // This function duplicates some checks that are performed by the
    // treediff::ast::parse_file in order to give better error messages.

    // Determine lexer and yacc files by extension. For example if the input
    // file is named Foo.java, the lexer should be grammars/java.l.
    // TODO: create a HashMap of file extensions -> lex/yacc files.
    let extension1 = match Path::new(&matches.free[0]).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => {
            writeln!(&mut stderr(),
                     "Cannot determine file type of {}.",
                     &matches.free[0])
                    .ok();
            process::exit(1);
        }
    };
    let lex_l_path1 = format!("grammars/{}.l", extension1);
    let yacc_y_path1 = format!("grammars/{}.y", extension1);
    if !Path::new(&lex_l_path1).exists() || !Path::new(&yacc_y_path1).exists() {
        writeln!(&mut stderr(), "Cannot parse .{} files.", extension1).ok();
        process::exit(1);;
    }

    let extension2 = match Path::new(&matches.free[1]).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => {
            writeln!(&mut stderr(),
                     "Cannot determine file type of {}.",
                     &matches.free[1])
                    .ok();
            process::exit(1);
        }
    };
    let lex_l_path2 = format!("grammars/{}.l", extension2);
    let yacc_y_path2 = format!("grammars/{}.y", extension2);
    if !Path::new(&lex_l_path2).exists() || !Path::new(&yacc_y_path2).exists() {
        writeln!(&mut stderr(), "Cannot parse .{} files.", extension2).ok();
        process::exit(1);
    }

    // Parse both input files.
    let ast_base = parse_file(&matches.free[0], &lex_l_path1, &yacc_y_path1);
    let ast_diff = parse_file(&matches.free[1], &lex_l_path2, &yacc_y_path2);

    // Dump ASTs to STDOUT, if requested.
    if dump_ast {
        println!("{}", ast_base);
        println!("{}", ast_diff);
    }
}
