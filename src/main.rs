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

extern crate docopt;
#[macro_use]
extern crate log;
extern crate env_logger;
extern crate rustc_serialize;

use std::{env, process};
use std::io::{stderr, Write};
use std::path::Path;

use docopt::Docopt;

extern crate treediff;
use treediff::ast;
use treediff::emitters;

const USAGE: &'static str = "
Usage: rstreediff [options] <base-file> <diff-file>
       rstreediff [options] <base-file> <diff-file> -d <file> ...
       rstreediff (--help | --version)

Diff two input files.

Options:
    -a, --ast         print AST of input files to STDOUT
    -d, --dot <file>  write out GraphViz representations of the input file(s)
    -h, --help        print this help menu and exit
    -v, --version     print version information and exit
";

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(RustcDecodable, Debug)]
struct Args {
    arg_base_file: String,
    arg_diff_file: String,
    flag_ast: bool,
    flag_dot: Vec<String>,
    flag_help: bool,
    flag_version: bool,
}

fn exit_with_message(message: &str) -> ! {
    writeln!(&mut stderr(), "{}", message).ok();
    process::exit(1);
}

fn write_dotfile_to_disk(filepath: &str, arena: &ast::Arena<String, String>) {
    if let Err(err) = emitters::write_dotfile_to_disk(filepath, arena) {
        use emitters::EmitterError::*;
        let action = match err {
            CouldNotCreateFile => "create",
            CouldNotWriteToFile => "write to",
        };
        exit_with_message(&format!("Could not {} file {}.", action, filepath));
    }
}

fn parse_file(filename: &str, lexer_path: &str, yacc_path: &str) -> ast::Arena<String, String> {
    let error_to_str = |err| {
        use ast::ParseError::*;
        match err {
            FileNotFound => "File not found. Check grammar and input files.".into(),
            BrokenLexer => format!("Could not build lexer {}.", lexer_path),
            BrokenParser => format!("Could not build parser {}.", yacc_path),
            LexicalError => format!("Lexical error in {}.", filename),
            SyntaxError => format!("Syntax error in {}.", filename),
            _ => format!("Error parsing {}.", filename),
        }
    };
    ast::parse_file(filename)
        .map_err(error_to_str)
        .map_err(|ref msg| exit_with_message(msg))
        .unwrap()
}

fn main() {
    env_logger::init().unwrap();

    let argv: Vec<_> = env::args().collect();
    debug!("argv from stdout: {:?}", argv);

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.argv(argv).decode())
        .unwrap_or_else(|e| e.exit());
    debug!("args from docopt: {:?}", args);

    if args.flag_help {
        println!("{}", USAGE);
        process::exit(0);
    }
    if args.flag_version {
        println!("{}", VERSION);
        process::exit(0);
    }

    // This function duplicates some checks that are performed by the
    // treediff::ast::parse_file in order to give better error messages.

    // Determine lexer and yacc files by extension. For example if the input
    // file is named Foo.java, the lexer should be grammars/java.l.
    // TODO: create a HashMap of file extensions -> lex/yacc files.
    let extension1 = match Path::new(&args.arg_base_file).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => {
            exit_with_message(&format!("Cannot determine file type of {}.", args.arg_base_file));
        }
    };
    let lex_l_path1 = format!("grammars/{}.l", extension1);
    let yacc_y_path1 = format!("grammars/{}.y", extension1);
    if !Path::new(&lex_l_path1).exists() || !Path::new(&yacc_y_path1).exists() {
        exit_with_message(&format!("Cannot parse .{} files.", extension1));
    }

    let extension2 = match Path::new(&args.arg_diff_file).extension() {
        Some(ext) => ext.to_str().unwrap(),
        None => {
            exit_with_message(&format!("Cannot determine file type of {}.", args.arg_diff_file));
        }
    };
    let lex_l_path2 = format!("grammars/{}.l", extension2);
    let yacc_y_path2 = format!("grammars/{}.y", extension2);
    if !Path::new(&lex_l_path2).exists() || !Path::new(&yacc_y_path2).exists() {
        exit_with_message(&format!("Cannot parse .{} files.", extension2));
    }

    // Parse both input files.
    let ast_base = parse_file(&args.arg_base_file, &lex_l_path1, &yacc_y_path1);
    let ast_diff = parse_file(&args.arg_diff_file, &lex_l_path2, &yacc_y_path2);

    // Dump ASTs to STDOUT, if requested.
    if args.flag_ast {
        println!("{}", ast_base);
        println!("{}", ast_diff);
    }

    // Generate graphviz file(s), if requested.
    if !args.flag_dot.is_empty() {
        info!("User wishes to create graphviz files {:?}.", args.flag_dot);
        write_dotfile_to_disk(&args.flag_dot[0], &ast_base);
    }
    if args.flag_dot.len() > 1 {
        write_dotfile_to_disk(&args.flag_dot[1], &ast_diff);
    }
}
