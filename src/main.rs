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

extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate log;
extern crate rustc_serialize;

use std::{env, process};
use std::fs::canonicalize;
use std::io::{stderr, stdout, Write};
use std::path::{Path, PathBuf};

use docopt::Docopt;

extern crate diffract;
use diffract::ast;
use diffract::edit_script;
use diffract::emitters;
use diffract::fingerprint;
use diffract::gt_matcher;
use diffract::myers_matcher;
use diffract::matchers::MatchTrees;
use diffract::parser;

const USAGE: &str = "
Usage: diffract [options] <base-file> <diff-file>
       diffract [options] <base-file> <diff-file> -d FILE ...
       diffract [options] <base-file> <diff-file> -g FILE ...
       diffract (--help | --list | --version)

Diff two input files.

Options:
    -a, --ast           print AST of input files to STDOUT.
    --debug LEVEL       debug level used by logger. Valid (case sensitive)
                        values are: Debug, Error, Info, Trace, Warn.
    -d, --dot FILE      write out GraphViz representations of the input file(s).
    -e, --edit ALGO     use a given edit script generation algorithm. Valid
                        (case sensitive) values for ALGO are Chawathe96 (default).
    -g, --grammar FILE  specify grammar to use. e.g. -g java would select a
                        Java lexer called java.l and a parser called java.y.
                        By default, this option applies to the first input file,
                        use the option twice to set the grammar for the second
                        input file, e.g. -g calc -g java.
    -h, --help          print this help menu and exit.
    -l, --list          print information about the available matchers and exit.
    --map FILE          write out GraphViz representation of the mapping store.
    -m, --matcher ALGO  use a given matching algorithm. Valid (case sensitive)
                        values for ALGO are: GumTree (default), Myers.
    --max-size VAL      consider subtrees for matching only if they have a size
                        less than VAL. GumTree only.
    --min-dice VAL      set the similarity threshold used for matching ASTs.
                        Two trees are mapped if they are mappable and have a
                        dice coefficient greater than VAL in [0, 1] (default:
                        0.3). GumTree only.
    --min-height VAL    match only nodes with a height greater than VAL
                        (default: 2). GumTree only.
    -o, --output FMT    select output format. Valid (case sensitive) values for
                        FMT are: Terminal (default), JSON, None.
    --store FILE        write a GraphViz representation of the mapping store to
                        file, after the edit script has been generated.
    -v, --version       print version information and exit.
";

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Clone, Copy, Debug, RustcDecodable)]
enum DebugLevel {
    Debug,
    Error,
    Info,
    Trace,
    Warn,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, RustcDecodable)]
enum EditScriptGenerator {
    Chawathe96, // Default.
    Chawathe98,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, RustcDecodable)]
enum Matchers {
    GumTree, // Default.
    Myers,
}

#[derive(Debug, Eq, PartialEq, RustcDecodable)]
enum Output {
    Terminal, // Default.
    JSON,
    None,
}

#[derive(RustcDecodable, Debug)]
struct Args {
    arg_base_file: String,
    arg_diff_file: String,
    flag_ast: bool,
    flag_debug: Option<DebugLevel>,
    flag_dot: Vec<String>,
    flag_edit: Option<EditScriptGenerator>,
    flag_grammar: Vec<String>,
    flag_help: bool,
    flag_list: bool,
    flag_map: Option<String>,
    flag_matcher: Option<Matchers>,
    flag_max_size: Option<u16>,
    flag_min_dice: Option<f32>,
    flag_min_height: Option<u16>,
    flag_output: Option<Output>,
    flag_store: Option<String>,
    flag_version: bool,
}

fn exit_with_message(message: &str) -> ! {
    writeln!(&mut stderr(), "{}", message).ok();
    process::exit(1);
}

fn consume_emitter_err(res: emitters::EmitterResult, filepath: &str) {
    if let Err(err) = res {
        use emitters::EmitterError::*;
        let action = match err {
            CouldNotCreateFile => "create",
            CouldNotWriteToFile => "write to",
            CouldNotOpenFile => "open",
            CouldNotReadFile => "read from",
        };
        exit_with_message(&format!("Could not {} file {}.", action, filepath));
    }
}

fn consume_edit_script_err(error: &ast::ArenaError) -> ! {
    use ast::ArenaError::*;
    let s: String;
    let message = match *error {
        EmtpyArena => "Could not create edit script, AST was empty.",
        NodeIdNotFound => "Could not create edit script, NodeId was not found.",
        NodeHasTooFewChildren(n) => {
            s = format!("Could not create edit script, NodeId had only {} children.",
                        n);
            &s
        }
        NodeHasTooManyChildren => "Could not create edit script, NodeId had more than one child.",
        NodeIdsAreIdentical => "Could not create edit script, NodeIds were identical.",
    };
    exit_with_message(message);
}

fn write_dotfile_to_disk<T: diffract::emitters::RenderDotfile>(filepath: &str, object: &T) {
    consume_emitter_err(emitters::write_dotfile_to_disk(filepath, object), filepath);
}

// Set any global constants requested by the user.
// This should be the ONLY block of code that mutates these values.
fn process_matcher_configs(args: &Args) -> Box<MatchTrees<String>> {
    let config: Box<MatchTrees<String>>;
    if args.flag_matcher != None && args.flag_matcher != Some(Matchers::GumTree) {
        if args.flag_max_size.is_some() {
            exit_with_message("--max-size only makes sense with the GumTree matcher.");
        }
        if args.flag_min_dice.is_some() {
            exit_with_message("--min-dice only makes sense with the GumTree matcher.");
        }
        if args.flag_min_height.is_some() {
            exit_with_message("--min_height only makes sense with the GumTree matcher.");
        }
    }
    if args.flag_matcher == Some(Matchers::Myers) {
        // Longest common subsequence matcher from Myers (1986).
        info!("Selecting the Myers matching algorithm.");
        config = Box::new(myers_matcher::MyersConfig::new());
    } else {
        // GumTree default.
        info!("Selecting the GumTree matching algorithm.");
        let mut unboxed_config = gt_matcher::GumTreeConfig::new();
        if let Some(value) = args.flag_max_size {
            info!("User has set value of MAX_SIZE to {}.", value);
            unboxed_config.max_size = value;
        }
        if let Some(value) = args.flag_min_dice {
            if value < 0. || value > 1. {
                exit_with_message("Value for --min-dice must be in interval [0, 1].");
            }
            info!("User has set value of MIN_DICE to {}.", value);
            unboxed_config.min_dice = value;
        }
        if let Some(value) = args.flag_min_height {
            info!("User has set value of MIN_HEIGHT to {}.", value);
            unboxed_config.min_height = value;
        }
        config = Box::new(unboxed_config);
    }
    config
}

fn get_matcher_descriptions() -> String {
    let mut descriptions = vec![];
    let myers: Box<MatchTrees<u16>> = Box::new(myers_matcher::MyersConfig::new());
    let gt: Box<MatchTrees<u16>> = Box::new(gt_matcher::GumTreeConfig::new());
    descriptions.push(format!("{}\n-----{}\n", "Myers:", myers.describe()));
    descriptions.push(format!("{}\n-------{}\n", "GumTree", gt.describe()));
    descriptions.join("\n")
}

fn parse_file<T: Copy + PartialEq>(filename: &str,
                                   lexer_path: &PathBuf,
                                   yacc_path: &PathBuf)
                                   -> ast::Arena<String, T> {
    let error_to_str = |err| {
        use parser::ParseError::*;
        match err {
            FileNotFound(path) => format!("File {} not found.", path),
            BrokenLexer => format!("Could not build lexer {:?}.", lexer_path),
            BrokenParser => format!("Could not build parser {:?}.", yacc_path),
            LexicalError => format!("Lexical error in {}.", filename),
            SyntaxError => format!("Syntax error in {}.", filename),
            _ => format!("Error parsing {}.", filename),
        }
    };
    parser::parse_file::<T>(filename, lexer_path, yacc_path).map_err(error_to_str)
                                                            .map_err(|ref msg| {
                                                                         exit_with_message(msg)
                                                                     })
                                                            .unwrap()
}

/// Process flags which merely request documentation.
fn process_doc_flags(args: &Args) {
    if args.flag_help {
        println!("{}", USAGE);
        process::exit(0);
    }
    if args.flag_version {
        println!("{}", VERSION);
        process::exit(0);
    }
    if args.flag_list {
        println!("{}", get_matcher_descriptions());
        process::exit(0);
    }
}

/// Determine lexer and yacc files by extension.
/// For example if the input file is named `Foo.java`, the lexer should be
/// `grammars/java.l`. This function duplicates some checks that are performed
/// by the `treediff::ast::parse_file` in order to give better error messages.
/// Will cause diffract to exit if user-requested lexer / parser does not exist.
fn get_parsers(args: &Args) -> (PathBuf, PathBuf, PathBuf, PathBuf) {
    // TODO: create a HashMap of file extensions -> lex/yacc files.
    match env::current_exe() {
        Ok(p) => p,
        Err(_) => exit_with_message("Cannot determine which directory the executable is in."),
    };

    match Path::new(&args.arg_base_file).extension() {
        Some(_) => (),
        None => {
            exit_with_message(&format!("Cannot determine file type of {}.", args.arg_base_file))
        }
    };
    // Lexer path for first input file.
    let lexer1 = if !args.flag_grammar.is_empty() {
        canonicalize(format!("{}.l", &args.flag_grammar[0])).unwrap()
    } else {
        parser::get_lexer(&args.arg_base_file)
    };
    // Parser path for first input file.
    let parser1 = if !args.flag_grammar.is_empty() {
        canonicalize(format!("{}.y", &args.flag_grammar[0])).unwrap()
    } else {
        parser::get_parser(&args.arg_base_file)
    };
    if !args.flag_grammar.is_empty() {
        if !Path::new(&lexer1).exists() {
            exit_with_message(&format!("Requested lexer {:?} does not exist.", lexer1));
        }
        if !Path::new(&parser1).exists() {
            exit_with_message(&format!("Requested parser {:?} does not exist.", parser1));
        }
    }

    match Path::new(&args.arg_diff_file).extension() {
        Some(_) => (),
        None => {
            exit_with_message(&format!("Cannot determine file type of {}.", args.arg_diff_file))
        }
    };
    // Lexer path for second input file.
    let lexer2 = if !args.flag_grammar.is_empty() && args.flag_grammar.len() > 1 {
        canonicalize(format!("{}.l", &args.flag_grammar[1])).unwrap()
    } else {
        parser::get_lexer(&args.arg_diff_file)
    };
    // Parser path for second input file.
    let parser2 = if !args.flag_grammar.is_empty() && args.flag_grammar.len() > 1 {
        canonicalize(format!("{}.y", &args.flag_grammar[1])).unwrap()
    } else {
        parser::get_parser(&args.arg_diff_file)
    };
    if !args.flag_grammar.is_empty() && args.flag_grammar.len() > 1 {
        if !Path::new(&lexer2).exists() {
            exit_with_message(&format!("Requested lexer {:?} does not exist.", lexer2));
        }
        if !Path::new(&parser2).exists() {
            exit_with_message(&format!("Requested parser {:?} does not exist.", parser2));
        }
    }
    (lexer1, parser1, lexer2, parser2)
}

fn main() {
    let argv: Vec<_> = env::args().collect();
    let args: Args = Docopt::new(USAGE).and_then(|d| d.argv(argv).decode())
                                       .unwrap_or_else(|e| e.exit());
    // If the user only asked for documentation, provide it and exit.
    process_doc_flags(&args);

    // Set up logging.
    if let Some(l) = args.flag_debug {
        env::set_var("RUST_LOG", &format!("{:?}", l).to_lowercase());
    }
    env_logger::init().unwrap();

    // Matcher configuration object.
    let matcher_config: Box<MatchTrees<String>> = process_matcher_configs(&args);

    let (lexer1, parser1, lexer2, parser2) = get_parsers(&args);

    // Parse both input files.
    let ast_base = parse_file::<ast::FromNodeId>(&args.arg_base_file, &lexer1, &parser1);
    let ast_diff = parse_file::<ast::ToNodeId>(&args.arg_diff_file, &lexer2, &parser2);

    // Dump ASTs to STDOUT, if requested.
    if args.flag_ast {
        println!("{:?}", ast_base);
        println!("{:?}", ast_diff);
    }

    // Generate graphviz file(s), if requested.
    if !args.flag_dot.is_empty() {
        info!("Creating dot representation of AST {:?}.", args.flag_dot);
        write_dotfile_to_disk(&args.flag_dot[0], &ast_base);
    }
    if args.flag_dot.len() > 1 {
        write_dotfile_to_disk(&args.flag_dot[1], &ast_diff);
    }

    // Create a mapping store.
    let store = matcher_config.match_trees(ast_base, ast_diff);

    // If the matcher needs it, set the fingerprint (hash) of each node in the
    // from and to ASTs.
    if args.flag_matcher == Some(Matchers::GumTree) {
        let mut hash_generator = fingerprint::Md5HashGenerator::new();
        fingerprint::apply_fingerprint_both(&mut hash_generator, &store);
    }

    // Generate mappings between ASTs.
    if args.flag_map.is_some() {
        let map_file = args.flag_map.unwrap();
        info!("Creating dot representation of mapping {:?}.", map_file);
        write_dotfile_to_disk(&map_file, &store);
    }

    // Generate edit script.
    // For now we ignore the content of args.flag_edit because we only
    // implement one edit script generation algorithm.
    info!("Selecting the Chawathe et al. (1996) edit script generator.");

    // Edit script generator configuration object.
    let generator_config: Box<edit_script::EditScriptGenerator<String>> =
        generator_script_config(args.flag_edit);

    // There are only 2 enum
    // 1) Chawathe96Config
    // 2) Chawathe98Config
    fn generator_script_config(input: Option<EditScriptGenerator>)
                               -> Box<edit_script::EditScriptGenerator<String>> {
        let config: Box<edit_script::EditScriptGenerator<String>>;
        match input {
            Some(EditScriptGenerator::Chawathe96) | None => {
                config = Box::new(edit_script::Chawathe96Config::new());
            }
            Some(EditScriptGenerator::Chawathe98) => {
                config = Box::new(edit_script::Chawathe98Config::new());
            }
        }
        config
    }

    let edit_script = match generator_config.generate_script(&store) {
        Ok(script) => script,
        Err(err) => consume_edit_script_err(&err),
    };
    if args.flag_store.is_some() {
        let edit_file = args.flag_store.unwrap();
        info!("Creating dot representation of edit script {:?}.",
              edit_file);
        write_dotfile_to_disk(&edit_file, &store);
    }

    // Generate output.
    if args.flag_output.is_none() || args.flag_output == Some(Output::Terminal) {
        info!("Writing terminal output to STDOUT.");
        consume_emitter_err(emitters::write_diff_to_stdout(&store,
                                                           &edit_script,
                                                           &args.arg_base_file,
                                                           &args.arg_diff_file),
                            &args.arg_base_file);
    } else if args.flag_output == Some(Output::None) {
        info!("No output requested by the user.");
        return;
    } else if args.flag_output == Some(Output::JSON) {
        info!("Writing JSON output to STDOUT.");
        consume_emitter_err(emitters::write_json_to_stream(Box::new(stdout()),
                                                           &store,
                                                           &edit_script),
                            "STDOUT");
    }

    debug!("Final 'from' AST:\n{:?}", store.from_arena.borrow());
    debug!("Final 'to' AST:\n{:?}", store.to_arena.borrow());
}
