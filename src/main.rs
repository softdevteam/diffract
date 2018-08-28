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
#[macro_use]
extern crate serde_derive;

use std::fs::canonicalize;
use std::io::{stderr, stdout, Write};
use std::path::{Path, PathBuf};
use std::{env, process};

use docopt::Docopt;

extern crate diffract;
use diffract::ast;
use diffract::chawathe98_matcher;
use diffract::edit_script;
use diffract::emitters;
use diffract::fingerprint;
use diffract::gt_matcher;
use diffract::matchers::MatchTrees;
use diffract::myers_matcher;
use diffract::parser;
use diffract::zs_matcher;

const USAGE: &str = "
Usage: diffract [options] <src-file> <dst-file>
       diffract [options] <src-file> <dst-file> -d FILE ...
       diffract [options] <src-file> <dst-file> -g FILE ...
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
                        values for ALGO are: GumTree (default), Myers, ZhangShasha.
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

#[derive(Clone, Copy, Debug, Deserialize)]
enum DebugLevel {
    Debug,
    Error,
    Info,
    Trace,
    Warn
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
enum EditScriptGenerator {
    Chawathe96, // Default.
    Chawathe98
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
enum Matchers {
    GumTree, // Default.
    Myers,
    ZhangShasha
}

#[derive(Debug, Deserialize, Eq, PartialEq)]
enum Output {
    Terminal, // Default.
    JSON,
    None
}

#[derive(Debug, Deserialize)]
struct Args {
    arg_src_file: String,
    arg_dst_file: String,
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
    flag_version: bool
}

fn exit_with_message(message: &str) -> ! {
    writeln!(&mut stderr(), "{}", message).ok();
    process::exit(1);
}

fn write_dotfile_to_disk<T: diffract::emitters::RenderDotfile>(filepath: &str, object: &T) {
    emitters::write_dotfile_to_disk(filepath, object).map_err(|ref err| {
                                                                  exit_with_message(&format!("{}",
                                                                                             err))
                                                              })
                                                     .unwrap();
}

// Set any global constants requested by the user.
// This should be the ONLY block of code that mutates these values.
fn process_matcher_configs(args: &Args,
                           ast_src: &ast::Arena<String, ast::SrcNodeId>,
                           ast_dst: &ast::Arena<String, ast::DstNodeId>)
                           -> Box<MatchTrees<String>> {
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
    } else if args.flag_matcher == Some(Matchers::ZhangShasha) {
        info!("Selecting the Zhang-Shasha matching algorithm.");
        config = Box::new(zs_matcher::ZhangShashaConfig::new(ast_src, ast_dst));
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
    let dummy_src: ast::Arena<String, ast::SrcNodeId> = ast::Arena::new();
    let zs: Box<MatchTrees<u16>> =
        Box::new(zs_matcher::ZhangShashaConfig::new(&dummy_src, &ast::Arena::new()));
    descriptions.push(format!("{}\n-----{}\n", "Myers:", myers.describe()));
    descriptions.push(format!("{}\n-------{}\n", "GumTree", gt.describe()));
    descriptions.push(format!("{}\n-------{}\n", "Zhang-Shasha", zs.describe()));
    descriptions.join("\n")
}

fn parse_file<T: Copy + PartialEq>(filename: &str,
                                   lexer_path: &PathBuf,
                                   yacc_path: &PathBuf)
                                   -> ast::Arena<String, T> {
    parser::parse_file::<T>(filename, lexer_path, yacc_path).map_err(|ref err| {
                                                                         exit_with_message(&format!("{}", err))
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
/// by the `treedst::ast::parse_file` in order to give better error messages.
/// Will cause diffract to exit if user-requested lexer / parser does not exist.
fn get_parsers(args: &Args) -> (PathBuf, PathBuf, PathBuf, PathBuf) {
    // TODO: create a HashMap of file extensions -> lex/yacc files.
    match env::current_exe() {
        Ok(p) => p,
        Err(_) => exit_with_message("Cannot determine which directory the executable is in.")
    };

    match Path::new(&args.arg_src_file).extension() {
        Some(_) => (),
        None => exit_with_message(&format!("Cannot determine file type of {}.", args.arg_src_file))
    };
    // Lexer path for first input file.
    let lexer1 = if !args.flag_grammar.is_empty() {
        canonicalize(format!("{}.l", &args.flag_grammar[0])).unwrap()
    } else {
        parser::get_lexer(&args.arg_src_file)
    };
    // Parser path for first input file.
    let parser1 = if !args.flag_grammar.is_empty() {
        canonicalize(format!("{}.y", &args.flag_grammar[0])).unwrap()
    } else {
        parser::get_parser(&args.arg_src_file)
    };
    if !args.flag_grammar.is_empty() {
        if !Path::new(&lexer1).exists() {
            exit_with_message(&format!("Requested lexer {:?} does not exist.", lexer1));
        }
        if !Path::new(&parser1).exists() {
            exit_with_message(&format!("Requested parser {:?} does not exist.", parser1));
        }
    }

    match Path::new(&args.arg_dst_file).extension() {
        Some(_) => (),
        None => exit_with_message(&format!("Cannot determine file type of {}.", args.arg_dst_file))
    };
    // Lexer path for second input file.
    let lexer2 = if !args.flag_grammar.is_empty() && args.flag_grammar.len() > 1 {
        canonicalize(format!("{}.l", &args.flag_grammar[1])).unwrap()
    } else {
        parser::get_lexer(&args.arg_dst_file)
    };
    // Parser path for second input file.
    let parser2 = if !args.flag_grammar.is_empty() && args.flag_grammar.len() > 1 {
        canonicalize(format!("{}.y", &args.flag_grammar[1])).unwrap()
    } else {
        parser::get_parser(&args.arg_dst_file)
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
    // Parse command-line arguments.
    let args: Args = Docopt::new(USAGE).and_then(|d| d.deserialize())
                                       .unwrap_or_else(|e| e.exit());
    // If the user only asked for documentation, provide it and exit.
    process_doc_flags(&args);

    // Set up logging.
    if let Some(l) = args.flag_debug {
        env::set_var("RUST_LOG", &format!("{:?}", l).to_lowercase());
    }
    env_logger::init();

    let (lexer1, parser1, lexer2, parser2) = get_parsers(&args);

    // Parse both input files.
    let ast_src = parse_file::<ast::SrcNodeId>(&args.arg_src_file, &lexer1, &parser1);
    let ast_dst = parse_file::<ast::DstNodeId>(&args.arg_dst_file, &lexer2, &parser2);

    // Matcher configuration object.
    let mut matcher_config: Box<MatchTrees<String>> =
        process_matcher_configs(&args, &ast_src, &ast_dst);

    // Dump ASTs to STDOUT, if requested.
    if args.flag_ast {
        println!("{:?}", ast_src);
        println!("{:?}", ast_dst);
    }

    // Generate graphviz file(s), if requested.
    if !args.flag_dot.is_empty() {
        info!("Creating dot representation of AST {:?}.", args.flag_dot);
        write_dotfile_to_disk(&args.flag_dot[0], &ast_src);
    }
    if args.flag_dot.len() > 1 {
        write_dotfile_to_disk(&args.flag_dot[1], &ast_dst);
    }
    // Check for Chawathe98

    match args.flag_edit {
        Some(EditScriptGenerator::Chawathe96) | None => {}
        Some(EditScriptGenerator::Chawathe98) => {
            let ast_src_clone = ast_src.clone();
            let ast_dst_clone = ast_dst.clone();
            fn input_from_user() -> Vec<usize> {
                println!("If the wrong values are entered, default values would be taken");
                print!("\nPlease enter cost for edges: Insert, Delete, Update, Move, Copy, Glue: ");
                let mut user_input = String::new();
                let _ = stdout().flush();
                std::io::stdin().read_line(&mut user_input)
                                .expect("Did not enter a correct string");
                if let Some('\n') = user_input.chars().next_back() {
                    user_input.pop();
                }
                if let Some('\r') = user_input.chars().next_back() {
                    user_input.pop();
                }
                // Get all the integer input from the user. Split by space.
                let vec_cost: Vec<usize> = user_input.split(' ')
                                                     .filter_map(|x| x.parse::<usize>().ok())
                                                     .collect();
                // Keep only values which are greater than zero
                println!("What user has entered => {:?}", vec_cost);
                vec_cost
            }

            let vec_cost = input_from_user();
            // When user inputs right values
            let cost = if vec_cost.len() == 6 {
                chawathe98_matcher::CostEdge::new(vec_cost[0],
                                                  vec_cost[1],
                                                  vec_cost[2],
                                                  vec_cost[3],
                                                  vec_cost[4],
                                                  vec_cost[5],
                                                  0,
                                                  0)
            } else {
                chawathe98_matcher::CostEdge::new(1, 1, 1, 1, 1, 1, 0, 0)
            };
            // Chawathe98 matcher does not yet have a way to generate a textual dst.
            chawathe98_matcher::chawathe_matching_actual(ast_src_clone, ast_dst_clone, cost)
                .expect("Chawathe98 matcher could not generate edit script.");
        }
    }

    // Create a mapping store.
    let store = matcher_config.match_trees(ast_src, ast_dst);

    // If the matcher needs it, set the fingerprint (hash) of each node in the
    // src and dst ASTs.
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

    let edit_script = generator_config.generate_script(&store)
                                      .map_err(|ref err| exit_with_message(&format!("{}", err)))
                                      .unwrap();

    if args.flag_store.is_some() {
        let edit_file = args.flag_store.unwrap();
        info!("Creating dot representation of edit script {:?}.",
              edit_file);
        write_dotfile_to_disk(&edit_file, &store);
    }

    // Generate output.
    if args.flag_output.is_none() || args.flag_output == Some(Output::Terminal) {
        info!("Writing terminal output to STDOUT.");
        emitters::write_diff_to_stdout(&store,
                                       &edit_script,
                                       &args.arg_src_file,
                                       &args.arg_dst_file).map_err(|ref err| { exit_with_message(&format!("{}", err)) }).unwrap();
    } else if args.flag_output == Some(Output::None) {
        info!("No output requested by the user.");
        return;
    } else if args.flag_output == Some(Output::JSON) {
        info!("Writing JSON output to STDOUT.");
        emitters::write_json_to_stream(Box::new(stdout()),
                                       &store,
                                       &edit_script).map_err(|ref err| { exit_with_message(&format!("{}", err)) }).unwrap();
    }

    debug!("Final 'src' AST:\n{:?}", store.src_arena.borrow());
    debug!("Final 'dst' AST:\n{:?}", store.dst_arena.borrow());
}
