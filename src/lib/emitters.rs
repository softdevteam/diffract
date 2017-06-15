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

#![warn(missing_docs)]

use std::borrow::Cow::Owned;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Debug;
use std::fs::File;
use std::io::{Error, Read, Write};

use dot::{Id, Edges, escape_html, GraphWalk, Labeller, LabelText, Nodes, render};
use term;

use action::{ActionType, EditScript, Patchify};
use ast::{Arena, EdgeId, NodeId};
use matchers::{MappingStore, MappingType};
use patch::{hunkify, Patch};

/// Errors produced in emitting new data.
pub enum EmitterError {
    /// Could not create a new file.
    CouldNotCreateFile,
    /// Could not write out to a file.
    CouldNotWriteToFile,
    /// Could not read from a file.
    CouldNotReadFile,
    /// Could not open an existing file.
    CouldNotOpenFile,
}

/// Result returned by emitters.
pub type EmitterResult = Result<(), EmitterError>;

/// Create a graphviz representation of this type and write to buffer.
pub trait RenderDotfile {
    /// Render a dotfile to `buffer`.
    fn render_dotfile<W: Write>(&self, buffer: &mut W) -> Result<(), Error>;
}

/// Create a JSON representation of this type.
pub trait RenderJson {
    /// Render `self` as a JSON object.
    fn render_json(&self, indent: usize) -> String;
}

/// Write a JSON representation of a `MappingStore` and `EditScript` to stream.
///
/// The JSON format here matches the output of gumtree. Because the format is
/// extremely simple, the JSON is constructed manually.
pub fn write_json_to_stream<T: RenderJson, U: RenderJson>(mut stream: Box<Write>,
                                                          store: &T,
                                                          script: &U)
                                                          -> EmitterResult {
    stream
        .write_all(format!("{{\n{},\n{}\n}}\n",
                           store.render_json(4),
                           script.render_json(4))
                       .as_bytes())
        .map_err(|_| EmitterError::CouldNotWriteToFile)
}

// Map action types to terminal colours.
fn build_colour_map() -> HashMap<ActionType, term::color::Color> {
    let mut map: HashMap<ActionType, term::color::Color> = HashMap::new();
    map.insert(ActionType::DELETE, term::color::BRIGHT_RED);
    map.insert(ActionType::INSERT, term::color::BRIGHT_GREEN);
    map.insert(ActionType::MOVE, term::color::BRIGHT_BLUE);
    map.insert(ActionType::UPDATE, term::color::BRIGHT_YELLOW);
    map
}

// Read file and return its contents or `ParseError`.
fn read_file(path: &str) -> Result<String, EmitterError> {
    let mut f = match File::open(path) {
        Ok(r) => r,
        Err(_) => return Err(EmitterError::CouldNotOpenFile),
    };
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    Ok(s)
}

/// Write diff to STDOUT, highlighting actions in the edit script.
///
/// This function prints the whole file, rather than just the relevant context.
/// Patch format is not supported, and hunk headers (e.g. `@@ -1 +1 @@`) are
/// not printed.
///
/// ## Panics
/// If the terminal foreground cannot be changed or reset by the
/// [term](https://crates.io/crates/term) crate, this function will panic.
/// However, `term` is cross-platform so this failure case should be rare.
pub fn write_diff_to_stdout<T: Clone + Debug + Eq>(store: &MappingStore<T>,
                                                   script: &EditScript<T>,
                                                   from_path: &str,
                                                   to_path: &str)
                                                   -> Result<(), EmitterError> {
    let colours = build_colour_map();
    // Turn edit script and mappings into hunks of related patches.
    let mut from_patches: Vec<Patch> = vec![];
    let mut to_patches: Vec<Patch> = vec![];
    script.patchify(store, &mut from_patches, &mut to_patches);
    // No patches in either AST, so input files must be identical.
    if from_patches.is_empty() && to_patches.is_empty() {
        return Ok(());
    }
    let to_hunks = hunkify(to_patches);
    let from_file = read_file(from_path)?;
    let to_file = read_file(to_path)?;
    let mut hunks: Vec<_> = to_hunks.keys().collect();
    hunks.sort();
    // Process patches on the "from" AST. Only the MOVE and DELETE actions are
    // useful here, so rather than creating a map of hunks in the file,  which
    // would be needed for a side-by-side diff, instead we create a map of the
    // patches that are useful for printing.
    let mut from_actions: BTreeMap<usize, (usize, ActionType)> = BTreeMap::new();
    for patch in &from_patches {
        if *patch.action() == ActionType::DELETE || *patch.action() == ActionType::MOVE {
            from_actions.insert(patch.start(), (patch.end(), patch.action().clone()));
        }
    }
    debug!("{} hunks in to AST {} patches in from AST.",
           hunks.len(),
           from_patches.len());
    // Write out "header" with input file names, similar to git-diff.
    let mut stream = term::stdout().unwrap();
    stream.attr(term::Attr::Bold).unwrap();
    stream.fg(term::color::BRIGHT_MAGENTA).unwrap();
    write!(stream, "--- a/{}\n+++ b/{}\n", from_path, to_path).unwrap();
    stream.reset().unwrap();
    // Iterate over each hunk and print file with context.
    let mut ch = 0;
    for &(start, end) in hunks {
        if ch < start {
            stream.reset().unwrap();
            for i in ch..start {
                // Look for deletions or moves from the "from" AST.
                if from_actions.contains_key(&i) {
                    let (f_end, ref f_ty) = from_actions[&i];
                    stream.fg(colours[f_ty]).unwrap();
                    if *f_ty == ActionType::DELETE {
                        write!(stream, "{}", &from_file[i..f_end]).unwrap();
                    } else {
                        write!(stream, "^").unwrap();
                    }
                    stream.reset().unwrap();
                }
                // Next character from the "to" AST.
                write!(stream, "{}", &to_file[i..i + 1]).unwrap();
            }
        }
        ch = start;
        let hunk = &to_hunks[&(start, end)];
        while ch < end {
            // Look for deletions or moves from the "from" AST.
            if from_actions.contains_key(&ch) {
                let (f_end, ref f_ty) = from_actions[&ch];
                stream.fg(colours[f_ty]).unwrap();
                if *f_ty == ActionType::DELETE {
                    write!(stream, "{}", &from_file[ch..f_end]).unwrap();
                } else {
                    write!(stream, "^").unwrap();
                }
                stream.reset().unwrap();
            }
            // Characters or actions from the "to" AST.
            let action = hunk.get_action(ch);
            if action.is_none() {
                write!(stream, "{}", &to_file[ch..ch + 1]).unwrap();
                ch += 1;
            } else {
                let (ty, len) = action.unwrap();
                stream.fg(colours[&ty]).unwrap();
                stream.attr(term::Attr::Bold).unwrap();
                write!(stream, "{}", &to_file[ch..ch + len]).unwrap();
                stream.reset().unwrap();
                ch += len;
            }
        }
    }
    // Print remainder of file, following the last hunk.
    if !from_patches.is_empty() && ch < to_file.len() {
        for i in ch..to_file.len() {
            // Look for deletions or moves from the "from" AST.
            if from_actions.contains_key(&i) {
                let (f_end, ref f_ty) = from_actions[&i];
                stream.fg(colours[f_ty]).unwrap();
                if *f_ty == ActionType::DELETE {
                    write!(stream, "{}", &from_file[i..f_end]).unwrap();
                } else {
                    write!(stream, "^").unwrap();
                }
                stream.reset().unwrap();
            }
            write!(stream, "{}", &to_file[i..i + 1]).unwrap();
        }
    }
    // Ensure the next shell prompt starts on a new line, otherwise the diff is
    // especially difficult to read.
    if to_file.len() > 1 && &to_file[to_file.len() - 2..to_file.len()] != "\n" {
        write!(stream, "\n").unwrap();
    }
    stream.reset().unwrap();
    Ok(())
}

/// Write out a graphviz file (in dot format) to `filepath`.
pub fn write_dotfile_to_disk<T: RenderDotfile>(filepath: &str, graph: &T) -> EmitterResult {
    let mut stream = File::create(&filepath)
        .map_err(|_| EmitterError::CouldNotCreateFile)?;
    graph
        .render_dotfile(&mut stream)
        .map_err(|_| EmitterError::CouldNotWriteToFile)
}

impl<'a> Labeller<'a, NodeId, EdgeId> for Arena<String> {
    fn graph_id(&self) -> Id {
        Id::new("AST").unwrap()
    }

    fn node_id(&self, id: &NodeId) -> Id {
        // Node ids must be unique dot identifiers, be non-empty strings made up
        // of alphanumeric or underscore characters, not beginning with a digit
        // (i.e. the regular expression [a-zA-Z_][a-zA-Z_0-9]*).
        Id::new(format!("N{}", id)).unwrap()
    }

    fn node_label(&self, id: &NodeId) -> LabelText {
        let label = format!("{} {}",
                            self[*id].label,
                            escape_html(self[*id].value.as_str()));
        LabelText::LabelStr(label.into())
    }
}

impl<'a, T: Clone> GraphWalk<'a, NodeId, EdgeId> for Arena<T> {
    fn nodes(&self) -> Nodes<'a, NodeId> {
        Owned((0..self.size()).map(NodeId::new).collect())
    }

    fn edges(&'a self) -> Edges<'a, EdgeId> {
        Owned(self.get_edges())
    }

    fn source(&self, e: &EdgeId) -> NodeId {
        e.0
    }

    fn target(&self, e: &EdgeId) -> NodeId {
        e.1
    }
}

impl RenderDotfile for Arena<String> {
    fn render_dotfile<W: Write>(&self, buffer: &mut W) -> Result<(), Error> {
        render(self, buffer)
    }
}

impl RenderDotfile for MappingStore<String> {
    // Because the `dot` crate cannot add arbitrary attributes to nodes and
    // edges, or create subgraphs and clusters, we render mapping stores to
    // a dot digraph manually. This produces output similar to the Figure 1
    // of Falleri et al. (2014).
    fn render_dotfile<W: Write>(&self, buffer: &mut W) -> Result<(), Error> {
        let mut digraph = vec![String::from("digraph MappingStore {\n"),
                               String::from("\tratio=fill;\n\tfontsize=16;\n")];
        let mut line: String;
        let mut node: NodeId;
        let mut attrs: &str;
        // Node labels for both ASTs.
        for id in 0..self.from_arena.size() {
            node = NodeId::new(id);
            if !self.contains_from(&node) {
                attrs = ", style=filled, fillcolor=lightgrey";
            } else {
                attrs = "";
            }
            if self.from_arena[node].value.is_empty() {
                digraph.push(format!("\tFROM{}[label=\"{}\"{}];\n",
                                     id,
                                     self.from_arena[node].label,
                                     attrs));
            } else {
                digraph.push(format!("\tFROM{}[label=\"{} {}\"{}];\n",
                                     id,
                                     self.from_arena[node].label,
                                     escape_html(self.from_arena[node].value.as_str()),
                                     attrs));
            }
        }
        for id in 0..self.to_arena.size() {
            node = NodeId::new(id);
            if !self.contains_to(&node) {
                attrs = ", style=filled, fillcolor=lightgrey";
            } else {
                attrs = "";
            }
            if self.to_arena[node].value.is_empty() {
                digraph.push(format!("\tTO{}[label=\"{}\"{}];\n",
                                     id,
                                     self.to_arena[node].label,
                                     attrs));
            } else {
                digraph.push(format!("\tTO{}[label=\"{} {}\"{}];\n",
                                     id,
                                     self.to_arena[node].label,
                                     escape_html(self.to_arena[node].value.as_str()),
                                     attrs));
            }
        }
        // From AST parent relationships.
        digraph.push(String::from("\tsubgraph clusterFROM {\n"));
        digraph.push(String::from("\t\tcolor=white;\n"));
        for (e0, e1) in self.from_arena.get_edges() {
            line = format!("\t\tFROM{} -> FROM{}[style=solid, arrowhead=vee, arrowsize=.75];\n",
                           e0.id(),
                           e1.id());
            digraph.push(line);
        }
        digraph.push(String::from("\t}\n"));
        // To AST parent relationships.
        digraph.push(String::from("\tsubgraph clusterTO {\n"));
        digraph.push(String::from("\t\tcolor=white;\n"));
        for (e0, e1) in self.to_arena.get_edges() {
            line = format!("\t\tTO{} -> TO{}[style=solid, arrowhead=vee, arrowsize=.75];\n",
                           e0.id(),
                           e1.id());
            digraph.push(line);
        }
        digraph.push(String::from("\t}\n"));
        // Mappings between ASTs.
        let common = "dir=both, arrowsize=.75, arrowhead=odot, arrowtail=odot";
        for (from, val) in &self.from {
            let &(to, ref ty) = val;
            attrs = match *ty {
                MappingType::ANCHOR => "[style=dashed, color=blue, ",
                MappingType::CONTAINER => "[style=dashed, color=red, ",
                MappingType::RECOVERY => "[style=dotted, color=green, ",
                MappingType::EDIT => "[style=dotted, color=indigo, ",
            };
            line = format!("\tFROM{} -> TO{}{}{}];\n",
                           from.id(),
                           to.id(),
                           attrs,
                           common);
            digraph.push(line);
        }
        digraph.push(String::from("}\n"));
        // Write dotfile to stream.
        buffer.write_all(digraph.join("").as_bytes())
    }
}
