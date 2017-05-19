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

#![warn(missing_docs)]

use std::borrow::Cow::Owned;
use std::fs::File;
use std::io::{Error, Write};

use dot::{Id, Edges, GraphWalk, Labeller, LabelText, Nodes, render};

use ast::{Arena, EdgeId, NodeId};
use matchers::{MappingStore, MappingType};

/// Errors produced in emitting new data.
pub enum EmitterError {
    /// Could not create a new file.
    CouldNotCreateFile,
    /// Could not write out to a file.
    CouldNotWriteToFile,
}

/// Result returned by emitters.
pub type EmitterResult = Result<(), EmitterError>;

/// Create a graphviz representation of this type and write to buffer.
pub trait RenderDotfile {
    /// Render a dotfile to `buffer`.
    fn render_dotfile<W: Write>(&self, buffer: &mut W) -> Result<(), Error>;
}

/// Write out a graphviz file (in dot format) to `filepath`.
pub fn write_dotfile_to_disk<'a, T: RenderDotfile>(filepath: &str, graph: &T) -> EmitterResult {
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
        let label = format!("{} {}", self[*id].label, self[*id].value);
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

/// Render a mapping store as a Graphviz digraph.
///
/// The `dot` crate cannot add arbitrary attributes to nodes or edges, or create
/// subgraphs. Therefore, we provide this custom function.
pub fn render_mapping_store(store: &MappingStore<String>, filepath: &str) -> EmitterResult {
    let mut digraph = vec![String::from("digraph MappingStore {\n"),
                           String::from("\tratio=fill;\n\tfontsize=16\n")];
    let mut line: String;
    let mut node: NodeId;
    let mut attrs: &str;
    // Node labels for both ASTs.
    for id in 0..store.from.size() {
        node = NodeId::new(id);
        if !store.is_mapped(node, true) {
            attrs = ", style=filled, fillcolor=lightgrey";
        } else {
            attrs = "";
        }
        if store.from[node].value.is_empty() {
            digraph.push(format!("\tFROM{}[label=\"{} {}\"{}];\n",
                                 id,
                                 store.from[node].label,
                                 store.from[node].value,
                                 attrs));
        } else {
            digraph.push(format!("\tFROM{}[label=\"{}\"{}];\n",
                                 id,
                                 store.from[node].label,
                                 attrs));
        }
    }
    for id in 0..store.to.size() {
        node = NodeId::new(id);
        if !store.is_mapped(node, false) {
            attrs = ", style=filled, fillcolor=lightgrey";
        } else {
            attrs = "";
        }
        if store.to[node].value.is_empty() {
            digraph.push(format!("\tTO{}[label=\"{} {}\"{}];\n",
                                 id,
                                 store.to[node].label,
                                 store.to[node].value,
                                 attrs));
        } else {
            digraph.push(format!("\tTO{}[label=\"{}\"{}];\n", id, store.to[node].label, attrs));
        }
    }
    // From AST parent relationships.
    digraph.push(String::from("\tsubgraph clusterFROM {\n"));
    digraph.push(String::from("\t\tcolor=white;\n"));
    for (e0, e1) in store.from.get_edges() {
        line = format!("\t\tFROM{} -> FROM{}[style=solid, arrowhead=vee];\n",
                       e0.id(),
                       e1.id());
        digraph.push(line);
    }
    digraph.push(String::from("\t}\n"));
    // To AST parent relationships.
    digraph.push(String::from("\tsubgraph clusterTO {\n"));
    digraph.push(String::from("\t\tcolor=white;\n"));
    for (e0, e1) in store.to.get_edges() {
        line = format!("\t\tTO{} -> TO{}[style=solid, arrowhead=vee];\n",
                       e0.id(),
                       e1.id());
        digraph.push(line);
    }
    digraph.push(String::from("\t}\n"));
    // Mappings between ASTs.
    for (from, val) in &store.from_map {
        let &(to, ref ty) = val;
        attrs = match *ty {
            MappingType::ANCHOR => "[style=dotted, color=blue, arrowhead=diamond]",
            MappingType::CONTAINER => "[style=dotted, color=red, arrowhead=diamond]",
            MappingType::RECOVERY => "[style=dashed, color=green, arrowhead=diamond]",
        };
        line = format!("\tFROM{} -> TO{}{};\n", from.id(), to.id(), attrs);
        digraph.push(line);
    }
    digraph.push(String::from("}\n"));
    // Write dotfile to disk.
    let mut stream = File::create(&filepath)
        .map_err(|_| EmitterError::CouldNotCreateFile)?;
    stream
        .write_all(digraph.join("").as_bytes())
        .map_err(|_| EmitterError::CouldNotWriteToFile)?;
    Ok(())
}
