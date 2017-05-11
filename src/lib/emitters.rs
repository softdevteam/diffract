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

#![warn(missing_docs)]

use std::borrow::Cow::Owned;
use std::fs::File;
use std::io::{Error, Write};

use dot::{Id, Edges, GraphWalk, Labeller, LabelText, Nodes, render};

use ast::{Arena, EdgeId, NodeId};

/// Errors produced in emitting new data.
pub enum EmitterError {
    /// Could not create a new file.
    CouldNotCreateFile,
    /// Could not write out to a file.
    CouldNotWriteToFile,
}

/// Write out a graphviz file (in dot format) to `filepath`.
pub fn write_dotfile_to_disk(filepath: &str,
                             arena: &Arena<String, String>)
                             -> Result<(), EmitterError> {

    let mut dotfile = File::create(&filepath)
        .map_err(|_| EmitterError::CouldNotCreateFile)?;
    arena
        .render_dotgraph(&mut dotfile)
        .map_err(|_| EmitterError::CouldNotWriteToFile)
}

impl<'a> Labeller<'a, NodeId, EdgeId> for Arena<String, String> {
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
        let label = format!("{} {}", self[*id].ty, self[*id].data);
        LabelText::LabelStr(label.into())
    }
}

impl<'a, T: Clone, U: Clone> GraphWalk<'a, NodeId, EdgeId> for Arena<T, U> {
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

impl Arena<String, String> {
    /// Create a graphviz representation of this arena and write to buffer.
    pub fn render_dotgraph<W: Write>(&self, buffer: &mut W) -> Result<(), Error> {
        render(self, buffer)
    }
}
