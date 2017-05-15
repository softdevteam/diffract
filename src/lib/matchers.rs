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

use ast::{Arena, NodeId};

#[derive(Debug, Clone, Eq, PartialEq)]
/// Mapping between nodes in distinct arenas.
pub struct Mapping {
    /// Source node.
    pub from: NodeId,
    /// Destination node.
    pub to: NodeId,
}

impl Mapping {
    /// Create a new mapping from one index to another.
    /// It is assumed that the `from` and `to` nodes are in different arenas.
    pub fn new(from: usize, to: usize) -> Mapping {
        Mapping {
            from: NodeId::new(from),
            to: NodeId::new(to),
        }
    }
}

/// A store of mappings between nodes in different arenas.
/// Direction is important.
pub struct MappingStore<T: Clone, U: Clone> {
    /// Mappings for the stored arenas.
    pub mappings: Vec<Mapping>,
    /// Source arena (treat as immutable).
    pub from: Arena<T, U>,
    /// Destination arena (treat as immutable).
    pub to: Arena<T, U>,
}

impl<T: Clone, U: Clone> MappingStore<T, U> {
    /// Create a new mapping store.
    fn new(base: Arena<T, U>, diff: Arena<T, U>) -> MappingStore<T, U> {
        MappingStore {
            mappings: vec![],
            from: base,
            to: diff,
        }
    }

    /// Push a new mapping into the store.
    pub fn push(&mut self, from: usize, to: usize) {
        self.mappings.push(Mapping::new(from, to));
    }

    /// True if `mapping` is contained within this store.
    pub fn contains(&self, mapping: Mapping) -> bool {
        self.mappings.contains(&mapping)
    }
}

/// Match locations in distinct ASTs.
pub fn match_trees<'a, T: Clone, U: Clone>(base: Arena<T, U>,
                                           diff: Arena<T, U>)
                                           -> MappingStore<T, U> {
    let mut store = MappingStore::new(base, diff);
    if store.from.size() == 0 || store.to.size() == 0 {
        return store;
    }
    // TODO: Implement classic GumTree matcher algorithm.
    store.push(0, 0);
    store
}
