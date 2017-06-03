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

use std::fmt::Debug;
use std::f64;

use ast::{Arena, FromNodeId, NodeId, ToNodeId};
use matchers::{MappingStore, MatchTrees};
use sequence::levenshtein;

/// The `ZhangShasha` matcher does not require any configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct ZhangShashaConfig {}

impl Default for ZhangShashaConfig {
    fn default() -> ZhangShashaConfig {
        ZhangShashaConfig {}
    }
}

impl ZhangShashaConfig {
    /// Create a new configuration object.
    pub fn new() -> ZhangShashaConfig {
        Default::default()
    }
}

impl<T: Clone + Debug + Eq + ToString + 'static> MatchTrees<T> for ZhangShashaConfig {
    fn describe(&self) -> String {
        let desc = "
The Zhang Shasha tree matching algorithm -- works by attempting to match
successive subtrees. For more information see Zhang and Shasha (1989) Simple
Fast Algorithms for the Editing Distance Between Trees and Related Problems.";
        String::from(desc)
    }

    /// Match locations in distinct ASTs.
    fn match_trees(&self, base: Arena<T, FromNodeId>, diff: Arena<T, ToNodeId>) -> MappingStore<T> {
        MappingStore::new(base, diff)
    }
}

/// Return cost of a deletion action.
fn get_deletion_cost<T: Clone>(_node: NodeId<FromNodeId>, _arena: &Arena<T, FromNodeId>) -> f64 {
    1.0
}

/// Return cost of an insertion action.
fn get_insertion_cost<T: Clone>(_node: &NodeId<FromNodeId>, _arena: &Arena<T, FromNodeId>) -> f64 {
    1.0
}

/// Return cost of an update action.
fn get_update_cost<T: Clone + ToString>(from: &NodeId<FromNodeId>,
                                        from_arena: &Arena<String, FromNodeId>,
                                        to: &NodeId<ToNodeId>,
                                        to_arena: &Arena<T, ToNodeId>)
                                        -> f64 {
    if from_arena[*from].label == to_arena[*to].label {
        let from_s = from_arena[*from].ty.to_string();
        let to_s = to_arena[*to].ty.to_string();
        if from_s.is_empty() && to_s.is_empty() {
            return 1.0;
        } else {
            // In the GT code, this is a q-gram distance.
            return levenshtein(&from_s, &to_s) as f64;
        }
    }
    f64::MAX
}
