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

use std::fmt::Display;

use ast::{Arena, NodeId};
use matchers::{MappingStore, MappingType, MatchTrees};
use sequence::lcss;

#[derive(Debug, Clone, PartialEq)]
/// The Myers matcher does not require any configuration.
pub struct MyersConfig {}

impl Default for MyersConfig {
    fn default() -> MyersConfig {
        MyersConfig {}
    }
}

impl MyersConfig {
    /// Create a new configuration object, with default values.
    pub fn new() -> MyersConfig {
        Default::default()
    }
}

impl<T: Clone + Display + Eq> MatchTrees<T> for MyersConfig {
    /// Describe this matcher for the user.
    fn describe(&self) -> String {
        let desc = "
This matcher finds the longest common subsequence between two ASTs. Myers is the
default algorithm used by git-diff.

For more information see Myers (1986) An O(ND) Difference Algorithm and its
Variations.";
        String::from(desc)
    }

    /// Match locations in distinct ASTs.
    fn match_trees(&self, base: Arena<T>, diff: Arena<T>) -> MappingStore<T> {
        let mut store = MappingStore::new(base, diff);
        if store.from_arena.is_empty() || store.to_arena.is_empty() {
            return store;
        }
        let base_pre = NodeId::new(0)
            .pre_order_traversal(&store.from_arena)
            .collect::<Vec<NodeId>>();
        let diff_pre = NodeId::new(0)
            .pre_order_traversal(&store.to_arena)
            .collect::<Vec<NodeId>>();

        let longest = lcss(&base_pre,
                           &store.from_arena,
                           &diff_pre,
                           &store.to_arena,
                           &eq);
        for &(n1, n2) in &longest {
            store.push(n1, n2, MappingType::ANCHOR);
        }
        store
    }
}

/// Test that two nodes have the same label and value.
fn eq<T: Clone + Display + Eq>(n1: &NodeId,
                               arena1: &Arena<T>,
                               n2: &NodeId,
                               arena2: &Arena<T>)
                               -> bool {
    arena1[*n1].label == arena2[*n2].label && arena1[*n1].value == arena2[*n2].value
}
