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

use std::fmt::Debug;

use ast::{Arena, DstNodeId, NodeId, SrcNodeId};
use matchers::{has_same_type_and_label, MappingStore, MappingType, MatchTrees};
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

impl<T: Clone + Debug + Eq + ToString + 'static> MatchTrees<T> for MyersConfig {
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
    fn match_trees(&mut self,
                   src: Arena<T, SrcNodeId>,
                   dst: Arena<T, DstNodeId>)
                   -> MappingStore<T> {
        let store = MappingStore::new(src, dst);
        if store.src_arena.borrow().is_empty() || store.dst_arena.borrow().is_empty() {
            return store;
        }
        let src_pre = store.src_arena
                           .borrow()
                           .root()
                           .unwrap()
                           .pre_order_traversal(&store.src_arena.borrow())
                           .collect::<Vec<NodeId<SrcNodeId>>>();
        let dst_pre = store.dst_arena
                           .borrow()
                           .root()
                           .unwrap()
                           .pre_order_traversal(&store.dst_arena.borrow())
                           .collect::<Vec<NodeId<DstNodeId>>>();

        let longest = lcss(&src_pre,
                           &store.src_arena.borrow(),
                           &dst_pre,
                           &store.dst_arena.borrow(),
                           &has_same_type_and_label);
        for &(n1, n2) in &longest {
            store.push(n1, n2, &MappingType::ANCHOR);
        }
        store
    }
}
