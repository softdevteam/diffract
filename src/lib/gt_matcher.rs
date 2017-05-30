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

use std::cmp::min;

use ast::{Arena, DstNodeId, NodeId, SrcNodeId};
use hqueue::HeightQueue;
use matchers::{MappingStore, MappingType, MatchTrees};

#[derive(Debug, Clone, PartialEq)]
/// Variables required by the matcher algorithm, set by the user.
pub struct GumTreeConfig {
    /// Only consider sub-trees for matching if they have a size `< MAX_SIZE`.
    pub max_size: u16,

    /// Only consider sub-trees for matching if they have a dice value `> MIN_DICE`.
    pub min_dice: f32,

    /// Only consider sub-trees for matching if they have a height `< MIN_HEIGHT`.
    pub min_height: u16
}

impl Default for GumTreeConfig {
    fn default() -> GumTreeConfig {
        GumTreeConfig { max_size: 100,
                        min_dice: 0.3,
                        min_height: 2 }
    }
}

impl GumTreeConfig {
    /// Create a new configuration object, with default values.
    pub fn new() -> GumTreeConfig {
        Default::default()
    }
}

impl<T: Clone + Debug + Eq + ToString + 'static> MatchTrees<T> for GumTreeConfig {
    /// Describe this matcher for the user.
    fn describe(&self) -> String {
        let desc = "
This matcher implements the GumTree algorithm, which takes account of move
operations between ASTs.

The GumTree algorithm can be configured with the --min-dice, --max-size and
--min-height switches. See --help for more details.

For more information see Falleri et al. (2014) Find-Grained and Accurate Source
Code Differencing.";
        String::from(desc)
    }

    /// Match locations in distinct ASTs.
    fn match_trees(&mut self,
                   src: Arena<T, SrcNodeId>,
                   dst: Arena<T, DstNodeId>)
                   -> MappingStore<T> {
        let store = MappingStore::new(src, dst);
        if store.src_arena.borrow().size() == 0 || store.dst_arena.borrow().size() == 0 {
            return store;
        }
        get_gt_anchor_matches(&store, self);
        get_gt_other_matches(&store, self);
        store
    }
}

fn get_gt_anchor_matches<T: Clone + Debug + Eq + ToString + 'static>(store: &MappingStore<T>,
                                                                     config: &GumTreeConfig) {
    let mut src_q: HeightQueue<SrcNodeId> = HeightQueue::new();
    let mut dst_q: HeightQueue<DstNodeId> = HeightQueue::new();
    let candidates = MappingStore::new(store.src_arena.borrow().clone(),
                                       store.dst_arena.borrow().clone());
    if store.src_arena.borrow().is_empty() || store.dst_arena.borrow().is_empty() {
        return;
    }
    src_q.push(NodeId::new(0), &store.src_arena.borrow());
    dst_q.push(NodeId::new(0), &store.dst_arena.borrow());
    while !src_q.is_empty() && !dst_q.is_empty()
          && min(src_q.peek_max().unwrap(), dst_q.peek_max().unwrap())
             > u32::from(config.min_height)
    {
        if src_q.peek_max().unwrap() != dst_q.peek_max().unwrap() {
            if src_q.peek_max().unwrap() > dst_q.peek_max().unwrap() {
                for id in src_q.pop() {
                    src_q.open(id, &store.src_arena.borrow());
                }
            } else {
                // FIXME this can't be correct.
                for id in src_q.pop() {
                    src_q.open(id, &store.src_arena.borrow());
                }
            }
        } else {
            // TODO
            let h1 = &src_q.pop();
            let h2 = &dst_q.pop();
            for t1 in h1 {
                for t2 in h2 {
                    if store.is_isomorphic(*t1, *t2) {
                        // FIXME something missing here.
                        candidates.push(*t1, *t2, &MappingType::ANCHOR);
                    }
                }
            }
        }
    }
}

fn get_gt_other_matches<T: Clone + Debug + Eq + ToString + 'static>(store: &MappingStore<T>,
                                                                    _config: &GumTreeConfig) {
    if store.src_arena.borrow().is_empty() || store.dst_arena.borrow().is_empty() {
        return;
    }
    // FIXME
}
