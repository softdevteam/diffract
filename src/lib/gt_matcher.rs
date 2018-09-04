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
use std::collections::HashSet;

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
        GumTreeConfig { max_size: 1000,
                        min_dice: 0.3,  // FIXME Used for Jaccard?
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
        get_gt_anchor_mappings(&store, self);
        get_gt_container_mappings(&store, self);
        store
    }
}

// Top-down algorithm to find isomorphic subtrees of decreasing height.
fn get_gt_anchor_mappings<T: Clone + Debug + Eq + ToString + 'static>(store: &MappingStore<T>,
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
    while !src_q.is_empty()
          && !dst_q.is_empty()
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

// Bottom-up matcher where two nodes match if their descendants include a large
// number of common anchors. When two nodes match, we apply an optimal algorithm
// to search for additional (recovery) mappings among their descendants.
fn get_gt_container_mappings<T: Clone + Debug + Eq + ToString + 'static>(store: &MappingStore<T>,
                                                                         config: &GumTreeConfig) {
    if store.src_arena.borrow().is_empty() || store.dst_arena.borrow().is_empty() {
        return;
    }
    let mut _src_ids: HashSet<NodeId<SrcNodeId>> = HashSet::new(); // TreeMap in GT
    let mut _dst_ids: HashSet<NodeId<DstNodeId>> = HashSet::new();
    let src_root = store.src_arena.borrow().root().unwrap();
    let dst_root = store.dst_arena.borrow().root().unwrap();
    for node in src_root.post_order_traversal(&store.src_arena.borrow()) {
        if node.is_root(&store.src_arena.borrow()) {
            store.push(node, dst_root, &MappingType::CONTAINER);
            last_chance_match(store, config, node, dst_root);
            break;
        } else if !(store.contains_src(node) || node.is_leaf(&store.src_arena.borrow())) {
            let candidates = get_dst_candidates(store, config, node);
            let mut best: Option<NodeId<DstNodeId>> = None;
            let mut max: f64 = -1.0;
            for candidate in candidates {
                let sim = store.jaccard_sim(node, candidate);
                if sim > max && sim >= f64::from(config.min_dice) {  // FIXME
                    max = sim;
                    best = Some(candidate);
                }
            }
            if best.is_some() {
                last_chance_match(store, config, node, best.unwrap());
                store.push(node, best.unwrap(), &MappingType::CONTAINER);
            }
        }
    }
    // clean();  // FIXME DEFINITION IS:
    // protected void clean() {
    //     for (ITree t : src.getTrees())
    //         if (!mappings.hasSrc(t))
    //             t.setMatched(false);
    //     for (ITree t : dst.getTrees())
    //         if (!mappings.hasDst(t))
    //             t.setMatched(false);
    // }
}

fn get_dst_candidates<T: Clone + Debug + Eq + ToString + 'static>(store: &MappingStore<T>,
                                                                  _config: &GumTreeConfig,
                                                                  src: NodeId<SrcNodeId>)
                                                                  -> Vec<NodeId<DstNodeId>> {
    let mut seeds: Vec<NodeId<DstNodeId>> = vec![];
    for child in src.descendants(&store.src_arena.borrow()) {
        let mapped = store.get_dst(child);
        if mapped.is_some() {
            seeds.push(mapped.unwrap());
        }
    }
    let mut candidates: Vec<NodeId<DstNodeId>> = vec![];
    let mut visited: HashSet<NodeId<DstNodeId>> = HashSet::new();
    for mut seed in seeds {
        while seed.parent(&store.dst_arena.borrow()).is_some() {
            let parent = seed.parent(&store.dst_arena.borrow()).unwrap();
            if visited.contains(&parent) {
                break;
            }
            visited.insert(parent);
            if parent.ty(&store.dst_arena.borrow()) == src.ty(&store.src_arena.borrow())
               && !store.contains_dst(parent)
               && parent.is_root(&store.dst_arena.borrow())
            {
                candidates.push(parent);
            }
            seed = parent;
        }
    }
    candidates
}

// GT comment suggests:
// checks if it is better or not to remove the already found mappings.
fn last_chance_match<T: Clone + Debug + Eq + ToString + 'static>(_store: &MappingStore<T>,
                                                                 _config: &GumTreeConfig,
                                                                 _src: NodeId<SrcNodeId>,
                                                                 _dst: NodeId<DstNodeId>) {
    // private void lastChanceMatch(ITree src, ITree dst) {
    //     ITree cSrc = src.deepCopy();
    //     ITree cDst = dst.deepCopy();
    //     TreeUtils.removeMatched(cSrc);
    //     TreeUtils.removeMatched(cDst);

    //     if (cSrc.getSize() < SIZE_THRESHOLD || cDst.getSize() < SIZE_THRESHOLD) {
    //         Matcher m = new ZsMatcher(cSrc, cDst, new MappingStore());
    //         m.match();
    //         for (Mapping candidate: m.getMappings()) {
    //             ITree left = srcIds.getTree(candidate.getFirst().getId());
    //             ITree right = dstIds.getTree(candidate.getSecond().getId());

    //             if (left.getId() == src.getId() || right.getId() == dst.getId()) {
    //                 //System.err.println("Trying to map already mapped source node.");
    //                 continue;
    //             } else if (!left.isMatchable(right)) {
    //                 //System.err.println("Trying to map not compatible nodes.");
    //                 continue;
    //             } else if (left.getParent().getType() != right.getParent().getType()) {
    //                 //System.err.println("Trying to map nodes with incompatible parents");
    //                 continue;
    //             } else addMapping(left, right);
    //         }
    //     }

    //     for (ITree t : src.getTrees())
    //         t.setMatched(true);
    //     for (ITree t : dst.getTrees())
    //         t.setMatched(true);
    // }
}
