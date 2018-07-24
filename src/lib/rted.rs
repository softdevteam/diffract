// Copyright (c) 2018 King's College London
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
#![allow(dead_code)]
#![allow(unused_variables)]

/// This matcher implements Pawlik and Augsten (2011).
use std::fmt::Debug;

use ast::{Arena, DstNodeId, SrcNodeId};
use info_tree::{InfoIdx, InfoTree, PathIdx};
use label_maps::LabelMap;
use matchers::{MappingStore, MatchTrees};
use std::cmp::max;
use std::rc::Rc;
use std::usize::MAX;

/// Size of cost vectors (insert / delete / match costs).
const COST_SIZE: usize = 3;

/// The RTED matcher needs no configuration.
#[derive(Debug, Clone, PartialEq)]
pub struct RTEDConfig<'a> {
    itree_src: InfoTree<'a>,
    itree_dst: InfoTree<'a>,
    size_src: usize,
    size_dst: usize,
    labels: Rc<LabelMap<'a>>,
    // Strategy vector.
    strategy: Vec<Vec<usize>>,
    // Distances between every pair of subtrees.
    delta: Vec<Vec<f64>>,
    // Distances between every pair of subtrees in the form
    // delta(F, G) - delta(F', G') which is at most 1.0.
    delta_bit: Vec<Vec<u8>>,
    // Stores a forest preorder for a given `i` and `j`.
    ij: Vec<Vec<usize>>,
    cost_v: Vec<Vec<Vec<usize>>>,
    cost_w: Vec<Vec<usize>>,
    // T array from Demaine's algorithm, stores delta(Fv,Gij), v on heavy path.
    t: Vec<Vec<f64>>,
    // Copy values from the `t` vector. This is used when, in a single compute
    // period, values are overwritten before they are read, due to a change of
    // forest ordering.
    t_copy: Vec<Vec<f64>>,
    t_tmp: Vec<Vec<f64>>,
    s: Vec<Vec<f64>>,
    q: Vec<f64>,
    da: f64,
    db: f64,
    dc: f64,
    previous_strategy: usize,
    // Statistics for strategies on left, right and heavy paths, and their sum.
    strategy_stats: [usize; 5],
    cost_del: f64,
    cost_ins: f64,
    cost_match: f64
}

impl<'a> RTEDConfig<'a> {
    /// Create a new configuration object, with default values.
    pub fn new<T: Clone + PartialEq, U: Copy + PartialEq>(src: &'a Arena<T, U>,
                                                          dst: &'a Arena<T, U>,
                                                          cost_del: f64,
                                                          cost_ins: f64,
                                                          cost_match: f64)
                                                          -> RTEDConfig<'a> {
        let label_map = Rc::new(LabelMap::new());
        let mut rted = RTEDConfig { labels: Rc::clone(&label_map),
                                    itree_src: InfoTree::new(src, Rc::clone(&label_map)),
                                    itree_dst: InfoTree::new(dst, Rc::clone(&label_map)),
                                    size_src: src.size(),
                                    size_dst: dst.size(),
                                    strategy: vec![],
                                    delta: vec![],
                                    delta_bit: vec![],
                                    ij: vec![],
                                    cost_v: vec![],
                                    cost_w: vec![],
                                    t: vec![],
                                    t_copy: vec![],
                                    t_tmp: vec![],
                                    s: vec![],
                                    q: vec![],
                                    da: 0.0,
                                    db: 0.0,
                                    dc: 0.0,
                                    previous_strategy: 0,
                                    strategy_stats: [0; 5],
                                    cost_del,
                                    cost_ins,
                                    cost_match };
        let max_size = max(rted.size_src, rted.size_dst);
        for _ in 0..max_size {
            rted.ij.push(vec![0; max_size]);
        }
        for _ in 0..rted.size_src {
            rted.delta.push(vec![0.0; rted.size_dst]);
        }
        for _ in 0..rted.size_src {
            rted.delta_bit.push(vec![0; rted.size_dst]);
        }
        for i in 0..COST_SIZE {
            rted.cost_v.push(vec![]);
            for _ in 0..rted.size_src {
                rted.cost_v[i].push(vec![0; rted.size_dst]);
            }
        }
        for _ in 0..COST_SIZE {
            rted.cost_w.push(vec![0; rted.size_dst]);
        }
        rted.calculate_deltas();
        rted
    }

    // Calculate delta between every leaf in dst and all the nodes in src.
    // Calculate it on both sides: leaves of src and nodes of dst, and
    // leaves of dst and nodes of src.
    fn calculate_deltas(&mut self) {
        let labels_src = &self.itree_src.get_info_vec(InfoIdx::Post2Label);
        let labels_dst = &self.itree_dst.get_info_vec(InfoIdx::Post2Label);
        let sizes_src = &self.itree_src.get_info_vec(InfoIdx::Post2Size);
        let sizes_dst = &self.itree_dst.get_info_vec(InfoIdx::Post2Size);
        for x in 0..sizes_src.len() {
            for y in 0..sizes_dst.len() {
                if labels_src[x] == labels_dst[y] {
                    self.delta_bit[x][y] = 0;
                } else {
                    // If `delta_bit[x][y]` is set, the labels in nodes `x` and
                    // `y` of the two input ASTs differ, and the cost of
                    // transforming one node to the other is `cost_match`.
                    self.delta_bit[x][y] = 1;
                }
                // If both nodes are leaves.
                if sizes_src[x].unwrap() == 1 && sizes_dst[y].unwrap() == 1 {
                    self.delta[x][y] = 0.0;
                } else {
                    if sizes_src[x].unwrap() == 1 {
                        self.delta[x][y] = sizes_dst[y].unwrap() as f64 - 1.0;
                    }
                    if sizes_dst[y].unwrap() == 1 {
                        self.delta[x][y] = sizes_src[x].unwrap() as f64 - 1.0;
                    }
                }
            }
        }
    }

    fn non_normalised_tree_distance(&mut self) -> f64 {
        self.strategy = vec![];
        for _ in 0..self.size_src {
            self.strategy.push(vec![0; self.size_dst]);
        }
        self.compute_optimal_strategy();
        self.compute_dist_using_strategy_array()
    }

    fn set_delta(&mut self, a: usize, b: usize, value: f64, switched: bool) {
        if switched {
            self.delta[b][a] = value;
        } else {
            self.delta[a][b] = value;
        }
    }

    fn set_delta_big(&mut self, a: usize, b: usize, value: u8, switched: bool) {
        if switched {
            self.delta_bit[b][a] = value;
        } else {
            self.delta_bit[a][b] = value;
        }
    }

    fn set_custom_costs(&mut self, cost_del: f64, cost_ins: f64, cost_match: f64) {
        self.cost_del = cost_del;
        self.cost_ins = cost_ins;
        self.cost_match = cost_match;
    }

    fn set_custom_strategy_vector(&mut self, strategy: Vec<Vec<usize>>) {
        self.strategy = strategy;
    }

    fn compute_optimal_strategy(&mut self) {
        let (heavy_min, rev_heavy_min, left_min, rev_left_min) = (0, 0, 0, 0);
        let (right_min, rev_right_min) = (0, 0);
        let mut min;
        let mut strategy;
        let mut parent_src: Option<usize>;
        let mut parent_dst: Option<usize>;
        let node_type_left_src = &self.itree_src.node_type[PathIdx::Left];
        let node_type_left_dst = &self.itree_dst.node_type[PathIdx::Left];
        let node_type_right_src = &self.itree_src.node_type[PathIdx::Right];
        let node_type_right_dst = &self.itree_dst.node_type[PathIdx::Right];
        let node_type_heavy_src = &self.itree_src.node_type[PathIdx::Heavy];
        let node_type_heavy_dst = &self.itree_dst.node_type[PathIdx::Heavy];
        let post2size_src = &self.itree_src.info[InfoIdx::Post2Size];
        let post2size_dst = &self.itree_dst.info[InfoIdx::Post2Size];
        let post2desc_sum_src = &self.itree_src.info[InfoIdx::Post2DescSum];
        let post2desc_sum_dst = &self.itree_dst.info[InfoIdx::Post2DescSum];
        let post2kr_sum_src = &self.itree_src.info[InfoIdx::Post2KRSum];
        let post2kr_sum_dst = &self.itree_dst.info[InfoIdx::Post2KRSum];
        let post2rev_kr_sum_src = &self.itree_src.info[InfoIdx::Post2RevKRSum];
        let post2rev_kr_sum_dst = &self.itree_dst.info[InfoIdx::Post2RevKRSum];
        let post2parent_src = &self.itree_src.info[InfoIdx::Post2Parent];
        let post2parent_dst = &self.itree_dst.info[InfoIdx::Post2Parent];
        self.strategy.clear();
        for _ in 0..self.size_src {
            self.strategy.push(vec![0; self.size_dst]);
        }
        // v represents nodes of src tree in postorder numbering.
        // w represents nodes of dst tree in postorder numbering.
        for v in 0..self.size_src {
            self.cost_w.clear();
            for _ in 0..3 {
                self.cost_w.push(vec![0; self.size_dst]);
            }
            for w in 0..self.size_dst {
                if post2size_dst[w].unwrap() == 1 {
                    self.cost_w[PathIdx::Left as usize][w] = 0;
                    self.cost_w[PathIdx::Right as usize][w] = 0;
                    self.cost_w[PathIdx::Heavy as usize][w] = 0;
                }
                if post2size_src[v].unwrap() == 1 {
                    self.cost_v[PathIdx::Left as usize][v][w] = 0;
                    self.cost_v[PathIdx::Right as usize][v][w] = 0;
                    self.cost_v[PathIdx::Heavy as usize][v][w] = 0;
                }
                // Count the minimum + get the strategy.
                let heavy_min = post2size_src[v].unwrap() * post2desc_sum_dst[w].unwrap()
                                + self.cost_v[PathIdx::Heavy as usize][v][w];
                let rev_heavy_min = post2size_dst[w].unwrap() * post2desc_sum_src[v].unwrap()
                                    + self.cost_w[PathIdx::Heavy as usize][w];
                let left_min = post2size_src[v].unwrap() * post2kr_sum_dst[w].unwrap()
                               + self.cost_v[PathIdx::Left as usize][v][w];
                let rev_left_min = post2size_dst[w].unwrap() * post2kr_sum_src[v].unwrap()
                                   + self.cost_w[PathIdx::Left as usize][w];
                let right_min = post2size_src[v].unwrap() * post2rev_kr_sum_dst[w].unwrap()
                                + self.cost_v[PathIdx::Right as usize][v][w];
                let rev_right_min = post2size_dst[w].unwrap() * post2rev_kr_sum_src[v].unwrap()
                                    + self.cost_w[PathIdx::Right as usize][w];
                let mins = [left_min,
                            right_min,
                            heavy_min,
                            MAX,
                            rev_left_min,
                            rev_right_min,
                            rev_heavy_min];
                min = left_min;
                strategy = 0;
                for (i, m) in mins.iter().enumerate().skip(1) {
                    if *m < min {
                        min = *m;
                        strategy = i;
                    }
                }
                // Store the strategy with the smallest cost.
                self.strategy[v][w] = strategy;
                // Populate the cost vectors.
                parent_src = post2parent_src[v];
                if parent_src.is_some() {
                    if node_type_heavy_src[v] {
                        self.cost_v[PathIdx::Heavy as usize][parent_src.unwrap()][w] +=
                            self.cost_v[PathIdx::Heavy as usize][v][w];
                    } else {
                        self.cost_v[PathIdx::Heavy as usize][parent_src.unwrap()][w] += min;
                    }
                    if node_type_right_src[v] {
                        self.cost_v[PathIdx::Right as usize][parent_src.unwrap()][w] +=
                            self.cost_v[PathIdx::Right as usize][v][w];
                    } else {
                        self.cost_v[PathIdx::Right as usize][parent_src.unwrap()][w] += min;
                    }
                    if node_type_left_src[v] {
                        self.cost_v[PathIdx::Left as usize][parent_src.unwrap()][w] +=
                            self.cost_v[PathIdx::Left as usize][v][w];
                    } else {
                        self.cost_v[PathIdx::Left as usize][parent_src.unwrap()][w] += min;
                    }
                }
                parent_dst = post2parent_dst[w];
                if parent_dst.is_some() {
                    if node_type_heavy_dst[w] {
                        self.cost_w[PathIdx::Heavy as usize][parent_dst.unwrap()] +=
                            self.cost_w[PathIdx::Heavy as usize][w];
                    } else {
                        self.cost_w[PathIdx::Heavy as usize][parent_dst.unwrap()] += min;
                    }
                    if node_type_left_dst[w] {
                        self.cost_w[PathIdx::Left as usize][parent_dst.unwrap()] +=
                            self.cost_w[PathIdx::Left as usize][w];
                    } else {
                        self.cost_w[PathIdx::Left as usize][parent_dst.unwrap()] += min;
                    }
                    if node_type_right_dst[w] {
                        self.cost_w[PathIdx::Right as usize][parent_dst.unwrap()] +=
                            self.cost_w[PathIdx::Right as usize][w];
                    } else {
                        self.cost_w[PathIdx::Left as usize][parent_dst.unwrap()] += min;
                    }
                }
            }
        }
    }

    fn compute_dist_using_strategy_array(&mut self) -> f64 {
        0.0
    }
}

impl<'a, T: Clone + Debug + Eq + ToString + 'static> MatchTrees<T> for RTEDConfig<'a> {
    /// Describe this matcher for the user.
    fn describe(&self) -> String {
        String::from(
                     "
The RTED tree matching algorithm is a left-right-heavy matching algorithm which
is efficient and worst-case optimal. For more information see Pawlik and Augsten
(2011) RTED: A Robust Algorithm For The Tree Edit Distance."
        )
    }

    fn match_trees(&mut self,
                   src: Arena<T, SrcNodeId>,
                   dst: Arena<T, DstNodeId>)
                   -> MappingStore<T> {
        MappingStore::new(src, dst)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_common::{create_mult_arena, create_plus_arena};

    #[test]
    fn test_new_rted_config() {
        let _ = RTEDConfig::new(&create_plus_arena(), &create_mult_arena(), 1.0, 1.0, 1.0);
    }

    #[test]
    fn test_non_normalised_distance() {
        let plus = create_plus_arena();
        let mult = create_mult_arena();
        let mut rted = RTEDConfig::new(&plus, &mult, 1.0, 1.0, 1.0);
        rted.non_normalised_tree_distance();
    }

    // Place-holder test.
    #[test]
    fn test_compute_dist_using_strategy_array() {
        let plus = create_plus_arena();
        let mult = create_mult_arena();
        let mut rted = RTEDConfig::new(&plus, &mult, 1.0, 1.0, 1.0);
        assert_eq!(0.0, rted.compute_dist_using_strategy_array());
    }
}
