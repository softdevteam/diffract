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

/// Compute the similarity of two subtrees in a `MappingStore`.

use std::collections::HashSet;
use std::fmt::Debug;

use ast::{FromNodeId, NodeId, ToNodeId};
use matchers::MappingStore;

impl<T: Clone + Debug + Eq + ToString + 'static> MappingStore<T> {
    /// Dice measure of similarity between subtrees.
    pub fn dice_sim(&self, from: &NodeId<FromNodeId>, to: &NodeId<ToNodeId>) -> f64 {
        let n_from = from.breadth_first_traversal(&self.from_arena.borrow())
                         .collect::<Vec<NodeId<FromNodeId>>>()
                         .len() as f64;
        let n_to =
            to.breadth_first_traversal(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>()
              .len() as f64;
        let dice = 2.0 * f64::from(self.num_common_descendants(from, to)) / (n_from + n_to);
        debug_assert!(dice >= 0. && dice <= 1.);
        dice
    }

    /// Jaccard measure of similarity between subtrees.
    pub fn jaccard_sim(&self, from: &NodeId<FromNodeId>, to: &NodeId<ToNodeId>) -> f64 {
        let n_from = from.breadth_first_traversal(&self.from_arena.borrow())
                         .collect::<Vec<NodeId<FromNodeId>>>()
                         .len() as f64;
        let n_to =
            to.breadth_first_traversal(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>()
              .len() as f64;
        let common = f64::from(self.num_common_descendants(from, to));
        let jaccard = common / (n_from + n_to - common);
        debug_assert!(jaccard >= 0. && jaccard <= 1.);
        jaccard
    }

    /// Measure of similarity between subtrees Described in Chawathe et al. (1996).
    pub fn chawathe_sim(&self, from: &NodeId<FromNodeId>, to: &NodeId<ToNodeId>) -> f64 {
        let n_from = from.breadth_first_traversal(&self.from_arena.borrow())
                         .collect::<Vec<NodeId<FromNodeId>>>()
                         .len() as f64;
        let n_to =
            to.breadth_first_traversal(&self.to_arena.borrow()).collect::<Vec<NodeId<ToNodeId>>>()
              .len() as f64;
        let common = f64::from(self.num_common_descendants(from, to));
        let chawathe = common / n_from.max(n_to);
        debug_assert!(chawathe >= 0. && chawathe <= 1.);
        chawathe
    }

    /// Find the number of "common" descendants in two matched subtrees.
    ///
    /// Two nodes are common if they have already been matched.
    fn num_common_descendants(&self, from: &NodeId<FromNodeId>, to: &NodeId<ToNodeId>) -> u32 {
        let mut dst_desc = HashSet::new();
        for node in to.breadth_first_traversal(&self.to_arena.borrow()) {
            dst_desc.insert(node);
        }
        let mut common = 0;
        let mut to: Option<NodeId<ToNodeId>>;
        for node in from.descendants(&self.from_arena.borrow()) {
            to = self.get_to(&node);
            if to.is_some() && dst_desc.contains(&to.unwrap()) {
                common += 1;
            }
        }
        common
    }
}

#[cfg(test)]
mod tests {
    use ast::Arena;
    use super::*;
    use test_common::{create_mult_arena, create_plus_arena};

    #[test]
    fn num_common_descendants() {
        let mult = create_mult_arena();
        let plus = create_plus_arena();
        let store = MappingStore::new(plus, Arena::<&'static str, ToNodeId>::from(mult));
        store.push(NodeId::new(0), NodeId::new(2), &Default::default());
        store.push(NodeId::new(1), NodeId::new(3), &Default::default());
        store.push(NodeId::new(2), NodeId::new(4), &Default::default());
        assert_eq!(2,
                   store.num_common_descendants(&NodeId::new(0), &NodeId::new(2)));
        assert_eq!(2,
                   store.num_common_descendants(&NodeId::new(0), &NodeId::new(0)));
        assert_eq!(0,
                   store.num_common_descendants(&NodeId::new(1), &NodeId::new(0)));
    }
}
