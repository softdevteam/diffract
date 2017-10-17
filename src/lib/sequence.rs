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

use std::cmp::{max, min};

use ast::{Arena, NodeId};

/// Compute the longest common subsequence of two sequences.
///
/// This implementation is designed to work on sequences of `NodeId`s from two
/// different arenas. The function returns a vector of `(NodeId, NodeId)` pairs,
/// which are the locations of mapped nodes from the two arenas.
pub fn lcss<T: Clone + Eq>(seq1: &[NodeId],
                           arena1: &Arena<T>,
                           seq2: &[NodeId],
                           arena2: &Arena<T>,
                           eq: &Fn(&NodeId, &Arena<T>, &NodeId, &Arena<T>) -> bool)
                           -> Vec<(NodeId, NodeId)> {
    let mut lcss: Vec<(NodeId, NodeId)> = vec![];
    if seq1.is_empty() || seq2.is_empty() {
        return lcss;
    }
    let mut grid = Vec::with_capacity(seq1.len() + 1);
    for _ in 0..seq1.len() + 1 {
        grid.push(vec![0; seq2.len() + 1]);
    }
    debug_assert_eq!(seq1.len() + 1,
                     grid.len(),
                     "Cost matrix not sized correctly.");
    debug_assert_eq!(seq2.len() + 1,
                     grid[0].len(),
                     "Cost matrix not sized correctly.");
    for (i, n1) in seq1.iter().enumerate() {
        for (j, n2) in seq2.iter().enumerate() {
            if eq(n1, arena1, n2, arena2) {
                grid[i + 1][j + 1] = 1 + grid[i][j];
            } else {
                grid[i + 1][j + 1] = max(grid[i + 1][j], grid[i][j + 1]);
            }
        }
    }
    let mut i = seq1.len();
    let mut j = seq2.len();
    while i != 0 && j != 0 {
        if grid[i][j] == grid[i - 1][j] {
            i -= 1;
        } else if grid[i][j] == grid[i][j - 1] {
            j -= 1;
        } else {
            lcss.push((seq1[i - 1], seq2[j - 1]));
            i -= 1;
            j -= 1;
        }
    }
    lcss.reverse();
    lcss
}

/// Three-way minimum.
fn min3<T: Ord>(v1: T, v2: T, v3: T) -> T {
    min(min(v1, v2), v3)
}

/// Levenshtein distance with equal weights for deletion, insertion and substitution.
pub fn levenshtein<T: Eq + PartialEq + ToString>(s1: &T, s2: &T) -> usize {
    if s1 == s2 {
        return 0;
    }
    let string1 = s1.to_string();
    let string2 = s2.to_string();
    if string1.is_empty() {
        return string2.len();
    }
    if string2.is_empty() {
        return string1.len();
    }
    let w1 = string1.chars().collect::<Vec<_>>();
    let w2 = string2.chars().collect::<Vec<_>>();
    // Compute action costs.
    let mut costs = Vec::with_capacity(w2.len() + 1);
    for j in 0..w2.len() + 1 {
        costs.push(vec![0 as usize; w1.len() + 1]);
        costs[j][0] = j;
    }
    debug_assert_eq!(w2.len() + 1,
                     costs.len(),
                     "Cost matrix not sized correctly.");
    debug_assert_eq!(w1.len() + 1,
                     costs[0].len(),
                     "Cost matrix not sized correctly.");
    for i in 0..w1.len() + 1 {
        costs[0][i] = i;
    }
    for j in 1..w2.len() + 1 {
        for i in 1..w1.len() + 1 {
            let cost = if w1[i - 1] == w2[j - 1] { 0 } else { 1 };
            let val = min3(costs[j][i - 1] + 1, // Deletion.
                           costs[j - 1][i] + 1, // Insertion.
                           costs[j - 1][i - 1] + cost); // Substitution.
            costs[j][i] = val;
        }
    }
    costs[w2.len()][w1.len()]
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Debug;
    use matchers::MappingStore;

    fn eq<T: Clone + Debug + Eq>(n1: &NodeId,
                                 arena1: &Arena<T>,
                                 n2: &NodeId,
                                 arena2: &Arena<T>)
                                 -> bool {
        arena1[*n1].label == arena2[*n2].label && arena1[*n1].value == arena2[*n2].value
    }

    fn assert_sequence_correct<T: Clone + Debug + Eq>(store: MappingStore<T>, expected: &[T]) {
        if expected.is_empty() {
            assert!(lcss(&vec![], &store.from_arena, &vec![], &store.to_arena, &eq).is_empty());
            return;
        }
        assert!(!store.from_arena.is_empty());
        assert!(!store.to_arena.is_empty());
        let base_root = store.from_arena.root().unwrap();
        let base = base_root.children(&store.from_arena).collect::<Vec<NodeId>>();
        let diff_root = store.to_arena.root().unwrap();
        let diff = diff_root.children(&store.to_arena).collect::<Vec<NodeId>>();
        let longest = lcss(&base, &store.from_arena, &diff, &store.to_arena, &eq);
        for (i, value) in longest.iter().enumerate() {
            assert_eq!(expected[i], store.from_arena[value.0].value);
            assert_eq!(expected[i], store.to_arena[value.1].value);
        }
    }

    fn create_mapping_store<T: Clone + Default + Debug + Eq + 'static>(base: &[T],
                                                                       diff: &[T])
                                                                       -> MappingStore<T> {
        let mut base_arena: Arena<T> = Arena::new();
        let mut id: NodeId;
        if !base.is_empty() {
            let root = base_arena.new_node(Default::default(),
                                           String::from("NULL"),
                                           0,
                                           None,
                                           None,
                                           None,
                                           None);
            for value in base {
                id = base_arena
                    .new_node(value.clone(), String::from("T"), 0, None, None, None, None);
                id.make_child_of(root, &mut base_arena).unwrap();

            }
        }
        let mut diff_arena: Arena<T> = Arena::new();
        if !diff.is_empty() {
            let root = diff_arena.new_node(Default::default(),
                                           String::from("NULL"),
                                           0,
                                           None,
                                           None,
                                           None,
                                           None);
            for value in diff {
                id = diff_arena
                    .new_node(value.clone(), String::from("T"), 0, None, None, None, None);
                id.make_child_of(root, &mut diff_arena).unwrap();
            }
        }
        let store = MappingStore::new(base_arena, diff_arena);
        store
    }

    #[test]
    fn lcss_chimpanzee() {
        let v1 = "HUMAN".chars().collect::<Vec<char>>();
        let v2 = "CHIMPANZEE".chars().collect::<Vec<char>>();
        let expected = vec!['H', 'M', 'A', 'N'];
        let store1 = create_mapping_store(&v1, &v2);
        assert_sequence_correct(store1, &expected);
        let store2 = create_mapping_store(&v2, &v1);
        assert_sequence_correct(store2, &expected);
    }

    #[test]
    fn lcss_empty() {
        let empty: Vec<String> = vec![];
        let store = create_mapping_store(&empty, &empty);
        assert_sequence_correct(store, &empty);
    }

    #[test]
    fn lcss_same() {
        let same = "THE VERY SAME TWO STRINGS.".chars().collect::<Vec<char>>();
        let store = create_mapping_store(&same, &same);
        assert_sequence_correct(store, &same);
    }

    #[test]
    fn lcss_dna() {
        let v1 = "AAACCGTGAGTTATTCGTTCTAGAA".chars().collect::<Vec<char>>();
        let v2 = "CACCCCTAAGGTACCTTTGGTTC".chars().collect::<Vec<char>>();
        let expected = "ACCTAGTATTGTTC".chars().collect::<Vec<char>>();
        let store1 = create_mapping_store(&v1, &v2);
        assert_sequence_correct(store1, &expected);
        let store2 = create_mapping_store(&v2, &v1);
        assert_sequence_correct(store2, &expected);
    }

    #[test]
    fn lcss_num() {
        let v1 = vec![1, 2, 3, 4, 1];
        let v2 = vec![3, 4, 1, 2, 1, 3];
        let expected1 = vec![1, 2, 3];
        let expected2 = vec![3, 4, 1];
        let store1 = create_mapping_store(&v1, &v2);
        assert_sequence_correct(store1, &expected1);
        let store2 = create_mapping_store(&v2, &v1);
        assert_sequence_correct(store2, &expected2);
    }

    #[test]
    fn lcss_wiki() {
        let v1 = "XMJYAUZ".chars().collect::<Vec<char>>();
        let v2 = "MZJAWXU".chars().collect::<Vec<char>>();
        let expected = vec!['M', 'J', 'A', 'U'];
        let store = create_mapping_store(&v1, &v2);
        assert_sequence_correct(store, &expected);
    }

    #[test]
    fn levenshtein_same() {
        assert_eq!(0,
                   levenshtein(&String::from("kitten"), &String::from("kitten")));
        assert_eq!(0, levenshtein(&String::from("test"), &String::from("test")));
        assert_eq!(0, levenshtein(&String::from(""), &String::from("")));
    }

    #[test]
    fn levenshtein_empty() {
        assert_eq!(0, levenshtein(&String::from(""), &String::from("")));
        assert_eq!(7, levenshtein(&String::from(""), &String::from("sitting")));
        assert_eq!(6, levenshtein(&String::from("kitten"), &String::from("")));
        assert_eq!(4, levenshtein(&String::from(""), &String::from("test")));
        assert_eq!(4, levenshtein(&String::from("tent"), &String::from("")));
        assert_eq!(6, levenshtein(&String::from(""), &String::from("gambol")));
        assert_eq!(5, levenshtein(&String::from("gumbo"), &String::from("")));
    }

    #[test]
    fn levenshtein_non_empty() {
        assert_eq!(1, levenshtein(&String::from("tent"), &String::from("test")));
        assert_eq!(2, levenshtein(&String::from("book"), &String::from("back")));
        assert_eq!(2,
                   levenshtein(&String::from("gumbo"), &String::from("gambol")));
        assert_eq!(3,
                   levenshtein(&String::from("saturday"), &String::from("sunday")));
        assert_eq!(3,
                   levenshtein(&String::from("kitten"), &String::from("sitting")));
        assert_eq!(6,
                   levenshtein(&String::from("YHCQPGK"), &String::from("LAHYQQKPGKA")));
        assert_eq!(8,
                   levenshtein(&String::from("rosettacode"), &String::from("raisethysword")));
        assert_eq!(8,
                   levenshtein(&String::from("raisethysword"), &String::from("rosettacode")));
    }
}
