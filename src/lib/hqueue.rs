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

use std::cmp::Ordering;
use std::fmt;

use ast::{Arena, NodeId};

/// A `PriorityNodeId` wraps the height of a node with its id.
///
/// This type should be completely opaque to clients of this module.
/// Client code should construct a `HeightQueue` and call its methods,
/// which will return `NodeId`s directly, rather than the `PriorityNodeId`
/// wrapper.
#[derive(Clone, Eq, PartialEq)]
struct PriorityNodeId<T: PartialEq + Copy> {
    index: NodeId<T>,
    height: u32,
}

impl<T: PartialEq + Copy> PriorityNodeId<T> {
    fn new(index: NodeId<T>, height: u32) -> PriorityNodeId<T> {
        PriorityNodeId { index: index,
                         height: height, }
    }

    fn id(&self) -> NodeId<T> {
        self.index
    }

    fn height(&self) -> u32 {
        self.height
    }
}

impl<T: Eq + PartialEq + Copy> Ord for PriorityNodeId<T> {
    fn cmp(&self, other: &PriorityNodeId<T>) -> Ordering {
        self.height.cmp(&other.height)
    }
}

impl<T: Eq + PartialEq + Copy> PartialOrd for PriorityNodeId<T> {
    fn partial_cmp(&self, other: &PriorityNodeId<T>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// A queue of `NodeId`s sorted on the height of their respective nodes.
#[derive(Clone, Eq, PartialEq)]
pub struct HeightQueue<T: PartialEq + Copy> {
    queue: Vec<PriorityNodeId<T>>, // Use Vec so we can call `sort()`.
}

impl<T: PartialEq + Copy> Default for HeightQueue<T> {
    fn default() -> HeightQueue<T> {
        HeightQueue { queue: vec![] }
    }
}

impl<T: fmt::Debug + PartialEq + Copy> fmt::Debug for HeightQueue<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[ ")?;
        for item in &self.queue {
            write!(f, "({:?}, {:?}) ", item.id(), item.height())?;
        }
        write!(f, "]")
    }
}

impl<T: PartialEq + Copy> HeightQueue<T> {
    /// Create empty priority queue.
    pub fn new() -> HeightQueue<T> {
        Default::default()
    }

    /// Remove (and discard) all items in this queue, leaving it empty.
    pub fn clear(&mut self) {
        self.queue.clear();
    }

    /// `true` if this queue is empty, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    /// Get the id of the `Node` with the greatest height in the current queue.
    pub fn peek_max(&self) -> Option<u32> {
        if self.queue.is_empty() {
            return None;
        }
        Some(self.queue[self.queue.len() - 1].height)
    }

    /// Remove information about the tallest node(s) and return their `NodeId`.
    pub fn pop(&mut self) -> Vec<NodeId<T>> {
        let mut nodes = vec![];
        if self.is_empty() {
            return nodes;
        }
        let max = self.queue[self.queue.len() - 1].height;
        while !self.is_empty() && self.queue[self.queue.len() - 1].height == max {
            nodes.push(self.queue.pop().unwrap().id());
        }
        nodes
    }

    /// Push a new node into this priority queue, keeping the queue sorted.
    ///
    /// This method has no effect if the new node is already in the queue.
    pub fn push<U: Clone>(&mut self, index: NodeId<T>, arena: &Arena<U, T>) {
        let height = index.height(arena);
        let new_node = PriorityNodeId::new(index, height);
        if self.queue.contains(&new_node) {
            // Case 1: new node is already in the queue.
            return;
        } else if self.is_empty() || height <= self.queue[0].height() {
            // Case 2: new node is the shortest in the queue.
            self.queue.insert(0, new_node);
        } else if height >= self.queue[self.queue.len() - 1].height() {
            // Case 3: new node is the tallest in the queue.
            self.queue.push(new_node);
        } else {
            // Case 4: new node needs to be somewhere in the middle of the queue.
            for index in 0..self.queue.len() - 1 {
                if self.queue[index].height() <= height && self.queue[index + 1].height() > height {
                    self.queue.insert(index + 1, new_node);
                    return;
                }
            }
        }
    }

    /// Insert all the children of `parent` into this queue, keeping it sorted.
    pub fn open<U: Clone>(&mut self, parent: &NodeId<T>, arena: &Arena<U, T>) {
        let children = parent.children(arena).collect::<Vec<NodeId<T>>>();
        for child in children {
            self.push(child, arena);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::FromNodeId;
    use test::Bencher;

    fn create_arena() -> Arena<&'static str, FromNodeId> {
        let mut arena = Arena::new();
        let root = arena.new_node("Expr", String::from("+"), None, None, None, None);
        let n1 = arena.new_node("INT", String::from("1"), None, None, None, None);
        n1.make_child_of(root, &mut arena).unwrap();
        let n2 = arena.new_node("Expr", String::from("*"), None, None, None, None);
        n2.make_child_of(root, &mut arena).unwrap();
        let n3 = arena.new_node("INT", String::from("3"), None, None, None, None);
        n3.make_child_of(n2, &mut arena).unwrap();
        let n4 = arena.new_node("INT", String::from("4"), None, None, None, None);
        n4.make_child_of(n2, &mut arena).unwrap();
        let format1 = "\"Expr\" +
  \"INT\" 1
  \"Expr\" *
    \"INT\" 3
    \"INT\" 4
";
        assert_eq!(format1, format!("{:?}", arena));
        arena
    }

    // Assert that `queue` is in sorted order and has the same size `arena`.
    fn assert_sorted<T: Clone>(queue: &HeightQueue<FromNodeId>, arena: &Arena<T, FromNodeId>) {
        let mut expected = arena.size();
        if expected == 0 {
            assert!(queue.is_empty());
            return;
        }
        let mut clone = queue.clone();
        let mut tallest: Vec<NodeId<FromNodeId>>;
        loop {
            tallest = clone.pop();
            println!("{:?}", tallest);
            expected -= tallest.len();
            for node in &tallest {
                assert!(node.height(arena) == tallest[0].height(arena));
                if !clone.is_empty() {
                    assert!(node.height(arena) > clone.peek_max().unwrap());
                }
            }
            if clone.is_empty() {
                break;
            }
        }
        assert_eq!(0, expected);
    }

    #[test]
    fn clear() {
        let arena = create_arena();
        let mut queue = arena.get_priority_queue();
        assert!(!queue.is_empty());
        queue.clear();
        assert!(queue.is_empty());
    }

    #[test]
    fn cmp_priority_node() {
        let p0 = PriorityNodeId::<FromNodeId>::new(NodeId::new(0), 0);
        let p1 = PriorityNodeId::<FromNodeId>::new(NodeId::new(0), 1);
        let p2 = PriorityNodeId::<FromNodeId>::new(NodeId::new(0), 2);
        let p3 = PriorityNodeId::<FromNodeId>::new(NodeId::new(0), 2);
        assert!(p0 < p1);
        assert!(p1 < p2);
        assert!(p2 == p3);
        assert!(p0 < p3);
        assert!(p3 > p1);
        assert!(p2 > p1);
        assert!(p3 > p0);
    }

    #[test]
    fn fmt_debug() {
        let arena = create_arena();
        let queue = arena.get_priority_queue();
        let s = format!("{:?}", queue);
        // Three leaves in this arena can be placed in the queue in any order,
        // so we don't check the whole string, we just check the start of the
        // formatted string and the branch nodes at the end.
        let expected = " (NodeId { index: 2 }, 2) (NodeId { index: 0 }, 3) ]";
        assert_eq!("[ (NodeId { index:", s[..18].to_string());
        assert_eq!(expected, s[76..].to_string());
        assert_eq!(128, s.len());
    }

    #[test]
    fn new() {
        assert!(HeightQueue::<FromNodeId>::new().is_empty());
    }

    #[test]
    fn open() {
        let arena = create_arena();
        let mut queue = HeightQueue::<FromNodeId>::new();
        queue.open(&NodeId::new(0), &arena);
        let expected1 = vec![NodeId::new(2)]; // Expr *
        assert_eq!(expected1, queue.pop());
        let expected2 = vec![NodeId::new(1)]; // INT 1
        assert_eq!(expected2, queue.pop());
    }

    #[test]
    fn peek_max() {
        let arena = create_arena();
        let queue = arena.get_priority_queue();
        let height = queue.peek_max().unwrap();
        assert_eq!(NodeId::new(0).height(&arena), height);
    }

    #[test]
    fn pop() {
        let arena = create_arena();
        let mut queue = arena.get_priority_queue();
        assert_eq!(vec![NodeId::new(0)], queue.pop());
        assert_eq!(vec![NodeId::new(2)], queue.pop());
        // Nodes 1, 3, 4 have the same height, and so may be stored in any order.
        let expected = vec![NodeId::new(1), NodeId::new(3), NodeId::new(4)];
        let leaves = queue.pop();
        assert_eq!(expected.len(), leaves.len());
        for leaf in leaves {
            assert!(expected.contains(&leaf));
        }
        assert!(queue.is_empty());
    }

    #[test]
    fn push() {
        let arena = create_arena();
        let queue = arena.get_priority_queue();
        assert_sorted(&queue, &arena);
    }

    #[test]
    fn push_identical_nodes() {
        let arena = create_arena();
        let mut queue = HeightQueue::new();
        queue.push(NodeId::new(0), &arena);
        let formatted = format!("{:?}", queue);
        let expected = "[ (NodeId { index: 0 }, 3) ]";
        assert_eq!(expected, formatted);
        queue.push(NodeId::new(0), &arena); // Should have no effect.
        assert_eq!(expected, formatted);
    }

    const BENCH_ITER: usize = 10000;

    #[bench]
    fn bench_push(bencher: &mut Bencher) {
        let mut arena: Arena<&str, FromNodeId> = Arena::new();
        for _ in 0..BENCH_ITER {
            arena.new_node("", String::from(""), None, None, None, None);
        }
        let mut queue = HeightQueue::new();
        // Because `HeightQueues` are sets, each iteration of this
        // microbenchmark must push a distinct `NodeId` to the queue, to avoid
        // the optimisation that does not attempt to push an existing value to
        // the structure.
        bencher.iter(|| {
                         for id in 0..BENCH_ITER {
                             queue.push(NodeId::new(id), &arena);
                             queue.clear();
                         }
                     });
    }
}
