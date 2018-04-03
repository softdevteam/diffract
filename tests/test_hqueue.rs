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

//! Integration tests for hqueue module.
//! All file paths are relative to the root of the repository.

extern crate diffract;

use diffract::ast::{Arena, FromNodeId, NodeId};
use diffract::hqueue::HeightQueue;
use diffract::parser::{get_lexer, get_parser, parse_file};

// Assert that `queue` is in sorted order and has the same size `arena`.
fn assert_sorted<T: Clone + PartialEq>(queue: &HeightQueue<FromNodeId>,
                                       arena: &Arena<T, FromNodeId>) {
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
            assert_eq!(node.height(arena), tallest[0].height(arena));
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

fn assert_sorted_from_file(filepath: &str) {
    let arena = parse_file(filepath, &get_lexer(filepath), &get_parser(filepath)).unwrap();
    let queue = arena.get_priority_queue();
    assert_sorted(&queue, &arena);
}

#[test]
fn test_empty_calc() {
    assert_sorted_from_file("tests/empty.calc");
}

#[test]
fn test_one_calc() {
    assert_sorted_from_file("tests/one.calc");
}

#[test]
fn test_add_calc() {
    assert_sorted_from_file("tests/add.calc");
}

#[test]
fn test_mult_calc() {
    assert_sorted_from_file("tests/mult.calc");
}

#[test]
fn test_hello_java() {
    assert_sorted_from_file("tests/Hello.java");
}
