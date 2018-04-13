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

#![feature(test)]
#![feature(try_from)]

extern crate cfgrammar;
extern crate crypto;
extern crate dot;
#[macro_use]
extern crate log;
extern crate lrlex;
extern crate lrpar;
extern crate lrtable;
extern crate multiset;
extern crate term;
extern crate test;

/// Actions are operations that transform abstract syntax trees.
pub mod action;

/// AST defines the abstract syntax tree types that the differ works on.
///
/// Routines are provided to create and iterate over ASTs, and to parse a file
/// into an AST.
pub mod ast;

/// Algorithms to generate edit scripts, based on an existing AST matching.
pub mod edit_script;

/// Emitters generate output for the user in a variety of formats (e.g. JSON, Graphviz).
pub mod emitters;

/// Matchers create mappings between abstract syntax trees.
pub mod matchers;

/// GT matching algorithm.
pub mod gt_matcher;

/// Longest common subsequence matching algorithm.
///
/// Described in Myers (1986).
pub mod myers_matcher;

/// Zhang-Shasha matching algorithm.
///
/// Described in Zhang & Shasha (1989).
pub mod zs_matcher;

/// The null matcher produces no matches and is only used for testing.
pub mod null_matcher;

/// A queue of `NodeId`s sorted on the height of their respective nodes.
pub mod hqueue;

/// Parse strings and files into `ast::Arena` types.
pub mod parser;

/// A patch represents a diff on a single AST node.
///
/// Also deals with turning nearby patches into hunks.
pub mod patch;

/// QGram distance algorithm.
pub mod qgram;

/// Algorithms which act on sequences of values.
pub mod sequence;

/// Compute the similarity of two subtrees in a `MappingStore`.
pub mod similarity;

/// Algorithms which were described in Myers(1996).
///
/// Induced, pruning algorithms currently implemented.
pub mod chawathe98_matcher;

/// Fingerprinting algorithms for tree isomorphism tests.
pub mod fingerprint;
