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

/// This module implements the algorithm described in M. Chilowicz, E. Duris, G.
/// Roussel (2009) Syntax Tree Fingerprinting: A Foundation For Source Code
/// Similarity Detection.
use std::fmt::Debug;
use std::hash::Hash;

use crypto::digest::Digest;
use crypto::md5::Md5;

use ast::{Arena, NodeId, SrcOrDstNodeId};
use matchers::MappingStore;

/// Type alias for hashes stored in Arenas.
pub type HashType = u64;

const BASE: u64 = 33;

/// Symbol used in calculating static hashes of AST nodes.
///
/// See `ast:: to_static_hash_string`.
pub const OPEN_SYMBOL: &str = "[(";

/// Symbol used in calculating static hashes of AST nodes.
///
/// See `ast:: to_static_hash_string`.
pub const CLOSE_SYMBOL: &str = ")]";

/// Symbol used in calculating static hashes of AST nodes.
///
/// See `ast:: to_static_hash_string`.
pub const SEPARATE_SYMBOL: &str = "@@";

fn in_seed<T: Clone + Eq + Hash + ToString, U: PartialEq + Copy>(id: NodeId<U>,
                                                                 arena: &Arena<T, U>)
                                                                 -> String {
    String::from(OPEN_SYMBOL) + &arena[id].label + SEPARATE_SYMBOL + &arena[id].ty.to_string()
}

fn out_seed<T: Clone + Eq + Hash + ToString, U: PartialEq + Copy>(id: NodeId<U>,
                                                                  arena: &Arena<T, U>)
                                                                  -> String {
    arena[id].ty.to_string() + SEPARATE_SYMBOL + &arena[id].label + CLOSE_SYMBOL
}

/// Convert a byte array (from digest) to integer.
fn bytes_to_int(b: &[u8]) -> HashType {
    debug_assert_eq!(4, b.len(), "Byte array from digest has length {}", b.len());
    HashType::from(b[3]) & 0xff
    | (HashType::from(b[2]) & 0xff) << 8
    | (HashType::from(b[1]) & 0xff) << 16
    | (HashType::from(b[0]) & 0xff) << 24
}

/// Integer exponent function, returns `b**e`.
fn fpow(b: u64, e: u64) -> u64 {
    if e == 1 {
        return b;
    }
    let mut base = b;
    let mut exp = e;
    let mut result: u64 = 1;
    while exp > 0 {
        if (exp & 1) != 0 {
            result *= base;
        }
        exp >>= 1;
        base *= base;
    }
    result
}

/// Apply an action to an AST node.
pub trait HashGenerator<T: Clone + Debug + Eq + Hash + ToString> {
    /// Hash a string.
    fn hash(&mut self, msg: &str) -> HashType;
    /// Generate a hash for a leaf node in an `Arena`.
    ///
    /// The type of `id` tells the implementation which `Arena` is to be used.
    fn hash_leaf(&mut self, id: SrcOrDstNodeId, store: &MappingStore<T>) -> HashType;
    /// Generate a hash for a branch node in a given `Arena`.
    ///
    /// The type of `id` tells the implementation which `Arena` is to be used.
    fn hash_branch(&mut self, id: SrcOrDstNodeId, store: &MappingStore<T>) -> HashType;
}

/// The md5 rolling hash generator from GumTree.
pub struct Md5HashGenerator {
    generator: Md5
}

impl Md5HashGenerator {
    /// Create a new md5 rolling hash generator.
    pub fn new() -> Md5HashGenerator {
        Md5HashGenerator { generator: Md5::new() }
    }
}

impl<T: Clone + Debug + Eq + Hash + ToString> HashGenerator<T> for Md5HashGenerator {
    fn hash(&mut self, msg: &str) -> HashType {
        self.generator.input_str(msg);
        let mut result_arr: Vec<u8> = vec![];
        self.generator.result(result_arr.as_mut_slice());
        self.generator.reset();
        bytes_to_int(result_arr.as_slice()) as HashType
    }

    fn hash_leaf(&mut self, id_enum: SrcOrDstNodeId, store: &MappingStore<T>) -> HashType {
        match id_enum {
            SrcOrDstNodeId::SrcNodeId(id) => {
                BASE * HashGenerator::<T>::hash(self, &in_seed(id, &store.src_arena.borrow()))
                + HashGenerator::<T>::hash(self, &out_seed(id, &store.src_arena.borrow()))
            }
            SrcOrDstNodeId::DstNodeId(id) => {
                BASE * HashGenerator::<T>::hash(self, &in_seed(id, &store.dst_arena.borrow()))
                + HashGenerator::<T>::hash(self, &out_seed(id, &store.dst_arena.borrow()))
            }
        }
    }

    fn hash_branch(&mut self, id_enum: SrcOrDstNodeId, store: &MappingStore<T>) -> HashType {
        match id_enum {
            SrcOrDstNodeId::SrcNodeId(id) => {
                let mut size = HashType::from(id.size(&store.src_arena.borrow()));
                let mut hash = HashGenerator::<T>::hash(self,
                                                        &in_seed(id, &store.src_arena.borrow()))
                               * fpow(BASE, size);
                for child in id.children(&store.src_arena.borrow()) {
                    size -= HashType::from(child.size(&store.src_arena.borrow())) * 2;
                    debug_assert!(child.get_hash(&store.src_arena.borrow()).is_some());
                    hash += child.get_hash(&store.src_arena.borrow()).unwrap()
                }
                hash + HashGenerator::<T>::hash(self, &out_seed(id, &store.src_arena.borrow()))
            }
            SrcOrDstNodeId::DstNodeId(id) => {
                let mut size = HashType::from(id.size(&store.dst_arena.borrow()));
                let mut hash = HashGenerator::<T>::hash(self,
                                                        &in_seed(id, &store.dst_arena.borrow()))
                               * fpow(BASE, size);
                for child in id.children(&store.dst_arena.borrow()) {
                    size -= HashType::from(child.size(&store.dst_arena.borrow())) * 2;
                    debug_assert!(child.get_hash(&store.dst_arena.borrow()).is_some());
                    hash += child.get_hash(&store.dst_arena.borrow()).unwrap()
                }
                hash + HashGenerator::<T>::hash(self, &out_seed(id, &store.dst_arena.borrow()))
            }
        }
    }
}

/// Decorate the `src` AST with fingerprints.
pub fn apply_fingerprint<T: Clone + Debug + Eq + Hash + ToString>(gen: &mut HashGenerator<T>,
                                                                  store: &MappingStore<T>) {
    debug_assert!(store.src_arena.borrow().root().is_some());
    for child in store.src_arena
                      .borrow()
                      .root()
                      .unwrap()
                      .post_order_traversal(&store.src_arena.borrow())
    {
        if child.is_leaf(&store.src_arena.borrow()) {
            child.set_hash(Some(gen.hash_leaf(SrcOrDstNodeId::SrcNodeId(child), store)),
                           &mut store.src_arena.borrow_mut());
        } else {
            child.set_hash(Some(gen.hash_branch(SrcOrDstNodeId::SrcNodeId(child), store)),
                           &mut store.src_arena.borrow_mut());
        }
    }
}

/// Decorate the `src` and `dst` ASTs with fingerprints.
pub fn apply_fingerprint_both<T: Clone + Debug + Eq + Hash + ToString>(gen: &mut HashGenerator<T>,
store: &MappingStore<T>){
    apply_fingerprint(gen, store);
    debug_assert!(store.dst_arena.borrow().root().is_some());
    for child in store.dst_arena
                      .borrow()
                      .root()
                      .unwrap()
                      .post_order_traversal(&store.dst_arena.borrow())
    {
        if child.is_leaf(&store.dst_arena.borrow()) {
            child.set_hash(Some(gen.hash_leaf(SrcOrDstNodeId::DstNodeId(child), store)),
                           &mut store.dst_arena.borrow_mut());
        } else {
            child.set_hash(Some(gen.hash_branch(SrcOrDstNodeId::DstNodeId(child), store)),
                           &mut store.dst_arena.borrow_mut());
        }
    }
}
