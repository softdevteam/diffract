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
#![allow(dead_code)]  // Temporary, this module will be used in RTED.

/// Store AST labels as small, unsigned integers.
/// This scheme is similar to modules in GumTree and Approxlib.

use std::collections::HashMap;

/// Map labels to integers, for size / speed improvement.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LabelMap<'a> {
    count: usize,
    str_to_uint: HashMap<&'a str, usize>,
    uint_to_str: HashMap<usize, &'a str>,
    new_labels_allowed: bool
}

impl<'a> LabelMap<'a> {
    /// Create a default label map.
    pub fn new() -> LabelMap<'a> {
        LabelMap {
            count: 0,
            str_to_uint: HashMap::new(),
            uint_to_str: HashMap::new(),
            new_labels_allowed: true
        }
    }

    /// Given an id, return the corresponding label, or `None`.
    pub fn read(&self, id: usize) -> Option<&'a str> {
        self.uint_to_str.get(&id).map(|lab| *lab)
    }

    /// Store a label in this mapping.
    ///
    /// Return `None` if label is new and `new_labels_allowed` is `false`.
    pub fn store(&mut self, label: &'a str) -> Option<usize> {
        if self.str_to_uint.contains_key(&label) {
            return Some(self.str_to_uint[label]);
        } else if !self.new_labels_allowed {
            return None
        }
        self.count += 1;
        let key = self.count;
        self.str_to_uint.insert(label, key);
        self.uint_to_str.insert(key, label);
        Some(key)
    }

    /// Set whether new labels allowed in this mapping.
    pub fn new_labels_allowed(&mut self, allowed: bool) {
        self.new_labels_allowed = allowed;
    }

    /// Are new labels allowed in this mapping? Default value is `true`.
    pub fn are_new_labels_allowed(&self) -> bool {
        self.new_labels_allowed
    }

    /// The number of labels in this mapping.
    pub fn size(&self) -> usize {
        self.count
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn label_maps() {
        let mut map = LabelMap::new();
        assert_eq!(0, map.size());
        assert_eq!(None, map.read(0));
        map.store("foobar");
        assert_eq!(1, map.size());
        assert_eq!(Some("foobar"), map.read(1));
    }

    #[test]
    fn new_labels_not_allowed() {
        let mut map = LabelMap::new();
        assert_eq!(0, map.size());
        assert_eq!(None, map.read(0));
        map.store("foobar");
        assert_eq!(1, map.size());
        assert_eq!(Some("foobar"), map.read(1));
        map.new_labels_allowed(false);
        map.store("foobar");
        assert_eq!(1, map.size());
        assert_eq!(Some("foobar"), map.read(1));
        map.store("barfoo");
        assert_eq!(1, map.size());
        assert_eq!(Some("foobar"), map.read(1));
    }
}
