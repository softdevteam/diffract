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

use std::collections::BTreeMap;
use std::cmp::{Ord, Ordering};

use action::ActionType;

#[derive(Eq, PartialEq)]
/// Store the location of a lexeme in its original file.
///
/// Assumes all characters are the same length.
/// This type should be opaque to client code.
struct LexemeLocation {
    /// Character number at the start of this lexeme.
    start: u32,
    /// Length of the lexeme in characters.
    length: u32,
}

impl LexemeLocation {
    fn new(start: u32, length: u32) -> LexemeLocation {
        LexemeLocation { start: start,
                         length: length, }
    }
}

impl Ord for LexemeLocation {
    fn cmp(&self, other: &LexemeLocation) -> Ordering {
        self.start.cmp(&other.start)
    }
}

impl PartialOrd for LexemeLocation {
    fn partial_cmp(&self, other: &LexemeLocation) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// A map between characters in a file and the action applied to them by and
/// edit script.
///
/// Intended to be used to look up whether an individual character in the
/// original file is attached to an AST node.
///
/// ## Assumptions
/// No two lexemes in the map overlap.
pub struct CharacterMap {
    tree: BTreeMap<LexemeLocation, ActionType>,
}

impl Default for CharacterMap {
    fn default() -> CharacterMap {
        CharacterMap { tree: BTreeMap::new(), }
    }
}

impl CharacterMap {
    /// Create an empty `CharacterMap`.
    pub fn new() -> CharacterMap {
        Default::default()
    }

    /// Insert information about a lexeme into the map.
    pub fn insert(&mut self, start: u32, length: u32, action: ActionType) {
        self.tree.insert(LexemeLocation::new(start, length), action);
    }

    /// Search the map for a lexeme that contains a given `character`.
    pub fn get(&self, character: u32) -> Option<ActionType> {
        for (key, value) in &self.tree {
            if character >= key.start && character < (key.start + key.length) {
                return Some(value.clone());
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use action::ActionType::*;

    #[test]
    fn test_character_map() {
        let mut map = CharacterMap::new();
        assert_eq!(None, map.get(0));
        // Character map for the following text, starting from character 0:
        //     public static void main(String[] args)
        // transformed into:
        //     private void main(Char[] args)
        // character numbers refer to the "from" text.
        map.insert(0, 6, UPDATE); // public
        map.insert(7, 6, DELETE); // static
        map.insert(24, 6, UPDATE); // String
        for i in 0..6 {
            assert_eq!(Some(UPDATE), map.get(i)); // public
        }
        assert_eq!(None, map.get(6));
        for i in 7..13 {
            assert_eq!(Some(DELETE), map.get(i)); // static
        }
        for i in 13..23 {
            assert_eq!(None, map.get(i)); // void
        }
        for i in 24..30 {
            assert_eq!(Some(UPDATE), map.get(i)); // String
        }
        for i in 33..42 {
            assert_eq!(None, map.get(i)); // args
        }
    }
}
