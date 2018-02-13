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

use std::clone::Clone;
use std::collections::BTreeMap;

use action::ActionType;

// How close do two diffs have to be (in characters) to be merged into one patch?
// We assume lines are 80 characters long.
const DIST_THRESHOLD: u64 = 3 * 80;

#[derive(Clone, Eq, Debug, PartialEq)]
/// A patch represents a single change in the input files.
///
/// See the `Hunk` struct which aggregates a number of related patches.
pub struct Patch {
    action: ActionType,
    start: usize,
    length: usize,
}

impl Patch {
    /// Create a new patch.
    pub fn new(ty: ActionType, start: usize, length: usize) -> Patch {
        Patch { action: ty,
                start: start,
                length: length, }
    }

    /// Character number where this patch begins in the original file.
    ///
    /// The lexeme affected by this patch lies in the interval `[start, end)`,
    /// as a convenience for printing slices from the original file, i.e.
    /// `println!("{}", &file[patch.start()..patch.end()])`.
    pub fn start(&self) -> usize {
        self.start
    }

    /// Character number where this patch ends in the original file.
    ///
    /// The lexeme affected by this patch lies in the interval `[start, end)`,
    /// as a convenience for printing slices from the original file, i.e.
    /// `println!("{}", &file[patch.start()..patch.end()])`.
    pub fn end(&self) -> usize {
        self.start + self.length
    }

    /// Action associated with this patch.
    pub fn action(&self) -> &ActionType {
        &self.action
    }
}

#[derive(Clone, Eq, Debug, PartialEq)]
/// A hunk is a group of patches close to one another in the original files.
///
/// Patches inside a hunk are not necessarily contiguous.
pub struct Hunk {
    patches: Vec<Patch>,
    start: usize,
    length: usize,
}

impl Hunk {
    /// Convert a patch into a hunk.
    pub fn new(patch: &Patch) -> Hunk {
        Hunk { patches: vec![patch.clone()],
               start: patch.start,
               length: patch.length, }
    }

    fn end(&self) -> usize {
        self.start + self.length
    }

    fn add_patch(&mut self, patch: Patch) {
        if patch.start < self.start {
            self.start = patch.start;
        }
        let p_end = patch.end();
        if p_end > self.end() {
            self.length += p_end - self.end();
        }
        self.patches.push(patch);
    }

    fn is_patch_related(&self, patch: &Patch) -> bool {
        (self.start as i64 - patch.start as i64).abs() <= DIST_THRESHOLD as i64
    }

    /// Given a character number, return type and length of any patches that
    /// the character is involved in.
    ///
    /// The return value is the action type and the number of characters from
    /// `ch` that are involved in that patch.
    pub fn get_action(&self, ch: usize) -> Option<(ActionType, usize)> {
        if ch >= self.start && ch < self.end() {
            for patch in &self.patches {
                if ch >= patch.start && ch < patch.end() {
                    return Some((patch.action.clone(), patch.end() - ch));
                }
            }
        }
        None
    }
}

/// Get the header for this hunk, in diff format.
pub fn header(from: &Hunk, to: &Hunk) -> String {
    format!("@@ -{},{} +{},{} @@",
            from.start, from.length, to.start, to.length)
}

/// Group related patches into hunks.
pub fn hunkify(patches: Vec<Patch>) -> BTreeMap<(usize, usize), Hunk> {
    let mut hunks: Vec<Hunk> = vec![];
    for patch in patches {
        let mut added = false;
        for hunk in &mut hunks {
            if hunk.is_patch_related(&patch) {
                hunk.add_patch(patch.clone());
                added = true;
                break;
            }
        }
        if !added {
            hunks.push(Hunk::new(&patch));
        }
    }
    // TODO: Could also merge nearby hunks.
    let mut map = BTreeMap::new();
    for hunk in hunks {
        map.insert((hunk.start, hunk.start + hunk.length), hunk);
    }
    map
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_patch() {
        let p = Patch::new(ActionType::DELETE, 27, 6);
        assert_eq!(&ActionType::DELETE, p.action());
        assert_eq!(27, p.start());
        assert_eq!(33, p.end());
    }

    #[test]
    fn test_get_action() {
        let p0 = Patch::new(ActionType::DELETE, 0, 6);
        let p1 = Patch::new(ActionType::UPDATE, 7, 6);
        let mut hunk = Hunk::new(&p0);
        let p2 = Patch::new(ActionType::MOVE, 20, 6);
        let p3 = Patch::new(ActionType::INSERT, 27, 10);
        hunk.add_patch(p1.clone());
        hunk.add_patch(p2.clone());
        hunk.add_patch(p3.clone());
        for i in 0..6 {
            assert_eq!(Some((ActionType::DELETE, 6 - i)), hunk.get_action(i));
        }
        assert_eq!(None, hunk.get_action(6));
        for i in 7..13 {
            assert_eq!(Some((ActionType::UPDATE, 13 - i)), hunk.get_action(i));
        }
        for i in 13..20 {
            assert_eq!(None, hunk.get_action(i));
        }
        for i in 20..26 {
            assert_eq!(Some((ActionType::MOVE, 26 - i)), hunk.get_action(i));
        }
        assert_eq!(None, hunk.get_action(26));
        for i in 27..37 {
            assert_eq!(Some((ActionType::INSERT, 37 - i)), hunk.get_action(i));
        }
        assert_eq!(None, hunk.get_action(37));
    }

    #[test]
    fn test_is_close() {
        let p0 = Patch::new(ActionType::DELETE, 0, 6);
        let p1 = Patch::new(ActionType::UPDATE, 7, 6);
        let p2 = Patch::new(ActionType::MOVE, 20, 6);
        let p3 = Patch::new(ActionType::INSERT, 507, 10);
        let hunk = Hunk::new(&p0);
        assert!(hunk.is_patch_related(&p1));
        assert!(hunk.is_patch_related(&p2));
        assert!(!hunk.is_patch_related(&p3));
    }

    #[test]
    fn test_hunkify() {
        let patches = vec![Patch::new(ActionType::DELETE, 0, 6),
                           Patch::new(ActionType::UPDATE, 7, 6),
                           Patch::new(ActionType::MOVE, 20, 6),
                           Patch::new(ActionType::INSERT, 507, 10)];

        let hunks = hunkify(patches);
        assert_eq!(2, hunks.len());
        assert!(hunks.contains_key(&(0, 26)));
        assert!(hunks.contains_key(&(507, 517)));
    }

    #[test]
    fn test_header() {
        let p0 = Patch::new(ActionType::DELETE, 0, 6);
        let p1 = Patch::new(ActionType::UPDATE, 7, 6);
        let mut hunk_from = Hunk::new(&p0);
        hunk_from.add_patch(p1);
        let p2 = Patch::new(ActionType::MOVE, 20, 6);
        let p3 = Patch::new(ActionType::INSERT, 27, 10);
        let mut hunk_to = Hunk::new(&p2);
        hunk_to.add_patch(p3);
        assert_eq!(String::from("@@ -0,13 +20,17 @@"),
                   header(&hunk_from, &hunk_to));
    }
}
