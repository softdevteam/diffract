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

/// Calculate a q-gram distance between strings. Implementation based on the
/// simmetrics library: https://github.com/Simmetrics/simmetrics

use std::char::{MAX, REPLACEMENT_CHARACTER};
use std::hash::Hash;

use multiset::HashMultiSet;

/// q-grams are padded at the start of the string, q-1 times.
///
/// E.g. if `q` is 3, the string "ABC" becomes "##ABC##" with the default character.
const DEFAULT_PADDING: char = REPLACEMENT_CHARACTER;

struct QGram {
    q: u32,
    filter: bool,
    padding: String,
}

impl QGram {
    fn new(q: u32, padding: String) -> QGram {
        QGram { q,
                filter: false,
                padding, }
    }

    fn compare<T: Clone + Eq + Hash>(&self, a: &HashMultiSet<T>, b: &HashMultiSet<T>) -> f64 {
        if a.is_empty() && b.is_empty() {
            return 1.0;
        }
        if a.is_empty() || b.is_empty() {
            return 0.0;
        }
        let dist = self.distance(a, b);
        1.0 - dist / f64::from(a.len() as i32 + b.len() as i32)
    }

    fn distance<T: Clone + Eq + Hash>(&self, a: &HashMultiSet<T>, b: &HashMultiSet<T>) -> f64 {
        let mut distance: f64 = 0.0;
        let union = a.clone() + b.clone();
        for token in union.distinct_elements() {
            distance += f64::from((a.count_of(token) as i32 - b.count_of(token) as i32).abs());
        }
        distance
    }

    /// Choose padding that does not appear in `input`.
    ///
    /// Use `self.padding` if possible, otherwise choose a character that does
    /// not appear in `input` and repeat that character `self.q -1` times.
    /// Panics if it cannot find a suitable padding character.
    fn choose_padding(&self, input: &str) -> String {
        let mut low = MAX;
        let mut high = 0 as char;
        let padding_char = self.padding.chars().nth(0).unwrap();
        let mut found_padding_char = false;
        for c in input.chars() {
            if c == padding_char {
                found_padding_char = true;
            }
            if c > high {
                high = c;
            } else if c < low {
                low = c;
            }
        }
        if !found_padding_char {
            return self.padding.clone();
        }
        if low > 0 as char {
            return ((low as u8 - 1) as char).to_string()
                                            .repeat(self.q as usize - 1);
        } else if high < MAX {
            return ((high as u8 + 1) as char).to_string()
                                             .repeat(self.q as usize - 1);
        }
        panic!("Cannot find a padding character that does not appear in {}",
               input);
    }

    /// Parse a string into a multiset of q-grams.
    fn tokenise(&self, input: &str) -> HashMultiSet<String> {
        let mut token_set = HashMultiSet::new();
        if input.is_empty() {
            return token_set;
        }
        let mut padded = self.choose_padding(input);
        padded.push_str(input);
        padded.push_str(&self.choose_padding(input));
        // A string must contain at least `self.q` characters.
        if self.filter && padded.len() < self.q as usize {
            return token_set;
        } else if padded.len() < self.q as usize {
            token_set.insert(padded);
            return token_set;
        }
        // To parse a string into a q-gram set of tokens we move a `self.q`-wide
        // sliding windows across the string.
        let chars = padded.chars().collect::<Vec<char>>();
        let mut next_qgram = String::new();
        for (nth_qgram, _) in chars.iter().enumerate() {
            if nth_qgram > chars.len() - self.q as usize {
                break;
            }
            for nth_char in 0..self.q {
                next_qgram.push(chars[nth_qgram + nth_char as usize]);
            }
            token_set.insert(next_qgram.clone());
            next_qgram = String::new();
        }
        token_set
    }
}

// Constructs a q-gram tokeniser with the given `q` and padding.
fn qgram_extended(q: u32, padding: char) -> QGram {
    QGram::new(q, padding.to_string().repeat(q as usize - 1))
}

/// Constructs a q-gram tokeniser with the given `q` and default padding.
fn qgram(q: u32) -> QGram {
    qgram_extended(q, DEFAULT_PADDING)
}

/// Return a q-grams distance between two strings.
///
/// Applies a block distance similarity metric over all trigrams in a string.
pub fn qgram_distance(q: u32, s1: &str, s2: &str) -> f64 {
    let qgram = qgram(q);
    qgram.compare(&qgram.tokenise(s1), &qgram.tokenise(s2))
}

/// Return a q-grams distance between two strings.
///
/// Applies a block distance similarity metric over all trigrams in a string.
pub fn trigram_distance(s1: &str, s2: &str) -> f64 {
    qgram_distance(3, s1, s2)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Floating-point numbers must differ by no more than `DELTA`.
    const DELTA: f64 = 0.0000001;

    #[test]
    fn test_qgram_padding() {
        // Number of padding characters should be 1 less than qgram.q.
        let qgram = qgram(3);
        assert_eq!("��", qgram.padding);
        assert_eq!("��", qgram.padding);
    }

    #[test]
    #[should_panic]
    fn test_qgram_zero() {
        let qgram = qgram(0);
        assert_eq!("", qgram.padding);
        assert_eq!("", qgram.padding);
    }

    fn check_distance(s1: &str, s2: &str, dist: f64) {
        assert!((dist - qgram_distance(3, s1, s2)).abs() < DELTA);
    }

    #[test]
    fn test_qgram_same() {
        check_distance("", "", 1.0);
        check_distance("S", "S", 1.0);
        check_distance("012345", "012345", 1.0);
    }

    #[test]
    fn test_one_empty() {
        check_distance("S", "", 0.0);
        check_distance("", "S", 0.0);
        check_distance("foo", "", 0.0);
        check_distance("", "foo", 0.0);
    }

    #[test]
    fn test_qgram_same_size() {
        check_distance("ABCD", "ABCE", 0.5);
    }

    #[test]
    fn test_qgram_sentence() {
        let s1 = "A quirky thing it is. This is a sentence.";
        let s2 = "This sentence is similar. A quirky thing it is.";
        check_distance(s1, s2, 0.695652186870575);
    }

    #[test]
    fn test_qgram_tokenise_empty() {
        let qgram = qgram(3);
        assert_eq!(qgram.tokenise(""), HashMultiSet::new());
    }

    #[test]
    #[should_panic]
    fn test_qgram_tokenise_panic() {
        let qgram = qgram(3);
        assert_eq!(qgram.tokenise("XX"), HashMultiSet::new());
    }

    fn check_tokenised(expected: Vec<&str>, input: &str) {
        let qgram = qgram(3);
        let got = qgram.tokenise(input);
        for trigram in got.iter() {
            assert_eq!(3, trigram.chars().count(), "{} is not a trigram.", trigram);
        }
        // Convert `expected` into a multiset.
        let mut set = HashMultiSet::new();
        for trigram in expected.iter() {
            set.insert(String::from(*trigram));
        }
        assert_eq!(set, got);
    }

    #[test]
    fn test_choose_padding() {
        let qgram = qgram(3);
        assert_eq!("��", qgram.choose_padding("abcde"));
        assert_eq!("��", qgram.choose_padding("ABCDE"));
        assert_eq!("��", qgram.choose_padding("12345"));
        // Cannot use default padding as it appears in input string.
        assert_eq!("@@", qgram.choose_padding("�ABC�"));
    }

    #[test]
    fn test_qgram_tokenise_dna() {
        check_tokenised(vec!["��A", "�AG", "AGC", "GCT", "CTT", "TTC", "TCG", "CGA",
                             "GA�", "A��"],
                        "AGCTTCGA");
    }

    #[test]
    fn test_qgram_tokenise_words() {
        check_tokenised(vec!["��t", "�to", "to_", "o_b", "_be", "be_", "e_o", "_or", "or_",
                             "r_n", "_no", "not", "ot_", "t_t", "_to", "to_", "o_b", "_be",
                             "be�", "e��"],
                        "to_be_or_not_to_be");
    }
}
