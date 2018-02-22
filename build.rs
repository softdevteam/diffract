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

//! Write out a file of integration tests which tests every pair of example
//! files in the `tests/` directory.

#![warn(missing_docs)]

extern crate glob;

use std::fs::File;
use std::io::Write;
use std::path::Path;

use glob::glob;

/// Directory containing example files for testing and to which the generated
/// tests should be written.
const DIR: &str = "tests/";

/// Generate a file of integration tests which run a diff (with both Null and
/// Myers matchers) on every pair of test files in the `tests/` directory.
/// Include diffs of files against themselves. This test takes a long time to
/// run (well over 5 minutes) and so should not be run on CI infrastructure such
/// as Travis.
///
/// Run with: `cargo test --no-fail-fast  -- --ignored --nocapture`
fn main() {
    // Glob all example files in the `tests/` directory.
    // Filter out files which have been seeded with deliberate lexical or
    // syntactic errors.
    let test_files: Vec<String> =
        glob(&format!("{}*.calc", DIR)).expect("Failed to read glob pattern")
                                       .into_iter()
                                       .chain(glob(&format!("{}*.java", DIR))
                                              .expect("Failed to read glob pattern")
                                              .into_iter())
                                       .chain(glob(&format!("{}*.txt", DIR))
                                              .expect("Failed to read glob pattern")
                                              .into_iter())
                                       .map(|res| res.unwrap()
                                                     .to_str()
                                                     .unwrap()
                                                     .to_string())
                                       .filter(|name| !name.contains("_err"))
                                       .collect::<Vec<String>>();

    let mut outfile = File::create(Path::new(&DIR).join("test_generated.rs")).unwrap();
    write!(
        outfile,
        r#"
extern crate diffract;

use diffract::myers_matcher::MyersConfig;
use diffract::null_matcher::NullConfig;

mod common;
use common::check_files;

    "#
    ).unwrap();

    for fname1 in test_files.iter() {
        for fname2 in test_files.iter() {
            // Remove directory and extension from `fname1`, `fname2`.
            // We include the file extension in the test name because some files
            // are intended to be parsed by different Yacc files (e.g.
            // `Hello.java` and `Hello.txt`).
            let name1_ext = fname1[DIR.len()..].split(".").collect::<Vec<&str>>();
            let name2_ext = fname2[DIR.len()..].split(".").collect::<Vec<&str>>();
            let name1 = format!("{}_{}", name1_ext[0], name1_ext[1]).to_lowercase();
            let name2 = format!("{}_{}", name2_ext[0], name2_ext[1]).to_lowercase();
            write!(outfile, r#"
#[test]
#[ignore]
fn test_globbed_{name1}_{name2}_null() {{
    check_files("{fname1}", "{fname2}", Box::new(NullConfig::new()));
}}

#[test]
#[ignore]
fn test_globbed_{name1}_{name2}_myers() {{
    check_files("{fname1}", "{fname2}", Box::new(MyersConfig::new()));
}}

            "#, name1=name1, name2=name2, fname1=fname1, fname2=fname2).unwrap();
        }
    }
}
