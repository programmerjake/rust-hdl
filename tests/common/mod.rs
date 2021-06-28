// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
#![allow(unused_macros)]

use once_cell::race::OnceBox;
use regex::{Captures, Regex};
use std::{panic::Location, path::Path};

#[track_caller]
pub fn assert_string_is_impl(test_name: &str, subtest_name: &str, formatted: &str) {
    let bless = option_env!("BLESS_TESTS") == Some("BLESS");
    static REGEX_DEBUG: OnceBox<Regex> = OnceBox::new();
    let regex_debug = REGEX_DEBUG.get_or_init(|| {
        Regex::new(
            r#"SourceLocation \{\n *file: "[^"]+",\n *line: ([0-9]+),\n *column: ([0-9]+),\n *\}"#,
        )
        .unwrap()
        .into()
    });
    let location = Location::caller();
    let formatted = regex_debug.replace_all(formatted, |captures: &Captures| {
        format!(
            "SourceLocation($FILE, $LINE{:+}, {})",
            captures[1].parse::<i64>().unwrap() - location.line() as i64,
            &captures[2],
        )
    });
    static REGEX_RTLIL: OnceBox<Regex> = OnceBox::new();
    let regex_rtlil = REGEX_RTLIL.get_or_init(|| {
        Regex::new(r#"(attribute \\src ")[^"]+:([0-9]+)\.([0-9]+)""#)
            .unwrap()
            .into()
    });
    let formatted = regex_rtlil.replace_all(&formatted, |captures: &Captures| {
        format!(
            "{}$FILE:$LINE{:+}.{}\"",
            &captures[1],
            captures[2].parse::<i64>().unwrap() - location.line() as i64,
            &captures[3],
        )
    });
    let formatted = &*formatted;
    let path = Path::new(location.file())
        .with_extension("")
        .join(test_name)
        .join(subtest_name)
        .with_extension("expected.txt");
    let expected = match std::fs::read_to_string(&path) {
        Ok(v) => Some(v),
        Err(e) => {
            println!("can't read from {}: {}", path.display(), e);
            None
        }
    };
    let expected = expected.as_deref();
    if Some(formatted) != expected && bless {
        println!("replacing {} with correct output", path.display());
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, formatted).unwrap();
        return;
    }
    assert!(
        Some(formatted) == expected,
        "doesn't match expected. value:\n~~~~~~~~~~\n{}\n~~~~~~~~~~\nrun with env var BLESS_TESTS=BLESS to update expected output",
        formatted
    );
}

macro_rules! assert_string_is {
    ($test_name:ident, $subtest_name:ident, $s:expr) => {
        $crate::common::assert_string_is_impl(stringify!($test_name), stringify!($subtest_name), $s)
    };
}

macro_rules! assert_display_formats_to {
    ($test_name:ident, $subtest_name:ident, $value:expr) => {
        assert_string_is!($test_name, $subtest_name, &format!("{}", $value))
    };
}

macro_rules! assert_formats_to {
    ($test_name:ident, $subtest_name:ident, $value:expr) => {
        assert_string_is!($test_name, $subtest_name, &format!("{:#?}", $value))
    };
}
