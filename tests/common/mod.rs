// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use once_cell::race::OnceBox;
use regex::{Captures, Regex};
use std::panic::Location;

#[track_caller]
pub fn assert_formats_to_impl(formatted: &str, expected: &str) {
    static REGEX: OnceBox<Regex> = OnceBox::new();
    let regex = REGEX.get_or_init(|| {
        Regex::new(
            r#"SourceLocation \{\n *file: "[^"]+",\n *line: ([0-9]+),\n *column: ([0-9]+),\n *\}"#,
        )
        .unwrap()
        .into()
    });
    let location = Location::caller();
    let value = regex.replace_all(formatted, |captures: &Captures| {
        format!(
            "SourceLocation($FILE, $LINE{:+}, {})",
            captures[1].parse::<i64>().unwrap() - location.line() as i64,
            &captures[2],
        )
    });
    assert!(value == expected, "doesn't match expected. value:{}", value);
}

macro_rules! assert_formats_to {
    ($value:expr, $expected:literal) => {
        $crate::common::assert_formats_to_impl(&format!("\n{:#?}", $value), $expected)
    };
}
