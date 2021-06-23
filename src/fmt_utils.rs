// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use core::fmt;

pub(crate) struct FormatAsNone;

impl fmt::Debug for FormatAsNone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<None>")
    }
}

pub(crate) struct FormatAsInvalid;

impl fmt::Debug for FormatAsInvalid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<Invalid>")
    }
}

pub(crate) fn debug_format_option_as_value_or_none<T: fmt::Debug>(
    v: Option<&T>,
) -> &dyn fmt::Debug {
    if let Some(v) = v {
        v
    } else {
        &FormatAsNone
    }
}

pub(crate) fn debug_format_option_as_value_or_invalid<T: fmt::Debug>(
    v: Option<&T>,
) -> &dyn fmt::Debug {
    if let Some(v) = v {
        v
    } else {
        &FormatAsInvalid
    }
}
