// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use core::fmt;
use quote::ToTokens;
use std::panic::Location;
use syn::Error;

#[allow(dead_code)]
#[track_caller]
pub(crate) fn make_todo_error(
    tokens: impl ToTokens,
    arg: Option<fmt::Arguments<'_>>,
) -> syn::Error {
    match arg {
        Some(arg) => Error::new_spanned(
            tokens,
            format_args!("not yet implemented: {}\nat {}", arg, Location::caller()),
        ),
        None => Error::new_spanned(
            tokens,
            format_args!("not yet implemented\nat {}", Location::caller()),
        ),
    }
}

macro_rules! todo_err {
    ($tokens:expr, $format_string:literal $($args:tt)*) => {
        return Err(crate::macros::make_todo_error($tokens, Some(format_args!($format_string $($args)*))))
    };
    ($tokens:expr) => {
        return Err(crate::macros::make_todo_error($tokens, None))
    };
}
