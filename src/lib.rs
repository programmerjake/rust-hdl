// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
#![no_std]
#![deny(elided_lifetimes_in_paths)]
#![deny(unsafe_op_in_unsafe_fn)]

extern crate alloc;
#[cfg(any(test, feature = "std"))]
extern crate std;

#[doc(hidden)]
pub use num_bigint as bigint;

pub mod clocking;
pub mod context;
pub mod export;
mod fmt_utils;
pub mod io;
pub mod ir;
pub mod logic;
pub mod module;
pub mod prelude;
pub mod values;

#[macro_export]
macro_rules! named {
    {
        let $name:ident$(: $ty:ty)? = $mod_or_ctx:ident.$method:ident($($args:expr),*)
    } => {
        let $name $(: $ty)? = $mod_or_ctx.$method(stringify!($name), $($args),*);
    };
    {
        let ($name:ident, $io:pat)$(: $ty:ty)? = $mod_or_ctx:ident.$method:ident($($args:expr),*)
    } => {
        let ($name, $io)$(: $ty)? = $mod_or_ctx.$method(stringify!($name), $($args),*);
    };
}
