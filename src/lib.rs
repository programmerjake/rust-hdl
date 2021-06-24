// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
#![no_std]
#![deny(elided_lifetimes_in_paths)]
#![deny(unsafe_op_in_unsafe_fn)]

extern crate alloc;
#[cfg(test)]
extern crate std;

pub use rust_hdl_macros::{Value, IO};

pub mod clocking;
pub mod context;
mod fmt_utils;
pub mod io;
pub mod ir;
pub mod logic;
pub mod module;
pub mod values;

#[macro_export]
macro_rules! named {
    {
        let $name:ident$(: $ty:ty)? = $mod_or_ctx:ident.$method:ident($($args:expr),*)
    } => {
        let $name $(: $ty)? = $mod_or_ctx.$method(stringify!($name), $($args),*);
    };
    {
        let ($name:ident, $io:pat) = $module:ident.$submodule:ident($io_arg:expr)
    } => {
        let ($name, $io) = $module.$submodule(stringify!($name), $io_arg);
    };
}
