// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
#![no_std]
#![deny(elided_lifetimes_in_paths)]
#![deny(unsafe_op_in_unsafe_fn)]

extern crate alloc;
#[cfg(test)]
extern crate std;

pub mod context;
mod fmt_utils;
pub mod io;
pub mod ir;
pub mod logic;
pub mod module;
pub mod values;
