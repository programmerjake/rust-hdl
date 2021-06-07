// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

pub trait ValueType: Clone {}

impl ValueType for () {}

impl ValueType for bool {}

pub mod ints;

pub use ints::{
    Int, Int1, Int128, Int16, Int32, Int64, Int8, SInt, UInt, UInt1, UInt128, UInt16, UInt32,
    UInt64, UInt8,
};
