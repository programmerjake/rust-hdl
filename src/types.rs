// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

pub trait ValueType: Clone {}

impl ValueType for () {}

impl ValueType for bool {}

pub mod ints;
