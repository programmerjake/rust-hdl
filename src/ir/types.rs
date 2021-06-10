// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum IrValueType {
    BitVector { bits: usize },
}
