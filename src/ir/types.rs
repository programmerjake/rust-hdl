// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::context::Interned;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum IrValueType<'ctx> {
    BitVector {
        bit_count: u32,
    },
    Array {
        element: IrValueTypeRef<'ctx>,
        length: usize,
    },
}

pub type IrValueTypeRef<'ctx> = Interned<'ctx, IrValueType<'ctx>>;
