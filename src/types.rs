// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Internable, Interned},
    ir::types::IrValueType,
};

pub trait Value: Clone {
    fn get_ir<'ctx>(&self, ctx: ContextRef<'ctx>) -> Interned<'ctx, IrValueType>;
}

impl Value for () {
    fn get_ir<'ctx>(&self, ctx: ContextRef<'ctx>) -> Interned<'ctx, IrValueType> {
        IrValueType::BitVector { bits: 0 }.intern(ctx)
    }
}

impl Value for bool {
    fn get_ir<'ctx>(&self, ctx: ContextRef<'ctx>) -> Interned<'ctx, IrValueType> {
        IrValueType::BitVector { bits: 1 }.intern(ctx)
    }
}

pub mod ints;

pub use ints::{
    Int, Int1, Int128, Int16, Int32, Int64, Int8, SInt, UInt, UInt1, UInt128, UInt16, UInt32,
    UInt64, UInt8,
};

pub struct IntType {}
