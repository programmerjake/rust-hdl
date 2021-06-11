// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Internable},
    ir::values::{IrValue, IrValueRef, LiteralBits},
};

pub trait Value<'ctx>: Clone + 'ctx {
    fn get_ir(&self, ctx: ContextRef<'ctx>) -> IrValueRef<'ctx>;
}

impl<'ctx> Value<'ctx> for () {
    fn get_ir(&self, ctx: ContextRef<'ctx>) -> IrValueRef<'ctx> {
        IrValue::LiteralBits(LiteralBits::new()).intern(ctx)
    }
}

impl<'ctx> Value<'ctx> for bool {
    fn get_ir(&self, ctx: ContextRef<'ctx>) -> IrValueRef<'ctx> {
        IrValue::LiteralBits(UInt1::unchecked_new(*self as u8).into()).intern(ctx)
    }
}

pub mod ints;

pub use ints::{
    Int, Int1, Int128, Int16, Int32, Int64, Int8, SInt, UInt, UInt1, UInt128, UInt16, UInt32,
    UInt64, UInt8,
};

impl<'ctx, T: Value<'ctx>, const N: usize> Value<'ctx> for [T; N] {
    fn get_ir(&self, ctx: ContextRef<'ctx>) -> IrValueRef<'ctx> {
        todo!()
    }
}
