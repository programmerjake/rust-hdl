// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::ContextRef,
    ir::{types::IrValueTypeRef, values::IrValueRef},
};
use core::marker::PhantomData;

pub trait ValueTypeTrait<'ctx>: Sized {
    fn get_value_type(&self, ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self>;
}

impl<'ctx> ValueTypeTrait<'ctx> for () {
    fn get_value_type(&self, ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        todo!()
    }
}

impl<'ctx> ValueTypeTrait<'ctx> for bool {
    fn get_value_type(&self, ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        todo!()
    }
}

impl<'ctx, Shape: integer::IntShapeTrait> ValueTypeTrait<'ctx> for Int<Shape> {
    fn get_value_type(&self, ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        todo!()
    }
}

impl<'ctx, T: ValueTypeTrait<'ctx>, const N: usize> ValueTypeTrait<'ctx> for [T; N] {
    fn get_value_type(&self, ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        todo!()
    }
}

pub struct Value<'ctx, T: ValueTypeTrait<'ctx>> {
    ir: IrValueRef<'ctx>,
    value_type: ValueType<'ctx, T>,
}

impl<'ctx, T: ValueTypeTrait<'ctx>> Value<'ctx, T> {
    pub fn value_type(self) -> ValueType<'ctx, T> {
        self.value_type
    }
    pub fn ir(self) -> IrValueRef<'ctx> {
        self.ir
    }
}

impl<'ctx, T: ValueTypeTrait<'ctx>> Copy for Value<'ctx, T> {}

impl<'ctx, T: ValueTypeTrait<'ctx>> Clone for Value<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub struct ValueType<'ctx, T: ValueTypeTrait<'ctx>> {
    ir: IrValueTypeRef<'ctx>,
    _phantom: PhantomData<T>,
}

impl<'ctx, T: ValueTypeTrait<'ctx>> ValueType<'ctx, T> {
    pub fn ir(self) -> IrValueTypeRef<'ctx> {
        self.ir
    }
}

impl<'ctx, T: ValueTypeTrait<'ctx>> Copy for ValueType<'ctx, T> {}

impl<'ctx, T: ValueTypeTrait<'ctx>> Clone for ValueType<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub mod integer;

pub use integer::{
    Int, Int1, Int128, Int16, Int32, Int64, Int8, SInt, UInt, UInt1, UInt128, UInt16, UInt32,
    UInt64, UInt8,
};
