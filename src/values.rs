// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Intern},
    ir::{
        types::{IrValueType, IrValueTypeRef},
        values::{IrValue, IrValueRef, LiteralArray, LiteralBits},
    },
};
use alloc::vec::Vec;
use core::{
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

pub trait ValueTrait<'ctx>: Sized {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self>;
    fn get_value_type(&self, ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        Self::static_value_type(ctx).unwrap_or_else(|| self.get_value(ctx).value_type())
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        let _ = ctx;
        None
    }
    fn default_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self>
    where
        Self: Default,
    {
        Self::static_value_type(ctx).unwrap_or_else(|| Self::default().get_value_type(ctx))
    }
}

impl<'ctx> ValueTrait<'ctx> for () {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        Val::from_ir_unchecked(ctx, IrValue::LiteralBits(LiteralBits::new()).intern(ctx))
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        Some(ValueType::from_ir_unchecked(
            IrValueType::BitVector { bit_count: 0 }.intern(ctx),
        ))
    }
}

impl<'ctx> ValueTrait<'ctx> for bool {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        Val::from_ir_unchecked(
            ctx,
            IrValue::LiteralBits(UInt1::unchecked_new(*self as u8).into()).intern(ctx),
        )
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        Some(ValueType::from_ir_unchecked(
            IrValueType::BitVector { bit_count: 1 }.intern(ctx),
        ))
    }
}

impl<'ctx, Shape: integer::IntShapeTrait> ValueTrait<'ctx> for Int<Shape> {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        Val::from_ir_unchecked(ctx, IrValue::LiteralBits(self.clone().into()).intern(ctx))
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        Some(ValueType::from_ir_unchecked(
            IrValueType::BitVector {
                bit_count: Shape::static_shape()?.bit_count,
            }
            .intern(ctx),
        ))
    }
}

impl<'ctx, T: ValueTrait<'ctx>, const N: usize> ValueTrait<'ctx> for [T; N] {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        let mut element_type = T::static_value_type(ctx);
        let elements: Vec<_> = self
            .iter()
            .map(|element| {
                let element = element.get_value(ctx);
                if let Some(element_type) = element_type {
                    assert_eq!(element_type, element.value_type());
                } else {
                    element_type = Some(element.value_type());
                }
                element.ir()
            })
            .collect();
        let element_type = element_type
            .expect("can't calculate the value type for a zero-length array of a dynamic type")
            .ir();
        Val::from_ir_unchecked(
            ctx,
            IrValue::LiteralArray(LiteralArray::new(ctx, element_type, elements)).intern(ctx),
        )
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        let element_type = T::static_value_type(ctx)?;
        Some(ValueType::from_ir_unchecked(
            IrValueType::Array {
                element: element_type.ir,
                length: N,
            }
            .intern(ctx),
        ))
    }
}

pub struct Val<'ctx, T: ValueTrait<'ctx>> {
    ir: IrValueRef<'ctx>,
    value_type: ValueType<'ctx, T>,
}

impl<'ctx, T: ValueTrait<'ctx>> Val<'ctx, T> {
    pub(crate) fn from_ir_unchecked(ctx: ContextRef<'ctx>, ir: IrValueRef<'ctx>) -> Self {
        let value_type = ValueType {
            ir: ir.get_type(ctx),
            _phantom: PhantomData,
        };
        Self { ir, value_type }
    }
    pub(crate) fn from_ir_and_type_unchecked(
        ir: IrValueRef<'ctx>,
        value_type: ValueType<'ctx, T>,
    ) -> Self {
        Self { ir, value_type }
    }
    pub fn value_type(self) -> ValueType<'ctx, T> {
        self.value_type
    }
    pub fn ir(self) -> IrValueRef<'ctx> {
        self.ir
    }
}

impl<'ctx, T: ValueTrait<'ctx>> Copy for Val<'ctx, T> {}

impl<'ctx, T: ValueTrait<'ctx>> Clone for Val<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub struct ValueType<'ctx, T: ValueTrait<'ctx>> {
    ir: IrValueTypeRef<'ctx>,
    _phantom: PhantomData<fn(T) -> T>,
}

impl<'ctx, T: ValueTrait<'ctx>> ValueType<'ctx, T> {
    pub(crate) fn from_ir_unchecked(ir: IrValueTypeRef<'ctx>) -> Self {
        ValueType {
            ir,
            _phantom: PhantomData,
        }
    }
    pub fn ir(self) -> IrValueTypeRef<'ctx> {
        self.ir
    }
}

impl<'ctx, T: ValueTrait<'ctx>> fmt::Debug for ValueType<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ValueType").field(&self.ir).finish()
    }
}

impl<'ctx, T: ValueTrait<'ctx>> Copy for ValueType<'ctx, T> {}

impl<'ctx, T: ValueTrait<'ctx>> Clone for ValueType<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'ctx, T: ValueTrait<'ctx>> Eq for ValueType<'ctx, T> {}

impl<'ctx, T: ValueTrait<'ctx>> PartialEq for ValueType<'ctx, T> {
    fn eq(&self, other: &Self) -> bool {
        self.ir == other.ir
    }
}

impl<'ctx, T: ValueTrait<'ctx>> Hash for ValueType<'ctx, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ir.hash(state)
    }
}

pub mod integer;

pub use integer::{
    Int, Int1, Int128, Int16, Int32, Int64, Int8, SInt, UInt, UInt1, UInt128, UInt16, UInt32,
    UInt64, UInt8,
};
