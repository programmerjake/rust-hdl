// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, ContextRef, Intern},
    io::Input,
    ir::{
        types::{IrArrayType, IrBitVectorType, IrValueType, IrValueTypeRef},
        values::{ExtractStructField, IrValue, IrValueRef, LiteralArray, LiteralBits},
        SourceLocation,
    },
    values::aggregate::StructValue,
};
use alloc::{boxed::Box, vec::Vec};
use core::{
    convert::Infallible,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

pub mod aggregate;
mod foreign_derives;
pub mod ops;

pub use integer::{
    Int, Int1, Int128, Int16, Int32, Int64, Int8, SInt, UInt, UInt1, UInt128, UInt16, UInt32,
    UInt64, UInt8,
};
pub use rust_hdl_int as integer;
pub use rust_hdl_macros::{val, FixedTypeValue, Value};

mod value_fns_sealed {
    pub trait Sealed {}
}

pub trait ValueFns<'ctx>: Sized + value_fns_sealed::Sealed + 'ctx {
    fn get_input<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Input<'ctx, Self>
    where
        Self: Value<'ctx>,
    {
        self.get_value(ctx.ctx()).into()
    }
    fn get_value_type<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> ValueType<'ctx, Self>
    where
        Self: Value<'ctx>,
    {
        let ctx = ctx.ctx();
        Self::static_value_type_opt(ctx).unwrap_or_else(|| self.get_value(ctx).value_type())
    }
    fn default_val<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Val<'ctx, Self>
    where
        Self: Value<'ctx> + Default,
    {
        Self::default().get_value(ctx.ctx())
    }
}

impl<'ctx, T: Value<'ctx>> value_fns_sealed::Sealed for T {}

impl<'ctx, T: Value<'ctx>> ValueFns<'ctx> for T {}

pub trait Value<'ctx>: ValueFns<'ctx> {
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self>;
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Option<ValueType<'ctx, Self>> {
        let _ = ctx;
        None
    }
}

pub trait FixedTypeValue<'ctx>: Value<'ctx> {
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self> {
        Self::static_value_type_opt(ctx.ctx()).unwrap()
    }
}

impl<'ctx> FixedTypeValue<'ctx> for bool {}

impl<'ctx> Value<'ctx> for bool {
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self> {
        let ctx = ctx.ctx();
        Val::from_ir_unchecked(
            ctx,
            IrValue::LiteralBits(LiteralBits::new_bool(*self)).intern(ctx),
        )
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Option<ValueType<'ctx, Self>> {
        let ctx = ctx.ctx();
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::BitVector(IrBitVectorType {
                bit_count: 1,
                signed: false,
            })
            .intern(ctx),
        ))
    }
}

impl<'ctx, Shape: integer::FixedIntShape> FixedTypeValue<'ctx> for Int<Shape> {}

impl<'ctx, Shape: integer::IntShapeTrait> Value<'ctx> for Int<Shape> {
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self> {
        let ctx = ctx.ctx();
        Val::from_ir_unchecked(ctx, IrValue::LiteralBits(self.clone().into()).intern(ctx))
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Option<ValueType<'ctx, Self>> {
        let ctx = ctx.ctx();
        let static_shape = Shape::static_shape()?;
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::BitVector(static_shape.into()).intern(ctx),
        ))
    }
}

fn array_get_value<'ctx, A: AsRef<[T]> + Value<'ctx>, T: Value<'ctx>>(
    this: &A,
    ctx: impl AsContext<'ctx>,
) -> Val<'ctx, A> {
    let ctx = ctx.ctx();
    let mut element_type = T::static_value_type_opt(ctx);
    let elements: Vec<_> = this
        .as_ref()
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
        IrValue::LiteralArray(LiteralArray::new(
            ctx,
            element_type,
            elements,
            &SourceLocation::caller(),
        ))
        .intern(ctx),
    )
}

impl<'ctx, T: FixedTypeValue<'ctx>, const N: usize> FixedTypeValue<'ctx> for [T; N] {}

impl<'ctx, T: Value<'ctx>, const N: usize> Value<'ctx> for [T; N] {
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self> {
        let ctx = ctx.ctx();
        array_get_value(self, ctx)
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Option<ValueType<'ctx, Self>> {
        let ctx = ctx.ctx();
        let element_type = T::static_value_type_opt(ctx)?;
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::Array(IrArrayType {
                element: element_type.ir,
                length: N,
            })
            .intern(ctx),
        ))
    }
}

impl<'ctx, T: Value<'ctx>, const N: usize> From<Val<'ctx, [T; N]>> for Val<'ctx, Box<[T]>> {
    fn from(v: Val<'ctx, [T; N]>) -> Self {
        Val::from_ir_and_type_unchecked(v.ir(), v.value_type().into())
    }
}

impl<'ctx, T: Value<'ctx>, const N: usize> From<Val<'ctx, [T; N]>> for Val<'ctx, Vec<T>> {
    fn from(v: Val<'ctx, [T; N]>) -> Self {
        Val::from_ir_and_type_unchecked(v.ir(), v.value_type().into())
    }
}

impl<'ctx, T: Value<'ctx>> From<Val<'ctx, Vec<T>>> for Val<'ctx, Box<[T]>> {
    fn from(v: Val<'ctx, Vec<T>>) -> Self {
        Val::from_ir_and_type_unchecked(v.ir(), v.value_type().into())
    }
}

impl<'ctx, T: Value<'ctx>> From<Val<'ctx, Box<[T]>>> for Val<'ctx, Vec<T>> {
    fn from(v: Val<'ctx, Box<[T]>>) -> Self {
        Val::from_ir_and_type_unchecked(v.ir(), v.value_type().into())
    }
}

impl<'ctx, T: Value<'ctx>, const N: usize> From<ValueType<'ctx, [T; N]>>
    for ValueType<'ctx, Box<[T]>>
{
    fn from(v: ValueType<'ctx, [T; N]>) -> Self {
        ValueType::from_ir_unchecked(v.ctx(), v.ir())
    }
}

impl<'ctx, T: Value<'ctx>, const N: usize> From<ValueType<'ctx, [T; N]>>
    for ValueType<'ctx, Vec<T>>
{
    fn from(v: ValueType<'ctx, [T; N]>) -> Self {
        ValueType::from_ir_unchecked(v.ctx(), v.ir())
    }
}

impl<'ctx, T: Value<'ctx>> From<ValueType<'ctx, Vec<T>>> for ValueType<'ctx, Box<[T]>> {
    fn from(v: ValueType<'ctx, Vec<T>>) -> Self {
        ValueType::from_ir_unchecked(v.ctx(), v.ir())
    }
}

impl<'ctx, T: Value<'ctx>> From<ValueType<'ctx, Box<[T]>>> for ValueType<'ctx, Vec<T>> {
    fn from(v: ValueType<'ctx, Box<[T]>>) -> Self {
        ValueType::from_ir_unchecked(v.ctx(), v.ir())
    }
}

impl<'ctx, T: Value<'ctx>> Value<'ctx> for Box<[T]> {
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self> {
        let ctx = ctx.ctx();
        array_get_value(self, ctx)
    }
}

impl<'ctx, T: Value<'ctx>> Value<'ctx> for Vec<T> {
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self> {
        let ctx = ctx.ctx();
        array_get_value(self, ctx)
    }
}

impl<'ctx, T: ?Sized + 'ctx> Value<'ctx> for PhantomData<T> {
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self> {
        let ctx = ctx.ctx();
        Val::from_ir_and_type_unchecked(
            IrValue::from(LiteralBits::new()).intern(ctx),
            Self::static_value_type(ctx),
        )
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Option<ValueType<'ctx, Self>> {
        let ctx = ctx.ctx();
        Some(Self::static_value_type(ctx))
    }
}

impl<'ctx, T: ?Sized + 'ctx> FixedTypeValue<'ctx> for PhantomData<T> {
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self> {
        let ctx = ctx.ctx();
        ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(IrBitVectorType {
                bit_count: 0,
                signed: false,
            })
            .intern(ctx),
        )
    }
}

pub trait ToVal<'ctx> {
    type ValueType: Value<'ctx>;
    #[track_caller]
    fn to_val<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self::ValueType>;
}

impl<'ctx, T: Value<'ctx>> ToVal<'ctx> for T {
    type ValueType = T;
    fn to_val<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, T> {
        self.get_value(ctx.ctx())
    }
}

impl<'ctx, T: Value<'ctx>> ToVal<'ctx> for Val<'ctx, T> {
    type ValueType = T;
    fn to_val<Ctx: AsContext<'ctx>>(&self, _ctx: Ctx) -> Val<'ctx, T> {
        *self
    }
}

pub struct Val<'ctx, T> {
    ir: IrValueRef<'ctx>,
    value_type: ValueType<'ctx, T>,
}

impl<'ctx, T> AsContext<'ctx> for Val<'ctx, T> {
    fn ctx(&self) -> ContextRef<'ctx> {
        self.value_type.ctx()
    }
}

impl<'ctx, T: Value<'ctx>> Val<'ctx, T> {
    pub fn from_ir_unchecked(ctx: impl AsContext<'ctx>, ir: IrValueRef<'ctx>) -> Self {
        let ctx = ctx.ctx();
        Self::from_ir_and_type_unchecked(ir, ValueType::from_ir_unchecked(ctx, ir.get_type(ctx)))
    }
    pub fn from_ir_and_type_unchecked(
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
    #[track_caller]
    pub fn extract_field_unchecked<Field: Value<'ctx>>(
        self,
        field_enum: T::FieldEnum,
    ) -> Val<'ctx, Field>
    where
        T: StructValue<'ctx>,
    {
        let extract_struct_field = ExtractStructField::new(
            self.ctx(),
            self.ir,
            field_enum.into(),
            &SourceLocation::caller(),
        );
        let ir = IrValue::from(extract_struct_field).intern(self.ctx());
        let value_type =
            ValueType::from_ir_unchecked(self.ctx(), extract_struct_field.value_type());
        Val::from_ir_and_type_unchecked(ir, value_type)
    }
    #[doc(hidden)]
    #[track_caller]
    pub fn extract_field_unchecked_macro_helper<
        Field: Value<'ctx>,
        GetFieldEnum: FnOnce(
            Option<(&T, Infallible)>,
            T::StructOfFieldEnums,
        ) -> (T::FieldEnum, Option<(&Field, Infallible)>),
    >(
        self,
        get_field_enum: GetFieldEnum,
    ) -> Val<'ctx, Field>
    where
        T: StructValue<'ctx>,
    {
        self.extract_field_unchecked(get_field_enum(None, T::STRUCT_OF_FIELD_ENUMS).0)
    }
}

impl<'ctx, T> fmt::Debug for Val<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Val")
            .field("ir", &self.ir)
            .field("value_type", &self.value_type)
            .finish()
    }
}

impl<'ctx, T> Copy for Val<'ctx, T> {}

impl<'ctx, T> Clone for Val<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub struct ValueType<'ctx, T> {
    ir: IrValueTypeRef<'ctx>,
    ctx: ContextRef<'ctx>,
    _phantom: PhantomData<fn(T) -> T>,
}

impl<'ctx, T> AsContext<'ctx> for ValueType<'ctx, T> {
    fn ctx(&self) -> ContextRef<'ctx> {
        self.ctx
    }
}

impl<'ctx, T: Value<'ctx>> ValueType<'ctx, T> {
    pub fn from_ir_unchecked(ctx: impl AsContext<'ctx>, ir: IrValueTypeRef<'ctx>) -> Self {
        let ctx = ctx.ctx();
        ValueType {
            ir,
            ctx,
            _phantom: PhantomData,
        }
    }
    pub fn ir(self) -> IrValueTypeRef<'ctx> {
        self.ir
    }
}

impl<'ctx, T> fmt::Debug for ValueType<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ValueType").field(&self.ir).finish()
    }
}

impl<'ctx, T> Copy for ValueType<'ctx, T> {}

impl<'ctx, T> Clone for ValueType<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'ctx, T> Eq for ValueType<'ctx, T> {}

impl<'ctx, T> PartialEq for ValueType<'ctx, T> {
    fn eq(&self, other: &Self) -> bool {
        self.ir == other.ir
    }
}

impl<'ctx, T> Hash for ValueType<'ctx, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ir.hash(state)
    }
}

impl<'ctx> From<Val<'ctx, UInt1>> for Val<'ctx, bool> {
    #[must_use]
    fn from(v: Val<'ctx, UInt1>) -> Self {
        Self::from_ir_unchecked(v.ctx(), v.ir())
    }
}

impl<'ctx> From<Val<'ctx, bool>> for Val<'ctx, UInt1> {
    #[must_use]
    fn from(v: Val<'ctx, bool>) -> Self {
        Self::from_ir_unchecked(v.ctx(), v.ir())
    }
}
