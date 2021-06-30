// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Intern},
    io::Input,
    ir::{
        types::{
            IrArrayType, IrBitVectorType, IrStructFieldType, IrStructType, IrValueType,
            IrValueTypeRef,
        },
        values::{
            ExtractStructField, IrValue, IrValueRef, LiteralArray, LiteralBits, LiteralStruct,
            LiteralStructField,
        },
    },
};
use alloc::{boxed::Box, vec::Vec};
use core::{
    convert::Infallible,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

pub trait Value<'ctx>: Sized {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self>;
    fn get_input(&self, ctx: ContextRef<'ctx>) -> Input<'ctx, Self> {
        self.get_value(ctx).into()
    }
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

pub struct StructFieldDescriptor<'a, FieldEnum> {
    pub name: &'a str,
    pub field: FieldEnum,
}

pub trait StructValue<'ctx>: Value<'ctx> {
    type FieldEnum: 'static + Copy + Send + Sync + Ord + Hash + Into<usize>;
    type StructOfFieldEnums;
    const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums;
    const FIELDS: &'static [StructFieldDescriptor<'static, Self::FieldEnum>];
    fn field_value_ir(&self, ctx: ContextRef<'ctx>, field: Self::FieldEnum) -> IrValueRef<'ctx>;
    fn field_static_value_type_ir(
        ctx: ContextRef<'ctx>,
        field: Self::FieldEnum,
    ) -> Option<IrValueTypeRef<'ctx>> {
        let _ = ctx;
        let _ = field;
        None
    }
}

impl<'ctx, T: StructValue<'ctx>> Value<'ctx> for T {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        let mut fields = Vec::with_capacity(Self::FIELDS.len());
        for field in Self::FIELDS {
            fields.push(LiteralStructField {
                name: field.name.intern(ctx),
                value: self.field_value_ir(ctx, field.field),
            });
        }
        Val::from_ir_unchecked(
            ctx,
            IrValue::from(LiteralStruct::new(ctx, fields)).intern(ctx),
        )
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        let mut fields = Vec::with_capacity(Self::FIELDS.len());
        for field in Self::FIELDS {
            fields.push(IrStructFieldType {
                name: field.name.intern(ctx),
                ty: Self::field_static_value_type_ir(ctx, field.field)?,
            });
        }
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(IrStructType {
                fields: fields.intern(ctx),
            })
            .intern(ctx),
        ))
    }
}

mod unit_impl {
    use super::*;

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
    pub enum UnitFields {}

    impl From<UnitFields> for usize {
        fn from(v: UnitFields) -> Self {
            match v {}
        }
    }

    impl<'ctx> StructValue<'ctx> for () {
        type FieldEnum = UnitFields;
        type StructOfFieldEnums = ();
        const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums = ();
        const FIELDS: &'static [StructFieldDescriptor<'static, Self::FieldEnum>] = &[];
        fn field_value_ir(
            &self,
            _ctx: ContextRef<'ctx>,
            field: Self::FieldEnum,
        ) -> IrValueRef<'ctx> {
            match field {}
        }
        fn field_static_value_type_ir(
            _ctx: ContextRef<'ctx>,
            field: Self::FieldEnum,
        ) -> Option<IrValueTypeRef<'ctx>> {
            match field {}
        }
    }
}

impl<'ctx> Value<'ctx> for bool {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        Val::from_ir_unchecked(
            ctx,
            IrValue::LiteralBits(LiteralBits::new_bool(*self)).intern(ctx),
        )
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
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

impl<'ctx, Shape: integer::IntShapeTrait> Value<'ctx> for Int<Shape> {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        Val::from_ir_unchecked(ctx, IrValue::LiteralBits(self.clone().into()).intern(ctx))
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        let static_shape = Shape::static_shape()?;
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::BitVector(IrBitVectorType {
                bit_count: static_shape.bit_count,
                signed: static_shape.signed,
            })
            .intern(ctx),
        ))
    }
}

fn array_get_value<'ctx, A: AsRef<[T]> + Value<'ctx>, T: Value<'ctx>>(
    this: &A,
    ctx: ContextRef<'ctx>,
) -> Val<'ctx, A> {
    let mut element_type = T::static_value_type(ctx);
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
        IrValue::LiteralArray(LiteralArray::new(ctx, element_type, elements)).intern(ctx),
    )
}

impl<'ctx, T: Value<'ctx>, const N: usize> Value<'ctx> for [T; N] {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        array_get_value(self, ctx)
    }
    fn static_value_type(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        let element_type = T::static_value_type(ctx)?;
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
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        array_get_value(self, ctx)
    }
}

impl<'ctx, T: Value<'ctx>> Value<'ctx> for Vec<T> {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, Self> {
        array_get_value(self, ctx)
    }
}

pub struct Val<'ctx, T: Value<'ctx>> {
    ir: IrValueRef<'ctx>,
    value_type: ValueType<'ctx, T>,
}

impl<'ctx, T: Value<'ctx>> Val<'ctx, T> {
    pub fn from_ir_unchecked(ctx: ContextRef<'ctx>, ir: IrValueRef<'ctx>) -> Self {
        let value_type = ValueType::from_ir_unchecked(ctx, ir.get_type(ctx));
        Self { ir, value_type }
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
    pub fn ctx(self) -> ContextRef<'ctx> {
        self.value_type.ctx()
    }
    pub fn ir(self) -> IrValueRef<'ctx> {
        self.ir
    }
    pub fn extract_field_unchecked<Field: Value<'ctx>>(
        self,
        field_enum: T::FieldEnum,
    ) -> Val<'ctx, Field>
    where
        T: StructValue<'ctx>,
    {
        let extract_struct_field = ExtractStructField::new_with_struct_type_unchecked(
            self.ir,
            self.value_type.ir,
            field_enum.into(),
        );
        let ir = IrValue::from(extract_struct_field).intern(self.ctx());
        let value_type =
            ValueType::from_ir_unchecked(self.ctx(), extract_struct_field.value_type());
        Val::from_ir_and_type_unchecked(ir, value_type)
    }
    #[doc(hidden)]
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

impl<'ctx, T: Value<'ctx>> fmt::Debug for Val<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Val")
            .field("ir", &self.ir)
            .field("value_type", &self.value_type)
            .finish()
    }
}

impl<'ctx, T: Value<'ctx>> Copy for Val<'ctx, T> {}

impl<'ctx, T: Value<'ctx>> Clone for Val<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub struct ValueType<'ctx, T: Value<'ctx>> {
    ir: IrValueTypeRef<'ctx>,
    ctx: ContextRef<'ctx>,
    _phantom: PhantomData<fn(T) -> T>,
}

impl<'ctx, T: Value<'ctx>> ValueType<'ctx, T> {
    pub(crate) fn from_ir_unchecked(ctx: ContextRef<'ctx>, ir: IrValueTypeRef<'ctx>) -> Self {
        ValueType {
            ir,
            ctx,
            _phantom: PhantomData,
        }
    }
    pub fn ir(self) -> IrValueTypeRef<'ctx> {
        self.ir
    }
    pub fn ctx(self) -> ContextRef<'ctx> {
        self.ctx
    }
}

impl<'ctx, T: Value<'ctx>> fmt::Debug for ValueType<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ValueType").field(&self.ir).finish()
    }
}

impl<'ctx, T: Value<'ctx>> Copy for ValueType<'ctx, T> {}

impl<'ctx, T: Value<'ctx>> Clone for ValueType<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'ctx, T: Value<'ctx>> Eq for ValueType<'ctx, T> {}

impl<'ctx, T: Value<'ctx>> PartialEq for ValueType<'ctx, T> {
    fn eq(&self, other: &Self) -> bool {
        self.ir == other.ir
    }
}

impl<'ctx, T: Value<'ctx>> Hash for ValueType<'ctx, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ir.hash(state)
    }
}

pub mod integer;

pub use integer::{
    Int, Int1, Int128, Int16, Int32, Int64, Int8, SInt, UInt, UInt1, UInt128, UInt16, UInt32,
    UInt64, UInt8,
};

pub mod ops;
