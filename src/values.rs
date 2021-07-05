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

pub use rust_hdl_macros::{FixedTypeValue, Value};

mod value_fns_sealed {
    pub trait Sealed {}
}

pub trait ValueFns<'ctx>: Sized + value_fns_sealed::Sealed {
    fn get_input(&self, ctx: ContextRef<'ctx>) -> Input<'ctx, Self>
    where
        Self: Value<'ctx>,
    {
        self.get_value(ctx).into()
    }
    fn get_value_type(&self, ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self>
    where
        Self: Value<'ctx>,
    {
        Self::static_value_type_opt(ctx).unwrap_or_else(|| self.get_value(ctx).value_type())
    }
}

impl<'ctx, T: Value<'ctx>> value_fns_sealed::Sealed for T {}

impl<'ctx, T: Value<'ctx>> ValueFns<'ctx> for T {}

pub trait Value<'ctx>: ValueFns<'ctx> {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self>;
    fn static_value_type_opt(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        let _ = ctx;
        None
    }
}

pub trait FixedTypeValue<'ctx>: Value<'ctx> {
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        Self::static_value_type_opt(ctx).unwrap()
    }
}

pub trait StructFieldVisitor<'ctx, Struct: StructValue<'ctx>>: Sized {
    type BreakType;
    fn field<FieldType: Value<'ctx>>(
        self,
        name: &'static str,
        field_enum: Struct::FieldEnum,
        field: &FieldType,
    ) -> Result<Self, Self::BreakType>;
}

pub trait StructFieldTypeVisitor<'ctx, Struct: StructValue<'ctx>>: Sized {
    type BreakType;
    fn field<FieldType: Value<'ctx>>(
        self,
        name: &'static str,
        field_enum: Struct::FieldEnum,
    ) -> Result<Self, Self::BreakType>;
    fn field_with_type_hint<
        FieldType: Value<'ctx>,
        TypeHint: FnOnce(&Struct, Infallible) -> &FieldType,
    >(
        self,
        name: &'static str,
        field_enum: Struct::FieldEnum,
        _: TypeHint,
    ) -> Result<Self, Self::BreakType> {
        self.field::<FieldType>(name, field_enum)
    }
}

pub trait StructFieldFixedTypeVisitor<'ctx, Struct: FixedTypeStructValue<'ctx>>: Sized {
    type BreakType;
    fn field<FieldType: FixedTypeValue<'ctx>>(
        self,
        name: &'static str,
        field_enum: Struct::FieldEnum,
    ) -> Result<Self, Self::BreakType>;
    fn field_with_type_hint<
        FieldType: FixedTypeValue<'ctx>,
        TypeHint: FnOnce(&Struct, Infallible) -> &FieldType,
    >(
        self,
        name: &'static str,
        field_enum: Struct::FieldEnum,
        _: TypeHint,
    ) -> Result<Self, Self::BreakType> {
        self.field::<FieldType>(name, field_enum)
    }
}

pub trait StructValue<'ctx>: Value<'ctx> {
    type FieldEnum: 'static + Copy + Send + Sync + Ord + Hash + Into<usize>;
    type StructOfFieldEnums;
    const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums;
    const FIELD_COUNT: usize;
    fn visit_fields<V: StructFieldVisitor<'ctx, Self>>(
        &self,
        visitor: V,
    ) -> Result<V, V::BreakType>;
    fn visit_field_types<V: StructFieldTypeVisitor<'ctx, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

impl<'ctx, T: StructValue<'ctx>> Value<'ctx> for T {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self> {
        struct ValueGetter<'ctx> {
            fields: Vec<LiteralStructField<'ctx>>,
            ctx: ContextRef<'ctx>,
        }
        impl<'ctx, T: StructValue<'ctx>> StructFieldVisitor<'ctx, T> for ValueGetter<'ctx> {
            type BreakType = Infallible;

            fn field<FieldType: Value<'ctx>>(
                mut self,
                name: &'static str,
                field_enum: T::FieldEnum,
                field: &FieldType,
            ) -> Result<Self, Self::BreakType> {
                let field_index: usize = field_enum.into();
                assert_eq!(self.fields.len(), field_index);
                self.fields.push(LiteralStructField {
                    name: name.intern(self.ctx),
                    value: field.get_value(self.ctx).ir(),
                });
                Ok(self)
            }
        }
        let fields = self
            .visit_fields(ValueGetter {
                fields: Vec::new(),
                ctx,
            })
            .unwrap()
            .fields;
        assert_eq!(fields.len(), Self::FIELD_COUNT);
        Val::from_ir_unchecked(
            ctx,
            IrValue::from(LiteralStruct::new(ctx, fields)).intern(ctx),
        )
    }
    fn static_value_type_opt(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        struct ValueTypeOptGetter<'ctx> {
            fields: Vec<IrStructFieldType<'ctx>>,
            ctx: ContextRef<'ctx>,
        }
        impl<'ctx, T: StructValue<'ctx>> StructFieldTypeVisitor<'ctx, T> for ValueTypeOptGetter<'ctx> {
            type BreakType = ();

            fn field<FieldType: Value<'ctx>>(
                mut self,
                name: &'static str,
                field_enum: T::FieldEnum,
            ) -> Result<Self, Self::BreakType> {
                let field_index: usize = field_enum.into();
                assert_eq!(self.fields.len(), field_index);
                self.fields.push(IrStructFieldType {
                    name: name.intern(self.ctx),
                    ty: FieldType::static_value_type_opt(self.ctx).ok_or(())?.ir(),
                });
                Ok(self)
            }
        }
        let fields = Self::visit_field_types(ValueTypeOptGetter {
            fields: Vec::new(),
            ctx,
        })
        .ok()?
        .fields;
        assert_eq!(fields.len(), Self::FIELD_COUNT);
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(IrStructType::new(ctx, fields)).intern(ctx),
        ))
    }
}

pub trait FixedTypeStructValue<'ctx>: FixedTypeValue<'ctx> + StructValue<'ctx> {
    fn visit_field_fixed_types<V: StructFieldFixedTypeVisitor<'ctx, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

impl<'ctx, T: FixedTypeStructValue<'ctx>> FixedTypeValue<'ctx> for T {
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        struct ValueTypeGetter<'ctx> {
            fields: Vec<IrStructFieldType<'ctx>>,
            ctx: ContextRef<'ctx>,
        }
        impl<'ctx, T: FixedTypeStructValue<'ctx>> StructFieldFixedTypeVisitor<'ctx, T>
            for ValueTypeGetter<'ctx>
        {
            type BreakType = Infallible;

            fn field<FieldType: FixedTypeValue<'ctx>>(
                mut self,
                name: &'static str,
                field_enum: T::FieldEnum,
            ) -> Result<Self, Self::BreakType> {
                let field_index: usize = field_enum.into();
                assert_eq!(self.fields.len(), field_index);
                self.fields.push(IrStructFieldType {
                    name: name.intern(self.ctx),
                    ty: FieldType::static_value_type(self.ctx).ir(),
                });
                Ok(self)
            }
        }
        let fields = Self::visit_field_fixed_types(ValueTypeGetter {
            fields: Vec::new(),
            ctx,
        })
        .unwrap()
        .fields;
        assert_eq!(fields.len(), Self::FIELD_COUNT);
        ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(IrStructType::new(ctx, fields)).intern(ctx),
        )
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

    impl<'ctx> FixedTypeStructValue<'ctx> for () {
        fn visit_field_fixed_types<V: StructFieldFixedTypeVisitor<'ctx, Self>>(
            visitor: V,
        ) -> Result<V, V::BreakType> {
            Ok(visitor)
        }
    }

    impl<'ctx> StructValue<'ctx> for () {
        type FieldEnum = UnitFields;
        type StructOfFieldEnums = ();
        const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums = ();
        const FIELD_COUNT: usize = 0;
        fn visit_fields<V: StructFieldVisitor<'ctx, Self>>(
            &self,
            visitor: V,
        ) -> Result<V, V::BreakType> {
            Ok(visitor)
        }
        fn visit_field_types<V: StructFieldTypeVisitor<'ctx, Self>>(
            visitor: V,
        ) -> Result<V, V::BreakType> {
            Ok(visitor)
        }
    }
}

impl<'ctx> FixedTypeValue<'ctx> for bool {}

impl<'ctx> Value<'ctx> for bool {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self> {
        Val::from_ir_unchecked(
            ctx,
            IrValue::LiteralBits(LiteralBits::new_bool(*self)).intern(ctx),
        )
    }
    fn static_value_type_opt(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
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
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self> {
        Val::from_ir_unchecked(ctx, IrValue::LiteralBits(self.clone().into()).intern(ctx))
    }
    fn static_value_type_opt(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        let static_shape = Shape::static_shape()?;
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::BitVector(static_shape.into()).intern(ctx),
        ))
    }
}

fn array_get_value<'ctx, 'scope, A: AsRef<[T]> + Value<'ctx>, T: Value<'ctx>>(
    this: &A,
    ctx: ContextRef<'ctx>,
) -> Val<'ctx, 'scope, A> {
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
        IrValue::LiteralArray(LiteralArray::new(ctx, element_type, elements)).intern(ctx),
    )
}

impl<'ctx, T: FixedTypeValue<'ctx>, const N: usize> FixedTypeValue<'ctx> for [T; N] {}

impl<'ctx, T: Value<'ctx>, const N: usize> Value<'ctx> for [T; N] {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self> {
        array_get_value(self, ctx)
    }
    fn static_value_type_opt(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
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

impl<'ctx, 'scope, T: Value<'ctx>, const N: usize> From<Val<'ctx, 'scope, [T; N]>>
    for Val<'ctx, 'scope, Box<[T]>>
{
    fn from(v: Val<'ctx, 'scope, [T; N]>) -> Self {
        Val::from_ir_and_type_unchecked(v.ir(), v.value_type().into())
    }
}

impl<'ctx, 'scope, T: Value<'ctx>, const N: usize> From<Val<'ctx, 'scope, [T; N]>>
    for Val<'ctx, 'scope, Vec<T>>
{
    fn from(v: Val<'ctx, 'scope, [T; N]>) -> Self {
        Val::from_ir_and_type_unchecked(v.ir(), v.value_type().into())
    }
}

impl<'ctx, 'scope, T: Value<'ctx>> From<Val<'ctx, 'scope, Vec<T>>> for Val<'ctx, 'scope, Box<[T]>> {
    fn from(v: Val<'ctx, 'scope, Vec<T>>) -> Self {
        Val::from_ir_and_type_unchecked(v.ir(), v.value_type().into())
    }
}

impl<'ctx, 'scope, T: Value<'ctx>> From<Val<'ctx, 'scope, Box<[T]>>> for Val<'ctx, 'scope, Vec<T>> {
    fn from(v: Val<'ctx, 'scope, Box<[T]>>) -> Self {
        Val::from_ir_and_type_unchecked(v.ir(), v.value_type().into())
    }
}

impl<'ctx, 'scope, T: Value<'ctx>, const N: usize> From<ValueType<'ctx, [T; N]>>
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
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self> {
        array_get_value(self, ctx)
    }
}

impl<'ctx, T: Value<'ctx>> Value<'ctx> for Vec<T> {
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self> {
        array_get_value(self, ctx)
    }
}

pub struct Val<'ctx, 'scope, T: Value<'ctx>> {
    ir: IrValueRef<'ctx>,
    value_type: ValueType<'ctx, T>,
    _phantom: PhantomData<&'scope &'ctx ()>,
}

impl<'ctx, 'scope, T: Value<'ctx>> Val<'ctx, 'scope, T> {
    pub fn from_ir_unchecked(ctx: ContextRef<'ctx>, ir: IrValueRef<'ctx>) -> Self {
        Self::from_ir_and_type_unchecked(ir, ValueType::from_ir_unchecked(ctx, ir.get_type(ctx)))
    }
    pub fn from_ir_and_type_unchecked(
        ir: IrValueRef<'ctx>,
        value_type: ValueType<'ctx, T>,
    ) -> Self {
        Self {
            ir,
            value_type,
            _phantom: PhantomData,
        }
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
    ) -> Val<'ctx, 'scope, Field>
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
    ) -> Val<'ctx, 'scope, Field>
    where
        T: StructValue<'ctx>,
    {
        self.extract_field_unchecked(get_field_enum(None, T::STRUCT_OF_FIELD_ENUMS).0)
    }
}

impl<'ctx, 'scope, T: Value<'ctx>> fmt::Debug for Val<'ctx, 'scope, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Val")
            .field("ir", &self.ir)
            .field("value_type", &self.value_type)
            .finish()
    }
}

impl<'ctx, 'scope, T: Value<'ctx>> Copy for Val<'ctx, 'scope, T> {}

impl<'ctx, 'scope, T: Value<'ctx>> Clone for Val<'ctx, 'scope, T> {
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
    pub fn from_ir_unchecked(ctx: ContextRef<'ctx>, ir: IrValueTypeRef<'ctx>) -> Self {
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
