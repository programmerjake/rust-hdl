// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Intern},
    ir::{
        types::{IrEnumType, IrEnumVariantType, IrStructFieldType, IrStructType, IrValueType},
        values::{IrValue, LiteralBits, LiteralStruct, LiteralStructField},
    },
    prelude::{FixedTypeValue, Int, Val, Value, ValueType},
    values::integer::{IntShapeTrait, UIntShape},
};
use alloc::vec::Vec;
use core::{convert::Infallible, hash::Hash, marker::PhantomData};

mod aggregate_value_kind_sealed {
    pub trait Sealed {}
}

pub trait AggregateValueKind<'ctx: 'scope, 'scope>: aggregate_value_kind_sealed::Sealed {
    type AggregateValue: AggregateValue<'ctx, 'scope, AggregateValueKind = Self>;
    fn get_value(
        value: &Self::AggregateValue,
        ctx: ContextRef<'ctx>,
    ) -> Val<'ctx, 'ctx, Self::AggregateValue>;
    fn static_value_type_opt(
        ctx: ContextRef<'ctx>,
    ) -> Option<ValueType<'ctx, Self::AggregateValue>>;
}

pub trait FixedTypeAggregateValueKind<'ctx: 'scope, 'scope>:
    AggregateValueKind<'ctx, 'scope>
where
    Self::AggregateValue: FixedTypeValue<'ctx>,
{
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self::AggregateValue>;
}

pub trait AggregateOfFieldValues<'ctx: 'scope, 'scope> {
    type AggregateOfFieldValues: 'scope + Copy;
}

pub trait AggregateValue<'ctx: 'scope, 'scope>:
    Value<'ctx> + AggregateOfFieldValues<'ctx, 'scope>
{
    type AggregateValueKind: AggregateValueKind<'ctx, 'scope>;
    type DiscriminantShape: IntShapeTrait + Default;
}

impl<'ctx: 'scope, 'scope, T: AggregateValue<'ctx, 'scope>> Value<'ctx> for T
where
    T::AggregateValueKind: AggregateValueKind<'ctx, 'scope, AggregateValue = T>,
{
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self> {
        T::AggregateValueKind::get_value(self, ctx)
    }
    fn static_value_type_opt(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        T::AggregateValueKind::static_value_type_opt(ctx)
    }
}

impl<'ctx: 'scope, 'scope, T: AggregateValue<'ctx, 'scope>> FixedTypeValue<'ctx> for T
where
    T::AggregateValueKind: AggregateValueKind<'ctx, 'scope, AggregateValue = T>
        + FixedTypeAggregateValueKind<'ctx, 'scope>,
{
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        T::AggregateValueKind::static_value_type(ctx)
    }
}

pub struct StructAggregateValueKind<
    'ctx: 'scope,
    'scope,
    T: StructValue<'ctx, 'scope> + AggregateValue<'ctx, 'scope, AggregateValueKind = Self>,
>(PhantomData<Val<'ctx, 'scope, T>>);

impl<'ctx: 'scope, 'scope, T: StructValue<'ctx, 'scope>> aggregate_value_kind_sealed::Sealed
    for StructAggregateValueKind<'ctx, 'scope, T>
{
}

impl<'ctx: 'scope, 'scope, T: StructValue<'ctx, 'scope>> AggregateValueKind<'ctx, 'scope>
    for StructAggregateValueKind<'ctx, 'scope, T>
{
    type AggregateValue = T;
    fn get_value(
        value: &Self::AggregateValue,
        ctx: ContextRef<'ctx>,
    ) -> Val<'ctx, 'ctx, Self::AggregateValue> {
        struct ValueGetter<'ctx> {
            fields: Vec<LiteralStructField<'ctx>>,
            ctx: ContextRef<'ctx>,
        }
        impl<'ctx: 'scope, 'scope, T: StructValue<'ctx, 'scope>> StructFieldVisitor<'ctx, 'scope, T>
            for ValueGetter<'ctx>
        {
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
        let fields = value
            .visit_fields(ValueGetter {
                fields: Vec::new(),
                ctx,
            })
            .unwrap()
            .fields;
        assert_eq!(fields.len(), T::FIELD_COUNT);
        Val::from_ir_unchecked(
            ctx,
            IrValue::from(LiteralStruct::new(ctx, fields)).intern(ctx),
        )
    }
    fn static_value_type_opt(
        ctx: ContextRef<'ctx>,
    ) -> Option<ValueType<'ctx, Self::AggregateValue>> {
        struct ValueTypeOptGetter<'ctx> {
            fields: Vec<IrStructFieldType<'ctx>>,
            ctx: ContextRef<'ctx>,
        }
        impl<'ctx: 'scope, 'scope, T: StructValue<'ctx, 'scope>>
            StructFieldTypeVisitor<'ctx, 'scope, T> for ValueTypeOptGetter<'ctx>
        {
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
        let fields = T::visit_field_types(ValueTypeOptGetter {
            fields: Vec::new(),
            ctx,
        })
        .ok()?
        .fields;
        assert_eq!(fields.len(), T::FIELD_COUNT);
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(IrStructType::new(ctx, fields)).intern(ctx),
        ))
    }
}

impl<'ctx: 'scope, 'scope, T: FixedTypeStructValue<'ctx, 'scope>>
    FixedTypeAggregateValueKind<'ctx, 'scope> for StructAggregateValueKind<'ctx, 'scope, T>
{
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self::AggregateValue> {
        ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(fixed_type_struct_ir_type::<T>(ctx)).intern(ctx),
        )
    }
}

pub trait StructValue<'ctx: 'scope, 'scope>:
    AggregateValue<
    'ctx,
    'scope,
    AggregateValueKind = StructAggregateValueKind<'ctx, 'scope, Self>,
    DiscriminantShape = UIntShape<0>,
>
{
    type FieldEnum: 'static + Copy + Send + Sync + Ord + Hash + Into<usize>;
    type StructOfFieldEnums: 'static + Copy + Send + Sync;
    const STRUCT_OF_FIELD_ENUMS: Self::StructOfFieldEnums;
    const FIELD_COUNT: usize;
    fn visit_fields<V: StructFieldVisitor<'ctx, 'scope, Self>>(
        &self,
        visitor: V,
    ) -> Result<V, V::BreakType>;
    fn visit_field_types<V: StructFieldTypeVisitor<'ctx, 'scope, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

pub trait StructFieldVisitor<'ctx: 'scope, 'scope, Struct: StructValue<'ctx, 'scope>>:
    Sized
{
    type BreakType;
    fn field<FieldType: Value<'ctx>>(
        self,
        name: &'static str,
        field_enum: Struct::FieldEnum,
        field: &FieldType,
    ) -> Result<Self, Self::BreakType>;
}

pub trait StructFieldTypeVisitor<'ctx: 'scope, 'scope, Struct: StructValue<'ctx, 'scope>>:
    Sized
{
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

pub trait StructFieldFixedTypeVisitor<
    'ctx: 'scope,
    'scope,
    Struct: FixedTypeStructValue<'ctx, 'scope>,
>: Sized
{
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

pub trait FixedTypeStructValue<'ctx: 'scope, 'scope>:
    FixedTypeValue<'ctx> + StructValue<'ctx, 'scope>
{
    fn visit_field_fixed_types<V: StructFieldFixedTypeVisitor<'ctx, 'scope, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

pub trait EnumVariantVisitor<'ctx: 'scope, 'scope, Enum: EnumValue<'ctx, 'scope>>: Sized {
    type ResultType;
    fn variant<VariantType: FixedTypeStructValue<'ctx, 'scope>>(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
        variant: &VariantType,
    ) -> Self::ResultType;
}

pub trait EnumVariantTypeVisitor<'ctx: 'scope, 'scope, Enum: EnumValue<'ctx, 'scope>>:
    Sized
{
    type BreakType;
    fn variant<VariantType: FixedTypeStructValue<'ctx, 'scope>>(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
    ) -> Result<Self, Self::BreakType>;
    fn variant_with_type_hint<
        VariantType: FixedTypeStructValue<'ctx, 'scope>,
        TypeHint: FnOnce(&Enum, Infallible) -> &VariantType,
    >(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
        _: TypeHint,
    ) -> Result<Self, Self::BreakType> {
        self.variant::<VariantType>(name, discriminant)
    }
}

pub trait EnumValue<'ctx: 'scope, 'scope>:
    AggregateValue<'ctx, 'scope, AggregateValueKind = EnumAggregateValueKind<'ctx, 'scope, Self>>
{
    fn visit_variant<V: EnumVariantVisitor<'ctx, 'scope, Self>>(&self, visitor: V)
        -> V::ResultType;
    fn visit_variant_types<V: EnumVariantTypeVisitor<'ctx, 'scope, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

pub trait FixedTypeEnumValue<'ctx: 'scope, 'scope>:
    EnumValue<'ctx, 'scope> + FixedTypeValue<'ctx>
{
}

pub struct EnumAggregateValueKind<'ctx: 'scope, 'scope, T: EnumValue<'ctx, 'scope>>(
    PhantomData<Val<'ctx, 'scope, T>>,
);

impl<'ctx: 'scope, 'scope, T: EnumValue<'ctx, 'scope>> aggregate_value_kind_sealed::Sealed
    for EnumAggregateValueKind<'ctx, 'scope, T>
{
}

fn fixed_type_struct_ir_type<'ctx: 'scope, 'scope, T: FixedTypeStructValue<'ctx, 'scope>>(
    ctx: ContextRef<'ctx>,
) -> IrStructType<'ctx> {
    struct ValueTypeGetter<'ctx> {
        fields: Vec<IrStructFieldType<'ctx>>,
        ctx: ContextRef<'ctx>,
    }
    impl<'ctx: 'scope, 'scope, T: FixedTypeStructValue<'ctx, 'scope>>
        StructFieldFixedTypeVisitor<'ctx, 'scope, T> for ValueTypeGetter<'ctx>
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
    let fields = T::visit_field_fixed_types(ValueTypeGetter {
        fields: Vec::new(),
        ctx,
    })
    .unwrap()
    .fields;
    assert_eq!(fields.len(), T::FIELD_COUNT);
    IrStructType::new(ctx, fields)
}

fn enum_ir_type<'ctx: 'scope, 'scope, T: EnumValue<'ctx, 'scope>>(
    ctx: ContextRef<'ctx>,
) -> IrEnumType<'ctx> {
    struct ValueTypeGetter<'ctx> {
        variants: Vec<IrEnumVariantType<'ctx>>,
        ctx: ContextRef<'ctx>,
    }
    impl<'ctx: 'scope, 'scope, Enum: EnumValue<'ctx, 'scope>>
        EnumVariantTypeVisitor<'ctx, 'scope, Enum> for ValueTypeGetter<'ctx>
    {
        type BreakType = Infallible;

        fn variant<VariantType: FixedTypeStructValue<'ctx, 'scope>>(
            mut self,
            name: &'static str,
            discriminant: Int<Enum::DiscriminantShape>,
        ) -> Result<Self, Self::BreakType> {
            self.variants.push(IrEnumVariantType {
                name: name.intern(self.ctx),
                discriminant: LiteralBits::from(discriminant),
                fields: fixed_type_struct_ir_type::<VariantType>(self.ctx),
            });
            Ok(self)
        }
    }
    let variants = T::visit_variant_types(ValueTypeGetter {
        variants: Vec::new(),
        ctx,
    })
    .unwrap()
    .variants;
    IrEnumType::new(
        ctx,
        T::DiscriminantShape::default().shape().into(),
        variants,
    )
}

impl<'ctx: 'scope, 'scope, T: EnumValue<'ctx, 'scope>> AggregateValueKind<'ctx, 'scope>
    for EnumAggregateValueKind<'ctx, 'scope, T>
{
    type AggregateValue = T;
    fn get_value(
        value: &Self::AggregateValue,
        ctx: ContextRef<'ctx>,
    ) -> Val<'ctx, 'ctx, Self::AggregateValue> {
        todo!()
    }
    fn static_value_type_opt(
        ctx: ContextRef<'ctx>,
    ) -> Option<ValueType<'ctx, Self::AggregateValue>> {
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(enum_ir_type::<T>(ctx)).intern(ctx),
        ))
    }
}

impl<'ctx: 'scope, 'scope, T: FixedTypeEnumValue<'ctx, 'scope>>
    FixedTypeAggregateValueKind<'ctx, 'scope> for EnumAggregateValueKind<'ctx, 'scope, T>
{
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self::AggregateValue> {
        ValueType::from_ir_unchecked(ctx, IrValueType::from(enum_ir_type::<T>(ctx)).intern(ctx))
    }
}
