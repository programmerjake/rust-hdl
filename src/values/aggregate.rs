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

pub trait AggregateValueKind<'ctx>: aggregate_value_kind_sealed::Sealed {
    type AggregateValue: AggregateValue<'ctx, AggregateValueKind = Self>;
    fn get_value(
        value: &Self::AggregateValue,
        ctx: ContextRef<'ctx>,
    ) -> Val<'ctx, 'ctx, Self::AggregateValue>;
    fn static_value_type_opt(
        ctx: ContextRef<'ctx>,
    ) -> Option<ValueType<'ctx, Self::AggregateValue>>;
}

pub trait FixedTypeAggregateValueKind<'ctx>: AggregateValueKind<'ctx>
where
    Self::AggregateValue: FixedTypeValue<'ctx>,
{
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self::AggregateValue>;
}

pub trait AggregateValue<'ctx>: Value<'ctx> {
    type AggregateValueKind: AggregateValueKind<'ctx>;
    type DiscriminantShape: IntShapeTrait + Default;
}

impl<
        'ctx,
        T: AggregateValue<'ctx, AggregateValueKind = K>,
        K: AggregateValueKind<'ctx, AggregateValue = T>,
    > Value<'ctx> for T
{
    fn get_value(&self, ctx: ContextRef<'ctx>) -> Val<'ctx, 'ctx, Self> {
        K::get_value(self, ctx)
    }
    fn static_value_type_opt(ctx: ContextRef<'ctx>) -> Option<ValueType<'ctx, Self>> {
        K::static_value_type_opt(ctx)
    }
}

impl<
        'ctx,
        T: AggregateValue<'ctx, AggregateValueKind = K>,
        K: FixedTypeAggregateValueKind<'ctx> + AggregateValueKind<'ctx, AggregateValue = T>,
    > FixedTypeValue<'ctx> for T
{
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self> {
        K::static_value_type(ctx)
    }
}

pub struct StructAggregateValueKind<
    'ctx,
    T: StructValue<'ctx> + AggregateValue<'ctx, AggregateValueKind = Self>,
>(PhantomData<ValueType<'ctx, T>>);

impl<'ctx, T: StructValue<'ctx>> aggregate_value_kind_sealed::Sealed
    for StructAggregateValueKind<'ctx, T>
{
}

impl<'ctx, T: StructValue<'ctx>> AggregateValueKind<'ctx> for StructAggregateValueKind<'ctx, T> {
    type AggregateValue = T;
    fn get_value(
        value: &Self::AggregateValue,
        ctx: ContextRef<'ctx>,
    ) -> Val<'ctx, 'ctx, Self::AggregateValue> {
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

impl<'ctx, T: FixedTypeStructValue<'ctx>> FixedTypeAggregateValueKind<'ctx>
    for StructAggregateValueKind<'ctx, T>
{
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self::AggregateValue> {
        ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(fixed_type_struct_ir_type::<T>(ctx)).intern(ctx),
        )
    }
}

pub trait StructValueStructOfFieldValues<'ctx: 'scope, 'scope> {
    type StructOfFieldValues: 'scope + Copy;
}

pub trait StructValue<'ctx>:
    for<'scope> StructValueStructOfFieldValues<'ctx, 'scope>
    + AggregateValue<
        'ctx,
        AggregateValueKind = StructAggregateValueKind<'ctx, Self>,
        DiscriminantShape = UIntShape<0>,
    >
{
    type FieldEnum: 'static + Copy + Send + Sync + Ord + Hash + Into<usize>;
    type StructOfFieldEnums: 'static + Copy + Send + Sync;
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

pub trait FixedTypeStructValue<'ctx>: FixedTypeValue<'ctx> + StructValue<'ctx> {
    fn visit_field_fixed_types<V: StructFieldFixedTypeVisitor<'ctx, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

pub trait EnumVariantVisitor<'ctx, Enum: EnumValue<'ctx>>: Sized {
    type ResultType;
    fn variant<VariantType: FixedTypeStructValue<'ctx>>(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
        variant: &VariantType,
    ) -> Self::ResultType;
}

pub trait EnumVariantTypeVisitor<'ctx, Enum: EnumValue<'ctx>>: Sized {
    type BreakType;
    fn variant<VariantType: FixedTypeStructValue<'ctx>>(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
    ) -> Result<Self, Self::BreakType>;
    fn variant_with_type_hint<
        VariantType: FixedTypeStructValue<'ctx>,
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

pub trait EnumValue<'ctx>:
    AggregateValue<'ctx, AggregateValueKind = EnumAggregateValueKind<'ctx, Self>>
{
    fn visit_variant<V: EnumVariantVisitor<'ctx, Self>>(&self, visitor: V) -> V::ResultType;
    fn visit_variant_types<V: EnumVariantTypeVisitor<'ctx, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

pub trait FixedTypeEnumValue<'ctx>: EnumValue<'ctx> + FixedTypeValue<'ctx> {}

pub struct EnumAggregateValueKind<'ctx, T: EnumValue<'ctx>>(PhantomData<ValueType<'ctx, T>>);

impl<'ctx, T: EnumValue<'ctx>> aggregate_value_kind_sealed::Sealed
    for EnumAggregateValueKind<'ctx, T>
{
}

fn fixed_type_struct_ir_type<'ctx, T: FixedTypeStructValue<'ctx>>(
    ctx: ContextRef<'ctx>,
) -> IrStructType<'ctx> {
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
    let fields = T::visit_field_fixed_types(ValueTypeGetter {
        fields: Vec::new(),
        ctx,
    })
    .unwrap()
    .fields;
    assert_eq!(fields.len(), T::FIELD_COUNT);
    IrStructType::new(ctx, fields)
}

fn enum_ir_type<'ctx, T: EnumValue<'ctx>>(ctx: ContextRef<'ctx>) -> IrEnumType<'ctx> {
    struct ValueTypeGetter<'ctx> {
        variants: Vec<IrEnumVariantType<'ctx>>,
        ctx: ContextRef<'ctx>,
    }
    impl<'ctx, Enum: EnumValue<'ctx>> EnumVariantTypeVisitor<'ctx, Enum> for ValueTypeGetter<'ctx> {
        type BreakType = Infallible;

        fn variant<VariantType: FixedTypeStructValue<'ctx>>(
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

impl<'ctx, T: EnumValue<'ctx>> AggregateValueKind<'ctx> for EnumAggregateValueKind<'ctx, T> {
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

impl<'ctx, T: FixedTypeEnumValue<'ctx>> FixedTypeAggregateValueKind<'ctx>
    for EnumAggregateValueKind<'ctx, T>
{
    fn static_value_type(ctx: ContextRef<'ctx>) -> ValueType<'ctx, Self::AggregateValue> {
        ValueType::from_ir_unchecked(ctx, IrValueType::from(enum_ir_type::<T>(ctx)).intern(ctx))
    }
}
