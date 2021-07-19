// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, ContextRef, Intern},
    ir::{
        types::{IrEnumType, IrEnumVariantType, IrStructFieldType, IrStructType, IrValueType},
        values::{IrValue, LiteralBits, LiteralEnumVariant, LiteralStruct, LiteralStructField},
        SourceLocation,
    },
    prelude::{FixedTypeValue, Int, Val, Value, ValueType},
    values::{
        integer::{IntShape, IntShapeTrait, UIntShape},
        LazyVal,
    },
};
use alloc::vec::Vec;
use core::{convert::Infallible, hash::Hash, marker::PhantomData};

mod aggregate_value_kind_sealed {
    pub trait Sealed {}
}

pub trait AggregateValueKind<'ctx: 'scope, 'scope>: aggregate_value_kind_sealed::Sealed {
    type AggregateValue: AggregateValue<'ctx, 'scope, AggregateValueKind = Self>;
    fn get_value<Ctx: AsContext<'ctx>>(
        value: &Self::AggregateValue,
        ctx: Ctx,
    ) -> Val<'ctx, 'ctx, Self::AggregateValue>;
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(
        ctx: Ctx,
    ) -> Option<ValueType<'ctx, Self::AggregateValue>>;
}

pub trait FixedTypeAggregateValueKind<'ctx: 'scope, 'scope>:
    AggregateValueKind<'ctx, 'scope>
where
    Self::AggregateValue: FixedTypeValue<'ctx>,
{
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self::AggregateValue>;
}

pub trait AggregateValueMatch<'ctx: 'scope, 'scope> {
    fn match_value_without_scope_check<
        R,
        E,
        F: FnMut(Self::AggregateOfFieldLazyValues) -> Result<R, E>,
    >(
        value: LazyVal<'ctx, 'scope, Self>,
        f: F,
    ) -> Result<R, E>
    where
        Self: AggregateValue<'ctx, 'scope>;
}

pub trait AggregateOfFieldLazyValues<'ctx: 'scope, 'scope>: 'scope + Clone {
    type Aggregate: AggregateValue<'ctx, 'scope, AggregateOfFieldLazyValues = Self>;
}

pub trait AggregateValue<'ctx: 'scope, 'scope>: Value<'ctx> {
    type AggregateValueKind: AggregateValueKind<'ctx, 'scope>;
    type DiscriminantShape: IntShapeTrait + Default;
    type AggregateOfFieldLazyValues: AggregateOfFieldLazyValues<'ctx, 'scope, Aggregate = Self>;
}

impl<'ctx: 'scope, 'scope, T: AggregateValue<'ctx, 'scope>> Value<'ctx> for T
where
    T::AggregateValueKind: AggregateValueKind<'ctx, 'scope, AggregateValue = T>,
{
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, 'ctx, Self> {
        T::AggregateValueKind::get_value(self, ctx.ctx())
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Option<ValueType<'ctx, Self>> {
        T::AggregateValueKind::static_value_type_opt(ctx.ctx())
    }
}

impl<'ctx: 'scope, 'scope, T: AggregateValue<'ctx, 'scope>> FixedTypeValue<'ctx> for T
where
    T::AggregateValueKind: AggregateValueKind<'ctx, 'scope, AggregateValue = T>
        + FixedTypeAggregateValueKind<'ctx, 'scope>,
{
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self> {
        T::AggregateValueKind::static_value_type(ctx.ctx())
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
    fn get_value<Ctx: AsContext<'ctx>>(
        value: &Self::AggregateValue,
        ctx: Ctx,
    ) -> Val<'ctx, 'ctx, Self::AggregateValue> {
        let ctx = ctx.ctx();
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
            IrValue::from(LiteralStruct::new(ctx, fields, &SourceLocation::caller())).intern(ctx),
        )
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(
        ctx: Ctx,
    ) -> Option<ValueType<'ctx, Self::AggregateValue>> {
        let ctx = ctx.ctx();
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
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self::AggregateValue> {
        let ctx = ctx.ctx();
        ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(fixed_type_struct_ir_type::<T, _>(ctx)).intern(ctx),
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
    fn visit_fields<'a, V: StructFieldVisitor<'ctx, 'scope, Self>>(
        &'a self,
        visitor: V,
    ) -> Result<V, V::BreakType>
    where
        'scope: 'a;
    fn visit_field_types<V: StructFieldTypeVisitor<'ctx, 'scope, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
    fn get_field_lazy_values(
        value: LazyVal<'ctx, 'scope, Self>,
    ) -> Self::AggregateOfFieldLazyValues;
}

impl<'ctx: 'scope, 'scope, T: StructValue<'ctx, 'scope>> AggregateValueMatch<'ctx, 'scope> for T {
    fn match_value_without_scope_check<
        R,
        E,
        F: FnMut(T::AggregateOfFieldLazyValues) -> Result<R, E>,
    >(
        value: LazyVal<'ctx, 'scope, Self>,
        mut f: F,
    ) -> Result<R, E> {
        f(Self::get_field_lazy_values(value))
    }
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

pub trait EnumVariantFieldVisitor<
    'ctx: 'scope,
    'scope,
    Enum: EnumValue<'ctx, 'scope>,
    EnumVariant: EnumVariantRef<'ctx, 'scope, Enum>,
>: Sized
{
    type BreakType;
    fn field<FieldType: FixedTypeValue<'ctx>>(
        self,
        name: &'static str,
        field_index: usize,
        field: &FieldType,
    ) -> Result<Self, Self::BreakType>;
}

pub trait EnumVariantFieldTypeVisitor<
    'ctx: 'scope,
    'scope,
    Enum: EnumValue<'ctx, 'scope>,
    EnumVariant: EnumVariantRef<'ctx, 'scope, Enum>,
>: Sized
{
    type BreakType;
    fn field<FieldType: FixedTypeValue<'ctx>>(
        self,
        name: &'static str,
        field_index: usize,
    ) -> Result<Self, Self::BreakType>;
}

pub trait EnumVariantRef<'ctx: 'scope, 'scope, Enum: EnumValue<'ctx, 'scope>>: Copy {
    fn visit_fields<V: EnumVariantFieldVisitor<'ctx, 'scope, Enum, Self>>(
        self,
        visitor: V,
    ) -> Result<V, V::BreakType>;
    fn visit_field_types<V: EnumVariantFieldTypeVisitor<'ctx, 'scope, Enum, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

pub trait EnumVariantVisitor<'ctx: 'scope, 'scope, Enum: EnumValue<'ctx, 'scope>>: Sized {
    type ResultType;
    fn variant<VariantType: EnumVariantRef<'ctx, 'scope, Enum>>(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
        variant: VariantType,
    ) -> Self::ResultType;
}

pub trait EnumVariantTypeVisitor<'ctx: 'scope, 'scope, Enum: EnumValue<'ctx, 'scope>>:
    Sized
{
    type BreakType;
    fn variant<VariantType: EnumVariantRef<'ctx, 'scope, Enum>>(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
    ) -> Result<Self, Self::BreakType>;
    fn variant_with_type_hint<
        VariantType: EnumVariantRef<'ctx, 'scope, Enum>,
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
    fn visit_variant<'a, V: EnumVariantVisitor<'ctx, 'scope, Self>>(
        &'a self,
        visitor: V,
    ) -> V::ResultType
    where
        'scope: 'a;
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

fn fixed_type_struct_ir_type<
    'ctx: 'scope,
    'scope,
    T: FixedTypeStructValue<'ctx, 'scope>,
    Ctx: AsContext<'ctx>,
>(
    ctx: Ctx,
) -> IrStructType<'ctx> {
    let ctx = ctx.ctx();
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

fn enum_ir_type<'ctx: 'scope, 'scope, T: EnumValue<'ctx, 'scope>, Ctx: AsContext<'ctx>>(
    ctx: Ctx,
) -> IrEnumType<'ctx> {
    let ctx = ctx.ctx();
    struct VariantValueTypeGetter<'ctx> {
        fields: Vec<IrStructFieldType<'ctx>>,
        ctx: ContextRef<'ctx>,
    }
    impl<
            'ctx: 'scope,
            'scope,
            Enum: EnumValue<'ctx, 'scope>,
            EnumVariant: EnumVariantRef<'ctx, 'scope, Enum>,
        > EnumVariantFieldTypeVisitor<'ctx, 'scope, Enum, EnumVariant>
        for VariantValueTypeGetter<'ctx>
    {
        type BreakType = Infallible;

        fn field<FieldType: FixedTypeValue<'ctx>>(
            mut self,
            name: &'static str,
            field_index: usize,
        ) -> Result<Self, Self::BreakType> {
            assert_eq!(field_index, self.fields.len());
            self.fields.push(IrStructFieldType {
                name: name.intern(self.ctx),
                ty: FieldType::static_value_type(self.ctx).ir(),
            });
            Ok(self)
        }
    }
    struct ValueTypeGetter<'ctx> {
        variants: Vec<IrEnumVariantType<'ctx>>,
        ctx: ContextRef<'ctx>,
    }
    impl<'ctx: 'scope, 'scope, Enum: EnumValue<'ctx, 'scope>>
        EnumVariantTypeVisitor<'ctx, 'scope, Enum> for ValueTypeGetter<'ctx>
    {
        type BreakType = Infallible;

        fn variant<VariantType: EnumVariantRef<'ctx, 'scope, Enum>>(
            mut self,
            name: &'static str,
            discriminant: Int<Enum::DiscriminantShape>,
        ) -> Result<Self, Self::BreakType> {
            let fields = VariantType::visit_field_types(VariantValueTypeGetter {
                ctx: self.ctx,
                fields: Vec::new(),
            })
            .unwrap()
            .fields;
            self.variants.push(IrEnumVariantType {
                name: name.intern(self.ctx),
                discriminant: LiteralBits::from(discriminant),
                fields: IrStructType::new(self.ctx, fields),
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
        &SourceLocation::caller(),
    )
}

impl<'ctx: 'scope, 'scope, T: EnumValue<'ctx, 'scope>> AggregateValueKind<'ctx, 'scope>
    for EnumAggregateValueKind<'ctx, 'scope, T>
{
    type AggregateValue = T;
    fn get_value<Ctx: AsContext<'ctx>>(
        value: &Self::AggregateValue,
        ctx: Ctx,
    ) -> Val<'ctx, 'ctx, Self::AggregateValue> {
        let ctx = ctx.ctx();
        struct VariantFieldsValueGetter<'ctx> {
            ctx: ContextRef<'ctx>,
            fields: Vec<LiteralStructField<'ctx>>,
        }
        impl<
                'ctx: 'scope,
                'scope,
                Enum: EnumValue<'ctx, 'scope>,
                EnumVariant: EnumVariantRef<'ctx, 'scope, Enum>,
            > EnumVariantFieldVisitor<'ctx, 'scope, Enum, EnumVariant>
            for VariantFieldsValueGetter<'ctx>
        {
            type BreakType = Infallible;
            fn field<FieldType: FixedTypeValue<'ctx>>(
                mut self,
                name: &'static str,
                field_index: usize,
                field: &FieldType,
            ) -> Result<Self, Self::BreakType> {
                assert_eq!(field_index, self.fields.len());
                self.fields.push(LiteralStructField {
                    name: name.intern(self.ctx),
                    value: field.get_value(self.ctx).ir(),
                });
                Ok(self)
            }
        }
        struct ValueGetter<'ctx> {
            ctx: ContextRef<'ctx>,
        }
        impl<'ctx: 'scope, 'scope, Enum: EnumValue<'ctx, 'scope>>
            EnumVariantVisitor<'ctx, 'scope, Enum> for ValueGetter<'ctx>
        {
            type ResultType = Val<'ctx, 'ctx, Enum>;

            fn variant<VariantType: EnumVariantRef<'ctx, 'scope, Enum>>(
                self,
                name: &'static str,
                discriminant: Int<Enum::DiscriminantShape>,
                variant: VariantType,
            ) -> Self::ResultType {
                let enum_type = enum_ir_type::<Enum, _>(self.ctx);
                let variant_index = enum_type
                    .get_variant_index(&discriminant.into())
                    .expect("variant not found");
                let value_type = ValueType::from_ir_unchecked(
                    self.ctx,
                    IrValueType::from(enum_type).intern(self.ctx),
                );
                let fields = variant
                    .visit_fields(VariantFieldsValueGetter {
                        ctx: self.ctx,
                        fields: Vec::new(),
                    })
                    .unwrap()
                    .fields;
                let fields_value = IrValue::from(LiteralStruct::new(
                    self.ctx,
                    fields,
                    &SourceLocation::caller(),
                ))
                .intern(self.ctx);
                let literal_enum_variant = LiteralEnumVariant::new(
                    self.ctx,
                    enum_type,
                    variant_index,
                    fields_value,
                    &SourceLocation::caller(),
                );
                assert_eq!(*name, *literal_enum_variant.variant().name);
                Val::from_ir_and_type_unchecked(
                    IrValue::from(literal_enum_variant).intern(self.ctx),
                    value_type,
                )
            }
        }
        value.visit_variant(ValueGetter { ctx })
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(
        ctx: Ctx,
    ) -> Option<ValueType<'ctx, Self::AggregateValue>> {
        let ctx = ctx.ctx();
        Some(ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(enum_ir_type::<T, _>(ctx)).intern(ctx),
        ))
    }
}

impl<'ctx: 'scope, 'scope, T: FixedTypeEnumValue<'ctx, 'scope>>
    FixedTypeAggregateValueKind<'ctx, 'scope> for EnumAggregateValueKind<'ctx, 'scope, T>
{
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self::AggregateValue> {
        let ctx = ctx.ctx();
        ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(enum_ir_type::<T, _>(ctx)).intern(ctx),
        )
    }
}

#[derive(Debug, Clone, Copy)]
#[must_use]
pub struct EnumDiscriminantShapeCalculator {
    min_discriminant: i128,
    max_discriminant: i128,
}

impl EnumDiscriminantShapeCalculator {
    pub const fn new() -> Self {
        Self {
            min_discriminant: i128::MAX,
            max_discriminant: i128::MIN,
        }
    }
    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.max_discriminant < self.min_discriminant
    }
    pub const fn add_discriminant(mut self, discriminant: i128) -> Self {
        if discriminant < self.min_discriminant {
            self.min_discriminant = discriminant;
        }
        if discriminant > self.max_discriminant {
            self.max_discriminant = discriminant;
        }
        self
    }
    #[must_use]
    pub const fn get_shape(self) -> IntShape {
        if self.is_empty() {
            IntShape {
                bit_count: 0,
                signed: false,
            }
        } else {
            let mut retval = IntShape {
                bit_count: 0,
                signed: self.min_discriminant < 0,
            };
            while retval.wrap_i128(self.min_discriminant) != self.min_discriminant
                || retval.wrap_i128(self.max_discriminant) != self.max_discriminant
            {
                retval.bit_count += 1;
            }
            retval
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enum_discriminant_shape_calculator() {
        assert!(EnumDiscriminantShapeCalculator::new().is_empty());
        assert!(!EnumDiscriminantShapeCalculator::new()
            .add_discriminant(0)
            .is_empty());
        assert!(!EnumDiscriminantShapeCalculator::new()
            .add_discriminant(1)
            .is_empty());
        assert!(!EnumDiscriminantShapeCalculator::new()
            .add_discriminant(-1)
            .is_empty());
        assert!(!EnumDiscriminantShapeCalculator::new()
            .add_discriminant(i128::MAX)
            .is_empty());
        assert!(!EnumDiscriminantShapeCalculator::new()
            .add_discriminant(i128::MIN)
            .is_empty());
        assert_eq!(
            EnumDiscriminantShapeCalculator::new().get_shape(),
            IntShape {
                bit_count: 0,
                signed: false
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(0)
                .get_shape(),
            IntShape {
                bit_count: 0,
                signed: false
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(1)
                .get_shape(),
            IntShape {
                bit_count: 1,
                signed: false
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(-1)
                .get_shape(),
            IntShape {
                bit_count: 1,
                signed: true
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(-1)
                .add_discriminant(0)
                .get_shape(),
            IntShape {
                bit_count: 1,
                signed: true
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(-1)
                .add_discriminant(1)
                .get_shape(),
            IntShape {
                bit_count: 2,
                signed: true
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(0)
                .add_discriminant(1)
                .get_shape(),
            IntShape {
                bit_count: 1,
                signed: false
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(i128::MIN)
                .get_shape(),
            IntShape {
                bit_count: 128,
                signed: true
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(i128::MAX)
                .get_shape(),
            IntShape {
                bit_count: 127,
                signed: false
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(u64::MAX as _)
                .get_shape(),
            IntShape {
                bit_count: 64,
                signed: false
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(i64::MIN as _)
                .get_shape(),
            IntShape {
                bit_count: 64,
                signed: true
            }
        );
        assert_eq!(
            EnumDiscriminantShapeCalculator::new()
                .add_discriminant(i64::MIN as i128 - 1)
                .get_shape(),
            IntShape {
                bit_count: 65,
                signed: true
            }
        );
    }
}
