// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, ContextRef, Intern},
    ir::{
        scope::{Scope, ScopeRef},
        types::{IrEnumType, IrEnumVariantType, IrStructFieldType, IrStructType, IrValueType},
        values::{
            ExpandScope, IrValue, LiteralBits, LiteralEnumVariant, LiteralStruct,
            LiteralStructField, ShrinkScope,
        },
        SourceLocation,
    },
    prelude::{FixedTypeValue, Int, ToVal, Val, Value, ValueType},
    values::integer::{IntShape, IntShapeTrait, UIntShape},
};
use alloc::vec::Vec;
use core::{convert::Infallible, hash::Hash, marker::PhantomData};

mod aggregate_value_kind_sealed {
    pub trait Sealed {}
}

pub trait AggregateValueKind<'ctx>: aggregate_value_kind_sealed::Sealed {
    type AggregateValue: AggregateValue<'ctx, AggregateValueKind = Self>;
    fn get_value<Ctx: AsContext<'ctx>>(
        value: &Self::AggregateValue,
        ctx: Ctx,
    ) -> Val<'ctx, Self::AggregateValue>;
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(
        ctx: Ctx,
    ) -> Option<ValueType<'ctx, Self::AggregateValue>>;
}

pub trait FixedTypeAggregateValueKind<'ctx>: AggregateValueKind<'ctx>
where
    Self::AggregateValue: FixedTypeValue<'ctx>,
{
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self::AggregateValue>;
}

pub trait AggregateOfFieldValues<'ctx>: 'ctx + Copy {
    type Aggregate: AggregateValue<'ctx, AggregateOfFieldValues = Self>;
}

pub struct AggregateValueMatchArm<'ctx, V> {
    pub value: V,
    pub match_arm_scope: ScopeRef<'ctx>,
}

pub trait AggregateValueMatch<'ctx>: Value<'ctx> {
    fn match_value<RV: FixedTypeValue<'ctx>, R: ToVal<'ctx, ValueType = RV>, E, F>(
        value: Val<'ctx, Self>,
        match_result_scope: ScopeRef<'ctx>,
        f: F,
        caller: &SourceLocation<'ctx>,
    ) -> Result<Val<'ctx, RV>, E>
    where
        Self: AggregateValue<'ctx>,
        F: FnMut(AggregateValueMatchArm<'ctx, Self::AggregateOfFieldValues>) -> Result<R, E>;
}

impl<'ctx, This> AggregateValueMatch<'ctx> for This
where
    Self: StructValue<'ctx>,
{
    fn match_value<RV: FixedTypeValue<'ctx>, R: ToVal<'ctx, ValueType = RV>, E, F>(
        value: Val<'ctx, Self>,
        match_result_scope: ScopeRef<'ctx>,
        mut f: F,
        caller: &SourceLocation<'ctx>,
    ) -> Result<Val<'ctx, RV>, E>
    where
        F: FnMut(
            AggregateValueMatchArm<'ctx, <Self as AggregateValue<'ctx>>::AggregateOfFieldValues>,
        ) -> Result<R, E>,
    {
        match_result_scope.assert_scope_can_access_value(value.ir(), caller);
        let match_arm_scope = Scope::new(match_result_scope, *caller);
        let shrunk_value = Val::from_ir_and_type_unchecked(
            IrValue::from(ShrinkScope::new(
                value.ctx(),
                value.ir(),
                match_arm_scope,
                caller,
            ))
            .intern(value.ctx()),
            value.value_type(),
        );
        let fields = This::get_field_values(shrunk_value);
        let result = f(AggregateValueMatchArm {
            match_arm_scope,
            value: fields,
        })?
        .to_val(value.ctx());
        Ok(Val::from_ir_and_type_unchecked(
            IrValue::from(ExpandScope::new(
                value.ctx(),
                result.ir(),
                match_arm_scope,
                match_result_scope,
                caller,
            ))
            .intern(value.ctx()),
            result.value_type(),
        ))
    }
}

pub trait AggregateValue<'ctx>: Value<'ctx> + AggregateValueMatch<'ctx> {
    type AggregateValueKind: AggregateValueKind<'ctx>;
    type DiscriminantShape: IntShapeTrait + Default;
    type AggregateOfFieldValues: AggregateOfFieldValues<'ctx, Aggregate = Self>;
}

impl<'ctx, T: AggregateValue<'ctx>> Value<'ctx> for T
where
    T::AggregateValueKind: AggregateValueKind<'ctx, AggregateValue = T>,
{
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self> {
        T::AggregateValueKind::get_value(self, ctx.ctx())
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Option<ValueType<'ctx, Self>> {
        T::AggregateValueKind::static_value_type_opt(ctx.ctx())
    }
}

impl<'ctx, T: AggregateValue<'ctx>> FixedTypeValue<'ctx> for T
where
    T::AggregateValueKind:
        AggregateValueKind<'ctx, AggregateValue = T> + FixedTypeAggregateValueKind<'ctx>,
{
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self> {
        T::AggregateValueKind::static_value_type(ctx.ctx())
    }
}

pub struct StructAggregateValueKind<'ctx, T>(PhantomData<Val<'ctx, T>>)
where
    T: StructValue<'ctx> + AggregateValue<'ctx, AggregateValueKind = Self>;

impl<'ctx, T: StructValue<'ctx>> aggregate_value_kind_sealed::Sealed
    for StructAggregateValueKind<'ctx, T>
{
}

impl<'ctx, T: StructValue<'ctx>> AggregateValueKind<'ctx> for StructAggregateValueKind<'ctx, T> {
    type AggregateValue = T;
    fn get_value<Ctx: AsContext<'ctx>>(
        value: &Self::AggregateValue,
        ctx: Ctx,
    ) -> Val<'ctx, Self::AggregateValue> {
        let ctx = ctx.ctx();
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
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self::AggregateValue> {
        let ctx = ctx.ctx();
        ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(fixed_type_struct_ir_type::<T, _>(ctx)).intern(ctx),
        )
    }
}

pub trait StructValue<'ctx>:
    AggregateValue<
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
    fn get_field_values(value: Val<'ctx, Self>) -> Self::AggregateOfFieldValues;
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

pub trait EnumVariantFieldVisitor<
    'ctx,
    Enum: EnumValue<'ctx>,
    EnumVariant: EnumVariantRef<'ctx, Enum>,
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
    'ctx,
    Enum: EnumValue<'ctx>,
    EnumVariant: EnumVariantRef<'ctx, Enum>,
>: Sized
{
    type BreakType;
    fn field<FieldType: FixedTypeValue<'ctx>>(
        self,
        name: &'static str,
        field_index: usize,
    ) -> Result<Self, Self::BreakType>;
}

pub trait EnumVariantRef<'ctx, Enum: EnumValue<'ctx>>: Copy {
    fn visit_fields<V: EnumVariantFieldVisitor<'ctx, Enum, Self>>(
        self,
        visitor: V,
    ) -> Result<V, V::BreakType>;
    fn visit_field_types<V: EnumVariantFieldTypeVisitor<'ctx, Enum, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

pub trait EnumVariantVisitor<'ctx, Enum: EnumValue<'ctx>>: Sized {
    type ResultType;
    fn variant<VariantType: EnumVariantRef<'ctx, Enum>>(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
        variant: VariantType,
    ) -> Self::ResultType;
}

pub trait EnumVariantTypeVisitor<'ctx, Enum: EnumValue<'ctx>>: Sized {
    type BreakType;
    fn variant<VariantType: EnumVariantRef<'ctx, Enum>>(
        self,
        name: &'static str,
        discriminant: Int<Enum::DiscriminantShape>,
    ) -> Result<Self, Self::BreakType>;
    fn variant_with_type_hint<
        VariantType: EnumVariantRef<'ctx, Enum>,
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

pub trait EnumGetIrType<'ctx>: AggregateValue<'ctx> {
    fn get_ir_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> IrEnumType<'ctx>
    where
        Self: EnumValue<'ctx>,
    {
        let ctx = ctx.ctx();
        struct VariantValueTypeGetter<'ctx> {
            fields: Vec<IrStructFieldType<'ctx>>,
            ctx: ContextRef<'ctx>,
        }
        impl<'ctx, Enum: EnumValue<'ctx>, EnumVariant: EnumVariantRef<'ctx, Enum>>
            EnumVariantFieldTypeVisitor<'ctx, Enum, EnumVariant> for VariantValueTypeGetter<'ctx>
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
        impl<'ctx, Enum: EnumValue<'ctx>> EnumVariantTypeVisitor<'ctx, Enum> for ValueTypeGetter<'ctx> {
            type BreakType = Infallible;

            fn variant<VariantType: EnumVariantRef<'ctx, Enum>>(
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
        let variants = Self::visit_variant_types(ValueTypeGetter {
            variants: Vec::new(),
            ctx,
        })
        .unwrap()
        .variants;
        IrEnumType::new(
            ctx,
            Self::DiscriminantShape::default().shape().into(),
            variants,
            &SourceLocation::caller(),
        )
    }
}

impl<'ctx, This> EnumGetIrType<'ctx> for This where This: AggregateValue<'ctx> {}

pub trait EnumValue<'ctx>:
    AggregateValue<'ctx, AggregateValueKind = EnumAggregateValueKind<'ctx, Self>> + EnumGetIrType<'ctx>
{
    fn visit_variant<V: EnumVariantVisitor<'ctx, Self>>(&self, visitor: V) -> V::ResultType;
    fn visit_variant_types<V: EnumVariantTypeVisitor<'ctx, Self>>(
        visitor: V,
    ) -> Result<V, V::BreakType>;
}

pub trait FixedTypeEnumValue<'ctx>: EnumValue<'ctx> + FixedTypeValue<'ctx> {}

pub struct EnumAggregateValueKind<'ctx, T: EnumValue<'ctx>>(PhantomData<Val<'ctx, T>>);

impl<'ctx, T: EnumValue<'ctx>> aggregate_value_kind_sealed::Sealed
    for EnumAggregateValueKind<'ctx, T>
{
}

fn fixed_type_struct_ir_type<'ctx, T: FixedTypeStructValue<'ctx>, Ctx: AsContext<'ctx>>(
    ctx: Ctx,
) -> IrStructType<'ctx> {
    let ctx = ctx.ctx();
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

impl<'ctx, T: EnumValue<'ctx>> AggregateValueKind<'ctx> for EnumAggregateValueKind<'ctx, T> {
    type AggregateValue = T;
    fn get_value<Ctx: AsContext<'ctx>>(
        value: &Self::AggregateValue,
        ctx: Ctx,
    ) -> Val<'ctx, Self::AggregateValue> {
        let ctx = ctx.ctx();
        struct VariantFieldsValueGetter<'ctx> {
            ctx: ContextRef<'ctx>,
            fields: Vec<LiteralStructField<'ctx>>,
        }
        impl<'ctx, Enum: EnumValue<'ctx>, EnumVariant: EnumVariantRef<'ctx, Enum>>
            EnumVariantFieldVisitor<'ctx, Enum, EnumVariant> for VariantFieldsValueGetter<'ctx>
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
        impl<'ctx, Enum: EnumValue<'ctx>> EnumVariantVisitor<'ctx, Enum> for ValueGetter<'ctx> {
            type ResultType = Val<'ctx, Enum>;

            fn variant<VariantType: EnumVariantRef<'ctx, Enum>>(
                self,
                name: &'static str,
                discriminant: Int<Enum::DiscriminantShape>,
                variant: VariantType,
            ) -> Self::ResultType {
                let enum_type = Enum::get_ir_type(self.ctx);
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
            IrValueType::from(T::get_ir_type(ctx)).intern(ctx),
        ))
    }
}

impl<'ctx, T: FixedTypeEnumValue<'ctx>> FixedTypeAggregateValueKind<'ctx>
    for EnumAggregateValueKind<'ctx, T>
{
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self::AggregateValue> {
        let ctx = ctx.ctx();
        ValueType::from_ir_unchecked(ctx, IrValueType::from(T::get_ir_type(ctx)).intern(ctx))
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
