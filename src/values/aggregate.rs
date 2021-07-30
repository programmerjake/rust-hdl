// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use core::{convert::Infallible, marker::PhantomData};

use alloc::vec::Vec;

use crate::{
    context::{AsContext, ContextRef, Intern},
    ir::{
        types::{IrAggregateType, IrFieldType, IrValueType, IrVariantType},
        values::{IrValue, IrValueRef, LiteralAggregateVariant, LiteralBits},
        SourceLocation,
    },
    prelude::{FixedTypeValue, Int, Val, Value, ValueType},
    values::integer::{IntShape, IntShapeTrait},
};

pub trait FieldValuesVisitor<'ctx, VV: VariantValue<'ctx>>: Sized {
    type BreakType;
    fn visit<FieldType: Value<'ctx>>(
        self,
        name: &'static str,
        value: Val<'ctx, FieldType>,
    ) -> Result<Self, Self::BreakType>;
}

pub trait FieldValueTypesVisitor<'ctx, VV: VariantValue<'ctx>>: Sized {
    type BreakType;
    fn visit<FieldType: Value<'ctx>>(self, name: &'static str) -> Result<Self, Self::BreakType>;
}

pub trait FieldValueFixedTypesVisitor<'ctx, VV: VariantFixedTypeValue<'ctx>>: Sized {
    type BreakType;
    fn visit<FieldType: FixedTypeValue<'ctx>>(
        self,
        name: &'static str,
    ) -> Result<Self, Self::BreakType>;
}

pub trait VariantValue<'ctx>: 'ctx + Copy {
    type Aggregate: AggregateValue<'ctx, DiscriminantShape = Self::DiscriminantShape>;
    type DiscriminantShape: IntShapeTrait + Default;
    fn discriminant() -> Int<Self::DiscriminantShape>;
    fn visit_fields<Visitor: FieldValuesVisitor<'ctx, Self>>(
        self,
        visitor: Visitor,
    ) -> Result<Visitor, Visitor::BreakType>;
    fn visit_field_types<Visitor: FieldValueTypesVisitor<'ctx, Self>>(
        visitor: Visitor,
    ) -> Result<Visitor, Visitor::BreakType>;
    fn visit_variants_with_self_as_active_variant<
        Ctx: AsContext<'ctx>,
        Visitor: VariantVisitor<'ctx, Self::Aggregate>,
    >(
        self,
        ctx: Ctx,
        visitor: Visitor,
    ) -> Result<Visitor::AfterActiveVariant, Visitor::BreakType>;
}

pub fn get_variant_value<'ctx, Ctx: AsContext<'ctx>, VV: VariantValue<'ctx>>(
    ctx: Ctx,
    variant: VV,
) -> Val<'ctx, VV::Aggregate> {
    let ctx = ctx.ctx();
    get_aggregate_value(ctx, |visitor| {
        variant.visit_variants_with_self_as_active_variant(ctx, visitor)
    })
}

pub trait VariantFixedTypeValue<'ctx>:
    VariantValue<'ctx, Aggregate = <Self as VariantFixedTypeValue<'ctx>>::Aggregate>
{
    type Aggregate: FixedTypeAggregateValue<'ctx>;
    fn visit_field_fixed_types<Visitor: FieldValueFixedTypesVisitor<'ctx, Self>>(
        visitor: Visitor,
    ) -> Result<Visitor, Visitor::BreakType>;
}

#[derive(Debug, Clone, Copy)]
pub struct Variant<V> {
    pub name: &'static str,
    pub value: V,
}

pub trait VariantValuesVisitor<'ctx, T: StructOfVariantValues<'ctx>>: Sized {
    type BreakType;
    fn visit<VV: VariantValue<'ctx, Aggregate = T::Aggregate>>(
        self,
        variant: Variant<VV>,
    ) -> Result<Self, Self::BreakType>;
}

pub trait VariantValueTypesVisitor<'ctx, T: StructOfVariantValues<'ctx>>: Sized {
    type BreakType;
    fn visit<VV: VariantValue<'ctx, Aggregate = T::Aggregate>>(
        self,
        variant: Variant<()>,
    ) -> Result<Self, Self::BreakType>;
}

pub trait VariantValueFixedTypesVisitor<'ctx, T: StructOfVariantFixedTypeValues<'ctx>>:
    Sized
{
    type BreakType;
    fn visit<
        VV: VariantFixedTypeValue<
            'ctx,
            Aggregate = <T as StructOfVariantFixedTypeValues<'ctx>>::Aggregate,
        >,
    >(
        self,
        variant: Variant<()>,
    ) -> Result<Self, Self::BreakType>;
}

pub trait InactiveFieldVisitor<'ctx, VR: InactiveVariantRef<'ctx>>: Sized {
    type BreakType;
    fn visit<FieldType: Value<'ctx>>(
        self,
        name: &'static str,
        value_type: ValueType<'ctx, FieldType>,
    ) -> Result<Self, Self::BreakType>;
}

pub trait InactiveVariantRef<'ctx>: Copy {
    type Aggregate: AggregateValue<'ctx, DiscriminantShape = Self::DiscriminantShape>;
    type DiscriminantShape: IntShapeTrait + Default;
    fn discriminant() -> Int<Self::DiscriminantShape>;
    fn visit_fields<Visitor: InactiveFieldVisitor<'ctx, Self>>(
        self,
        visitor: Visitor,
    ) -> Result<Visitor, Visitor::BreakType>;
}

pub trait ActiveFieldVisitor<'ctx, VR: ActiveVariantRef<'ctx>>: Sized {
    type BreakType;
    fn visit<FieldType: Value<'ctx>>(
        self,
        name: &'static str,
        value: Val<'ctx, FieldType>,
    ) -> Result<Self, Self::BreakType>;
}

pub trait ActiveVariantRef<'ctx>: Copy {
    type Aggregate: AggregateValue<'ctx, DiscriminantShape = Self::DiscriminantShape>;
    type DiscriminantShape: IntShapeTrait + Default;
    fn discriminant() -> Int<Self::DiscriminantShape>;
    fn visit_fields<Visitor: ActiveFieldVisitor<'ctx, Self>>(
        self,
        visitor: Visitor,
    ) -> Result<Visitor, Visitor::BreakType>;
}

pub trait VariantVisitor<'ctx, A: AggregateValue<'ctx>>: Sized {
    type BreakType;
    type AfterActiveVariant;
    fn visit_before_active_variant<VR: InactiveVariantRef<'ctx, Aggregate = A>>(
        self,
        variant: Variant<VR>,
    ) -> Result<Self, Self::BreakType>;
    fn visit_active_variant<VR: ActiveVariantRef<'ctx, Aggregate = A>>(
        self,
        variant: Variant<VR>,
    ) -> Result<Self::AfterActiveVariant, Self::BreakType>;
    fn visit_after_active_variant<VR: InactiveVariantRef<'ctx, Aggregate = A>>(
        visitor: Self::AfterActiveVariant,
        variant: Variant<VR>,
    ) -> Result<Self::AfterActiveVariant, Self::BreakType>;
}

struct GetAggregateValueInactiveFieldVisitor<'ctx> {
    ctx: ContextRef<'ctx>,
    field_types: Vec<IrFieldType<'ctx>>,
}

impl<'ctx, VR: InactiveVariantRef<'ctx>> InactiveFieldVisitor<'ctx, VR>
    for GetAggregateValueInactiveFieldVisitor<'ctx>
{
    type BreakType = Infallible;
    fn visit<FieldType: Value<'ctx>>(
        mut self,
        name: &'static str,
        value_type: ValueType<'ctx, FieldType>,
    ) -> Result<Self, Self::BreakType> {
        self.field_types.push(IrFieldType {
            name: name.intern(self.ctx),
            ty: value_type.ir(),
        });
        Ok(self)
    }
}

struct GetAggregateValueActiveFieldVisitor<'ctx> {
    field_types: Vec<IrFieldType<'ctx>>,
    field_values: Vec<IrValueRef<'ctx>>,
}

impl<'ctx, VR: ActiveVariantRef<'ctx>> ActiveFieldVisitor<'ctx, VR>
    for GetAggregateValueActiveFieldVisitor<'ctx>
{
    type BreakType = Infallible;
    fn visit<FieldType: Value<'ctx>>(
        mut self,
        name: &'static str,
        value: Val<'ctx, FieldType>,
    ) -> Result<Self, Self::BreakType> {
        self.field_types.push(IrFieldType {
            name: name.intern(value.ctx()),
            ty: value.value_type().ir(),
        });
        self.field_values.push(value.ir());
        Ok(self)
    }
}

pub struct GetAggregateValueVisitor<'ctx, A: AggregateValue<'ctx>> {
    ctx: ContextRef<'ctx>,
    variant_types: Vec<IrVariantType<'ctx>>,
    _phantom: PhantomData<A>,
}

pub struct GetAggregateValueVisitorAfter<'ctx, A: AggregateValue<'ctx>> {
    base: GetAggregateValueVisitor<'ctx, A>,
    active_discriminant: LiteralBits,
    active_field_values: Vec<IrValueRef<'ctx>>,
}

impl<'ctx, A: AggregateValue<'ctx>> VariantVisitor<'ctx, A> for GetAggregateValueVisitor<'ctx, A> {
    type BreakType = Infallible;
    type AfterActiveVariant = GetAggregateValueVisitorAfter<'ctx, A>;
    fn visit_before_active_variant<VR: InactiveVariantRef<'ctx, Aggregate = A>>(
        mut self,
        variant: Variant<VR>,
    ) -> Result<Self, Self::BreakType> {
        let field_types = variant
            .value
            .visit_fields(GetAggregateValueInactiveFieldVisitor {
                ctx: self.ctx,
                field_types: Vec::new(),
            })
            .unwrap()
            .field_types;
        self.variant_types.push(IrVariantType::new(
            self.ctx,
            variant.name,
            VR::discriminant().into(),
            field_types,
        ));
        Ok(self)
    }
    fn visit_active_variant<VR: ActiveVariantRef<'ctx, Aggregate = A>>(
        mut self,
        variant: Variant<VR>,
    ) -> Result<Self::AfterActiveVariant, Self::BreakType> {
        let GetAggregateValueActiveFieldVisitor {
            field_types,
            field_values,
        } = variant
            .value
            .visit_fields(GetAggregateValueActiveFieldVisitor {
                field_types: Vec::new(),
                field_values: Vec::new(),
            })?;
        let active_discriminant = LiteralBits::from(VR::discriminant());
        self.variant_types.push(IrVariantType::new(
            self.ctx,
            variant.name,
            active_discriminant.clone(),
            field_types,
        ));
        Ok(GetAggregateValueVisitorAfter {
            base: self,
            active_discriminant,
            active_field_values: field_values,
        })
    }
    fn visit_after_active_variant<VR: InactiveVariantRef<'ctx, Aggregate = A>>(
        visitor: Self::AfterActiveVariant,
        variant: Variant<VR>,
    ) -> Result<Self::AfterActiveVariant, Self::BreakType> {
        let GetAggregateValueVisitorAfter {
            base,
            active_discriminant,
            active_field_values,
        } = visitor;
        let base = base.visit_before_active_variant(variant)?;
        Ok(GetAggregateValueVisitorAfter {
            base,
            active_discriminant,
            active_field_values,
        })
    }
}

pub fn get_aggregate_value<
    'ctx,
    Ctx: AsContext<'ctx>,
    A: AggregateValue<'ctx>,
    F: FnOnce(
        GetAggregateValueVisitor<'ctx, A>,
    ) -> Result<GetAggregateValueVisitorAfter<'ctx, A>, Infallible>,
>(
    ctx: Ctx,
    visit_variants: F,
) -> Val<'ctx, A> {
    let GetAggregateValueVisitorAfter {
        base:
            GetAggregateValueVisitor {
                ctx,
                variant_types,
                _phantom: _,
            },
        active_discriminant,
        active_field_values,
    } = visit_variants(GetAggregateValueVisitor {
        ctx: ctx.ctx(),
        variant_types: Vec::new(),
        _phantom: PhantomData,
    })
    .unwrap();
    let source_location = A::source_location();
    let value_type = IrAggregateType::new(
        ctx,
        active_discriminant.value_type(),
        variant_types,
        &source_location,
    );
    let variant_index = value_type
        .get_variant_index(&active_discriminant)
        .unwrap_or_else(|| {
            panic!(
                "discriminant for active variant not found\nat {}",
                source_location
            )
        });
    Val::from_ir_unchecked(
        ctx,
        IrValue::from(LiteralAggregateVariant::new(
            ctx,
            value_type,
            variant_index,
            active_field_values,
            &source_location,
        ))
        .intern(ctx),
    )
}

pub trait StructOfVariantValues<'ctx>: 'ctx + Copy {
    type Aggregate: AggregateValue<'ctx, StructOfVariantValues = Self>;
    fn visit_variant_values<Visitor: VariantValuesVisitor<'ctx, Self>>(
        self,
        visitor: Visitor,
    ) -> Result<Visitor, Visitor::BreakType>;
    fn visit_variant_types<Visitor: VariantValueTypesVisitor<'ctx, Self>>(
        visitor: Visitor,
    ) -> Result<Visitor, Visitor::BreakType>;
}

pub trait StructOfVariantFixedTypeValues<'ctx>:
    StructOfVariantValues<'ctx, Aggregate = <Self as StructOfVariantFixedTypeValues<'ctx>>::Aggregate>
{
    type Aggregate: FixedTypeAggregateValue<'ctx>;
    fn visit_variant_fixed_types<Visitor: VariantValueFixedTypesVisitor<'ctx, Self>>(
        visitor: Visitor,
    ) -> Result<Visitor, Visitor::BreakType>;
}

pub trait AggregateValue<'ctx>: Value<'ctx> {
    type DiscriminantShape: IntShapeTrait + Default;
    type StructOfVariantValues: StructOfVariantValues<'ctx, Aggregate = Self>;
    fn source_location() -> SourceLocation<'static>;
    fn struct_of_variant_values(aggregate: Val<'ctx, Self>) -> Self::StructOfVariantValues;
    fn visit_variants<Ctx: AsContext<'ctx>, Visitor: VariantVisitor<'ctx, Self>>(
        &self,
        ctx: Ctx,
        visitor: Visitor,
    ) -> Result<Visitor::AfterActiveVariant, Visitor::BreakType>;
}

impl<'ctx, T: AggregateValue<'ctx>> Value<'ctx> for T {
    fn get_value<Ctx: AsContext<'ctx>>(&self, ctx: Ctx) -> Val<'ctx, Self> {
        let ctx = ctx.ctx();
        get_aggregate_value(ctx, |visitor| self.visit_variants(ctx, visitor))
    }
    fn static_value_type_opt<Ctx: AsContext<'ctx>>(ctx: Ctx) -> Option<ValueType<'ctx, Self>> {
        struct MyFieldVisitor<'ctx> {
            ctx: ContextRef<'ctx>,
            fields: Vec<IrFieldType<'ctx>>,
        }
        impl<'ctx, T: VariantValue<'ctx>> FieldValueTypesVisitor<'ctx, T> for MyFieldVisitor<'ctx> {
            type BreakType = ();

            fn visit<FieldType: Value<'ctx>>(
                mut self,
                name: &'static str,
            ) -> Result<Self, Self::BreakType> {
                self.fields.push(IrFieldType {
                    name: name.intern(self.ctx),
                    ty: FieldType::static_value_type_opt(self.ctx).ok_or(())?.ir(),
                });
                Ok(self)
            }
        }
        struct MyVariantVisitor<'ctx> {
            ctx: ContextRef<'ctx>,
            variants: Vec<IrVariantType<'ctx>>,
        }
        impl<'ctx, T: StructOfVariantValues<'ctx>> VariantValueTypesVisitor<'ctx, T>
            for MyVariantVisitor<'ctx>
        {
            type BreakType = ();
            fn visit<VV: VariantValue<'ctx, Aggregate = T::Aggregate>>(
                mut self,
                variant: Variant<()>,
            ) -> Result<Self, Self::BreakType> {
                let fields = VV::visit_field_types(MyFieldVisitor {
                    ctx: self.ctx,
                    fields: Vec::new(),
                })?
                .fields;
                self.variants.push(IrVariantType::new(
                    self.ctx,
                    variant.name,
                    VV::discriminant().into(),
                    fields,
                ));
                Ok(self)
            }
        }
        match <T::StructOfVariantValues as StructOfVariantValues<'ctx>>::visit_variant_types(
            MyVariantVisitor {
                ctx: ctx.ctx(),
                variants: Vec::new(),
            },
        ) {
            Ok(MyVariantVisitor { ctx, variants }) => Some(ValueType::from_ir_unchecked(
                ctx,
                IrValueType::from(IrAggregateType::new(
                    ctx,
                    T::DiscriminantShape::default().shape().into(),
                    variants,
                    &Self::source_location(),
                ))
                .intern(ctx),
            )),
            Err(()) => None,
        }
    }
}

pub trait FixedTypeAggregateValue<'ctx>:
    AggregateValue<
        'ctx,
        StructOfVariantValues = <Self as FixedTypeAggregateValue<'ctx>>::StructOfVariantValues,
    > + FixedTypeValue<'ctx>
{
    type StructOfVariantValues: StructOfVariantFixedTypeValues<'ctx>;
}

impl<'ctx, T: FixedTypeAggregateValue<'ctx>> FixedTypeValue<'ctx> for T {
    fn static_value_type<Ctx: AsContext<'ctx>>(ctx: Ctx) -> ValueType<'ctx, Self> {
        struct MyFieldVisitor<'ctx> {
            ctx: ContextRef<'ctx>,
            fields: Vec<IrFieldType<'ctx>>,
        }
        impl<'ctx, T: VariantFixedTypeValue<'ctx>> FieldValueFixedTypesVisitor<'ctx, T>
            for MyFieldVisitor<'ctx>
        {
            type BreakType = Infallible;

            fn visit<FieldType: FixedTypeValue<'ctx>>(
                mut self,
                name: &'static str,
            ) -> Result<Self, Self::BreakType> {
                self.fields.push(IrFieldType {
                    name: name.intern(self.ctx),
                    ty: FieldType::static_value_type(self.ctx).ir(),
                });
                Ok(self)
            }
        }
        struct MyVariantVisitor<'ctx> {
            ctx: ContextRef<'ctx>,
            variants: Vec<IrVariantType<'ctx>>,
        }
        impl<'ctx, T: StructOfVariantFixedTypeValues<'ctx>> VariantValueFixedTypesVisitor<'ctx, T>
            for MyVariantVisitor<'ctx>
        {
            type BreakType = Infallible;
            fn visit<
                VV: VariantFixedTypeValue<
                    'ctx,
                    Aggregate = <T as StructOfVariantFixedTypeValues<'ctx>>::Aggregate,
                >,
            >(
                mut self,
                variant: Variant<()>,
            ) -> Result<Self, Self::BreakType> {
                let fields = VV::visit_field_fixed_types(MyFieldVisitor {
                    ctx: self.ctx,
                    fields: Vec::new(),
                })
                .unwrap()
                .fields;
                self.variants.push(IrVariantType::new(
                    self.ctx,
                    variant.name,
                    VV::discriminant().into(),
                    fields,
                ));
                Ok(self)
            }
        }
        let ctx = ctx.ctx();
        let variants =
            <T as FixedTypeAggregateValue<'ctx>>::StructOfVariantValues::visit_variant_fixed_types(
                MyVariantVisitor {
                    ctx: ctx.ctx(),
                    variants: Vec::new(),
                },
            )
            .unwrap()
            .variants;
        ValueType::from_ir_unchecked(
            ctx,
            IrValueType::from(IrAggregateType::new(
                ctx,
                T::DiscriminantShape::default().shape().into(),
                variants,
                &Self::source_location(),
            ))
            .intern(ctx),
        )
    }
}

#[derive(Debug, Clone, Copy)]
#[must_use]
pub struct AggregateDiscriminantShapeCalculator {
    min_discriminant: i128,
    max_discriminant: i128,
}

impl AggregateDiscriminantShapeCalculator {
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
        assert!(AggregateDiscriminantShapeCalculator::new().is_empty());
        assert!(!AggregateDiscriminantShapeCalculator::new()
            .add_discriminant(0)
            .is_empty());
        assert!(!AggregateDiscriminantShapeCalculator::new()
            .add_discriminant(1)
            .is_empty());
        assert!(!AggregateDiscriminantShapeCalculator::new()
            .add_discriminant(-1)
            .is_empty());
        assert!(!AggregateDiscriminantShapeCalculator::new()
            .add_discriminant(i128::MAX)
            .is_empty());
        assert!(!AggregateDiscriminantShapeCalculator::new()
            .add_discriminant(i128::MIN)
            .is_empty());
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new().get_shape(),
            IntShape {
                bit_count: 0,
                signed: false
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(0)
                .get_shape(),
            IntShape {
                bit_count: 0,
                signed: false
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(1)
                .get_shape(),
            IntShape {
                bit_count: 1,
                signed: false
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(-1)
                .get_shape(),
            IntShape {
                bit_count: 1,
                signed: true
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(-1)
                .add_discriminant(0)
                .get_shape(),
            IntShape {
                bit_count: 1,
                signed: true
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(-1)
                .add_discriminant(1)
                .get_shape(),
            IntShape {
                bit_count: 2,
                signed: true
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(0)
                .add_discriminant(1)
                .get_shape(),
            IntShape {
                bit_count: 1,
                signed: false
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(i128::MIN)
                .get_shape(),
            IntShape {
                bit_count: 128,
                signed: true
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(i128::MAX)
                .get_shape(),
            IntShape {
                bit_count: 127,
                signed: false
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(u64::MAX as _)
                .get_shape(),
            IntShape {
                bit_count: 64,
                signed: false
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(i64::MIN as _)
                .get_shape(),
            IntShape {
                bit_count: 64,
                signed: true
            }
        );
        assert_eq!(
            AggregateDiscriminantShapeCalculator::new()
                .add_discriminant(i64::MIN as i128 - 1)
                .get_shape(),
            IntShape {
                bit_count: 65,
                signed: true
            }
        );
    }
}
