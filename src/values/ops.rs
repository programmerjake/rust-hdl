// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, Intern},
    ir::{
        scope::ScopeRef,
        values::{
            BoolOutBinOp, BoolOutBinOpKind, BoolOutUnOp, BoolOutUnOpKind, ExpandScope,
            ExtractAggregateField, IrValue, IsAggregateVariant, Mux, SameSizeBinOp,
            SameSizeBinOpKind, SameSizeUnOp, SameSizeUnOpKind, ShrinkScope,
        },
        SourceLocation,
    },
    prelude::*,
    values::{
        aggregate::{
            ActiveFieldVisitor, ActiveVariantRef, AggregateValue, InactiveVariantRef, Variant,
            VariantVisitor,
        },
        integer::IntShapeTrait,
    },
};
use core::{
    convert::Infallible,
    ops::{Range, RangeInclusive},
};

fn same_size_bin_op_unchecked<'ctx, T: Value<'ctx>>(
    kind: SameSizeBinOpKind,
    lhs: Val<'ctx, T>,
    rhs: Val<'ctx, T>,
    caller: &SourceLocation<'ctx>,
) -> Val<'ctx, T> {
    Val::from_ir_unchecked(
        lhs.ctx(),
        IrValue::from(SameSizeBinOp::new(
            lhs.ctx(),
            kind,
            lhs.ir(),
            rhs.ir(),
            caller,
        ))
        .intern(lhs.ctx()),
    )
}

fn same_size_un_op_unchecked<'ctx, T: Value<'ctx>>(
    kind: SameSizeUnOpKind,
    input: Val<'ctx, T>,
    caller: &SourceLocation<'ctx>,
) -> Val<'ctx, T> {
    Val::from_ir_unchecked(
        input.ctx(),
        IrValue::from(SameSizeUnOp::new(input.ctx(), kind, input.ir(), caller)).intern(input.ctx()),
    )
}

fn bool_out_bin_op_unchecked<'ctx, T: Value<'ctx>>(
    kind: BoolOutBinOpKind,
    lhs: Val<'ctx, T>,
    rhs: Val<'ctx, T>,
    caller: &SourceLocation<'ctx>,
) -> Val<'ctx, bool> {
    Val::from_ir_unchecked(
        lhs.ctx(),
        IrValue::from(BoolOutBinOp::new(
            lhs.ctx(),
            kind,
            lhs.ir(),
            rhs.ir(),
            caller,
        ))
        .intern(lhs.ctx()),
    )
}

fn bool_out_un_op_unchecked<'ctx, T: Value<'ctx>, O: Value<'ctx>>(
    kind: BoolOutUnOpKind,
    input: Val<'ctx, T>,
    caller: &SourceLocation<'ctx>,
) -> Val<'ctx, O> {
    Val::from_ir_unchecked(
        input.ctx(),
        IrValue::from(BoolOutUnOp::new(input.ctx(), kind, input.ir(), caller)).intern(input.ctx()),
    )
}

macro_rules! trait_binary {
    (
        $(#[$trait_attr:meta])*
        trait $trait:ident {
            $(#[$fn_attr:meta])*
            fn $fn:ident();
        }
    ) => {
        $(#[$trait_attr])*
        pub trait $trait<'ctx, RhsType: Value<'ctx>>: Value<'ctx> {
            type Output: Value<'ctx>;

            $(#[$fn_attr])*
            #[track_caller]
            fn $fn(lhs: Val<'ctx, Self>, rhs: Val<'ctx, RhsType>) -> Val<'ctx, Self::Output>;
        }
    };
}

trait_binary! {
    trait HdlAdd {
        fn add();
    }
}

trait_binary! {
    trait HdlSub {
        fn sub();
    }
}

trait_binary! {
    trait HdlMul {
        fn mul();
    }
}

trait_binary! {
    trait HdlAnd {
        fn and();
    }
}

trait_binary! {
    trait HdlOr {
        fn or();
    }
}

trait_binary! {
    trait HdlXor {
        fn xor();
    }
}

trait_binary! {
    trait HdlShiftLeft {
        fn shift_left();
    }
}

trait_binary! {
    trait HdlShiftRight {
        fn shift_right();
    }
}

trait_binary! {
    trait HdlCompareEqual {
        fn compare_equal();
    }
}

trait_binary! {
    trait HdlCompareNotEqual {
        fn compare_not_equal();
    }
}

trait_binary! {
    trait HdlCompareLess {
        fn compare_less();
    }
}

trait_binary! {
    trait HdlCompareLessEqual {
        fn compare_less_equal();
    }
}

trait_binary! {
    trait HdlCompareGreater {
        fn compare_greater();
    }
}

trait_binary! {
    trait HdlCompareGreaterEqual {
        fn compare_greater_equal();
    }
}

pub trait HdlEq<'ctx>:
    Value<'ctx>
    + HdlCompareEqual<'ctx, Self, Output = bool>
    + HdlCompareNotEqual<'ctx, Self, Output = bool>
{
}

impl<'ctx, This> HdlEq<'ctx> for This where
    This: Value<'ctx>
        + HdlCompareEqual<'ctx, Self, Output = bool>
        + HdlCompareNotEqual<'ctx, Self, Output = bool>
{
}

pub trait HdlOrd<'ctx>:
    HdlEq<'ctx>
    + HdlCompareGreater<'ctx, Self, Output = bool>
    + HdlCompareGreaterEqual<'ctx, Self, Output = bool>
    + HdlCompareLess<'ctx, Self, Output = bool>
    + HdlCompareLessEqual<'ctx, Self, Output = bool>
{
}

impl<'ctx, This> HdlOrd<'ctx> for This where
    This: HdlEq<'ctx>
        + HdlCompareGreater<'ctx, Self, Output = bool>
        + HdlCompareGreaterEqual<'ctx, Self, Output = bool>
        + HdlCompareLess<'ctx, Self, Output = bool>
        + HdlCompareLessEqual<'ctx, Self, Output = bool>
{
}

macro_rules! trait_unary {
    (
        $(#[$trait_attr:meta])*
        trait $trait:ident {
            $(#[$fn_attr:meta])*
            fn $fn:ident();
        }
    ) => {
        $(#[$trait_attr])*
        pub trait $trait<'ctx>: Value<'ctx> {
            type Output: Value<'ctx>;

            $(#[$fn_attr])*
            #[track_caller]
            fn $fn(input: Val<'ctx, Self>) -> Val<'ctx, Self::Output>;
        }
    };
}

trait_unary! {
    trait HdlNot {
        fn not();
    }
}

trait_unary! {
    trait HdlNeg {
        fn neg();
    }
}

trait_unary! {
    trait HdlReduceAnd {
        fn reduce_and();
    }
}

trait_unary! {
    trait HdlReduceOr {
        fn reduce_or();
    }
}

trait_unary! {
    trait HdlReduceXor {
        fn reduce_xor();
    }
}

macro_rules! define_same_size_bin_op {
    ([$($args:tt)*] $trait:ident::$fn:ident, $ty:ty, $kind:ident) => {
        impl<'ctx, $($args)*> $trait<'ctx, $ty>
            for $ty
        {
            type Output = $ty;

            #[track_caller]
            fn $fn(lhs: Val<'ctx, Self>, rhs: Val<'ctx, $ty>) -> Val<'ctx, Self::Output> {
                let caller = SourceLocation::caller();
                same_size_bin_op_unchecked(
                    SameSizeBinOpKind::$kind,
                    lhs,
                    rhs,
                    &caller,
                )
            }
        }
    };
}

define_same_size_bin_op!([Shape: IntShapeTrait] HdlAdd::add, Int<Shape>, Add);
define_same_size_bin_op!([Shape: IntShapeTrait] HdlSub::sub, Int<Shape>, Sub);
define_same_size_bin_op!([Shape: IntShapeTrait] HdlMul::mul, Int<Shape>, Mul);
define_same_size_bin_op!([Shape: IntShapeTrait] HdlAnd::and, Int<Shape>, And);
define_same_size_bin_op!([] HdlAnd::and, bool, And);
define_same_size_bin_op!([Shape: IntShapeTrait] HdlOr::or, Int<Shape>, Or);
define_same_size_bin_op!([] HdlOr::or, bool, Or);
define_same_size_bin_op!([Shape: IntShapeTrait] HdlXor::xor, Int<Shape>, Xor);
define_same_size_bin_op!([] HdlXor::xor, bool, Xor);
define_same_size_bin_op!([Shape: IntShapeTrait] HdlShiftLeft::shift_left, Int<Shape>, ShiftLeft);
define_same_size_bin_op!([Shape: IntShapeTrait] HdlShiftRight::shift_right, Int<Shape>, ShiftRight);

macro_rules! define_bool_out_bin_op_expr {
    ([$ctx:lifetime $($args:tt)*] $trait:ident::$fn:ident, $ty:ty, ($lhs:ident, $rhs:ident, $caller:ident) { $body:expr }) => {
        impl<$ctx $($args)*> $trait<$ctx, $ty>
            for $ty
        {
            type Output = bool;

            #[track_caller]
            fn $fn($lhs: Val<$ctx, Self>, $rhs: Val<$ctx, $ty>) -> Val<$ctx, Self::Output> {
                let $caller = SourceLocation::caller();
                $body
            }
        }
    };
}

macro_rules! define_bool_out_bin_op {
    ([$($args:tt)*] $trait:ident::$fn:ident, $ty:ty, $kind:ident) => {
        define_bool_out_bin_op_expr!(['ctx, $($args)*] $trait::$fn, $ty, (lhs, rhs, caller) {
            bool_out_bin_op_unchecked(
                BoolOutBinOpKind::$kind,
                lhs,
                rhs,
                &caller,
            )
        });
    };
}

macro_rules! define_compare_ops {
    ([$($args:tt)*] $ty:ty) => {
        define_bool_out_bin_op!([$($args)*] HdlCompareEqual::compare_equal, $ty, CompareEq);
        define_bool_out_bin_op!([$($args)*] HdlCompareLess::compare_less, $ty, CompareLt);
        define_bool_out_bin_op_expr!(['ctx, $($args)*] HdlCompareNotEqual::compare_not_equal, $ty, (lhs, rhs, caller) {
            same_size_un_op_unchecked(
                SameSizeUnOpKind::Not,
                bool_out_bin_op_unchecked(
                    BoolOutBinOpKind::CompareEq,
                    lhs,
                    rhs,
                    &caller,
                ),
                &caller,
            )
        });
        define_bool_out_bin_op_expr!(['ctx, $($args)*] HdlCompareGreaterEqual::compare_greater_equal, $ty, (lhs, rhs, caller) {
            same_size_un_op_unchecked(
                SameSizeUnOpKind::Not,
                bool_out_bin_op_unchecked(
                    BoolOutBinOpKind::CompareLt,
                    lhs,
                    rhs,
                    &caller,
                ),
                &caller,
            )
        });
        define_bool_out_bin_op_expr!(['ctx, $($args)*] HdlCompareLessEqual::compare_less_equal, $ty, (lhs, rhs, caller) {
            same_size_un_op_unchecked(
                SameSizeUnOpKind::Not,
                bool_out_bin_op_unchecked(
                    BoolOutBinOpKind::CompareLt,
                    rhs,
                    lhs,
                    &caller,
                ),
                &caller,
            )
        });
        define_bool_out_bin_op_expr!(['ctx, $($args)*] HdlCompareGreater::compare_greater, $ty, (lhs, rhs, caller) {
            bool_out_bin_op_unchecked(
                BoolOutBinOpKind::CompareLt,
                rhs,
                lhs,
                &caller,
            )
        });
    };
}

define_compare_ops!([] bool);
define_compare_ops!([Shape: IntShapeTrait] Int<Shape>);

macro_rules! define_same_size_un_op {
    ([$($args:tt)*] $trait:ident::$fn:ident, $ty:ty, $kind:ident) => {
        impl<'ctx, $($args)*> $trait<'ctx>
            for $ty
        {
            type Output = $ty;

            #[track_caller]
            fn $fn(input: Val<'ctx, Self>) -> Val<'ctx, Self::Output> {
                let caller = SourceLocation::caller();
                same_size_un_op_unchecked(
                    SameSizeUnOpKind::$kind,
                    input,
                    &caller,
                )
            }
        }
    };
}

define_same_size_un_op!([] HdlNot::not, bool, Not);
define_same_size_un_op!([Shape: IntShapeTrait] HdlNot::not, Int<Shape>, Not);
define_same_size_un_op!([Shape: IntShapeTrait] HdlNeg::neg, Int<Shape>, Neg);

macro_rules! define_bool_out_un_op {
    ([$($args:tt)*] $trait:ident::$fn:ident, $ty:ty => $out:ty, $kind:ident) => {
        impl<'ctx, $($args)*> $trait<'ctx>
            for $ty
        {
            type Output = $out;

            #[track_caller]
            fn $fn(input: Val<'ctx, Self>) -> Val<'ctx, Self::Output> {
                let caller = SourceLocation::caller();
                bool_out_un_op_unchecked(
                    BoolOutUnOpKind::$kind,
                    input,
                    &caller,
                )
            }
        }
    };
}

define_bool_out_un_op!(
    [Shape: IntShapeTrait]
    HdlReduceAnd::reduce_and,
    Int<Shape> => UInt1,
    ReduceAnd
);
define_bool_out_un_op!(
    [Shape: IntShapeTrait]
    HdlReduceOr::reduce_or,
    Int<Shape> => UInt1,
    ReduceOr
);
define_bool_out_un_op!(
    [Shape: IntShapeTrait]
    HdlReduceXor::reduce_xor,
    Int<Shape> => UInt1,
    ReduceXor
);

#[track_caller]
pub fn mux<'ctx, T: Value<'ctx>>(
    condition: Val<'ctx, bool>,
    true_value: Val<'ctx, T>,
    false_value: Val<'ctx, T>,
) -> Val<'ctx, T> {
    let caller = SourceLocation::caller();
    Val::from_ir_unchecked(
        condition.ctx(),
        IrValue::from(Mux::new(
            condition.ctx(),
            condition.ir(),
            true_value.ir(),
            false_value.ir(),
            &caller,
        ))
        .intern(condition.ctx()),
    )
}

#[track_caller]
pub fn shrink_scope<'ctx, T: Value<'ctx>>(
    value: Val<'ctx, T>,
    scope: ScopeRef<'ctx>,
) -> Val<'ctx, T> {
    Val::from_ir_and_type_unchecked(
        IrValue::from(ShrinkScope::new(
            value.ctx(),
            value.ir(),
            scope,
            &SourceLocation::caller(),
        ))
        .intern(value.ctx()),
        value.value_type(),
    )
}

#[track_caller]
pub fn expand_scope<'ctx, T: Value<'ctx>>(
    value: Val<'ctx, T>,
    input_scope: ScopeRef<'ctx>,
    result_scope: ScopeRef<'ctx>,
) -> Val<'ctx, T> {
    Val::from_ir_and_type_unchecked(
        IrValue::from(ExpandScope::new(
            value.ctx(),
            value.ir(),
            input_scope,
            result_scope,
            &SourceLocation::caller(),
        ))
        .intern(value.ctx()),
        value.value_type(),
    )
}

pub fn assert_type_is_aggregate<'ctx, T: AggregateValue<'ctx>>() {}

pub fn assert_arg_is_val<'ctx, T: Value<'ctx>>(arg: Val<'ctx, T>) -> Val<'ctx, T> {
    arg
}

#[track_caller]
pub fn assert_variant_is_empty<'ctx, Ctx: AsContext<'ctx>, T: AggregateValue<'ctx>>(
    ctx: Ctx,
    variant: &T,
) {
    struct MyFieldVisitor {
        caller: SourceLocation<'static>,
        variant_name: &'static str,
    }
    impl<'ctx, VR: ActiveVariantRef<'ctx>> ActiveFieldVisitor<'ctx, VR> for MyFieldVisitor {
        type BreakType = Infallible;
        fn visit<FieldType: Value<'ctx>>(
            self,
            _name: &'static str,
            _value: Val<'ctx, FieldType>,
        ) -> Result<Self, Self::BreakType> {
            panic!(
                "enum variant `{}` is not empty\nat {}",
                self.variant_name, self.caller
            );
        }
    }
    struct MyVariantVisitor {
        caller: SourceLocation<'static>,
    }
    impl<'ctx, A: AggregateValue<'ctx>> VariantVisitor<'ctx, A> for MyVariantVisitor {
        type BreakType = Infallible;
        type AfterActiveVariant = ();
        fn visit_before_active_variant<VR: InactiveVariantRef<'ctx, Aggregate = A>>(
            self,
            _variant: Variant<VR>,
        ) -> Result<Self, Self::BreakType> {
            Ok(self)
        }
        fn visit_active_variant<VR: ActiveVariantRef<'ctx, Aggregate = A>>(
            self,
            variant: Variant<VR>,
        ) -> Result<Self::AfterActiveVariant, Self::BreakType> {
            let Self { caller } = self;
            variant.value.visit_fields(MyFieldVisitor {
                caller,
                variant_name: variant.name,
            })?;
            Ok(())
        }
        fn visit_after_active_variant<VR: InactiveVariantRef<'ctx, Aggregate = A>>(
            visitor: Self::AfterActiveVariant,
            _variant: Variant<VR>,
        ) -> Result<Self::AfterActiveVariant, Self::BreakType> {
            Ok(visitor)
        }
    }
    variant
        .visit_variants(
            ctx.ctx(),
            MyVariantVisitor {
                caller: SourceLocation::caller(),
            },
        )
        .unwrap();
}

#[track_caller]
pub fn get_aggregate_variant_index<'ctx, Ctx: AsContext<'ctx>, T: AggregateValue<'ctx>>(
    ctx: Ctx,
    variant: &T,
) -> usize {
    let ctx = ctx.ctx();
    struct Visitor<'ctx, 'a, T: AggregateValue<'ctx>> {
        caller: SourceLocation<'ctx>,
        ctx: ContextRef<'ctx>,
        variant: &'a T,
    }
    impl<'ctx, 'a, A: AggregateValue<'ctx>> VariantVisitor<'ctx, A> for Visitor<'ctx, 'a, A> {
        type BreakType = Infallible;
        type AfterActiveVariant = usize;
        fn visit_before_active_variant<VR: InactiveVariantRef<'ctx, Aggregate = A>>(
            self,
            _variant: Variant<VR>,
        ) -> Result<Self, Self::BreakType> {
            Ok(self)
        }
        fn visit_active_variant<VR: ActiveVariantRef<'ctx, Aggregate = A>>(
            self,
            variant: Variant<VR>,
        ) -> Result<Self::AfterActiveVariant, Self::BreakType> {
            let aggregate_type = self
                .variant
                .get_value_type(self.ctx)
                .ir()
                .aggregate()
                .unwrap();
            let discriminant = VR::discriminant().into();
            aggregate_type
                .get_variant_index(&discriminant)
                .ok_or_else(|| {
                    panic!(
                        "variant not found for discriminant {:?}\nvariant name {:?}\nat {}",
                        discriminant, variant.name, self.caller
                    )
                })
        }
        fn visit_after_active_variant<VR: InactiveVariantRef<'ctx, Aggregate = A>>(
            visitor: Self::AfterActiveVariant,
            _variant: Variant<VR>,
        ) -> Result<Self::AfterActiveVariant, Self::BreakType> {
            Ok(visitor)
        }
    }
    variant
        .visit_variants(
            ctx,
            Visitor {
                caller: SourceLocation::caller(),
                ctx,
                variant,
            },
        )
        .unwrap()
}

#[track_caller]
pub fn is_aggregate_variant<'ctx, T: AggregateValue<'ctx>>(
    value: Val<'ctx, T>,
    variant_index: usize,
) -> Val<'ctx, bool> {
    Val::from_ir_unchecked(
        value.ctx(),
        IrValue::from(IsAggregateVariant::new(
            value.ctx(),
            value.ir(),
            variant_index,
            &SourceLocation::caller(),
        ))
        .intern(value.ctx()),
    )
}

#[track_caller]
pub fn extract_aggregate_field_unchecked<'ctx, A: AggregateValue<'ctx>, F: Value<'ctx>>(
    aggregate: Val<'ctx, A>,
    variant_index: usize,
    field_index: usize,
) -> Val<'ctx, F> {
    Val::from_ir_unchecked(
        aggregate.ctx(),
        IrValue::from(ExtractAggregateField::new(
            aggregate.ctx(),
            aggregate.ir(),
            variant_index,
            field_index,
            &SourceLocation::caller(),
        ))
        .intern(aggregate.ctx()),
    )
}

#[track_caller]
pub fn match_eq<'ctx, T: HdlCompareEqual<'ctx, T, Output = bool>>(
    value: Val<'ctx, T>,
    pattern: &T,
) -> Val<'ctx, bool> {
    HdlCompareEqual::compare_equal(value, pattern.get_value(value.ctx()))
}

#[track_caller]
pub fn match_range<'ctx, Shape: IntShapeTrait>(
    value: Val<'ctx, Int<Shape>>,
    range: Range<&Int<Shape>>,
) -> Val<'ctx, bool> {
    HdlAnd::and(
        HdlCompareGreaterEqual::compare_greater_equal(value, range.start.get_value(value.ctx())),
        HdlCompareLess::compare_less(value, range.end.get_value(value.ctx())),
    )
}

#[track_caller]
pub fn match_range_inclusive<'ctx, Shape: IntShapeTrait>(
    value: Val<'ctx, Int<Shape>>,
    range: RangeInclusive<&Int<Shape>>,
) -> Val<'ctx, bool> {
    HdlAnd::and(
        HdlCompareGreaterEqual::compare_greater_equal(value, range.start().get_value(value.ctx())),
        HdlCompareLessEqual::compare_less_equal(value, range.end().get_value(value.ctx())),
    )
}

pub fn check_val_type<'ctx, T: Value<'ctx>, F: FnOnce(T, Infallible)>(_value: Val<'ctx, T>, _f: F) {
}
