// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, Intern},
    ir::{
        scope::ScopeRef,
        values::{
            BoolOutBinOp, BoolOutBinOpKind, BoolOutUnOp, BoolOutUnOpKind, ExtractEnumVariantFields,
            ExtractStructField, IrValue, LiteralBits, MatchArmForEnum, MatchEnum, Mux,
            SameSizeBinOp, SameSizeBinOpKind, SameSizeUnOp, SameSizeUnOpKind,
        },
        SourceLocation,
    },
    prelude::*,
    values::{
        aggregate::{AggregateValue, AggregateValueMatch, AggregateValueMatchArm, EnumValue},
        integer::IntShapeTrait,
        ToVal,
    },
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
pub fn match_value<
    'ctx,
    Ctx: AsContext<'ctx>,
    T: Value<'ctx>,
    V: ToVal<'ctx, ValueType = T>,
    RV: FixedTypeValue<'ctx>,
    R: ToVal<'ctx, ValueType = RV>,
    E,
    F: FnMut(AggregateValueMatchArm<'ctx, T::AggregateOfFieldValues>) -> Result<R, E>,
>(
    ctx: Ctx,
    value: V,
    match_result_scope: ScopeRef<'ctx>,
    f: F,
) -> Result<Val<'ctx, RV>, E>
where
    T: AggregateValue<'ctx> + AggregateValueMatch<'ctx>,
{
    let ctx = ctx.ctx();
    let value = value.to_val(ctx);
    AggregateValueMatch::match_value(value, match_result_scope, f, &SourceLocation::caller())
}

#[track_caller]
pub fn get_enum_variant_field_unchecked<'ctx, T: EnumValue<'ctx>, F: Value<'ctx>>(
    enum_value: Val<'ctx, T>,
    discriminant: i128,
    field_index: usize,
    match_arm_scope: ScopeRef<'ctx>,
) -> Val<'ctx, F> {
    let caller = SourceLocation::caller();
    let ctx = enum_value.ctx();
    let enum_value = enum_value.ir();
    let discriminant = LiteralBits::from(Int::<T::DiscriminantShape>::wrapping_new(discriminant));
    let variant_index = T::get_ir_type(ctx)
        .get_variant_index(&discriminant)
        .unwrap_or_else(|| panic!("invalid discriminant\nat {}", caller));
    let fields = IrValue::from(ExtractEnumVariantFields::new(
        ctx,
        enum_value,
        variant_index,
        match_arm_scope,
        &caller,
    ))
    .intern(ctx);
    Val::from_ir_unchecked(
        ctx,
        IrValue::from(ExtractStructField::new(ctx, fields, field_index, &caller)).intern(ctx),
    )
}

pub struct MatchEnumUncheckedMatchArm<'ctx, R: Value<'ctx>> {
    pub discriminant: i128,
    pub result: Val<'ctx, R>,
    pub match_arm_scope: ScopeRef<'ctx>,
}

#[track_caller]
pub fn match_enum_unchecked<
    'ctx,
    T: EnumValue<'ctx>,
    R: FixedTypeValue<'ctx>,
    MatchArms: IntoIterator<Item = MatchEnumUncheckedMatchArm<'ctx, R>>,
>(
    enum_value: Val<'ctx, T>,
    match_arms: MatchArms,
    result_scope: ScopeRef<'ctx>,
) -> Val<'ctx, R> {
    let ctx = enum_value.ctx();
    let caller = SourceLocation::caller();
    let enum_value = enum_value.ir();
    let result_type = R::static_value_type(ctx).ir();
    let enum_type = T::get_ir_type(ctx);
    Val::from_ir_unchecked(
        ctx,
        IrValue::from(MatchEnum::new(
            ctx,
            enum_value,
            result_type,
            match_arms.into_iter().map(|match_arm| {
                let MatchEnumUncheckedMatchArm {
                    discriminant,
                    result,
                    match_arm_scope,
                } = match_arm;
                let discriminant =
                    LiteralBits::from(Int::<T::DiscriminantShape>::wrapping_new(discriminant));
                let result = result.to_val(ctx).ir();
                let variant_index = enum_type
                    .get_variant_index(&discriminant)
                    .unwrap_or_else(|| panic!("invalid discriminant\nat {}", caller));
                (
                    variant_index,
                    MatchArmForEnum {
                        result,
                        match_arm_scope,
                    },
                )
            }),
            result_scope,
            &caller,
        ))
        .intern(ctx),
    )
}
