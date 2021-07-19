// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, Intern},
    ir::{
        values::{
            BoolOutBinOp, BoolOutBinOpKind, BoolOutUnOp, BoolOutUnOpKind, IrValue, Mux,
            SameSizeBinOp, SameSizeBinOpKind, SameSizeUnOp, SameSizeUnOpKind,
        },
        SourceLocation,
    },
    prelude::*,
    values::{
        aggregate::{AggregateValue, AggregateValueMatch},
        integer::IntShapeTrait,
        LazyVal, ToVal,
    },
};

fn same_size_bin_op_unchecked<'ctx: 'scope, 'scope, T: Value<'ctx>>(
    kind: SameSizeBinOpKind,
    lhs: Val<'ctx, 'scope, T>,
    rhs: Val<'ctx, 'scope, T>,
    caller: &SourceLocation<'ctx>,
) -> Val<'ctx, 'scope, T> {
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

fn same_size_un_op_unchecked<'ctx: 'scope, 'scope, T: Value<'ctx>>(
    kind: SameSizeUnOpKind,
    input: Val<'ctx, 'scope, T>,
    caller: &SourceLocation<'ctx>,
) -> Val<'ctx, 'scope, T> {
    Val::from_ir_unchecked(
        input.ctx(),
        IrValue::from(SameSizeUnOp::new(input.ctx(), kind, input.ir(), caller)).intern(input.ctx()),
    )
}

fn bool_out_bin_op_unchecked<'ctx: 'scope, 'scope, T: Value<'ctx>>(
    kind: BoolOutBinOpKind,
    lhs: Val<'ctx, 'scope, T>,
    rhs: Val<'ctx, 'scope, T>,
    caller: &SourceLocation<'ctx>,
) -> Val<'ctx, 'scope, bool> {
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

fn bool_out_un_op_unchecked<'ctx: 'scope, 'scope, T: Value<'ctx>, O: Value<'ctx>>(
    kind: BoolOutUnOpKind,
    input: Val<'ctx, 'scope, T>,
    caller: &SourceLocation<'ctx>,
) -> Val<'ctx, 'scope, O> {
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
        pub trait $trait<'ctx: 'scope, 'scope, RhsType: Value<'ctx>>: Value<'ctx> {
            type Output: Value<'ctx>;

            $(#[$fn_attr])*
            #[track_caller]
            fn $fn<
                Lhs: ToVal<'ctx, 'scope, ValueType = Self> + 'scope,
                Rhs: ToVal<'ctx, 'scope, ValueType = RhsType> + 'scope,
            >(
                lhs: Lhs,
                rhs: Rhs,
            ) -> LazyVal<'ctx, 'scope, Self::Output>;
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

pub trait HdlEq<'ctx: 'scope, 'scope>:
    Value<'ctx>
    + HdlCompareEqual<'ctx, 'scope, Self, Output = bool>
    + HdlCompareNotEqual<'ctx, 'scope, Self, Output = bool>
{
}

impl<'ctx: 'scope, 'scope, This> HdlEq<'ctx, 'scope> for This where
    This: Value<'ctx>
        + HdlCompareEqual<'ctx, 'scope, Self, Output = bool>
        + HdlCompareNotEqual<'ctx, 'scope, Self, Output = bool>
{
}

pub trait HdlOrd<'ctx: 'scope, 'scope>:
    HdlEq<'ctx, 'scope>
    + HdlCompareGreater<'ctx, 'scope, Self, Output = bool>
    + HdlCompareGreaterEqual<'ctx, 'scope, Self, Output = bool>
    + HdlCompareLess<'ctx, 'scope, Self, Output = bool>
    + HdlCompareLessEqual<'ctx, 'scope, Self, Output = bool>
{
}

impl<'ctx: 'scope, 'scope, This> HdlOrd<'ctx, 'scope> for This where
    This: HdlEq<'ctx, 'scope>
        + HdlCompareGreater<'ctx, 'scope, Self, Output = bool>
        + HdlCompareGreaterEqual<'ctx, 'scope, Self, Output = bool>
        + HdlCompareLess<'ctx, 'scope, Self, Output = bool>
        + HdlCompareLessEqual<'ctx, 'scope, Self, Output = bool>
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
        pub trait $trait<'ctx: 'scope, 'scope>: Value<'ctx> {
            type Output: Value<'ctx>;

            $(#[$fn_attr])*
            #[track_caller]
            fn $fn<
                T: ToVal<'ctx, 'scope, ValueType = Self> + 'scope,
            >(
                input: T,
            ) -> LazyVal<'ctx, 'scope, Self::Output>;
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
        impl<'ctx: 'scope, 'scope, $($args)*> $trait<'ctx, 'scope, $ty>
            for $ty
        {
            type Output = $ty;

            #[track_caller]
            fn $fn<
                Lhs: ToVal<'ctx, 'scope, ValueType = Self> + 'scope,
                Rhs: ToVal<'ctx, 'scope, ValueType = $ty> + 'scope,
            >(
                lhs: Lhs,
                rhs: Rhs,
            ) -> LazyVal<'ctx, 'scope, Self::Output> {
                let caller = SourceLocation::caller();
                LazyVal::from_fn(move |ctx| {
                    same_size_bin_op_unchecked(
                        SameSizeBinOpKind::$kind,
                        lhs.to_val(ctx),
                        rhs.to_val(ctx),
                        &caller,
                    )
                })
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
    ([$ctx:lifetime, $scope:lifetime $($args:tt)*] $trait:ident::$fn:ident, $ty:ty, ($ctx_var:ident, $lhs:ident, $rhs:ident, $caller:ident) { $body:expr }) => {
        impl<$ctx: $scope, $scope $($args)*> $trait<$ctx, $scope, $ty>
            for $ty
        {
            type Output = bool;

            #[track_caller]
            fn $fn<
                Lhs: ToVal<$ctx, $scope, ValueType = Self> + $scope,
                Rhs: ToVal<$ctx, $scope, ValueType = $ty> + $scope,
            >(
                $lhs: Lhs,
                $rhs: Rhs,
            ) -> LazyVal<$ctx, $scope, Self::Output> {
                let $caller = SourceLocation::caller();
                LazyVal::from_fn(move |$ctx_var| {
                    $body
                })
            }
        }
    };
}

macro_rules! define_bool_out_bin_op {
    ([$($args:tt)*] $trait:ident::$fn:ident, $ty:ty, $kind:ident) => {
        define_bool_out_bin_op_expr!(['ctx, 'scope, $($args)*] $trait::$fn, $ty, (ctx, lhs, rhs, caller) {
            bool_out_bin_op_unchecked(
                BoolOutBinOpKind::$kind,
                lhs.to_val(ctx),
                rhs.to_val(ctx),
                &caller,
            )
        });
    };
}

macro_rules! define_compare_ops {
    ([$($args:tt)*] $ty:ty) => {
        define_bool_out_bin_op!([$($args)*] HdlCompareEqual::compare_equal, $ty, CompareEq);
        define_bool_out_bin_op!([$($args)*] HdlCompareLess::compare_less, $ty, CompareLt);
        define_bool_out_bin_op_expr!(['ctx, 'scope, $($args)*] HdlCompareNotEqual::compare_not_equal, $ty, (ctx, lhs, rhs, caller) {
            same_size_un_op_unchecked(
                SameSizeUnOpKind::Not,
                bool_out_bin_op_unchecked(
                    BoolOutBinOpKind::CompareEq,
                    lhs.to_val(ctx),
                    rhs.to_val(ctx),
                    &caller,
                ),
                &caller,
            )
        });
        define_bool_out_bin_op_expr!(['ctx, 'scope, $($args)*] HdlCompareGreaterEqual::compare_greater_equal, $ty, (ctx, lhs, rhs, caller) {
            same_size_un_op_unchecked(
                SameSizeUnOpKind::Not,
                bool_out_bin_op_unchecked(
                    BoolOutBinOpKind::CompareLt,
                    lhs.to_val(ctx),
                    rhs.to_val(ctx),
                    &caller,
                ),
                &caller,
            )
        });
        define_bool_out_bin_op_expr!(['ctx, 'scope, $($args)*] HdlCompareLessEqual::compare_less_equal, $ty, (ctx, lhs, rhs, caller) {
            same_size_un_op_unchecked(
                SameSizeUnOpKind::Not,
                bool_out_bin_op_unchecked(
                    BoolOutBinOpKind::CompareLt,
                    rhs.to_val(ctx),
                    lhs.to_val(ctx),
                    &caller,
                ),
                &caller,
            )
        });
        define_bool_out_bin_op_expr!(['ctx, 'scope, $($args)*] HdlCompareGreater::compare_greater, $ty, (ctx, lhs, rhs, caller) {
            bool_out_bin_op_unchecked(
                BoolOutBinOpKind::CompareLt,
                rhs.to_val(ctx),
                lhs.to_val(ctx),
                &caller,
            )
        });
    };
}

define_compare_ops!([] bool);
define_compare_ops!([Shape: IntShapeTrait] Int<Shape>);

macro_rules! define_same_size_un_op {
    ([$($args:tt)*] $trait:ident::$fn:ident, $ty:ty, $kind:ident) => {
        impl<'ctx: 'scope, 'scope, $($args)*> $trait<'ctx, 'scope>
            for $ty
        {
            type Output = $ty;

            #[track_caller]
            fn $fn<
                T: ToVal<'ctx, 'scope, ValueType = Self> + 'scope,
            >(
                input: T,
            ) -> LazyVal<'ctx, 'scope, Self::Output> {
                let caller = SourceLocation::caller();
                LazyVal::from_fn(move |ctx| {
                    same_size_un_op_unchecked(
                        SameSizeUnOpKind::$kind,
                        input.to_val(ctx),
                        &caller,
                    )
                })
            }
        }
    };
}

define_same_size_un_op!([] HdlNot::not, bool, Not);
define_same_size_un_op!([Shape: IntShapeTrait] HdlNot::not, Int<Shape>, Not);
define_same_size_un_op!([Shape: IntShapeTrait] HdlNeg::neg, Int<Shape>, Neg);

macro_rules! define_bool_out_un_op {
    ([$($args:tt)*] $trait:ident::$fn:ident, $ty:ty => $out:ty, $kind:ident) => {
        impl<'ctx: 'scope, 'scope, $($args)*> $trait<'ctx, 'scope>
            for $ty
        {
            type Output = $out;

            #[track_caller]
            fn $fn<
                T: ToVal<'ctx, 'scope, ValueType = Self> + 'scope,
            >(
                input: T,
            ) -> LazyVal<'ctx, 'scope, Self::Output> {
                let caller = SourceLocation::caller();
                LazyVal::from_fn(move |ctx| {
                    bool_out_un_op_unchecked(
                        BoolOutUnOpKind::$kind,
                        input.to_val(ctx),
                        &caller,
                    )
                })
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

pub fn identity<
    'ctx: 'scope,
    'scope,
    T: Value<'ctx>,
    V: ToVal<'ctx, 'scope, ValueType = T> + 'scope,
>(
    v: V,
) -> LazyVal<'ctx, 'scope, T> {
    LazyVal::new(v)
}

#[track_caller]
pub fn mux<
    'ctx: 'scope,
    'scope,
    T: Value<'ctx>,
    Condition: 'scope + ToVal<'ctx, 'scope, ValueType = bool>,
    TrueValue: 'scope + ToVal<'ctx, 'scope, ValueType = T>,
    FalseValue: 'scope + ToVal<'ctx, 'scope, ValueType = T>,
>(
    condition: Condition,
    true_value: TrueValue,
    false_value: FalseValue,
) -> LazyVal<'ctx, 'scope, T> {
    let caller = SourceLocation::caller();
    LazyVal::from_fn(move |ctx| {
        Val::from_ir_unchecked(
            ctx,
            IrValue::from(Mux::new(
                ctx,
                condition.to_val(ctx).ir(),
                true_value.to_val(ctx).ir(),
                false_value.to_val(ctx).ir(),
                &caller,
            ))
            .intern(ctx),
        )
    })
}

pub fn match_value_without_scope_check<
    'ctx: 'scope,
    'scope,
    T: Value<'ctx>,
    V: ToVal<'ctx, 'scope, ValueType = T> + 'scope,
    R,
    E,
    F: FnMut(T::AggregateOfFieldLazyValues) -> Result<R, E>,
>(
    value: V,
    f: F,
) -> Result<R, E>
where
    T: AggregateValue<'ctx, 'scope> + AggregateValueMatch<'ctx, 'scope>,
{
    AggregateValueMatch::match_value_without_scope_check(LazyVal::new(value), f)
}
