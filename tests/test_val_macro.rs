// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::{export::rtlil::RtlilExporter, ir::module::IrModuleRef, prelude::*};
#[macro_use]
mod common;

mod functions {
    #![no_implicit_prelude]

    #[track_caller]
    pub fn my_add<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlAdd<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs + my_rhs)
    }

    #[track_caller]
    pub fn my_sub<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlSub<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs - my_rhs)
    }

    #[track_caller]
    pub fn my_mul<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlMul<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs * my_rhs)
    }

    #[track_caller]
    pub fn my_and<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlAnd<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs & my_rhs)
    }

    #[track_caller]
    pub fn my_logical_and<'my_ctx>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
    ) -> ::rust_hdl::values::Val<'my_ctx, bool> {
        ::rust_hdl::prelude::val!(my_module, my_lhs && my_rhs)
    }

    #[track_caller]
    pub fn my_or<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlOr<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs | my_rhs)
    }

    #[track_caller]
    pub fn my_logical_or<'my_ctx>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
    ) -> ::rust_hdl::values::Val<'my_ctx, bool> {
        ::rust_hdl::prelude::val!(my_module, my_lhs || my_rhs)
    }

    #[track_caller]
    pub fn my_xor<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlXor<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs ^ my_rhs)
    }

    #[track_caller]
    pub fn my_shl<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlShiftLeft<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs << my_rhs)
    }

    #[track_caller]
    pub fn my_shr<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlShiftRight<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs >> my_rhs)
    }

    #[track_caller]
    pub fn my_compare_eq<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareEqual<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs == my_rhs)
    }

    #[track_caller]
    pub fn my_compare_ne<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareNotEqual<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs != my_rhs)
    }

    #[track_caller]
    pub fn my_compare_lt<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareLess<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs < my_rhs)
    }

    #[track_caller]
    pub fn my_compare_le<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareLessEqual<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs <= my_rhs)
    }

    #[track_caller]
    pub fn my_compare_gt<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareGreater<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs > my_rhs)
    }

    #[track_caller]
    pub fn my_compare_ge<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareGreaterEqual<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs >= my_rhs)
    }

    #[track_caller]
    pub fn my_not<'my_ctx, MyInput: ::rust_hdl::values::ops::HdlNot<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_input: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyInput>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyInput::Output> {
        ::rust_hdl::prelude::val!(my_module, !my_input)
    }

    #[track_caller]
    pub fn my_neg<'my_ctx, MyInput: ::rust_hdl::values::ops::HdlNeg<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_input: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyInput>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyInput::Output> {
        ::rust_hdl::prelude::val!(my_module, -my_input)
    }

    #[track_caller]
    pub fn my_mux<'my_ctx, MyType: ::rust_hdl::values::Value<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_cond: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_true_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_false_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyType> {
        ::rust_hdl::prelude::val!(
            my_module,
            if my_cond {
                my_true_value
            } else {
                my_false_value
            }
        )
    }

    #[track_caller]
    pub fn my_mux2<'my_ctx, MyType: ::rust_hdl::values::Value<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_cond: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_true_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_cond2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_false_true_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_false_false_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyType> {
        ::rust_hdl::prelude::val!(
            my_module,
            if my_cond {
                my_true_value
            } else if my_cond2 {
                my_false_true_value
            } else {
                my_false_false_value
            }
        )
    }

    #[track_caller]
    pub fn my_literal_array0<'my_ctx, MyType: ::rust_hdl::values::FixedTypeValue<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [MyType; 0]> {
        ::rust_hdl::prelude::val!(my_module, [])
    }

    #[track_caller]
    pub fn my_literal_array1<'my_ctx, MyType: ::rust_hdl::values::Value<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [MyType; 1]> {
        ::rust_hdl::prelude::val!(my_module, [v0])
    }

    #[track_caller]
    pub fn my_literal_array2<'my_ctx, MyType: ::rust_hdl::values::Value<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [MyType; 2]> {
        ::rust_hdl::prelude::val!(my_module, [v0, v1])
    }

    #[track_caller]
    pub fn my_literal_array3<'my_ctx, MyType: ::rust_hdl::values::Value<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        v2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [MyType; 3]> {
        ::rust_hdl::prelude::val!(my_module, [v0, v1, v2])
    }

    #[track_caller]
    pub fn my_literal_array_repeat<
        'my_ctx,
        MyType: ::rust_hdl::values::Value<'my_ctx>,
        const N: usize,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        element: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [MyType; N]> {
        ::rust_hdl::prelude::val!(my_module, [element; N])
    }

    #[track_caller]
    pub fn my_byte_literal<'my_ctx>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [::rust_hdl::values::UInt8; 12]> {
        ::rust_hdl::prelude::val!(my_module, b"Hello World!")
    }

    #[track_caller]
    pub fn my_literal_tuple0<'my_ctx>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, ()> {
        ::rust_hdl::prelude::val!(my_module, ())
    }

    #[track_caller]
    pub fn my_literal_tuple1<'my_ctx, T0: ::rust_hdl::values::Value<'my_ctx>>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
    ) -> ::rust_hdl::values::Val<'my_ctx, (T0,)> {
        ::rust_hdl::prelude::val!(my_module, (v0,))
    }

    #[track_caller]
    pub fn my_literal_tuple2<
        'my_ctx,
        T0: ::rust_hdl::values::Value<'my_ctx>,
        T1: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T1>,
    ) -> ::rust_hdl::values::Val<'my_ctx, (T0, T1)> {
        ::rust_hdl::prelude::val!(my_module, (v0, v1))
    }

    #[track_caller]
    pub fn my_literal_tuple3<
        'my_ctx,
        T0: ::rust_hdl::values::Value<'my_ctx>,
        T1: ::rust_hdl::values::Value<'my_ctx>,
        T2: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T1>,
        v2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T2>,
    ) -> ::rust_hdl::values::Val<'my_ctx, (T0, T1, T2)> {
        ::rust_hdl::prelude::val!(my_module, (v0, v1, v2))
    }
}

#[track_caller]
fn test_ternary_fn<
    T: for<'ctx> FixedTypeValue<'ctx>,
    R: for<'ctx> FixedTypeValue<'ctx>,
    F: for<'ctx> Fn(IrModuleRef<'ctx>, Val<'ctx, T>, Val<'ctx, T>, Val<'ctx, T>) -> Val<'ctx, R>,
>(
    f: F,
    test_name: &str,
    subtest_name: &str,
) {
    #[derive(IO, PlainIO)]
    struct IO<I, O> {
        in0: I,
        in1: I,
        in2: I,
        out: O,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO { in0, in1, in2, out }: IO<Input<T>, Output<R>> = io;
        out.assign(f(top.ir(), in0.get(), in1.get(), in2.get()));
        common::assert_string_is_impl(test_name, subtest_name, &format!("{:#?}", top));
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        common::assert_string_is_impl(test_name, &(subtest_name.to_owned() + "-rtlil"), &exported);
    })
}

#[track_caller]
fn test_binary_fn<
    T: for<'ctx> FixedTypeValue<'ctx>,
    R: for<'ctx> FixedTypeValue<'ctx>,
    F: for<'ctx> Fn(IrModuleRef<'ctx>, Val<'ctx, T>, Val<'ctx, T>) -> Val<'ctx, R>,
>(
    f: F,
    test_name: &str,
    subtest_name: &str,
) {
    #[derive(IO, PlainIO)]
    struct IO<I, O> {
        lhs: I,
        rhs: I,
        out: O,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO { lhs, rhs, out }: IO<Input<T>, Output<R>> = io;
        out.assign(f(top.ir(), lhs.get(), rhs.get()));
        common::assert_string_is_impl(test_name, subtest_name, &format!("{:#?}", top));
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        common::assert_string_is_impl(test_name, &(subtest_name.to_owned() + "-rtlil"), &exported);
    })
}

#[track_caller]
fn test_unary_fn<
    T: for<'ctx> FixedTypeValue<'ctx>,
    R: for<'ctx> FixedTypeValue<'ctx>,
    F: for<'ctx> Fn(IrModuleRef<'ctx>, Val<'ctx, T>) -> Val<'ctx, R>,
>(
    f: F,
    test_name: &str,
    subtest_name: &str,
) {
    #[derive(IO, PlainIO)]
    struct IO<I, O> {
        input: I,
        out: O,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO { input, out }: IO<Input<T>, Output<R>> = io;
        out.assign(f(top.ir(), input.get()));
        common::assert_string_is_impl(test_name, subtest_name, &format!("{:#?}", top));
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        common::assert_string_is_impl(test_name, &(subtest_name.to_owned() + "-rtlil"), &exported);
    })
}

#[track_caller]
fn test_nullary_fn<
    R: for<'ctx> FixedTypeValue<'ctx>,
    F: for<'ctx> Fn(IrModuleRef<'ctx>) -> Val<'ctx, R>,
>(
    f: F,
    test_name: &str,
    subtest_name: &str,
) {
    #[derive(IO, PlainIO)]
    struct IO<O> {
        out: O,
    }
    Context::with(|ctx| {
        named!(let (top, io): (_, Output<R>) = ctx.top_module());
        io.assign(f(top.ir()));
        common::assert_string_is_impl(test_name, subtest_name, &format!("{:#?}", top));
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        common::assert_string_is_impl(test_name, &(subtest_name.to_owned() + "-rtlil"), &exported);
    })
}

#[test]
fn test_add() {
    test_binary_fn::<UInt8, UInt8, _>(|m, l, r| functions::my_add(m, l, r), "test_add", "u8");
    test_binary_fn::<Int8, Int8, _>(|m, l, r| functions::my_add(m, l, r), "test_add", "i8");
}

#[test]
fn test_sub() {
    test_binary_fn::<UInt8, UInt8, _>(|m, l, r| functions::my_sub(m, l, r), "test_sub", "u8");
    test_binary_fn::<Int8, Int8, _>(|m, l, r| functions::my_sub(m, l, r), "test_sub", "i8");
}

#[test]
fn test_mul() {
    test_binary_fn::<UInt8, UInt8, _>(|m, l, r| functions::my_mul(m, l, r), "test_mul", "u8");
    test_binary_fn::<Int8, Int8, _>(|m, l, r| functions::my_mul(m, l, r), "test_mul", "i8");
}

#[test]
fn test_and() {
    test_binary_fn::<UInt8, UInt8, _>(|m, l, r| functions::my_and(m, l, r), "test_and", "u8");
    test_binary_fn::<Int8, Int8, _>(|m, l, r| functions::my_and(m, l, r), "test_and", "i8");
}

#[test]
fn test_logical_and() {
    test_binary_fn::<bool, bool, _>(
        |m, l, r| functions::my_logical_and(m, l, r),
        "test_logical_and",
        "bool",
    );
}

#[test]
fn test_or() {
    test_binary_fn::<UInt8, UInt8, _>(|m, l, r| functions::my_or(m, l, r), "test_or", "u8");
    test_binary_fn::<Int8, Int8, _>(|m, l, r| functions::my_or(m, l, r), "test_or", "i8");
}

#[test]
fn test_logical_or() {
    test_binary_fn::<bool, bool, _>(
        |m, l, r| functions::my_logical_or(m, l, r),
        "test_logical_or",
        "bool",
    );
}

#[test]
fn test_xor() {
    test_binary_fn::<UInt8, UInt8, _>(|m, l, r| functions::my_xor(m, l, r), "test_xor", "u8");
    test_binary_fn::<Int8, Int8, _>(|m, l, r| functions::my_xor(m, l, r), "test_xor", "i8");
}

#[test]
fn test_shl() {
    test_binary_fn::<UInt8, UInt8, _>(|m, l, r| functions::my_shl(m, l, r), "test_shl", "u8");
    test_binary_fn::<Int8, Int8, _>(|m, l, r| functions::my_shl(m, l, r), "test_shl", "i8");
}

#[test]
fn test_shr() {
    test_binary_fn::<UInt8, UInt8, _>(|m, l, r| functions::my_shr(m, l, r), "test_shr", "u8");
    test_binary_fn::<Int8, Int8, _>(|m, l, r| functions::my_shr(m, l, r), "test_shr", "i8");
}

#[test]
fn test_compare_eq() {
    test_binary_fn::<UInt8, bool, _>(
        |m, l, r| functions::my_compare_eq(m, l, r),
        "test_compare_eq",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |m, l, r| functions::my_compare_eq(m, l, r),
        "test_compare_eq",
        "i8",
    );
}

#[test]
fn test_compare_ne() {
    test_binary_fn::<UInt8, bool, _>(
        |m, l, r| functions::my_compare_ne(m, l, r),
        "test_compare_ne",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |m, l, r| functions::my_compare_ne(m, l, r),
        "test_compare_ne",
        "i8",
    );
}

#[test]
fn test_compare_lt() {
    test_binary_fn::<UInt8, bool, _>(
        |m, l, r| functions::my_compare_lt(m, l, r),
        "test_compare_lt",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |m, l, r| functions::my_compare_lt(m, l, r),
        "test_compare_lt",
        "i8",
    );
}

#[test]
fn test_compare_le() {
    test_binary_fn::<UInt8, bool, _>(
        |m, l, r| functions::my_compare_le(m, l, r),
        "test_compare_le",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |m, l, r| functions::my_compare_le(m, l, r),
        "test_compare_le",
        "i8",
    );
}

#[test]
fn test_compare_gt() {
    test_binary_fn::<UInt8, bool, _>(
        |m, l, r| functions::my_compare_gt(m, l, r),
        "test_compare_gt",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |m, l, r| functions::my_compare_gt(m, l, r),
        "test_compare_gt",
        "i8",
    );
}

#[test]
fn test_compare_ge() {
    test_binary_fn::<UInt8, bool, _>(
        |m, l, r| functions::my_compare_ge(m, l, r),
        "test_compare_ge",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |m, l, r| functions::my_compare_ge(m, l, r),
        "test_compare_ge",
        "i8",
    );
}

#[test]
fn test_not() {
    test_unary_fn::<UInt8, UInt8, _>(|m, input| functions::my_not(m, input), "test_not", "u8");
    test_unary_fn::<Int8, Int8, _>(|m, input| functions::my_not(m, input), "test_not", "i8");
}

#[test]
fn test_neg() {
    test_unary_fn::<UInt8, UInt8, _>(|m, input| functions::my_neg(m, input), "test_neg", "u8");
    test_unary_fn::<Int8, Int8, _>(|m, input| functions::my_neg(m, input), "test_neg", "i8");
}

#[test]
fn test_mux() {
    #[derive(IO, PlainIO)]
    struct IO<'ctx, I, O> {
        cond: Input<'ctx, bool>,
        true_value: I,
        false_value: I,
        out: O,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO {
            cond,
            true_value,
            false_value,
            out,
        }: IO<Input<Int8>, Output<Int8>> = io;
        out.assign(functions::my_mux(
            &top,
            cond.get(),
            true_value.get(),
            false_value.get(),
        ));
        assert_formats_to!(test_mux, test, top);
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(test_mux, output, exported);
    })
}

#[test]
fn test_mux2() {
    #[derive(IO, PlainIO)]
    struct IO<'ctx, I, O> {
        cond: Input<'ctx, bool>,
        true_value: I,
        cond2: Input<'ctx, bool>,
        false_true_value: I,
        false_false_value: I,
        out: O,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO {
            cond,
            true_value,
            cond2,
            false_true_value,
            false_false_value,
            out,
        }: IO<Input<Int8>, Output<Int8>> = io;
        out.assign(functions::my_mux2(
            &top,
            cond.get(),
            true_value.get(),
            cond2.get(),
            false_true_value.get(),
            false_false_value.get(),
        ));
        assert_formats_to!(test_mux2, test, top);
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(test_mux2, output, exported);
    })
}

#[test]
fn test_literal_array0() {
    test_nullary_fn::<[UInt8; 0], _>(
        |m| functions::my_literal_array0(m),
        "test_literal_array0",
        "u8",
    );
    test_nullary_fn::<[bool; 0], _>(
        |m| functions::my_literal_array0(m),
        "test_literal_array0",
        "bool",
    );
}

#[test]
fn test_literal_array1() {
    test_unary_fn::<UInt8, [UInt8; 1], _>(
        |m, v0| functions::my_literal_array1(m, v0),
        "test_literal_array1",
        "u8",
    );
    test_unary_fn::<bool, [bool; 1], _>(
        |m, v0| functions::my_literal_array1(m, v0),
        "test_literal_array1",
        "bool",
    );
}

#[test]
fn test_literal_array2() {
    test_binary_fn::<UInt8, [UInt8; 2], _>(
        |m, v0, v1| functions::my_literal_array2(m, v0, v1),
        "test_literal_array2",
        "u8",
    );
    test_binary_fn::<bool, [bool; 2], _>(
        |m, v0, v1| functions::my_literal_array2(m, v0, v1),
        "test_literal_array2",
        "bool",
    );
}

#[test]
fn test_literal_array3() {
    test_ternary_fn::<UInt8, [UInt8; 3], _>(
        |m, v0, v1, v2| functions::my_literal_array3(m, v0, v1, v2),
        "test_literal_array3",
        "u8",
    );
    test_ternary_fn::<bool, [bool; 3], _>(
        |m, v0, v1, v2| functions::my_literal_array3(m, v0, v1, v2),
        "test_literal_array3",
        "bool",
    );
}

#[test]
fn test_literal_array_repeat0() {
    test_unary_fn::<UInt8, [UInt8; 0], _>(
        |m, v| functions::my_literal_array_repeat(m, v),
        "test_literal_array_repeat0",
        "u8",
    );
    test_unary_fn::<bool, [bool; 0], _>(
        |m, v| functions::my_literal_array_repeat(m, v),
        "test_literal_array_repeat0",
        "bool",
    );
}

#[test]
fn test_literal_array_repeat10() {
    test_unary_fn::<UInt8, [UInt8; 10], _>(
        |m, v| functions::my_literal_array_repeat(m, v),
        "test_literal_array_repeat10",
        "u8",
    );
    test_unary_fn::<bool, [bool; 10], _>(
        |m, v| functions::my_literal_array_repeat(m, v),
        "test_literal_array_repeat10",
        "bool",
    );
}

#[test]
fn test_my_byte_literal() {
    test_nullary_fn(
        |m| functions::my_byte_literal(m),
        "test_my_byte_literal",
        "test",
    );
}

#[test]
fn test_my_literal_tuple0() {
    test_nullary_fn(
        |m| functions::my_literal_tuple0(m),
        "test_my_literal_tuple0",
        "test",
    );
}

#[test]
fn test_my_literal_tuple1() {
    test_unary_fn(
        |m, v0: Val<UInt8>| functions::my_literal_tuple1(m, v0),
        "test_my_literal_tuple1",
        "u8",
    );
    test_unary_fn(
        |m, v0: Val<bool>| functions::my_literal_tuple1(m, v0),
        "test_my_literal_tuple1",
        "bool",
    );
}

#[test]
fn test_my_literal_tuple2() {
    test_binary_fn(
        |m, v0: Val<UInt8>, v1| functions::my_literal_tuple2(m, v0, v1),
        "test_my_literal_tuple2",
        "u8",
    );
    test_binary_fn(
        |m, v0: Val<bool>, v1| functions::my_literal_tuple2(m, v0, v1),
        "test_my_literal_tuple2",
        "bool",
    );
}

#[test]
fn test_my_literal_tuple3() {
    test_ternary_fn(
        |m, v0: Val<UInt8>, v1, v2| functions::my_literal_tuple3(m, v0, v1, v2),
        "test_my_literal_tuple3",
        "u8",
    );
    test_ternary_fn(
        |m, v0: Val<bool>, v1, v2| functions::my_literal_tuple3(m, v0, v1, v2),
        "test_my_literal_tuple3",
        "bool",
    );
}
