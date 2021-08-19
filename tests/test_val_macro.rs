// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::{export::rtlil::RtlilExporter, ir::module::IrModuleRef, prelude::*};
#[macro_use]
mod common;

#[derive(Value, FixedTypeValue)]
pub struct MyStruct<T, U> {
    f1: T,
    f2: U,
    f3: Int32,
}

#[derive(Value, FixedTypeValue)]
pub struct MyTupleStruct<T, U>(T, U, Int32);

#[derive(Value, FixedTypeValue)]
pub enum MyEnum {
    Tuple(Int32, bool),
    Struct { f1: UInt8, f2: (bool, bool) },
}

mod functions {
    #![no_implicit_prelude]

    #[track_caller]
    pub fn my_add<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlAdd<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs + my_rhs)
    }

    #[track_caller]
    pub fn my_sub<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlSub<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs - my_rhs)
    }

    #[track_caller]
    pub fn my_mul<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlMul<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs * my_rhs)
    }

    #[track_caller]
    pub fn my_and<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlAnd<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs & my_rhs)
    }

    #[track_caller]
    pub fn my_logical_and<'my_ctx>(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, bool> {
        ::rust_hdl::prelude::val!(my_module, my_lhs && my_rhs)
    }

    #[track_caller]
    pub fn my_or<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlOr<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs | my_rhs)
    }

    #[track_caller]
    pub fn my_logical_or<'my_ctx>(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, bool> {
        ::rust_hdl::prelude::val!(my_module, my_lhs || my_rhs)
    }

    #[track_caller]
    pub fn my_xor<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlXor<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs ^ my_rhs)
    }

    #[track_caller]
    pub fn my_shl<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlShiftLeft<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs << my_rhs)
    }

    #[track_caller]
    pub fn my_shr<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlShiftRight<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs >> my_rhs)
    }

    #[track_caller]
    pub fn my_compare_eq<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareEqual<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs == my_rhs)
    }

    #[track_caller]
    pub fn my_compare_ne<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareNotEqual<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs != my_rhs)
    }

    #[track_caller]
    pub fn my_compare_lt<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareLess<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs < my_rhs)
    }

    #[track_caller]
    pub fn my_compare_le<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareLessEqual<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs <= my_rhs)
    }

    #[track_caller]
    pub fn my_compare_gt<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareGreater<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs > my_rhs)
    }

    #[track_caller]
    pub fn my_compare_ge<
        'my_ctx,
        MyLhs: ::rust_hdl::values::ops::HdlCompareGreaterEqual<'my_ctx, MyRhs>,
        MyRhs: ::rust_hdl::values::Value<'my_ctx>,
    >(
        my_lhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyLhs>,
        my_rhs: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyRhs>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyLhs::Output> {
        ::rust_hdl::prelude::val!(my_module, my_lhs >= my_rhs)
    }

    #[track_caller]
    pub fn my_not<'my_ctx, MyInput: ::rust_hdl::values::ops::HdlNot<'my_ctx>>(
        my_input: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyInput>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyInput::Output> {
        ::rust_hdl::prelude::val!(my_module, !my_input)
    }

    #[track_caller]
    pub fn my_neg<'my_ctx, MyInput: ::rust_hdl::values::ops::HdlNeg<'my_ctx>>(
        my_input: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyInput>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, MyInput::Output> {
        ::rust_hdl::prelude::val!(my_module, -my_input)
    }

    #[track_caller]
    pub fn my_mux<'my_ctx, MyType: ::rust_hdl::values::Value<'my_ctx>>(
        my_cond: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_true_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_false_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
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
        my_cond: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_true_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_cond2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_false_true_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_false_false_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
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
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [MyType; 1]> {
        ::rust_hdl::prelude::val!(my_module, [v0])
    }

    #[track_caller]
    pub fn my_literal_array2<'my_ctx, MyType: ::rust_hdl::values::Value<'my_ctx>>(
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [MyType; 2]> {
        ::rust_hdl::prelude::val!(my_module, [v0, v1])
    }

    #[track_caller]
    pub fn my_literal_array3<'my_ctx, MyType: ::rust_hdl::values::Value<'my_ctx>>(
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        v2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, [MyType; 3]> {
        ::rust_hdl::prelude::val!(my_module, [v0, v1, v2])
    }

    #[track_caller]
    pub fn my_literal_array_repeat<
        'my_ctx,
        MyType: ::rust_hdl::values::Value<'my_ctx>,
        const N: usize,
    >(
        element: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = MyType>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
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
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, (T0,)> {
        ::rust_hdl::prelude::val!(my_module, (v0,))
    }

    #[track_caller]
    pub fn my_literal_tuple2<
        'my_ctx,
        T0: ::rust_hdl::values::Value<'my_ctx>,
        T1: ::rust_hdl::values::Value<'my_ctx>,
    >(
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T1>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
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
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T1>,
        v2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T2>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, (T0, T1, T2)> {
        ::rust_hdl::prelude::val!(my_module, (v0, v1, v2))
    }

    #[track_caller]
    pub fn my_literal_struct<
        'my_ctx,
        T0: ::rust_hdl::values::Value<'my_ctx>,
        T1: ::rust_hdl::values::Value<'my_ctx>,
    >(
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T1>,
        f3: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = ::rust_hdl::values::Int32>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, super::MyStruct<T0, T1>> {
        ::rust_hdl::prelude::val!(my_module, super::MyStruct::<_, _> { f1: v0, f2: v1, f3 })
    }

    #[track_caller]
    pub fn my_literal_tuple_struct<
        'my_ctx,
        T0: ::rust_hdl::values::Value<'my_ctx>,
        T1: ::rust_hdl::values::Value<'my_ctx>,
    >(
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T1>,
        v2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = ::rust_hdl::values::Int32>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, super::MyTupleStruct<T0, T1>> {
        ::rust_hdl::prelude::val!(
            my_module,
            super::MyTupleStruct::<_, _> {
                0: v0,
                1: v1,
                2: v2
            }
        )
    }

    #[track_caller]
    pub fn my_literal_tuple_struct2<
        'my_ctx,
        T0: ::rust_hdl::values::Value<'my_ctx>,
        T1: ::rust_hdl::values::Value<'my_ctx>,
    >(
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T1>,
        v2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = ::rust_hdl::values::Int32>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, super::MyTupleStruct<T0, T1>> {
        ::rust_hdl::prelude::val!(my_module, super::MyTupleStruct::<_, _>(v0, v1, v2))
    }

    #[track_caller]
    pub fn my_literal_struct_variant<'my_ctx>(
        f1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = ::rust_hdl::values::UInt8>,
        f2: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = (bool, bool)>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, super::MyEnum> {
        ::rust_hdl::prelude::val!(my_module, super::MyEnum::Struct { f1, f2 })
    }

    #[track_caller]
    pub fn my_literal_tuple_struct_variant<'my_ctx>(
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = ::rust_hdl::values::Int32>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, super::MyEnum> {
        ::rust_hdl::prelude::val!(my_module, super::MyEnum::Tuple { 0: v0, 1: v1 })
    }

    #[track_caller]
    pub fn my_literal_tuple_variant<'my_ctx>(
        v0: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = ::rust_hdl::values::Int32>,
        v1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = bool>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, super::MyEnum> {
        ::rust_hdl::prelude::val!(my_module, super::MyEnum::Tuple(v0, v1))
    }

    #[track_caller]
    pub fn my_literal_struct_update_callee<
        'my_ctx,
        T0: ::rust_hdl::values::Value<'my_ctx>,
        T1: ::rust_hdl::values::Value<'my_ctx>,
    >(
        f1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        rest: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = super::MyStruct<T0, T1>>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, super::MyStruct<T0, T1>> {
        ::rust_hdl::prelude::val!(my_module, super::MyStruct::<_, _> { f1, ..rest })
    }

    trait MyTrait<'my_ctx, T: ::rust_hdl::values::Value<'my_ctx>> {
        #[track_caller]
        fn my_method(
            self,
            rest: ::rust_hdl::values::Val<'my_ctx, T>,
            my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        ) -> ::rust_hdl::values::Val<'my_ctx, T>;
    }

    impl<
            'my_ctx,
            T0: ::rust_hdl::values::Value<'my_ctx>,
            T1: ::rust_hdl::values::Value<'my_ctx>,
        > MyTrait<'my_ctx, super::MyStruct<T0, T1>> for ::rust_hdl::values::Val<'my_ctx, T0>
    {
        #[track_caller]
        fn my_method(
            self,
            rest: ::rust_hdl::values::Val<'my_ctx, super::MyStruct<T0, T1>>,
            my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        ) -> ::rust_hdl::values::Val<'my_ctx, super::MyStruct<T0, T1>> {
            ::rust_hdl::prelude::val!(my_module, my_literal_struct_update_callee(self, rest))
        }
    }

    #[track_caller]
    pub fn my_literal_struct_update<
        'my_ctx,
        T0: ::rust_hdl::values::Value<'my_ctx>,
        T1: ::rust_hdl::values::Value<'my_ctx>,
    >(
        f1: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = T0>,
        rest: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = super::MyStruct<T0, T1>>,
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
    ) -> ::rust_hdl::values::Val<'my_ctx, super::MyStruct<T0, T1>> {
        ::rust_hdl::prelude::val!(my_module, f1.my_method(rest))
    }
}

#[track_caller]
fn test_ternary_fn<
    T: for<'ctx> FixedTypeValue<'ctx>,
    R: for<'ctx> FixedTypeValue<'ctx>,
    F: for<'ctx> Fn(Val<'ctx, T>, Val<'ctx, T>, Val<'ctx, T>, IrModuleRef<'ctx>) -> Val<'ctx, R>,
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
        out.assign(f(in0.get(), in1.get(), in2.get(), top.ir()));
        common::assert_string_is_impl(test_name, subtest_name, &format!("{:#?}", top));
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        common::assert_string_is_impl(test_name, &(subtest_name.to_owned() + "-rtlil"), &exported);
    })
}

#[track_caller]
fn test_binary_fn<
    T: for<'ctx> FixedTypeValue<'ctx>,
    R: for<'ctx> FixedTypeValue<'ctx>,
    F: for<'ctx> Fn(Val<'ctx, T>, Val<'ctx, T>, IrModuleRef<'ctx>) -> Val<'ctx, R>,
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
        out.assign(f(lhs.get(), rhs.get(), top.ir()));
        common::assert_string_is_impl(test_name, subtest_name, &format!("{:#?}", top));
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        common::assert_string_is_impl(test_name, &(subtest_name.to_owned() + "-rtlil"), &exported);
    })
}

#[track_caller]
fn test_unary_fn<
    T: for<'ctx> FixedTypeValue<'ctx>,
    R: for<'ctx> FixedTypeValue<'ctx>,
    F: for<'ctx> Fn(Val<'ctx, T>, IrModuleRef<'ctx>) -> Val<'ctx, R>,
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
        out.assign(f(input.get(), top.ir()));
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
    test_binary_fn::<UInt8, UInt8, _>(|l, r, m| functions::my_add(l, r, m), "test_add", "u8");
    test_binary_fn::<Int8, Int8, _>(|l, r, m| functions::my_add(l, r, m), "test_add", "i8");
}

#[test]
fn test_sub() {
    test_binary_fn::<UInt8, UInt8, _>(|l, r, m| functions::my_sub(l, r, m), "test_sub", "u8");
    test_binary_fn::<Int8, Int8, _>(|l, r, m| functions::my_sub(l, r, m), "test_sub", "i8");
}

#[test]
fn test_mul() {
    test_binary_fn::<UInt8, UInt8, _>(|l, r, m| functions::my_mul(l, r, m), "test_mul", "u8");
    test_binary_fn::<Int8, Int8, _>(|l, r, m| functions::my_mul(l, r, m), "test_mul", "i8");
}

#[test]
fn test_and() {
    test_binary_fn::<UInt8, UInt8, _>(|l, r, m| functions::my_and(l, r, m), "test_and", "u8");
    test_binary_fn::<Int8, Int8, _>(|l, r, m| functions::my_and(l, r, m), "test_and", "i8");
}

#[test]
fn test_logical_and() {
    test_binary_fn::<bool, bool, _>(
        |l, r, m| functions::my_logical_and(l, r, m),
        "test_logical_and",
        "bool",
    );
}

#[test]
fn test_or() {
    test_binary_fn::<UInt8, UInt8, _>(|l, r, m| functions::my_or(l, r, m), "test_or", "u8");
    test_binary_fn::<Int8, Int8, _>(|l, r, m| functions::my_or(l, r, m), "test_or", "i8");
}

#[test]
fn test_logical_or() {
    test_binary_fn::<bool, bool, _>(
        |l, r, m| functions::my_logical_or(l, r, m),
        "test_logical_or",
        "bool",
    );
}

#[test]
fn test_xor() {
    test_binary_fn::<UInt8, UInt8, _>(|l, r, m| functions::my_xor(l, r, m), "test_xor", "u8");
    test_binary_fn::<Int8, Int8, _>(|l, r, m| functions::my_xor(l, r, m), "test_xor", "i8");
}

#[test]
fn test_shl() {
    test_binary_fn::<UInt8, UInt8, _>(|l, r, m| functions::my_shl(l, r, m), "test_shl", "u8");
    test_binary_fn::<Int8, Int8, _>(|l, r, m| functions::my_shl(l, r, m), "test_shl", "i8");
}

#[test]
fn test_shr() {
    test_binary_fn::<UInt8, UInt8, _>(|l, r, m| functions::my_shr(l, r, m), "test_shr", "u8");
    test_binary_fn::<Int8, Int8, _>(|l, r, m| functions::my_shr(l, r, m), "test_shr", "i8");
}

#[test]
fn test_compare_eq() {
    test_binary_fn::<UInt8, bool, _>(
        |l, r, m| functions::my_compare_eq(l, r, m),
        "test_compare_eq",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |l, r, m| functions::my_compare_eq(l, r, m),
        "test_compare_eq",
        "i8",
    );
}

#[test]
fn test_compare_ne() {
    test_binary_fn::<UInt8, bool, _>(
        |l, r, m| functions::my_compare_ne(l, r, m),
        "test_compare_ne",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |l, r, m| functions::my_compare_ne(l, r, m),
        "test_compare_ne",
        "i8",
    );
}

#[test]
fn test_compare_lt() {
    test_binary_fn::<UInt8, bool, _>(
        |l, r, m| functions::my_compare_lt(l, r, m),
        "test_compare_lt",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |l, r, m| functions::my_compare_lt(l, r, m),
        "test_compare_lt",
        "i8",
    );
}

#[test]
fn test_compare_le() {
    test_binary_fn::<UInt8, bool, _>(
        |l, r, m| functions::my_compare_le(l, r, m),
        "test_compare_le",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |l, r, m| functions::my_compare_le(l, r, m),
        "test_compare_le",
        "i8",
    );
}

#[test]
fn test_compare_gt() {
    test_binary_fn::<UInt8, bool, _>(
        |l, r, m| functions::my_compare_gt(l, r, m),
        "test_compare_gt",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |l, r, m| functions::my_compare_gt(l, r, m),
        "test_compare_gt",
        "i8",
    );
}

#[test]
fn test_compare_ge() {
    test_binary_fn::<UInt8, bool, _>(
        |l, r, m| functions::my_compare_ge(l, r, m),
        "test_compare_ge",
        "u8",
    );
    test_binary_fn::<Int8, bool, _>(
        |l, r, m| functions::my_compare_ge(l, r, m),
        "test_compare_ge",
        "i8",
    );
}

#[test]
fn test_not() {
    test_unary_fn::<UInt8, UInt8, _>(|input, m| functions::my_not(input, m), "test_not", "u8");
    test_unary_fn::<Int8, Int8, _>(|input, m| functions::my_not(input, m), "test_not", "i8");
}

#[test]
fn test_neg() {
    test_unary_fn::<UInt8, UInt8, _>(|input, m| functions::my_neg(input, m), "test_neg", "u8");
    test_unary_fn::<Int8, Int8, _>(|input, m| functions::my_neg(input, m), "test_neg", "i8");
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
            cond.get(),
            true_value.get(),
            false_value.get(),
            &top,
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
            cond.get(),
            true_value.get(),
            cond2.get(),
            false_true_value.get(),
            false_false_value.get(),
            &top,
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
        |v0, m| functions::my_literal_array1(v0, m),
        "test_literal_array1",
        "u8",
    );
    test_unary_fn::<bool, [bool; 1], _>(
        |v0, m| functions::my_literal_array1(v0, m),
        "test_literal_array1",
        "bool",
    );
}

#[test]
fn test_literal_array2() {
    test_binary_fn::<UInt8, [UInt8; 2], _>(
        |v0, v1, m| functions::my_literal_array2(v0, v1, m),
        "test_literal_array2",
        "u8",
    );
    test_binary_fn::<bool, [bool; 2], _>(
        |v0, v1, m| functions::my_literal_array2(v0, v1, m),
        "test_literal_array2",
        "bool",
    );
}

#[test]
fn test_literal_array3() {
    test_ternary_fn::<UInt8, [UInt8; 3], _>(
        |v0, v1, v2, m| functions::my_literal_array3(v0, v1, v2, m),
        "test_literal_array3",
        "u8",
    );
    test_ternary_fn::<bool, [bool; 3], _>(
        |v0, v1, v2, m| functions::my_literal_array3(v0, v1, v2, m),
        "test_literal_array3",
        "bool",
    );
}

#[test]
fn test_literal_array_repeat0() {
    test_unary_fn::<UInt8, [UInt8; 0], _>(
        |v, m| functions::my_literal_array_repeat(v, m),
        "test_literal_array_repeat0",
        "u8",
    );
    test_unary_fn::<bool, [bool; 0], _>(
        |v, m| functions::my_literal_array_repeat(v, m),
        "test_literal_array_repeat0",
        "bool",
    );
}

#[test]
fn test_literal_array_repeat10() {
    test_unary_fn::<UInt8, [UInt8; 10], _>(
        |v, m| functions::my_literal_array_repeat(v, m),
        "test_literal_array_repeat10",
        "u8",
    );
    test_unary_fn::<bool, [bool; 10], _>(
        |v, m| functions::my_literal_array_repeat(v, m),
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
        |v0: Val<UInt8>, m| functions::my_literal_tuple1(v0, m),
        "test_my_literal_tuple1",
        "u8",
    );
    test_unary_fn(
        |v0: Val<bool>, m| functions::my_literal_tuple1(v0, m),
        "test_my_literal_tuple1",
        "bool",
    );
}

#[test]
fn test_my_literal_tuple2() {
    test_binary_fn(
        |v0: Val<UInt8>, v1, m| functions::my_literal_tuple2(v0, v1, m),
        "test_my_literal_tuple2",
        "u8",
    );
    test_binary_fn(
        |v0: Val<bool>, v1, m| functions::my_literal_tuple2(v0, v1, m),
        "test_my_literal_tuple2",
        "bool",
    );
}

#[test]
fn test_my_literal_tuple3() {
    test_ternary_fn(
        |v0: Val<UInt8>, v1, v2, m| functions::my_literal_tuple3(v0, v1, v2, m),
        "test_my_literal_tuple3",
        "u8",
    );
    test_ternary_fn(
        |v0: Val<bool>, v1, v2, m| functions::my_literal_tuple3(v0, v1, v2, m),
        "test_my_literal_tuple3",
        "bool",
    );
}

#[test]
fn test_my_literal_struct() {
    test_ternary_fn(
        |v0, v1, v2, m| functions::my_literal_struct(v0, v1, v2, m),
        "test_my_literal_struct",
        "test",
    );
}

#[test]
fn test_my_literal_tuple_struct() {
    test_ternary_fn(
        |v0, v1, v2, m| functions::my_literal_tuple_struct(v0, v1, v2, m),
        "test_my_literal_tuple_struct",
        "test",
    );
}

#[test]
fn test_my_literal_tuple_struct2() {
    test_ternary_fn(
        |v0, v1, v2, m| functions::my_literal_tuple_struct2(v0, v1, v2, m),
        "test_my_literal_tuple_struct2",
        "test",
    );
}

#[test]
fn test_my_literal_struct_update() {
    #[derive(IO, PlainIO)]
    struct IO<'ctx> {
        out: Output<'ctx, MyStruct<bool, UInt8>>,
        field: Input<'ctx, bool>,
        rest: Input<'ctx, MyStruct<bool, UInt8>>,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO { out, field, rest } = io;
        out.assign(functions::my_literal_struct_update(
            field.get(),
            rest.get(),
            &top,
        ));
        assert_formats_to!(test_my_literal_struct_update, test, top);
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(test_my_literal_struct_update, output, exported);
    })
}

#[test]
fn test_my_literal_struct_variant() {
    #[derive(IO, PlainIO)]
    struct IO<'ctx> {
        out: Output<'ctx, MyEnum>,
        f1: Input<'ctx, UInt8>,
        f2: Input<'ctx, (bool, bool)>,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO { out, f1, f2 } = io;
        out.assign(functions::my_literal_struct_variant(
            f1.get(),
            f2.get(),
            &top,
        ));
        assert_formats_to!(test_my_literal_struct_variant, test, top);
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(test_my_literal_struct_variant, output, exported);
    })
}

#[test]
fn test_my_literal_tuple_struct_variant() {
    #[derive(IO, PlainIO)]
    struct IO<'ctx> {
        out: Output<'ctx, MyEnum>,
        v0: Input<'ctx, Int32>,
        v1: Input<'ctx, bool>,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO { out, v0, v1 } = io;
        out.assign(functions::my_literal_tuple_struct_variant(
            v0.get(),
            v1.get(),
            &top,
        ));
        assert_formats_to!(test_my_literal_tuple_struct_variant, test, top);
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(test_my_literal_tuple_struct_variant, output, exported);
    })
}

#[test]
fn test_my_literal_tuple_variant() {
    #[derive(IO, PlainIO)]
    struct IO<'ctx> {
        out: Output<'ctx, MyEnum>,
        v0: Input<'ctx, Int32>,
        v1: Input<'ctx, bool>,
    }
    Context::with(|ctx| {
        named!(let (top, io) = ctx.top_module());
        let IO { out, v0, v1 } = io;
        out.assign(functions::my_literal_tuple_variant(
            v0.get(),
            v1.get(),
            &top,
        ));
        assert_formats_to!(test_my_literal_tuple_variant, test, top);
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(test_my_literal_tuple_variant, output, exported);
    })
}
