// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::{export::rtlil::RtlilExporter, prelude::*};
#[macro_use]
mod common;

#[derive(Value, FixedTypeValue)]
#[allow(dead_code)]
enum MyEnum1 {
    B = 15,
    C = 10,
    Z = 0,
    A = 5,
}

mod functions {
    #![no_implicit_prelude]

    pub(crate) fn my_match1<'my_ctx>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = super::MyEnum1>,
    ) -> ::rust_hdl::values::Val<'my_ctx, bool> {
        use super::MyEnum1;
        ::rust_hdl::prelude::val!(
            my_module,
            match my_value {
                MyEnum1::A => true,
                _ => false,
            }
        )
    }

    pub(crate) fn my_match2<'my_ctx>(
        my_module: impl ::rust_hdl::module::AsIrModule<'my_ctx>,
        my_value: impl ::rust_hdl::values::ToVal<'my_ctx, ValueType = super::MyEnum1>,
    ) -> ::rust_hdl::values::Val<'my_ctx, ::rust_hdl::prelude::UInt8> {
        use super::MyEnum1;
        ::rust_hdl::prelude::val!(
            my_module,
            match my_value {
                MyEnum1::A | MyEnum1::Z => 0u8,
                MyEnum1::B => 5u8,
                MyEnum1::C => 2u8,
            }
        )
    }
}

#[test]
fn test_match1() {
    #[derive(IO, PlainIO)]
    struct IO<'ctx> {
        value: Input<'ctx, MyEnum1>,
        out: Output<'ctx, bool>,
    }
    Context::with(|ctx| {
        named!(let (top, IO { value, out }) = ctx.top_module());
        out.assign(functions::my_match1(&top, value.get()));
        assert_formats_to!(test_match1, test, top);
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(test_match1, output, exported);
    })
}

#[test]
fn test_match2() {
    #[derive(IO, PlainIO)]
    struct IO<'ctx> {
        value: Input<'ctx, MyEnum1>,
        out: Output<'ctx, UInt8>,
    }
    Context::with(|ctx| {
        named!(let (top, IO { value, out }) = ctx.top_module());
        out.assign(functions::my_match2(&top, value.get()));
        assert_formats_to!(test_match2, test, top);
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(test_match2, output, exported);
    })
}
