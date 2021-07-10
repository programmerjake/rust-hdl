// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::{prelude::*, values::integer::U8Shape};
#[macro_use]
mod common;

mod enum_mod {
    #[derive(rust_hdl::values::Value, rust_hdl::values::FixedTypeValue)]
    pub enum Enum0 {}

    #[derive(rust_hdl::values::Value, rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum1 {
        A,
    }

    #[derive(rust_hdl::values::Value, rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum2 {
        A,
        B,
    }

    #[derive(rust_hdl::values::Value, rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum3 {
        A,
        B,
        C,
    }

    #[derive(rust_hdl::values::Value, rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum4 {
        A,
        B,
        C,
        D,
    }

    #[derive(rust_hdl::values::Value, rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum EnumStruct<Shape: 'static>
    where
        Shape: rust_hdl::values::integer::IntShapeTrait,
        rust_hdl::values::Int<Shape>: for<'ctx2> rust_hdl::values::FixedTypeValue<'ctx2>,
    {
        A {
            f1: rust_hdl::values::Int8,
        },
        B(),
        C {},
        D(rust_hdl::values::Int<Shape>),
        E {
            #[rust_hdl(ignored)]
            f1: rust_hdl::values::Int8,
        },
        F(#[rust_hdl(ignored)] String),
    }
}

#[test]
fn test_enum0() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<enum_mod::Enum0>) = ctx.top_module());
        assert_formats_to!(test_enum0, top_0, top);
    });
}

#[test]
fn test_enum1() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<enum_mod::Enum1>) = ctx.top_module());
        assert_formats_to!(test_enum1, top_0, top);
    });
}

#[test]
fn test_enum2() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<enum_mod::Enum2>) = ctx.top_module());
        assert_formats_to!(test_enum2, top_0, top);
    });
}

#[test]
fn test_enum3() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<enum_mod::Enum3>) = ctx.top_module());
        assert_formats_to!(test_enum3, top_0, top);
    });
}

#[test]
fn test_enum4() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<enum_mod::Enum4>) = ctx.top_module());
        assert_formats_to!(test_enum4, top_0, top);
    });
}

#[test]
fn test_enum_struct() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<enum_mod::EnumStruct<U8Shape>>) = ctx.top_module());
        assert_formats_to!(test_enum_struct, top_0, top);
    });
}
