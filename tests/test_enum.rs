// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::prelude::*;
#[macro_use]
mod common;

mod enum_mod {
    #![no_implicit_prelude]

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    pub enum Enum0 {}

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum1 {
        A,
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum2 {
        A,
        B,
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum3 {
        A,
        B,
        C,
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum4 {
        A,
        B,
        C,
        D,
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum Enum6 {
        A = 3,
        B,
        C,
        D = 10,
        E = 12,
        F,
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub enum EnumStruct<T> {
        A {
            f1: ::rust_hdl::values::Int8,
        },
        B(),
        C {},
        D(T),
        E {
            #[rust_hdl(ignored)]
            f1: ::rust_hdl::values::Int8,
        },
        F(#[rust_hdl(ignored)] ::std::string::String),
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    enum PrivateEnum {
        A {
            f1: ::rust_hdl::values::Int8,
        },
        B(),
        C {},
        D(bool),
        E {
            #[rust_hdl(ignored)]
            f1: ::rust_hdl::values::Int8,
        },
        F(#[rust_hdl(ignored)] ::std::string::String),
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub(crate) enum CrateEnum {
        A {
            f1: ::rust_hdl::values::Int8,
        },
        B(),
        C {},
        D(bool),
        E {
            #[rust_hdl(ignored)]
            f1: ::rust_hdl::values::Int8,
        },
        F(#[rust_hdl(ignored)] ::std::string::String),
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
fn test_enum6() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<enum_mod::Enum6>) = ctx.top_module());
        assert_formats_to!(test_enum6, top_0, top);
    });
}

#[test]
fn test_enum_struct() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<enum_mod::EnumStruct<UInt8>>) = ctx.top_module());
        assert_formats_to!(test_enum_struct, top_0, top);
    });
}
