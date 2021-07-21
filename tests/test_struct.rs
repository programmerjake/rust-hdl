// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::prelude::*;
#[macro_use]
mod common;

mod struct_mod {
    #![no_implicit_prelude]

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    pub struct EmptyStruct;

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    pub struct TupleStruct0();

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    pub struct Struct0 {}

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub struct Struct1 {
        pub field1: bool,
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub struct TupleStruct1(bool);

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub struct Struct2 {
        pub field1: bool,
        field2: (),
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub struct TupleStruct2(bool, ());

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub struct Struct<T> {
        pub f1: ::rust_hdl::values::Int8,
        pub f2: T,
        #[rust_hdl(ignored)]
        pub f3: ::rust_hdl::values::Int8,
        #[rust_hdl(ignored)]
        pub f4: ::std::string::String,
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub struct TupleStruct<T>(
        pub ::rust_hdl::values::Int8,
        pub T,
        #[rust_hdl(ignored)] pub ::rust_hdl::values::Int8,
        #[rust_hdl(ignored)] pub ::std::string::String,
    );

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    struct PrivateStruct {
        f1: ::rust_hdl::values::Int8,
        f2: bool,
        #[rust_hdl(ignored)]
        f3: ::rust_hdl::values::Int8,
        #[rust_hdl(ignored)]
        f4: ::std::string::String,
    }

    #[derive(::rust_hdl::values::Value, ::rust_hdl::values::FixedTypeValue)]
    #[allow(dead_code)]
    pub(crate) struct CrateStruct {
        pub(crate) f1: ::rust_hdl::values::Int8,
        pub(crate) f2: bool,
        #[rust_hdl(ignored)]
        pub(crate) f3: ::rust_hdl::values::Int8,
        #[rust_hdl(ignored)]
        pub(crate) f4: ::std::string::String,
    }
}

#[test]
fn test_empty_struct() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::EmptyStruct>) = ctx.top_module());
        assert_formats_to!(test_empty_struct, top_0, top);
    });
}

#[test]
fn test_tuple_struct0() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::TupleStruct0>) = ctx.top_module());
        assert_formats_to!(test_tuple_struct0, top_0, top);
    });
}

#[test]
fn test_struct0() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::Struct0>) = ctx.top_module());
        assert_formats_to!(test_struct0, top_0, top);
    });
}

#[test]
fn test_struct1() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::Struct1>) = ctx.top_module());
        assert_formats_to!(test_struct1, top_0, top);
    });
}

#[test]
fn test_tuple_struct1() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::TupleStruct1>) = ctx.top_module());
        assert_formats_to!(test_tuple_struct1, top_0, top);
    });
}

#[test]
fn test_struct2() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::Struct2>) = ctx.top_module());
        assert_formats_to!(test_struct2, top_0, top);
    });
}

#[test]
fn test_tuple_struct2() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::TupleStruct2>) = ctx.top_module());
        assert_formats_to!(test_tuple_struct2, top_0, top);
    });
}

#[test]
fn test_struct() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::Struct<bool>>) = ctx.top_module());
        assert_formats_to!(test_struct, top_0, top);
    });
}

#[test]
fn test_tuple_struct() {
    Context::with(|ctx| {
        named!(let (top, _io): (_, Input<struct_mod::TupleStruct<bool>>) = ctx.top_module());
        assert_formats_to!(test_tuple_struct, top_0, top);
    });
}
