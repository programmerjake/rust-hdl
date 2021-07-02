// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::prelude::*;
#[macro_use]
mod common;

#[derive(Value, IO, PlainIO, Default, FixedTypeValue)]
struct EmptyType;

#[derive(Value, IO, PlainIO, Default, FixedTypeValue)]
struct EmptyType2 {}

#[derive(Value, IO, PlainIO, Default, FixedTypeValue)]
struct EmptyType3();

#[derive(Value, Default, FixedTypeValue)]
struct Struct {
    a: bool,
    b: EmptyType,
    c: TupleStruct,
}

#[derive(IO)]
struct IOStruct<'ctx> {
    a: Input<'ctx, bool>,
    b: Output<'ctx, bool>,
    c: EmptyType,
    d: IOTupleStruct<'ctx>,
}

#[derive(Value, Default, FixedTypeValue)]
struct TupleStruct(bool, Int8);

#[derive(IO)]
struct IOTupleStruct<'ctx>(Output<'ctx, Struct>);

#[test]
fn test_structs() {
    Context::with(|ctx| {
        named!(let (top, ()) = ctx.top_module());
        named!(let _wire: Wire<Struct> = top.wire());
        assert_formats_to!(test_structs, top_0, top);
        let io = IOStruct {
            a: false.get_value(ctx).into(),
            b: top.output(),
            c: EmptyType,
            d: IOTupleStruct(top.output()),
        };
        named!(let (submodule, io) = top.submodule(io));
        assert_formats_to!(test_structs, top_1, top);
        assert_formats_to!(test_structs, submodule_0, submodule);
        let field = field!((io.d.0.read()).c.1);
        assert_formats_to!(test_structs, field_0, field);
    });
}

#[test]
fn test1() {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, ()) = ctx.top_module());
        assert_formats_to!(test1, top_0, top);
        named!(let wire: Wire<[bool; 4]> = top.wire());
        assert_formats_to!(test1, top_1, top);
        wire.assign([true, false, true, true].get_value(ctx));
        assert_formats_to!(test1, top_2, top);
    });
}

#[test]
fn test_submodule() {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, ()) = ctx.top_module());
        assert_formats_to!(test_submodule, top_0, top);
        let submodule_io: (Input<bool>, Output<UInt32>, Input<[Int8; 1]>) = (
            true.get_value(ctx).into(),
            top.output(),
            [Int8::wrapping_new(0x23)].get_value(ctx).into(),
        );
        assert_formats_to!(test_submodule, top_1, top);
        named!(let (submodule, submodule_io) = top.submodule(submodule_io));
        assert_formats_to!(test_submodule, top_2, top);
        assert_formats_to!(test_submodule, submodule_0, submodule);
        let (first_input, output, last_input) = submodule_io;
        assert_formats_to!(test_submodule, first_input_0, first_input);
        assert_formats_to!(test_submodule, output_0, output);
        assert_formats_to!(test_submodule, last_input_0, last_input);
        output.assign(UInt32::wrapping_new(0xDEADBEEFu32).get_value(ctx));
        assert_formats_to!(test_submodule, submodule_1, submodule);
    });
}

#[test]
fn test_sub_submodule() {
    Context::with(|ctx| {
        named!(let (top, ()) = ctx.top_module());
        assert_formats_to!(test_sub_submodule, top_0, top);
        let submodule_io: (Input<bool>, Output<bool>) = (true.get_value(ctx).into(), top.output());
        assert_formats_to!(test_sub_submodule, top_1, top);
        named!(let (submodule, submodule_io) = top.submodule(submodule_io));
        assert_formats_to!(test_sub_submodule, top_2, top);
        assert_formats_to!(test_sub_submodule, submodule_0, submodule);
        named!(let (sub_submodule, sub_submodule_io) = submodule.submodule(submodule_io));
        assert_formats_to!(test_sub_submodule, top_3, top);
        assert_formats_to!(test_sub_submodule, submodule_1, submodule);
        assert_formats_to!(test_sub_submodule, sub_submodule_0, sub_submodule);
        sub_submodule_io.1.assign(false.get_value(ctx));
        assert_formats_to!(test_sub_submodule, sub_submodule_1, sub_submodule);
    });
}
