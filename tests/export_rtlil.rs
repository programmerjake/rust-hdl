// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::{
    context::Intern,
    export::rtlil::RtlilExporter,
    ir::values::{ConcatBitVectors, IrValue},
    prelude::*,
};
#[macro_use]
mod common;

#[derive(Value, Default)]
struct MyValue {
    a: bool,
    b: [Int8; 3],
    c: UInt<0>,
}

#[derive(IO, PlainIO)]
struct TopIO<'ctx> {
    cd: Input<'ctx, ClockDomain>,
    input: Input<'ctx, MyValue>,
    wire_output: Output<'ctx, MyValue>,
    reg_output: Output<'ctx, MyValue>,
}

#[test]
fn export_rtlil() {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, io) = ctx.top_module());
        let TopIO {
            cd,
            input,
            wire_output,
            reg_output,
        } = io;
        named!(let wire = top.wire());
        let wire = wire.assign(input.get());
        wire_output.assign(wire.read());
        named!(let reg = top.reg(cd.get(), MyValue::default()));
        let reg = reg.assign_data_in(input.get());
        reg_output.assign(reg.output());
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(export_rtlil, output, exported);
    });
}

#[test]
fn export_rtlil_submodule() {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, io) = ctx.top_module());
        named!(let (submodule, io) = top.submodule(io));
        let TopIO {
            cd,
            input,
            wire_output,
            reg_output,
        } = io;
        named!(let wire = submodule.wire());
        let wire = wire.assign(input.get());
        wire_output.assign(wire.read());
        named!(let reg = submodule.reg(cd.get(), MyValue::default()));
        let reg = reg.assign_data_in(input.get());
        reg_output.assign(reg.output());
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(export_rtlil_submodule, output, exported);
        assert_formats_to!(export_rtlil_submodule, top_0, top);
    });
}

#[test]
fn export_rtlil_concat() {
    Context::with(|ctx: ContextRef<'_>| {
        #[derive(IO, PlainIO)]
        struct TopIO<'ctx> {
            a: Input<'ctx, UInt8>,
            b: Input<'ctx, UInt8>,
            c: Input<'ctx, UInt8>,
            o: Output<'ctx, UInt<24>>,
        }
        named!(let (top, io) = ctx.top_module());
        let TopIO { a, b, c, o } = io;
        o.assign(Val::from_ir_unchecked(
            ctx,
            IrValue::from(ConcatBitVectors::new(
                ctx,
                [a.get().ir(), b.get().ir(), c.get().ir()],
            ))
            .intern(ctx),
        ));
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(export_rtlil_concat, output, exported);
        assert_formats_to!(export_rtlil_concat, top_0, top);
    });
}

#[test]
fn export_rtlil_slice() {
    Context::with(|ctx: ContextRef<'_>| {
        #[derive(IO, PlainIO)]
        struct TopIO<'ctx> {
            i: Input<'ctx, UInt32>,
            o: Output<'ctx, UInt8>,
        }
        named!(let (top, io) = ctx.top_module());
        let TopIO { i, o } = io;
        o.assign(i.get().slice(8..16).wrap_into());
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(export_rtlil_slice, output, exported);
        assert_formats_to!(export_rtlil_slice, top_0, top);
    });
}
