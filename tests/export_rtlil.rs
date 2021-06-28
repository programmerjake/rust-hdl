// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::{export::rtlil::RtlilExporter, prelude::*};
#[macro_use]
mod common;

#[derive(Value, Default)]
struct MyValue {
    a: bool,
    b: [Int8; 3],
}

#[derive(IO, PlainIO)]
struct TopIO<'ctx> {
    cd: Input<'ctx, ClockDomain>,
    input: Input<'ctx, MyValue>,
    output: Output<'ctx, MyValue>,
}

#[test]
fn export_rtlil() {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, io) = ctx.top_module());
        let TopIO { cd, input, output } = io;
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(export_rtlil, output, exported);
    });
}
