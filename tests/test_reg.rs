// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::prelude::*;
#[macro_use]
mod common;

#[derive(IO, PlainIO)]
struct TopIO<'ctx> {
    cd: Input<'ctx, ClockDomain>,
    output: Output<'ctx, bool>,
    input: Input<'ctx, bool>,
}

#[test]
fn test_reg() {
    Context::with(|ctx: ContextRef| {
        named!(let (top, io): (_, TopIO) = ctx.top_module());
        named!(let reg = top.reg(io.cd.get(), bool::default_val(ctx)));
        assert_formats_to!(test_reg, top_0, top);
        assert_formats_to!(test_reg, reg_0, reg);
        io.output.assign(reg.output());
        reg.assign_data_in(io.input.get());
        assert_formats_to!(test_reg, reg_output_0, reg.output());
        assert_formats_to!(test_reg, reg_1, reg);
        assert_formats_to!(test_reg, top_1, top);
    });
}
