// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use rust_hdl::{export::rtlil::RtlilExporter, prelude::*};

#[derive(IO, PlainIO)]
pub struct BlinkyIO<'ctx> {
    pub cd: Input<'ctx, ClockDomain>,
    pub led0: Output<'ctx, bool>,
}

pub fn blinky<'ctx>(parent: &Module<'ctx>, io: BlinkyIO<'ctx>) {
    let (m, io) = parent.submodule("blinky", io);
    named!(let counter = m.reg(io.cd.get(), UInt32::default()));

    let limit = UInt32::wrapping_new(1_000_000).get_value(m.ctx());

    let overflowed = counter.output().ge(limit);
    let counter_next = overflowed.mux(
        UInt32::wrapping_new(0).get_value(m.ctx()),
        counter.output() + UInt32::wrapping_new(1).get_value(m.ctx()),
    );
    counter.assign_data_in(counter_next);

    named!(let led_toggle = m.reg(io.cd.get(), false));
    let led_toggle_next = led_toggle.output() ^ overflowed;
    let led_toggle = led_toggle.assign_data_in(led_toggle_next);
    io.led0.assign(led_toggle.output());
}

fn main() -> std::io::Result<()> {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, io) = ctx.top_module());
        blinky(&top, io);
        top.export(RtlilExporter::new_io(std::fs::File::create("blinky.il")?))?;
        Ok(())
    })
}
