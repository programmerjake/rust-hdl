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
    named!(let counter = m.reg(io.cd.get(), val!(m, 0)));

    let overflowed = val!(m, counter >= 1_000_000_u20);
    counter.assign_data_in(val!(m, if overflowed { 0 } else { counter + 1 }));
    named!(let led_toggle = m.reg(io.cd.get(), val!(m, false)));
    led_toggle.assign_data_in(val!(m, led_toggle ^ overflowed));
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
