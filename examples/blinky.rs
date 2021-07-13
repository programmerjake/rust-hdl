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

    let counter_output = counter.output();
    let overflowed = val!(counter_output >= ({ UInt32::wrapping_new(1_000_000) }));
    counter.assign_data_in(val!(if overflowed {
        ({ UInt32::default() })
    } else {
        counter_output + ({ UInt32::wrapping_new(1) })
    }));
    named!(let led_toggle = m.reg(io.cd.get(), false));
    let led_toggle_output = led_toggle.output();
    let led_toggle = led_toggle.assign_data_in(val!(led_toggle_output ^ overflowed));
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
