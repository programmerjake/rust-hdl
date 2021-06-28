// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
use rust_hdl::{export::rtlil::RtlilExporter, prelude::*};
#[macro_use]
mod common;

#[derive(IO, PlainIO)]
struct TopIO<'ctx> {
    cd: Input<'ctx, ClockDomain>,
}

#[test]
fn export_rtlil() {
    Context::with(|ctx: ContextRef<'_>| {
        named!(let (top, io) = ctx.top_module());
        let TopIO { cd } = io;
        let exported = top.export(RtlilExporter::new_str()).unwrap().into_output();
        assert_display_formats_to!(
            exported,
            r#"
attribute \generator "rust-hdl"
module \top
end
"#
        );
    });
}
