// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::Value;

#[derive(Debug, Clone, Copy, Default, Value)]
#[rust_hdl(crate = crate)]
pub struct ClockDomainData {
    pub clk: bool,
    pub rst: bool,
}
