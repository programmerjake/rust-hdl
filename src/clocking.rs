// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::prelude::{FixedTypeValue, Value};

#[cfg(todo)]
#[derive(Debug, Clone, Copy, FixedTypeValue, Value)]
#[rust_hdl(crate = crate)]
pub struct ClockDomain {
    /// positive edge-triggered clock
    pub clk: bool,
    /// active-high reset
    pub rst: bool,
}

#[cfg(not(todo))]
pub struct ClockDomain;
