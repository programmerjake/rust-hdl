// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::context::{ContextRef, Intern, Interned};
use core::{fmt, panic::Location};

pub mod io;
pub mod logic;
pub mod module;
pub mod symbols;
pub mod types;
pub mod values;

#[derive(Debug, Clone, Copy)]
pub struct SourceLocation<'ctx> {
    file: &'ctx str,
    line: u32,
    column: u32,
}

impl fmt::Display for SourceLocation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

impl<'ctx> From<&'ctx Location<'ctx>> for SourceLocation<'ctx> {
    fn from(v: &'ctx Location<'ctx>) -> Self {
        Self {
            file: v.file(),
            line: v.line(),
            column: v.column(),
        }
    }
}

impl<'ctx> SourceLocation<'ctx> {
    #[track_caller]
    pub fn caller() -> Self {
        Location::caller().into()
    }
    pub fn new_borrowed(file: &'ctx str, line: u32, column: u32) -> Self {
        Self { file, line, column }
    }
    pub fn new_cloned(ctx: ContextRef<'ctx>, file: &str, line: u32, column: u32) -> Self {
        let file: Interned<'_, str> = file.intern(ctx);
        Self::new_borrowed(file.get(), line, column)
    }
    pub fn file(self) -> &'ctx str {
        self.file
    }
    pub fn line(self) -> u32 {
        self.line
    }
    pub fn column(self) -> u32 {
        self.column
    }
}
