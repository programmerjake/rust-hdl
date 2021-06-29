// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use core::fmt;

use alloc::borrow::Cow;

use crate::{
    context::ContextRef,
    ir::{module::IrModuleRef, symbols::IrSymbolTable},
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct RtlilId<'ctx> {
    id: &'ctx str,
    module: Option<IrModuleRef<'ctx>>,
}

impl<'ctx> From<RtlilId<'ctx>> for Cow<'ctx, str> {
    fn from(v: RtlilId<'ctx>) -> Self {
        Cow::Borrowed(v.id)
    }
}

impl<'ctx> RtlilId<'ctx> {
    #[track_caller]
    pub(crate) fn assert_module_is(self, module: IrModuleRef<'ctx>) {
        assert!(
            self.module == Some(module),
            "module mismatch: {:?} {:?}",
            self.module.map(|v| v.path()),
            module.path()
        );
    }
}

impl fmt::Display for RtlilId<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.id.is_empty() {
            return write!(f, "$$");
        }
        match self.id.strip_prefix("#") {
            Some(id) if !id.contains(|ch: char| !ch.is_ascii_digit()) => {
                return write!(f, "${}", id);
            }
            _ => write!(f, "\\")?,
        }
        /// rtlil identifiers can't have some whitespace and other special characters, replace them with Unicode control pictures.
        fn replace_char(ch: char) -> Option<char> {
            Some(match ch {
                '\0' => '\u{2400}',
                '\t' => '\u{2409}',
                '\n' => '\u{2424}',
                '\r' => '\u{240D}',
                ' ' => '\u{2423}',
                _ => return None,
            })
        }
        if self.id.contains(|ch| replace_char(ch).is_some()) {
            for ch in self.id.chars() {
                write!(f, "{}", replace_char(ch).unwrap_or(ch))?;
            }
            Ok(())
        } else {
            write!(f, "{}", self.id)
        }
    }
}

#[derive(Debug)]
pub(crate) struct SymbolTable<'ctx> {
    symbol_table: IrSymbolTable<'ctx>,
    module: IrModuleRef<'ctx>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub(crate) fn new(module: IrModuleRef<'ctx>) -> Self {
        Self {
            symbol_table: IrSymbolTable::new(),
            module,
        }
    }
    #[track_caller]
    pub(crate) fn new_id<'a>(
        &self,
        module: IrModuleRef<'ctx>,
        name: impl Into<Cow<'a, str>>,
    ) -> RtlilId<'ctx> {
        let retval = RtlilId {
            id: self
                .symbol_table
                .insert_uniquified(self.module.ctx(), name)
                .get(),
            module: Some(module),
        };
        retval.assert_module_is(self.module);
        retval
    }
}

#[derive(Default, Debug)]
pub(crate) struct GlobalSymbolTable<'ctx>(IrSymbolTable<'ctx>);

impl<'ctx> GlobalSymbolTable<'ctx> {
    pub(crate) fn new_id<'a>(
        &self,
        ctx: ContextRef<'ctx>,
        name: impl Into<Cow<'a, str>>,
    ) -> RtlilId<'ctx> {
        RtlilId {
            id: self.0.insert_uniquified(ctx, name).get(),
            module: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString;

    #[test]
    fn test_rtlil_id() {
        let test = |id| RtlilId { id, module: None }.to_string();
        assert_eq!(test(""), r"$$");
        assert_eq!(test("abc"), r"\abc");
        assert_eq!(test("abc\\def"), r"\abc\def");
        assert_eq!(test("abc def"), r"\abc␣def");
        assert_eq!(test("abc\rdef"), r"\abc␍def");
        assert_eq!(test("abc\ndef"), r"\abc␤def");
        assert_eq!(test("abc\tdef"), r"\abc␉def");
        assert_eq!(test("abc\0def"), r"\abc␀def");
        assert_eq!(test("⛄"), r"\⛄");
        assert_eq!(test("😀"), r"\😀");
        assert_eq!(test("abc#3"), r"\abc#3");
        assert_eq!(test("#3"), r"$3");
        assert_eq!(test("#123"), r"$123");
        assert_eq!(test("#123a"), r"\#123a");
    }
}
