// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{export::Exporter, ir::module::IrModuleRef};
use alloc::string::{String, ToString};
use core::fmt;
use hashbrown::HashSet;

pub trait Write {
    type Error: fmt::Display + fmt::Debug + Send + 'static;
    type Output: ?Sized;
    fn into_output(self) -> Self::Output
    where
        Self: Sized,
        Self::Output: Sized;
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error>;
}

#[cfg(feature = "std")]
pub struct IOWrite<T: ?Sized + std::io::Write>(pub T);

#[cfg(feature = "std")]
impl<T: ?Sized + std::io::Write> Write for IOWrite<T> {
    type Error = std::io::Error;
    type Output = T;
    fn into_output(self) -> Self::Output
    where
        Self: Sized,
        Self::Output: Sized,
    {
        self.0
    }
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        self.0.write_all(s.as_bytes())
    }
}

pub struct FmtWrite<T: ?Sized + fmt::Write>(pub T);

impl<T: ?Sized + fmt::Write> Write for FmtWrite<T> {
    type Error = fmt::Error;
    type Output = T;
    fn into_output(self) -> Self::Output
    where
        Self: Sized,
        Self::Output: Sized,
    {
        self.0
    }
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        self.0.write_str(s)
    }
}

impl<T: ?Sized + Write> Write for &'_ mut T {
    type Error = T::Error;
    type Output = Self;
    fn into_output(self) -> Self::Output
    where
        Self: Sized,
        Self::Output: Sized,
    {
        self
    }
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        T::write_str(self, s)
    }
}

struct Writer<W: Write + ?Sized>(W);

impl<W: Write + ?Sized> Writer<W> {
    fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> Result<(), W::Error> {
        struct Adaptor<T, E> {
            writer: T,
            error: Result<(), E>,
        }
        impl<W: ?Sized + Write> fmt::Write for Adaptor<&'_ mut Writer<W>, W::Error> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                match self.writer.0.write_str(s) {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        self.error = Err(e);
                        Err(fmt::Error)
                    }
                }
            }
        }
        let mut adaptor = Adaptor {
            writer: self,
            error: Ok(()),
        };
        if fmt::Write::write_fmt(&mut adaptor, args).is_err() {
            adaptor.error?;
            // shouldn't happen:
            unreachable!("formatter generated an error")
        } else {
            Ok(())
        }
    }
}

struct Id<'a>(&'a str);

impl fmt::Display for Id<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.0;
        if id.is_empty()
            || (id.starts_with("#") && !id[1..].contains(|ch: char| !ch.is_ascii_digit()))
        {
            write!(f, "$")?;
        } else {
            write!(f, "\\")?;
        };
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
        if id.contains(|ch| replace_char(ch).is_some()) {
            for ch in id.chars() {
                write!(f, "{}", replace_char(ch).unwrap_or(ch))?;
            }
            Ok(())
        } else {
            write!(f, "{}", id)
        }
    }
}

pub struct RtlilExporter<'ctx, W: Write + ?Sized> {
    written_modules: HashSet<IrModuleRef<'ctx>>,
    writer: Writer<W>,
}

impl<'ctx, W: ?Sized + Write> RtlilExporter<'ctx, W> {
    pub fn new(writer: W) -> Self
    where
        W: Sized,
    {
        Self {
            written_modules: HashSet::new(),
            writer: Writer(writer),
        }
    }
    pub fn into_output(self) -> W::Output
    where
        W: Sized,
        W::Output: Sized,
    {
        self.writer.0.into_output()
    }
}

impl<'ctx, W: ?Sized + Write> Exporter<'ctx> for RtlilExporter<'ctx, W> {
    type Error = W::Error;

    fn export_ir(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error> {
        if !self.written_modules.insert(module) {
            return Ok(());
        }
        writeln!(self.writer, r#"attribute \generator "rust-hdl""#)?;
        writeln!(self.writer, "module {}", Id(&module.path().to_string()))?;
        // TODO: finish
        writeln!(self.writer, "end")
    }
}

impl<T: fmt::Write> RtlilExporter<'_, FmtWrite<T>> {
    pub fn new_fmt(writer: T) -> Self {
        Self::new(FmtWrite(writer))
    }
}

impl RtlilExporter<'_, FmtWrite<String>> {
    pub fn new_str() -> Self {
        Self::new_fmt(String::new())
    }
}

#[cfg(feature = "std")]
impl<T: std::io::Write> RtlilExporter<'_, IOWrite<T>> {
    pub fn new_io(writer: T) -> Self {
        Self::new(IOWrite(writer))
    }
}
