// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{export::Exporter, ir::module::IrModuleRef};
use core::{
    borrow::Borrow,
    fmt::{self, Arguments},
};

pub trait Write {
    type Error: fmt::Display + fmt::Debug + Send + 'static;
    fn write_str(&mut self, s: &str) -> Result<(), Self::Error>;
}

#[cfg(std)]
pub struct IoWrite<T: ?Sized + std::io::Write>(pub T);

#[cfg(std)]
impl<T: ?Sized + std::io::Write> Write for IoWrite<T> {
    type Error = std::io::Error;

    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        self.0.write_all(s.as_bytes())
    }
}

pub struct FmtWrite<T: ?Sized + fmt::Write>(pub T);

impl<T: ?Sized + fmt::Write> Write for FmtWrite<T> {
    type Error = fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        self.0.write_str(s)
    }
}

impl<T: ?Sized + Write> Write for &'_ mut T {
    type Error = T::Error;

    fn write_str(&mut self, s: &str) -> Result<(), Self::Error> {
        T::write_str(self, s)
    }
}

pub struct RtlilExporter<W: Write + ?Sized> {
    writer: W,
}

impl<W: ?Sized + Write> RtlilExporter<W> {
    pub fn new(writer: W) -> Self
    where
        W: Sized,
    {
        Self { writer }
    }
    fn write_fmt(&mut self, args: Arguments<'_>) -> Result<(), W::Error> {
        struct Adaptor<T, E> {
            this: T,
            error: Result<(), E>,
        }
        impl<W: ?Sized + Write> fmt::Write for Adaptor<&'_ mut RtlilExporter<W>, W::Error> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                match self.this.writer.write_str(s) {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        self.error = Err(e);
                        Err(fmt::Error)
                    }
                }
            }
        }
        let mut adaptor = Adaptor {
            this: self,
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
    fn write_id<'a>(&mut self, id: impl Borrow<str>) -> Result<(), W::Error> {
        let id: &str = id.borrow();
        if id.is_empty()
            || (id.starts_with("#") && !id[1..].contains(|ch: char| !ch.is_ascii_digit()))
        {
            write!(self, "$")?;
        } else {
            write!(self, "\\")?;
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
                write!(self, "{}", replace_char(ch).unwrap_or(ch))?;
            }
            Ok(())
        } else {
            write!(self, "{}", id)
        }
    }
}

impl<W: ?Sized + Write> Exporter for RtlilExporter<W> {
    type Error = W::Error;

    fn export_ir<'ctx>(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error> {
        todo!()
    }
}

impl<T: fmt::Write> RtlilExporter<FmtWrite<T>> {
    pub fn new_fmt(writer: T) -> Self {
        Self::new(FmtWrite(writer))
    }
}

#[cfg(std)]
impl<T: std::io::Write> RtlilExporter<IoWrite<T>> {
    pub fn new_io(writer: T) -> Self {
        Self::new(IoWrite(writer))
    }
}
