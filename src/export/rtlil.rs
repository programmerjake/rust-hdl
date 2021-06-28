// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::Intern,
    export::Exporter,
    ir::{
        io::InOrOut,
        logic::IrWireRef,
        module::IrModuleRef,
        types::{IrStructFieldType, IrStructType, IrValueType, IrValueTypeRef},
        values::{IrValue, IrValueRef},
        SourceLocation,
    },
};
use alloc::{
    format,
    rc::Rc,
    string::{String, ToString},
    vec::Vec,
};
use core::fmt::{self, Write as _};
use hashbrown::{HashMap, HashSet};

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

struct RtlilId<'a>(&'a str);

impl fmt::Display for RtlilId<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.0;
        if id.is_empty() {
            return write!(f, "$$");
        }
        match id.strip_prefix("#") {
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

struct RtlilStr<'a>(&'a str);

impl fmt::Display for RtlilStr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"")?;
        for b in self.0.bytes() {
            match b {
                b'\n' => write!(f, r"\n")?,
                b'\t' => write!(f, r"\t")?,
                b'\\' => write!(f, r"\\")?,
                b'\"' => write!(f, r#"\""#)?,
                b' ' => write!(f, " ")?,
                b'\0' => {
                    for b in "\u{2400}".bytes() {
                        write!(f, "\\{:03o}", b)?;
                    }
                }
                _ => {
                    if b.is_ascii_graphic() {
                        write!(f, "{}", b as char)?;
                    } else {
                        write!(f, "\\{:03o}", b)?;
                    }
                }
            }
        }
        write!(f, "\"")
    }
}

struct RtlilLocation<'ctx>(SourceLocation<'ctx>);

impl fmt::Display for RtlilLocation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        RtlilStr(&format!(
            "{}:{}.{}",
            self.0.file(),
            self.0.line(),
            self.0.column()
        ))
        .fmt(f)
    }
}

#[derive(Debug, Clone, Copy)]
struct RtlilWireType {
    bit_count: u32,
}

#[derive(Debug, Clone)]
struct RtlilWire {
    name: Rc<str>,
    ty: RtlilWireType,
}

pub struct RtlilExporter<'ctx, W: Write + ?Sized> {
    written_modules: HashSet<IrModuleRef<'ctx>>,
    wires_for_values: HashMap<IrValueRef<'ctx>, Rc<[RtlilWire]>>,
    writer: Writer<W>,
}

impl<'ctx, W: ?Sized + Write> RtlilExporter<'ctx, W> {
    pub fn new(writer: W) -> Self
    where
        W: Sized,
    {
        Self {
            written_modules: HashSet::new(),
            wires_for_values: HashMap::new(),
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

struct PathBuilder<'a> {
    path_prefix: &'a mut String,
    initial_len: usize,
}

impl<'a> From<&'a mut String> for PathBuilder<'a> {
    fn from(path_prefix: &'a mut String) -> Self {
        Self::new(path_prefix)
    }
}

impl<'a> PathBuilder<'a> {
    fn new(path_prefix: &'a mut String) -> Self {
        Self {
            initial_len: path_prefix.len(),
            path_prefix,
        }
    }
    fn push_fmt<'b>(&'b mut self, args: fmt::Arguments<'_>) -> PathBuilder<'b> {
        let retval = PathBuilder::new(self.path_prefix);
        retval.path_prefix.write_fmt(args).unwrap();
        retval
    }
    fn push_array_index<'b>(&'b mut self, index: usize) -> PathBuilder<'b> {
        self.push_fmt(format_args!("[{}]", index))
    }
    fn push_struct_member<'b>(&'b mut self, name: &str) -> PathBuilder<'b> {
        self.push_fmt(format_args!(".{}", name))
    }
    fn get(&self) -> &str {
        &self.path_prefix
    }
}

impl Drop for PathBuilder<'_> {
    fn drop(&mut self) {
        self.path_prefix.truncate(self.initial_len)
    }
}

fn visit_wire_types_in_type<'ctx, 'a, E>(
    ty: IrValueTypeRef<'ctx>,
    path_builder: impl Into<PathBuilder<'a>>,
    visitor: &mut impl FnMut(RtlilWireType, &str) -> Result<(), E>,
) -> Result<(), E> {
    let mut path_builder = path_builder.into();
    match *ty {
        IrValueType::BitVector { bit_count } => {
            visitor(RtlilWireType { bit_count }, path_builder.get())?
        }
        IrValueType::Array { element, length } => {
            for index in 0..length {
                visit_wire_types_in_type(element, path_builder.push_array_index(index), visitor)?;
            }
        }
        IrValueType::Struct(IrStructType { fields }) => {
            for &IrStructFieldType { name, ty } in &*fields {
                visit_wire_types_in_type(ty, path_builder.push_struct_member(&name), visitor)?;
            }
        }
    }
    Ok(())
}

impl<'ctx, W: ?Sized + Write> RtlilExporter<'ctx, W> {
    fn export_wire(
        &mut self,
        wire: IrWireRef<'ctx>,
        mut io_index: Option<&mut usize>,
    ) -> Result<(), W::Error> {
        let key = wire.read();
        if self.wires_for_values.contains_key(&key) {
            return Ok(());
        }
        let mut wires = Vec::new();
        visit_wire_types_in_type(
            wire.value_type(),
            &mut wire.name().to_string(),
            &mut |ty, path| {
                wires.push(RtlilWire {
                    name: path.into(),
                    ty,
                });
                write!(self.writer, "  wire width {}", ty.bit_count)?;
                if let Some(io_index) = io_index.as_deref_mut() {
                    write!(self.writer, " output {}", io_index)?;
                    *io_index += 1;
                }
                writeln!(self.writer, " {}", RtlilId(path))?;
                Ok(())
            },
        )?;
        self.wires_for_values.insert(key, wires.into());
        Ok(())
    }
}

impl<'ctx, W: ?Sized + Write> Exporter<'ctx> for RtlilExporter<'ctx, W> {
    type Error = W::Error;

    fn export_ir(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error> {
        if !self.written_modules.insert(module) {
            return Ok(());
        }
        writeln!(self.writer, r#"attribute \generator "rust-hdl""#)?;
        writeln!(
            self.writer,
            r"attribute \src {}",
            RtlilLocation(module.source_location())
        )?;
        writeln!(
            self.writer,
            "module {}",
            RtlilId(&module.path().to_string())
        )?;
        let ctx = module.ctx();
        let interface = module.interface().expect("module is missing its interface");
        let mut io_index = 0usize;
        for io in interface {
            match io {
                InOrOut::Input(input) => {
                    let mut wires = Vec::new();
                    visit_wire_types_in_type(
                        input.external_value().value_type(ctx),
                        &mut input.module_input().path().to_string(),
                        &mut |ty, path| {
                            wires.push(RtlilWire {
                                name: path.into(),
                                ty,
                            });
                            writeln!(
                                self.writer,
                                "  wire width {} input {} {}",
                                ty.bit_count,
                                io_index,
                                RtlilId(path)
                            )?;
                            io_index += 1;
                            Ok(())
                        },
                    )?;
                    self.wires_for_values.insert(
                        IrValue::Input(input.module_input()).intern(ctx),
                        wires.into(),
                    );
                }
                InOrOut::Output(output) => self.export_wire(output, Some(&mut io_index))?,
            }
        }
        for wire in module.wires() {
            self.export_wire(wire, None)?;
        }
        for reg in module.registers() {
            let mut wires = Vec::new();
            visit_wire_types_in_type(
                reg.value_type(),
                &mut reg.name().to_string(),
                &mut |ty, path| {
                    wires.push(RtlilWire {
                        name: path.into(),
                        ty,
                    });
                    writeln!(
                        self.writer,
                        "  wire width {} {}",
                        ty.bit_count,
                        RtlilId(path)
                    )?;
                    Ok(())
                },
            )?;
            self.wires_for_values.insert(reg.output(), wires.into());
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rtlil_id() {
        assert_eq!(RtlilId("").to_string(), r"$$");
        assert_eq!(RtlilId("abc").to_string(), r"\abc");
        assert_eq!(RtlilId("abc\\def").to_string(), r"\abc\def");
        assert_eq!(RtlilId("abc def").to_string(), r"\abc␣def");
        assert_eq!(RtlilId("abc\rdef").to_string(), r"\abc␍def");
        assert_eq!(RtlilId("abc\ndef").to_string(), r"\abc␤def");
        assert_eq!(RtlilId("abc\tdef").to_string(), r"\abc␉def");
        assert_eq!(RtlilId("abc\0def").to_string(), r"\abc␀def");
        assert_eq!(RtlilId("⛄").to_string(), r"\⛄");
        assert_eq!(RtlilId("😀").to_string(), r"\😀");
        assert_eq!(RtlilId("abc#3").to_string(), r"\abc#3");
        assert_eq!(RtlilId("#3").to_string(), r"$3");
        assert_eq!(RtlilId("#123").to_string(), r"$123");
        assert_eq!(RtlilId("#123a").to_string(), r"\#123a");
    }

    #[test]
    fn test_rtlil_str() {
        assert_eq!(RtlilStr("").to_string(), r#""""#);
        assert_eq!(RtlilStr("␀").to_string(), r#""\342\220\200""#);
        assert_eq!(RtlilStr("\0").to_string(), r#""\342\220\200""#);
        assert_eq!(RtlilStr("\r").to_string(), r#""\015""#);
        assert_eq!(RtlilStr("\t").to_string(), r#""\t""#);
        assert_eq!(RtlilStr("\n").to_string(), r#""\n""#);
        assert_eq!(RtlilStr("\\").to_string(), r#""\\""#);
        assert_eq!(RtlilStr("\"").to_string(), r#""\"""#);
        assert_eq!(RtlilStr("'").to_string(), r#""'""#);
        assert_eq!(RtlilStr("\u{7F}").to_string(), r#""\177""#);
        assert_eq!(RtlilStr("⛄").to_string(), r#""\342\233\204""#);
        assert_eq!(RtlilStr("😀").to_string(), r#""\360\237\230\200""#);
        assert_eq!(RtlilStr("abc def").to_string(), r#""abc def""#);
    }

    #[test]
    fn test_rtlil_location() {
        assert_eq!(
            RtlilLocation(SourceLocation::new_borrowed(
                "/home/me/my-project/my_file.rs",
                123,
                45
            ))
            .to_string(),
            r#""/home/me/my-project/my_file.rs:123.45""#
        );
        assert_eq!(
            RtlilLocation(SourceLocation::new_borrowed(
                "D:\\my-project\\my_file.rs",
                123,
                45
            ))
            .to_string(),
            r#""D:\\my-project\\my_file.rs:123.45""#
        );
    }
}
