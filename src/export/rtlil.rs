// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
//! Exporter for Yosys's RTLIL/ILANG

use crate::{
    context::{AsContext, Intern},
    export::{
        rtlil::id::{GlobalSymbolTable, RtlilId, SymbolTable},
        Exporter,
    },
    ir::{
        io::InOrOut,
        logic::IrRegReset,
        module::{combine_owning_modules, IrModuleRef, OwningModule},
        types::{IrArrayType, IrBitVectorType, IrValueType, IrValueTypeRef},
        values::{
            BoolOutBinOpKind, BoolOutUnOpKind, IrValue, IrValueRef, LiteralBits, Mux,
            SameSizeBinOpKind, SameSizeUnOpKind,
        },
        SourceLocation,
    },
    prelude::{Int, UInt1},
    values::integer::IntShape,
};
use alloc::{
    borrow::Cow,
    format,
    rc::Rc,
    string::{String, ToString},
    vec::Vec,
};
use core::{
    cell::{Cell, RefCell},
    convert::{Infallible, TryFrom},
    fmt::{self, Write as _},
    num::NonZeroU32,
    ops::Range,
};
use hashbrown::HashMap;

mod id;

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

struct RtlilLiteral<'a>(&'a LiteralBits);

impl fmt::Display for RtlilLiteral<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{bit_count}'{value:0bit_count$b}",
            bit_count = usize::try_from(self.0.bit_count()).expect("value too big to write"),
            value = self.0.value().clone().wrap_to_unsigned().value()
        )
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RtlilWireType {
    bit_count: NonZeroU32,
}

#[derive(Debug, Clone)]
struct RtlilWire<'ctx> {
    name: RtlilId<'ctx>,
    ty: RtlilWireType,
    io_index: Option<usize>,
}

struct ModuleData<'ctx> {
    name: RtlilId<'ctx>,
    symbol_table: SymbolTable<'ctx>,
    interface: Rc<[InOrOut<Rc<[RtlilWire<'ctx>]>, Rc<[RtlilWire<'ctx>]>>]>,
    added_to_module_worklist: Cell<bool>,
    added_to_submodule_worklist: Cell<bool>,
    wires_for_values: RefCell<HashMap<IrValueRef<'ctx>, Rc<[RtlilWire<'ctx>]>>>,
    submodule_worklist: RefCell<Vec<IrModuleRef<'ctx>>>,
}

impl<'ctx> ModuleData<'ctx> {
    fn new(module: IrModuleRef<'ctx>, global_symbol_table: &GlobalSymbolTable<'ctx>) -> Self {
        let symbol_table = SymbolTable::new(module);
        let name = global_symbol_table.new_id(module.ctx(), module.path().to_string());
        let mut wires_for_values = HashMap::new();
        let mut io_index = 0usize;
        let ir_interface = module.interface().expect("module is missing its interface");
        let mut interface = Vec::with_capacity(ir_interface.len());
        for io in ir_interface {
            interface.push(match io {
                InOrOut::Input(input) => {
                    let mut wires = Vec::new();
                    visit_wire_types_in_type(
                        &input.external_value().value_type(module.ctx()),
                        &mut input.module_input().path().to_string(),
                        RelationToSelectedField::InSelectedField(&[0; 0]),
                        &mut |ty, path, _relation_to_selected_field| {
                            let name = symbol_table.new_id(module, path);
                            wires.push(RtlilWire {
                                name,
                                ty,
                                io_index: Some(io_index),
                            });
                            io_index += 1;
                            Ok::<_, Infallible>(())
                        },
                    )
                    .unwrap();
                    let wires: Rc<[_]> = wires.into();
                    wires_for_values.insert(
                        IrValue::Input(input.module_input()).intern(module.ctx()),
                        wires.clone(),
                    );
                    InOrOut::Input(wires)
                }
                InOrOut::Output(output) => {
                    let mut wires = Vec::new();
                    visit_wire_types_in_type(
                        &output.wire().value_type(),
                        &mut output.wire().name().to_string(),
                        RelationToSelectedField::InSelectedField(&[0; 0]),
                        &mut |ty, path, _relation_to_selected_field| {
                            let name = symbol_table.new_id(module, path);
                            wires.push(RtlilWire {
                                name,
                                ty,
                                io_index: Some(io_index),
                            });
                            io_index += 1;
                            Ok::<_, Infallible>(())
                        },
                    )
                    .unwrap();
                    let wires: Rc<[_]> = wires.into();
                    wires_for_values.insert(output.wire().read(), wires.clone());
                    InOrOut::Output(wires)
                }
            });
        }
        let interface = interface.into();
        ModuleData {
            name,
            symbol_table,
            interface,
            added_to_module_worklist: Cell::new(false),
            added_to_submodule_worklist: Cell::new(false),
            wires_for_values: RefCell::new(wires_for_values),
            submodule_worklist: RefCell::new(Vec::new()),
        }
    }
}

/// Exporter for Yosys's RTLIL/ILANG
pub struct RtlilExporter<'ctx, W: Write + ?Sized> {
    modules: HashMap<IrModuleRef<'ctx>, Rc<ModuleData<'ctx>>>,
    global_symbol_table: GlobalSymbolTable<'ctx>,
    module_worklist: Vec<IrModuleRef<'ctx>>,
    writer: Writer<W>,
}

impl<'ctx, W: ?Sized + Write> RtlilExporter<'ctx, W> {
    pub fn new(writer: W) -> Self
    where
        W: Sized,
    {
        Self {
            modules: HashMap::new(),
            global_symbol_table: GlobalSymbolTable::default(),
            module_worklist: Vec::new(),
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
    fn get_module_data(&mut self, module: IrModuleRef<'ctx>) -> Rc<ModuleData<'ctx>> {
        let global_symbol_table = &self.global_symbol_table;
        self.modules
            .entry(module)
            .or_insert_with(|| Rc::new(ModuleData::new(module, global_symbol_table)))
            .clone()
    }
    fn add_uniquified_symbol<'a>(
        &mut self,
        module: IrModuleRef<'ctx>,
        name: impl Into<Cow<'a, str>>,
    ) -> RtlilId<'ctx> {
        self.get_module_data(module)
            .symbol_table
            .new_id(module, name.into())
    }
    fn new_anonymous_symbol(&mut self, module: IrModuleRef<'ctx>) -> RtlilId<'ctx> {
        self.add_uniquified_symbol(module, "")
    }
    fn get_int_wire_for_value(
        &mut self,
        module: IrModuleRef<'ctx>,
        value: IrValueRef<'ctx>,
    ) -> Result<Option<RtlilWire<'ctx>>, W::Error> {
        let wires = self.get_wires_for_value(module, value)?;
        if value.get_type(module.ctx()).bit_vector().unwrap().bit_count != 0 {
            assert_eq!(wires.len(), 1);
            Ok(Some(wires[0].clone()))
        } else {
            assert!(wires.is_empty());
            Ok(None)
        }
    }
    fn get_bool_wire_for_value(
        &mut self,
        module: IrModuleRef<'ctx>,
        value: IrValueRef<'ctx>,
    ) -> Result<RtlilWire<'ctx>, W::Error> {
        let retval = self
            .get_int_wire_for_value(module, value)?
            .expect("expected 1-bit wire");
        assert_eq!(retval.ty.bit_count.get(), 1);
        Ok(retval)
    }
    fn export_submodule(
        &mut self,
        parent_module: IrModuleRef<'ctx>,
        submodule: IrModuleRef<'ctx>,
    ) -> Result<(), W::Error> {
        let submodule_data = self.get_module_data(submodule);
        self.add_module_to_worklist(submodule);
        let submodule_ir_interface = submodule
            .interface()
            .expect("module is missing its interface");
        struct Connection<'ctx> {
            submodule_port: RtlilId<'ctx>,
            parent_module_wire: RtlilId<'ctx>,
        }
        let mut connections = Vec::new();
        for (submodule_ir_io, submodule_rtlil_io) in submodule_ir_interface
            .iter()
            .zip(submodule_data.interface.iter())
        {
            match submodule_ir_io {
                InOrOut::Input(submodule_ir_input) => {
                    let submodule_rtlil_input = submodule_rtlil_io.as_ref().input().unwrap();
                    let parent_module_wires = self.get_wires_for_value(
                        parent_module,
                        submodule_ir_input
                            .external_value()
                            .get_wrapped_value()
                            .expect("a submodule input can't be an external input"),
                    )?;
                    for (submodule_port, parent_module_wire) in
                        submodule_rtlil_input.iter().zip(parent_module_wires.iter())
                    {
                        connections.push(Connection {
                            submodule_port: submodule_port.name,
                            parent_module_wire: parent_module_wire.name,
                        });
                    }
                }
                InOrOut::Output(submodule_ir_output) => {
                    let submodule_rtlil_output = submodule_rtlil_io.as_ref().output().unwrap();
                    let parent_module_wires = self.get_wires_for_value(
                        parent_module,
                        submodule_ir_output.output_read().read(),
                    )?;
                    for (submodule_port, parent_module_wire) in submodule_rtlil_output
                        .iter()
                        .zip(parent_module_wires.iter())
                    {
                        connections.push(Connection {
                            submodule_port: submodule_port.name,
                            parent_module_wire: parent_module_wire.name,
                        });
                    }
                }
            }
        }
        writeln!(
            self.writer,
            r"  attribute \src {}",
            RtlilLocation(submodule.source_location())
        )?;
        let instance_name = self.add_uniquified_symbol(parent_module, &**submodule.name());
        writeln!(
            self.writer,
            "  cell {} {}",
            submodule_data.name, instance_name
        )?;
        for Connection {
            submodule_port,
            parent_module_wire,
        } in connections
        {
            submodule_port.assert_module_is(submodule);
            parent_module_wire.assert_module_is(parent_module);
            writeln!(
                self.writer,
                r"    connect {} {}",
                submodule_port, parent_module_wire,
            )?;
        }
        writeln!(self.writer, "  end")
    }
    fn get_wires_for_value(
        &mut self,
        module: IrModuleRef<'ctx>,
        value: IrValueRef<'ctx>,
    ) -> Result<Rc<[RtlilWire<'ctx>]>, W::Error> {
        combine_owning_modules([Some(module), value.owning_module()]);
        let module_data = self.get_module_data(module);
        if let Some(retval) = module_data.wires_for_values.borrow().get(&value) {
            return Ok(retval.clone());
        }
        let wires: Rc<[RtlilWire<'ctx>]> = match *value {
            IrValue::LiteralBits(ref v) => {
                if let Some(bit_count) = NonZeroU32::new(v.bit_count()) {
                    let name = self.new_anonymous_symbol(module);
                    writeln!(self.writer, "  wire width {} {}", bit_count, name)?;
                    writeln!(self.writer, "  connect {} {}", name, RtlilLiteral(v))?;
                    Rc::new([RtlilWire {
                        name,
                        ty: RtlilWireType { bit_count },
                        io_index: None,
                    }])
                } else {
                    Rc::new([])
                }
            }
            IrValue::LiteralArray(v) => {
                let mut wires = Vec::with_capacity(v.elements().len());
                for element in v.elements().iter() {
                    wires.extend_from_slice(&self.get_wires_for_value(module, *element)?);
                }
                wires.into()
            }
            IrValue::LiteralStruct(v) => {
                let mut wires = Vec::with_capacity(v.fields().len());
                for field in v.fields().iter() {
                    wires.extend_from_slice(&self.get_wires_for_value(module, field.value)?);
                }
                wires.into()
            }
            IrValue::LiteralEnumVariant(v) => {
                let discriminant_wire =
                    if let Some(bit_count) = NonZeroU32::new(v.discriminant().bit_count()) {
                        let name = self.new_anonymous_symbol(module);
                        writeln!(self.writer, "  wire width {} {}", bit_count, name)?;
                        writeln!(
                            self.writer,
                            "  connect {} {}",
                            name,
                            RtlilLiteral(v.discriminant())
                        )?;
                        Some(RtlilWire {
                            name,
                            ty: RtlilWireType { bit_count },
                            io_index: None,
                        })
                    } else {
                        None
                    };
                let fields_wire = if let Some(fields_bit_count) =
                    NonZeroU32::new(v.value_type().flattened_fields().bit_count)
                {
                    let name = self.new_anonymous_symbol(module);
                    writeln!(self.writer, "  wire width {} {}", fields_bit_count, name)?;
                    let wires = self.get_wires_for_value(module, v.fields_value())?;
                    write!(self.writer, "  connect {} {{", name)?;
                    for wire in wires.iter().rev() {
                        write!(self.writer, " {}", wire.name)?;
                    }
                    writeln!(self.writer, " }}")?;
                    Some(RtlilWire {
                        name,
                        ty: RtlilWireType {
                            bit_count: fields_bit_count,
                        },
                        io_index: None,
                    })
                } else {
                    None
                };
                discriminant_wire.into_iter().chain(fields_wire).collect()
            }
            IrValue::WireRead(v) => {
                let mut wires = Vec::new();
                visit_wire_types_in_type(
                    &v.0.value_type(),
                    &mut v.0.name().to_string(),
                    RelationToSelectedField::InSelectedField(&[0; 0]),
                    &mut |ty, path, _relation_to_selected_field| {
                        let name = self.add_uniquified_symbol(module, path);
                        writeln!(
                            self.writer,
                            r"  attribute \src {}",
                            RtlilLocation(v.0.source_location())
                        )?;
                        writeln!(self.writer, "  wire width {} {}", ty.bit_count, name)?;
                        wires.push(RtlilWire {
                            name,
                            ty,
                            io_index: None,
                        });
                        Ok(())
                    },
                )?;
                wires.into()
            }
            IrValue::Input(v) => panic!(
                "input {:?} not available in module {:?}",
                v.path(),
                module.path()
            ),
            IrValue::OutputRead(v) => {
                let write_data =
                    v.0.write_data()
                        .expect("can't read from an unconnected output");
                let submodule = write_data.writing_module();
                let submodule_data = self.get_module_data(submodule);
                if !submodule_data.added_to_submodule_worklist.replace(true) {
                    module_data.submodule_worklist.borrow_mut().push(submodule);
                }
                let rtlil_submodule_output = submodule_data.interface[write_data.index()]
                    .clone()
                    .output()
                    .unwrap();
                let ir_submodule_interface = submodule
                    .interface()
                    .expect("module is missing its interface");
                let ir_submodule_output = ir_submodule_interface[write_data.index()]
                    .as_ref()
                    .output()
                    .unwrap();
                let mut wires = Vec::with_capacity(rtlil_submodule_output.len());
                visit_wire_types_in_type(
                    &v.0.value_type(),
                    &mut format!("{}.{}", submodule.name(), ir_submodule_output.wire().name()),
                    RelationToSelectedField::InSelectedField(&[0; 0]),
                    &mut |ty, path, _relation_to_selected_field| {
                        let name = self.add_uniquified_symbol(module, path);
                        wires.push(RtlilWire {
                            name,
                            ty,
                            io_index: None,
                        });
                        writeln!(self.writer, "  wire width {} {}", ty.bit_count, name)
                    },
                )?;
                wires.into()
            }
            IrValue::ExtractStructField(v) => {
                let struct_wires = self.get_wires_for_value(module, v.struct_value())?;
                let mut struct_wires = struct_wires.iter();
                let mut wires = Vec::new();
                visit_wire_types_in_type(
                    &IrValueType::from(v.struct_type()),
                    &mut String::new(),
                    RelationToSelectedField::InSelectedField(&[v.field_index()]),
                    &mut |_wire_type, _path, relation_to_selected_field| {
                        let struct_wire = struct_wires.next().unwrap();
                        if let RelationToSelectedField::InSelectedField(_) =
                            relation_to_selected_field
                        {
                            wires.push(struct_wire.clone());
                        }
                        Ok::<_, Infallible>(())
                    },
                )
                .unwrap();
                wires.into()
            }
            IrValue::ExtractArrayElement(v) => {
                let array_wires = self.get_wires_for_value(module, v.array_value())?;
                let mut array_wires = array_wires.iter();
                let mut wires = Vec::new();
                visit_wire_types_in_type(
                    &IrValueType::from(v.array_type()),
                    &mut String::new(),
                    RelationToSelectedField::InSelectedField(&[v.element_index()]),
                    &mut |_wire_type, _path, relation_to_selected_field| {
                        let array_wire = array_wires.next().unwrap();
                        if let RelationToSelectedField::InSelectedField(_) =
                            relation_to_selected_field
                        {
                            wires.push(array_wire.clone());
                        }
                        Ok::<_, Infallible>(())
                    },
                )
                .unwrap();
                wires.into()
            }
            IrValue::SliceArray(v) => {
                let array_wires = self.get_wires_for_value(module, v.array_value())?;
                let mut array_wires = array_wires.iter();
                let mut wires = Vec::new();
                visit_wire_types_in_type(
                    &IrValueType::from(v.array_type()),
                    &mut String::new(),
                    RelationToSelectedField::InSelectedField(&[v.element_indexes()]),
                    &mut |_wire_type, _path, relation_to_selected_field| {
                        let array_wire = array_wires.next().unwrap();
                        if let RelationToSelectedField::InSelectedField(_) =
                            relation_to_selected_field
                        {
                            wires.push(array_wire.clone());
                        }
                        Ok::<_, Infallible>(())
                    },
                )
                .unwrap();
                wires.into()
            }
            IrValue::RegOutput(v) => {
                let mut wires = Vec::new();
                visit_wire_types_in_type(
                    &v.0.value_type(),
                    &mut v.0.name().to_string(),
                    RelationToSelectedField::InSelectedField(&[0; 0]),
                    &mut |ty, path, _relation_to_selected_field| {
                        let name = self.add_uniquified_symbol(module, path);
                        writeln!(
                            self.writer,
                            r"  attribute \src {}",
                            RtlilLocation(v.0.source_location())
                        )?;
                        writeln!(self.writer, "  wire width {} {}", ty.bit_count, name)?;
                        wires.push(RtlilWire {
                            name,
                            ty,
                            io_index: None,
                        });
                        Ok(())
                    },
                )?;
                wires.into()
            }
            IrValue::Mux(v) => {
                let condition_wire = self.get_bool_wire_for_value(module, v.condition())?;
                let true_value_wires = self.get_wires_for_value(module, v.true_value())?;
                let false_value_wires = self.get_wires_for_value(module, v.false_value())?;
                assert_eq!(true_value_wires.len(), false_value_wires.len());
                let mut wires = Vec::with_capacity(true_value_wires.len());
                for (true_value_wire, false_value_wire) in
                    true_value_wires.iter().zip(false_value_wires.iter())
                {
                    assert_eq!(true_value_wire.ty, false_value_wire.ty);
                    let name = self.new_anonymous_symbol(module);
                    let cell_name = self.new_anonymous_symbol(module);
                    let ty = true_value_wire.ty;
                    writeln!(
                        self.writer,
                        r"  wire width {bit_count} {output}
  cell $mux {cell_name}
    parameter \WIDTH {bit_count}
    connect \S {condition}
    connect \A {false_value}
    connect \B {true_value}
    connect \Y {output}
  end",
                        cell_name = cell_name,
                        bit_count = ty.bit_count,
                        condition = condition_wire.name,
                        false_value = false_value_wire.name,
                        true_value = true_value_wire.name,
                        output = name,
                    )?;
                    wires.push(RtlilWire {
                        name,
                        ty,
                        io_index: None,
                    });
                }
                wires.into()
            }
            IrValue::ConcatBitVectors(v) => {
                if let Some(bit_count) = NonZeroU32::new(v.value_type().bit_count) {
                    let wires = v
                        .bit_vectors()
                        .iter()
                        .map(|bit_vector| self.get_int_wire_for_value(module, *bit_vector))
                        .collect::<Result<Vec<_>, W::Error>>()?;
                    let name = self.new_anonymous_symbol(module);
                    writeln!(self.writer, "  wire width {} {}", bit_count, name)?;
                    write!(self.writer, "  connect {} {{", name)?;
                    for wire in wires.into_iter().rev().flatten() {
                        write!(self.writer, " {}", wire.name)?;
                    }
                    writeln!(self.writer, " }}")?;
                    Rc::new([RtlilWire {
                        ty: RtlilWireType { bit_count },
                        name,
                        io_index: None,
                    }])
                } else {
                    Rc::new([])
                }
            }
            IrValue::SliceBitVector(v) => {
                if let Some(bit_count) = NonZeroU32::new(v.value_type().bit_count) {
                    let base_wire = self
                        .get_int_wire_for_value(module, v.base_value())?
                        .unwrap();
                    let name = self.new_anonymous_symbol(module);
                    writeln!(self.writer, "  wire width {} {}", bit_count, name)?;
                    // convert to an inclusive range
                    let last_bit_index = v.bit_indexes().end - 1;
                    let first_bit_index = v.bit_indexes().start;
                    writeln!(
                        self.writer,
                        "  connect {} {} [{}:{}]",
                        name, base_wire.name, last_bit_index, first_bit_index
                    )?;
                    Rc::new([RtlilWire {
                        ty: RtlilWireType { bit_count },
                        name,
                        io_index: None,
                    }])
                } else {
                    Rc::new([])
                }
            }
            IrValue::SameSizeBinOp(v) => {
                if let Some(bit_count) = NonZeroU32::new(v.value_type().bit_count) {
                    let lhs_wire = self.get_int_wire_for_value(module, v.lhs())?.unwrap();
                    let rhs_wire = self.get_int_wire_for_value(module, v.rhs())?.unwrap();
                    let name = self.new_anonymous_symbol(module);
                    writeln!(self.writer, "  wire width {} {}", bit_count, name)?;
                    let cell_name = self.new_anonymous_symbol(module);
                    let mut lhs_signed = false;
                    let rhs_signed = false;
                    let cell_kind = match v.kind() {
                        SameSizeBinOpKind::Add => "$add",
                        SameSizeBinOpKind::Sub => "$sub",
                        SameSizeBinOpKind::Mul => "$mul",
                        SameSizeBinOpKind::And => "$and",
                        SameSizeBinOpKind::Or => "$or",
                        SameSizeBinOpKind::Xor => "$xor",
                        SameSizeBinOpKind::ShiftLeft => "$shl",
                        SameSizeBinOpKind::LogicalShiftRight => "$shr",
                        SameSizeBinOpKind::ArithmeticShiftRight => {
                            lhs_signed = true;
                            "$sshr"
                        }
                    };
                    writeln!(
                        self.writer,
                        r"  cell {cell_kind} {cell_name}
    parameter \A_SIGNED {lhs_signed}
    parameter \A_WIDTH {bit_count}
    parameter \B_SIGNED {rhs_signed}
    parameter \B_WIDTH {bit_count}
    parameter \Y_WIDTH {bit_count}
    connect \A {lhs}
    connect \B {rhs}
    connect \Y {name}
  end",
                        cell_kind = cell_kind,
                        cell_name = cell_name,
                        lhs_signed = &(lhs_signed as u8),
                        bit_count = bit_count,
                        rhs_signed = &(rhs_signed as u8),
                        lhs = lhs_wire.name,
                        rhs = rhs_wire.name,
                        name = name,
                    )?;
                    Rc::new([RtlilWire {
                        ty: RtlilWireType { bit_count },
                        name,
                        io_index: None,
                    }])
                } else {
                    Rc::new([])
                }
            }
            IrValue::SameSizeUnOp(v) => {
                if let Some(bit_count) = NonZeroU32::new(v.value_type().bit_count) {
                    let input_wire = self.get_int_wire_for_value(module, v.input())?.unwrap();
                    let name = self.new_anonymous_symbol(module);
                    writeln!(self.writer, "  wire width {} {}", bit_count, name)?;
                    let cell_name = self.new_anonymous_symbol(module);
                    let signed = false;
                    let cell_kind = match v.kind() {
                        SameSizeUnOpKind::Not => "$not",
                        SameSizeUnOpKind::Neg => "$neg",
                    };
                    writeln!(
                        self.writer,
                        r"  cell {cell_kind} {cell_name}
    parameter \A_SIGNED {signed}
    parameter \A_WIDTH {bit_count}
    parameter \Y_WIDTH {bit_count}
    connect \A {input}
    connect \Y {name}
  end",
                        cell_kind = cell_kind,
                        cell_name = cell_name,
                        signed = &(signed as u8),
                        bit_count = bit_count,
                        input = input_wire.name,
                        name = name,
                    )?;
                    Rc::new([RtlilWire {
                        ty: RtlilWireType { bit_count },
                        name,
                        io_index: None,
                    }])
                } else {
                    Rc::new([])
                }
            }
            IrValue::BoolOutBinOp(v) => {
                if let Some(input_bit_count) = NonZeroU32::new(v.input_type().bit_count) {
                    let lhs_wire = self.get_int_wire_for_value(module, v.lhs())?.unwrap();
                    let rhs_wire = self.get_int_wire_for_value(module, v.rhs())?.unwrap();
                    let name = self.new_anonymous_symbol(module);
                    writeln!(self.writer, "  wire width 1 {}", name)?;
                    let cell_name = self.new_anonymous_symbol(module);
                    let mut lhs_signed = false;
                    let mut rhs_signed = false;
                    let cell_kind = match v.kind() {
                        BoolOutBinOpKind::CompareEq => "$eq",
                        BoolOutBinOpKind::CompareUnsignedLt => "$lt",
                        BoolOutBinOpKind::CompareSignedLt => {
                            lhs_signed = true;
                            rhs_signed = true;
                            "$lt"
                        }
                    };
                    writeln!(
                        self.writer,
                        r"  cell {cell_kind} {cell_name}
    parameter \A_SIGNED {lhs_signed}
    parameter \A_WIDTH {input_bit_count}
    parameter \B_SIGNED {rhs_signed}
    parameter \B_WIDTH {input_bit_count}
    parameter \Y_WIDTH 1
    connect \A {lhs}
    connect \B {rhs}
    connect \Y {name}
  end",
                        cell_kind = cell_kind,
                        cell_name = cell_name,
                        lhs_signed = &(lhs_signed as u8),
                        input_bit_count = input_bit_count,
                        rhs_signed = &(rhs_signed as u8),
                        lhs = lhs_wire.name,
                        rhs = rhs_wire.name,
                        name = name,
                    )?;
                    Rc::new([RtlilWire {
                        ty: RtlilWireType {
                            bit_count: NonZeroU32::new(1).unwrap(),
                        },
                        name,
                        io_index: None,
                    }])
                } else {
                    let retval = match v.kind() {
                        BoolOutBinOpKind::CompareEq => true,
                        BoolOutBinOpKind::CompareUnsignedLt => false,
                        BoolOutBinOpKind::CompareSignedLt => false,
                    };
                    self.get_wires_for_value(
                        module,
                        IrValue::LiteralBits(LiteralBits::new_bool(retval)).intern(module.ctx()),
                    )?
                }
            }
            IrValue::BoolOutUnOp(v) => {
                if let Some(input_bit_count) = NonZeroU32::new(v.input_type().bit_count) {
                    let input_wire = self.get_int_wire_for_value(module, v.input())?.unwrap();
                    let name = self.new_anonymous_symbol(module);
                    writeln!(self.writer, "  wire width 1 {}", name)?;
                    let cell_name = self.new_anonymous_symbol(module);
                    let signed = false;
                    let cell_kind = match v.kind() {
                        BoolOutUnOpKind::ReduceXor => "$reduce_xor",
                        BoolOutUnOpKind::ReduceAnd => "$reduce_and",
                        BoolOutUnOpKind::ReduceOr => "$reduce_or",
                    };
                    writeln!(
                        self.writer,
                        r"  cell {cell_kind} {cell_name}
    parameter \A_SIGNED {signed}
    parameter \A_WIDTH {input_bit_count}
    parameter \Y_WIDTH 1
    connect \A {input}
    connect \Y {name}
  end",
                        cell_kind = cell_kind,
                        cell_name = cell_name,
                        signed = &(signed as u8),
                        input_bit_count = input_bit_count,
                        input = input_wire.name,
                        name = name,
                    )?;
                    Rc::new([RtlilWire {
                        ty: RtlilWireType {
                            bit_count: NonZeroU32::new(1).unwrap(),
                        },
                        name,
                        io_index: None,
                    }])
                } else {
                    let retval = match v.kind() {
                        BoolOutUnOpKind::ReduceXor => false,
                        BoolOutUnOpKind::ReduceAnd => false,
                        BoolOutUnOpKind::ReduceOr => false,
                    };
                    self.get_wires_for_value(
                        module,
                        IrValue::LiteralBits(LiteralBits::new_bool(retval)).intern(module.ctx()),
                    )?
                }
            }
            IrValue::ConvertIntWrapping(v) => {
                if v.value_type() == v.input_type() {
                    self.get_wires_for_value(module, v.input())?
                } else if let Some(bit_count) = NonZeroU32::new(v.value_type().bit_count) {
                    if let Some(input_wire) = self.get_int_wire_for_value(module, value)? {
                        let name = self.new_anonymous_symbol(module);
                        writeln!(self.writer, "  wire width {} {}", bit_count, name)?;
                        let cell_name = self.new_anonymous_symbol(module);
                        writeln!(
                            self.writer,
                            r"  cell $pos {cell_name}
        parameter \A_SIGNED {signed}
        parameter \A_WIDTH {input_bit_count}
        parameter \Y_WIDTH {bit_count}
        connect \A {input}
        connect \Y {name}
      end",
                            cell_name = cell_name,
                            signed = &(v.input_type().signed as u8),
                            input_bit_count = v.input_type().bit_count,
                            bit_count = bit_count,
                            input = input_wire.name,
                            name = name,
                        )?;
                        Rc::new([RtlilWire {
                            ty: RtlilWireType { bit_count },
                            name,
                            io_index: None,
                        }])
                    } else {
                        self.get_wires_for_value(
                            module,
                            IrValue::LiteralBits(
                                Int::wrapping_with_shape(
                                    0,
                                    IntShape {
                                        bit_count: v.value_type().bit_count,
                                        signed: v.value_type().signed,
                                    },
                                )
                                .into(),
                            )
                            .intern(module.ctx()),
                        )?
                    }
                } else {
                    Rc::new([])
                }
            }
        };
        module_data
            .wires_for_values
            .borrow_mut()
            .insert(value, wires.clone());
        Ok(wires)
    }
    fn add_module_to_worklist(&mut self, module: IrModuleRef<'ctx>) {
        if !self
            .get_module_data(module)
            .added_to_module_worklist
            .replace(true)
        {
            self.module_worklist.push(module);
        }
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

#[derive(Clone, Copy, Debug)]
enum RelationToSelectedField<T> {
    BeforeSelectedField,
    InSelectedField(T),
    AfterSelectedField,
}

impl<T> RelationToSelectedField<T> {
    fn map<F: FnOnce(T) -> R, R>(self, f: F) -> RelationToSelectedField<R> {
        self.and_then(|v| RelationToSelectedField::InSelectedField(f(v)))
    }
    fn and_then<F: FnOnce(T) -> RelationToSelectedField<R>, R>(
        self,
        f: F,
    ) -> RelationToSelectedField<R> {
        match self {
            RelationToSelectedField::BeforeSelectedField => {
                RelationToSelectedField::BeforeSelectedField
            }
            RelationToSelectedField::InSelectedField(v) => f(v),
            RelationToSelectedField::AfterSelectedField => {
                RelationToSelectedField::AfterSelectedField
            }
        }
    }
}

trait FieldRange {
    fn field_range(&self) -> Range<usize>;
}

impl FieldRange for usize {
    fn field_range(&self) -> Range<usize> {
        *self..(1 + *self)
    }
}

impl FieldRange for Range<usize> {
    fn field_range(&self) -> Range<usize> {
        self.clone()
    }
}

fn visit_wire_types_in_type_field<'ctx, E, FR: FieldRange>(
    field_type: IrValueTypeRef<'ctx>,
    field_index: usize,
    field_path_builder: PathBuilder<'_>,
    relation_to_selected_field: RelationToSelectedField<&[FR]>,
    visitor: &mut impl FnMut(RtlilWireType, &str, RelationToSelectedField<()>) -> Result<(), E>,
) -> Result<(), E> {
    visit_wire_types_in_type(
        &field_type,
        field_path_builder,
        relation_to_selected_field.and_then(|subfield_path| -> RelationToSelectedField<&[FR]> {
            if let Some((indexes, subfield_path)) = subfield_path.split_first() {
                let indexes = indexes.field_range();
                if field_index < indexes.start {
                    RelationToSelectedField::BeforeSelectedField
                } else if field_index >= indexes.end {
                    RelationToSelectedField::AfterSelectedField
                } else {
                    RelationToSelectedField::InSelectedField(subfield_path)
                }
            } else {
                RelationToSelectedField::InSelectedField(&[])
            }
        }),
        visitor,
    )
}

fn visit_wire_types_in_type<'ctx, 'a, E, FR: FieldRange>(
    ty: &IrValueType<'ctx>,
    path_builder: impl Into<PathBuilder<'a>>,
    relation_to_selected_field: RelationToSelectedField<&[FR]>,
    visitor: &mut impl FnMut(RtlilWireType, &str, RelationToSelectedField<()>) -> Result<(), E>,
) -> Result<(), E> {
    let mut path_builder = path_builder.into();
    match *ty {
        IrValueType::BitVector(IrBitVectorType {
            bit_count,
            signed: _,
        }) => {
            if let Some(bit_count) = NonZeroU32::new(bit_count) {
                visitor(
                    RtlilWireType { bit_count },
                    path_builder.get(),
                    relation_to_selected_field.map(|subfield_path| {
                        assert!(subfield_path.is_empty());
                    }),
                )?
            }
        }
        IrValueType::Array(IrArrayType { element, length }) => {
            for index in 0..length {
                visit_wire_types_in_type_field(
                    element,
                    index,
                    path_builder.push_array_index(index),
                    relation_to_selected_field,
                    visitor,
                )?;
            }
        }
        IrValueType::Struct(s) => {
            for (index, field) in s.fields().iter().enumerate() {
                visit_wire_types_in_type_field(
                    field.ty,
                    index,
                    path_builder.push_struct_member(&field.name),
                    relation_to_selected_field,
                    visitor,
                )?;
            }
        }
        IrValueType::Enum(e) => {
            let checked_relation_to_selected_field =
                relation_to_selected_field.map(|subfield_path| {
                    // can't access subfields of enum
                    assert!(subfield_path.is_empty());
                });
            if let Some(bit_count) = NonZeroU32::new(e.discriminant_type().bit_count) {
                visitor(
                    RtlilWireType { bit_count },
                    path_builder.push_struct_member("$discriminant").get(),
                    checked_relation_to_selected_field,
                )?;
            }
            if let Some(bit_count) = NonZeroU32::new(e.flattened_fields().bit_count) {
                visitor(
                    RtlilWireType { bit_count },
                    path_builder.push_struct_member("$fields").get(),
                    checked_relation_to_selected_field,
                )?;
            }
        }
    }
    Ok(())
}

impl<'ctx, W: ?Sized + Write> Exporter<'ctx> for RtlilExporter<'ctx, W> {
    type Error = W::Error;

    fn export_ir(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error> {
        self.add_module_to_worklist(module);
        while let Some(module) = self.module_worklist.pop() {
            let module_data = self.get_module_data(module);
            writeln!(self.writer, r#"attribute \generator "rust-hdl""#)?;
            writeln!(
                self.writer,
                r"attribute \src {}",
                RtlilLocation(module.source_location())
            )?;
            writeln!(self.writer, "module {}", module_data.name)?;
            for io in module_data.interface.iter() {
                match io {
                    InOrOut::Input(input) => {
                        for wire in input.iter() {
                            writeln!(
                                self.writer,
                                r"  attribute \src {}",
                                RtlilLocation(module.source_location())
                            )?;
                            writeln!(
                                self.writer,
                                "  wire width {} input {} {}",
                                wire.ty.bit_count,
                                wire.io_index.unwrap(),
                                wire.name,
                            )?;
                        }
                    }
                    InOrOut::Output(output) => {
                        for wire in output.iter() {
                            writeln!(
                                self.writer,
                                r"  attribute \src {}",
                                RtlilLocation(module.source_location())
                            )?;
                            writeln!(
                                self.writer,
                                "  wire width {} output {} {}",
                                wire.ty.bit_count,
                                wire.io_index.unwrap(),
                                wire.name,
                            )?;
                        }
                    }
                }
            }
            for wire in module.wires() {
                let lhs_wires = self.get_wires_for_value(module, wire.read())?;
                let rhs_wires = self.get_wires_for_value(
                    module,
                    wire.assigned_value().unwrap_or_else(|| {
                        panic!(
                            "{}: wire {:?} is not assigned a value",
                            wire.source_location(),
                            wire.path(),
                        )
                    }),
                )?;
                assert_eq!(lhs_wires.len(), rhs_wires.len());
                for (lhs_wire, rhs_wire) in lhs_wires.iter().zip(rhs_wires.iter()) {
                    lhs_wire.name.assert_module_is(module);
                    rhs_wire.name.assert_module_is(module);
                    writeln!(self.writer, "  connect {} {}", lhs_wire.name, rhs_wire.name)?;
                }
            }
            for reg in module.registers() {
                let clk_wire = self.get_bool_wire_for_value(module, reg.clk())?;
                let mut input_value = reg.data_in().unwrap_or_else(|| {
                    panic!(
                        "{}: reg {:?} is not assigned an input data value",
                        reg.source_location(),
                        reg.path(),
                    )
                });
                if let Some(IrRegReset {
                    reset_enable,
                    reset_value,
                }) = reg.rst()
                {
                    input_value = IrValue::from(Mux::new(
                        module.ctx(),
                        reset_enable,
                        reset_value,
                        input_value,
                    ))
                    .intern(module.ctx());
                }
                let input_wires = self.get_wires_for_value(module, input_value)?;
                let output_wires = self.get_wires_for_value(module, reg.output())?;
                assert_eq!(input_wires.len(), output_wires.len());
                let clk_polarity = LiteralBits::from(UInt1::wrapping_new(1));
                for (input_wire, output_wire) in input_wires.iter().zip(output_wires.iter()) {
                    assert_eq!(input_wire.ty, output_wire.ty);
                    let cell_name = self.add_uniquified_symbol(module, output_wire.name);
                    writeln!(
                        self.writer,
                        r"  attribute \src {source_location}
  cell $dff {cell_name}
    parameter \WIDTH {bit_count}
    parameter \CLK_POLARITY {clk_polarity}
    connect \CLK {clk}
    connect \D {d}
    connect \Q {q}
  end",
                        source_location = RtlilLocation(reg.source_location()),
                        cell_name = cell_name,
                        bit_count = output_wire.ty.bit_count,
                        clk_polarity = RtlilLiteral(&clk_polarity),
                        clk = clk_wire.name,
                        d = input_wire.name,
                        q = output_wire.name,
                    )?;
                }
            }
            while let Some(submodule) = module_data.submodule_worklist.borrow_mut().pop() {
                self.export_submodule(module, submodule)?;
            }
            writeln!(self.writer, "end")?;
        }
        Ok(())
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
