// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::Intern,
    fmt_utils::debug_format_option_as_value_or_none,
    ir::{
        module::IrModuleRef,
        symbols::IrSymbol,
        types::{IrValueType, IrValueTypeRef},
        values::{IrValue, IrValueRef},
        SourceLocation,
    },
};
use alloc::borrow::Cow;
use core::{
    cell::Cell,
    fmt,
    hash::{Hash, Hasher},
    ptr,
};
use once_cell::unsync::OnceCell;

pub struct IrWire<'ctx> {
    module: IrModuleRef<'ctx>,
    source_location: Cell<SourceLocation<'ctx>>,
    name: IrSymbol<'ctx>,
    value_type: IrValueTypeRef<'ctx>,
    assigned_value: OnceCell<IrValueRef<'ctx>>,
}

pub type IrWireRef<'ctx> = &'ctx IrWire<'ctx>;

impl fmt::Debug for IrWire<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrWire")
            .field("path", &self.path())
            .field("source_location", &self.source_location())
            .field("value_type", &self.value_type())
            .field(
                "assigned_value",
                debug_format_option_as_value_or_none(self.assigned_value.get()),
            )
            .finish_non_exhaustive()
    }
}

pub struct IrWirePath<'a, 'ctx>(&'a IrWire<'ctx>);

impl fmt::Debug for IrWirePath<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}.{:?}", self.0.module.path(), self.0.name)
    }
}

impl<'ctx> IrWire<'ctx> {
    pub fn module(&self) -> IrModuleRef<'ctx> {
        self.module
    }
    pub fn source_location(&self) -> SourceLocation<'ctx> {
        self.source_location.get()
    }
    pub fn set_source_location(&self, source_location: SourceLocation<'ctx>) {
        self.source_location.set(source_location);
    }
    pub fn path(&self) -> IrWirePath<'_, 'ctx> {
        IrWirePath(self)
    }
    pub fn name(&self) -> IrSymbol<'ctx> {
        self.name
    }
    pub fn value_type(&self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn new(
        module: IrModuleRef<'ctx>,
        source_location: SourceLocation<'ctx>,
        name: Cow<'_, str>,
        value_type: IrValueTypeRef<'ctx>,
    ) -> IrWireRef<'ctx> {
        let retval = module.ctx().wires_arena.alloc(Self {
            module,
            source_location: Cell::new(source_location),
            name: module.symbol_table().insert_uniquified(module.ctx(), name),
            value_type,
            assigned_value: OnceCell::new(),
        });
        module.wires.borrow_mut().push(retval);
        retval
    }
    pub fn read(&'ctx self) -> IrValueRef<'ctx> {
        IrValue::WireRead(IrWireRead(self)).intern(self.module().ctx())
    }
    #[track_caller]
    pub fn assign(&self, value: IrValueRef<'ctx>) {
        let value_type = value.get_type(self.module.ctx());
        if let Some(owning_module) = value.owning_module() {
            assert_eq!(self.module(), owning_module);
        }
        assert_eq!(self.value_type, value_type);
        if let Err(_) = self.assigned_value.set(value) {
            panic!("Wire already assigned");
        }
    }
    pub fn debug_fmt_without_name<'a: 'ctx>(&'a self) -> impl fmt::Debug + 'a {
        struct FmtWithoutName<'a, 'ctx>(&'a IrWire<'ctx>);
        impl fmt::Debug for FmtWithoutName<'_, '_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct("IrWire")
                    .field("source_location", &self.0.source_location())
                    .field("value_type", &self.0.value_type())
                    .field(
                        "assigned_value",
                        debug_format_option_as_value_or_none(self.0.assigned_value.get()),
                    )
                    .finish_non_exhaustive()
            }
        }
        FmtWithoutName(self)
    }
}

#[derive(Clone, Copy)]
pub struct IrWireRead<'ctx>(pub IrWireRef<'ctx>);

impl fmt::Debug for IrWireRead<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrWireRead")
            .field("path", &self.0.path())
            .field("value_type", &self.0.value_type())
            .finish_non_exhaustive()
    }
}

impl<'ctx> Eq for IrWireRead<'ctx> {}

impl<'ctx> PartialEq for IrWireRead<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'ctx> Hash for IrWireRead<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const IrWire<'ctx>).hash(state)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IrRegReset<'ctx> {
    pub reset_enable: IrValueRef<'ctx>,
    pub reset_value: IrValueRef<'ctx>,
}

pub struct IrRegPath<'a, 'ctx>(&'a IrReg<'ctx>);

impl fmt::Debug for IrRegPath<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}.{:?}", self.0.module.path(), self.0.name)
    }
}

pub struct IrReg<'ctx> {
    module: IrModuleRef<'ctx>,
    source_location: Cell<SourceLocation<'ctx>>,
    name: IrSymbol<'ctx>,
    value_type: IrValueTypeRef<'ctx>,
    data_in: OnceCell<IrValueRef<'ctx>>,
    clk: IrValueRef<'ctx>,
    rst: Option<IrRegReset<'ctx>>,
}

impl<'ctx> IrReg<'ctx> {
    pub fn new(
        module: IrModuleRef<'ctx>,
        source_location: SourceLocation<'ctx>,
        name: Cow<'_, str>,
        value_type: IrValueTypeRef<'ctx>,
        clk: IrValueRef<'ctx>,
        rst: Option<IrRegReset<'ctx>>,
    ) -> IrRegRef<'ctx> {
        assert_eq!(
            *clk.get_type(module.ctx()),
            IrValueType::BitVector { bit_count: 1 }
        );
        if let Some(owning_module) = clk.owning_module() {
            assert_eq!(module, owning_module);
        }
        if let Some(IrRegReset {
            reset_enable,
            reset_value,
        }) = rst
        {
            assert_eq!(
                *reset_enable.get_type(module.ctx()),
                IrValueType::BitVector { bit_count: 1 }
            );
            if let Some(owning_module) = reset_enable.owning_module() {
                assert_eq!(module, owning_module);
            }
            assert_eq!(reset_value.get_type(module.ctx()), value_type);
            if let Some(owning_module) = reset_value.owning_module() {
                assert_eq!(module, owning_module);
            }
        }
        let retval = module.ctx().registers_arena.alloc(Self {
            module,
            source_location: Cell::new(source_location),
            name: module.symbol_table().insert_uniquified(module.ctx(), name),
            value_type,
            data_in: OnceCell::new(),
            rst,
            clk,
        });
        module.registers.borrow_mut().push(retval);
        retval
    }
    pub fn module(&self) -> IrModuleRef<'ctx> {
        self.module
    }
    pub fn source_location(&self) -> SourceLocation<'ctx> {
        self.source_location.get()
    }
    pub fn set_source_location(&self, source_location: SourceLocation<'ctx>) {
        self.source_location.set(source_location);
    }
    pub fn path(&self) -> IrRegPath<'_, 'ctx> {
        IrRegPath(self)
    }
    pub fn name(&self) -> IrSymbol<'ctx> {
        self.name
    }
    pub fn value_type(&self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn data_in(&self) -> Option<IrValueRef<'ctx>> {
        self.data_in.get().copied()
    }
    #[track_caller]
    pub fn assign_data_in(&self, data_in: IrValueRef<'ctx>) {
        let value_type = data_in.get_type(self.module.ctx());
        if let Some(owning_module) = data_in.owning_module() {
            assert_eq!(self.module(), owning_module);
        }
        assert_eq!(self.value_type, value_type);
        if let Err(_) = self.data_in.set(data_in) {
            panic!("register's input already assigned");
        }
    }
    pub fn rst(&self) -> Option<IrRegReset<'ctx>> {
        self.rst
    }
    pub fn clk(&self) -> IrValueRef<'ctx> {
        self.clk
    }
    pub fn reset_enable(&self) -> Option<IrValueRef<'ctx>> {
        self.rst.map(|v| v.reset_enable)
    }
    pub fn reset_value(&self) -> Option<IrValueRef<'ctx>> {
        self.rst.map(|v| v.reset_value)
    }
    pub fn output(&'ctx self) -> IrValueRef<'ctx> {
        IrValue::RegOutput(IrRegOutput(self)).intern(self.module().ctx())
    }
    pub fn debug_fmt_without_name<'a: 'ctx>(&'a self) -> impl fmt::Debug + 'a {
        struct FmtWithoutName<'a, 'ctx>(&'a IrReg<'ctx>);
        impl fmt::Debug for FmtWithoutName<'_, '_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct("IrReg")
                    .field("source_location", &self.0.source_location())
                    .field("value_type", &self.0.value_type())
                    .field(
                        "data_in",
                        debug_format_option_as_value_or_none(self.0.data_in.get()),
                    )
                    .field("clk", &self.0.clk())
                    .field(
                        "reset_enable",
                        debug_format_option_as_value_or_none(self.0.reset_enable().as_ref()),
                    )
                    .field(
                        "reset_value",
                        debug_format_option_as_value_or_none(self.0.reset_value().as_ref()),
                    )
                    .finish_non_exhaustive()
            }
        }
        FmtWithoutName(self)
    }
}

impl fmt::Debug for IrReg<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrReg")
            .field("path", &self.path())
            .field("source_location", &self.source_location())
            .field("value_type", &self.value_type())
            .field(
                "data_in",
                debug_format_option_as_value_or_none(self.data_in.get()),
            )
            .field("clk", &self.clk())
            .field(
                "reset_enable",
                debug_format_option_as_value_or_none(self.reset_enable().as_ref()),
            )
            .field(
                "reset_value",
                debug_format_option_as_value_or_none(self.reset_value().as_ref()),
            )
            .finish_non_exhaustive()
    }
}

pub type IrRegRef<'ctx> = &'ctx IrReg<'ctx>;

#[derive(Clone, Copy)]
pub struct IrRegOutput<'ctx>(pub IrRegRef<'ctx>);

impl fmt::Debug for IrRegOutput<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrRegOutput")
            .field("path", &self.0.path())
            .field("value_type", &self.0.value_type())
            .finish_non_exhaustive()
    }
}

impl<'ctx> Eq for IrRegOutput<'ctx> {}

impl<'ctx> PartialEq for IrRegOutput<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'ctx> Hash for IrRegOutput<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const IrReg<'ctx>).hash(state)
    }
}
