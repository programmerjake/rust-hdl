// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, ContextRef, Internable, Interned},
    fmt_utils::{debug_format_option_as_value_or_none, NestedDebugTracking},
    io::{IOVisitor, IO},
    ir::{
        io::{InOrOut, IrIOMutRef, IrInput, IrModuleInput, IrOutputRead},
        logic::{IrRegRef, IrWireRef},
        symbols::{IrSymbol, IrSymbolTable},
        types::IrValueTypeRef,
        SourceLocation,
    },
};
use alloc::{borrow::Cow, vec::Vec};
use core::{
    cell::{Cell, RefCell},
    convert::Infallible,
    fmt,
    hash::{Hash, Hasher},
    ops::Deref,
};
use once_cell::unsync::OnceCell;

pub trait OwningModule<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>>;
}

impl<'ctx> OwningModule<'ctx> for IrModuleRef<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        Some(*self)
    }
}

impl<'ctx, T: OwningModule<'ctx>> OwningModule<'ctx> for Option<T> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.as_ref().and_then(OwningModule::owning_module)
    }
}

impl<'ctx, T: ?Sized + OwningModule<'ctx>> OwningModule<'ctx> for &'_ T {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        (**self).owning_module()
    }
}

impl<'ctx, T: ?Sized + OwningModule<'ctx> + Internable<'ctx>> OwningModule<'ctx>
    for Interned<'ctx, T>
{
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        (**self).owning_module()
    }
}

pub fn combine_owning_modules<'ctx, T: OwningModule<'ctx>, I: IntoIterator<Item = T>>(
    values: I,
    caller: &SourceLocation<'ctx>,
) -> Option<IrModuleRef<'ctx>> {
    let mut owning_module = None::<IrModuleRef<'ctx>>;
    for value in values {
        match (owning_module, value.owning_module()) {
            (Some(a), Some(b)) if a != b => {
                panic!(
                    "owning modules don't match: {} != {}\nat {}",
                    a.path(),
                    b.path(),
                    caller
                )
            }
            (None, Some(v)) => owning_module = Some(v),
            _ => {}
        }
    }
    owning_module
}

#[derive(Debug, Clone)]
pub struct IrModuleInputData<'ctx> {
    external_value: IrInput<'ctx>,
    module_input: IrModuleInput<'ctx>,
}

impl<'ctx> IrModuleInputData<'ctx> {
    pub fn new(external_value: IrInput<'ctx>, module_input: IrModuleInput<'ctx>) -> Self {
        Self {
            external_value,
            module_input,
        }
    }
    pub fn external_value(&self) -> &IrInput<'ctx> {
        &self.external_value
    }
    pub fn module_input(&self) -> IrModuleInput<'ctx> {
        self.module_input
    }
}

#[derive(Debug, Clone)]
pub struct IrModuleOutputData<'ctx> {
    output_read: IrOutputRead<'ctx>,
    wire: IrWireRef<'ctx>,
}

impl<'ctx> IrModuleOutputData<'ctx> {
    pub fn new(output_read: IrOutputRead<'ctx>, wire: IrWireRef<'ctx>) -> Self {
        Self { output_read, wire }
    }
    pub fn output_read(&self) -> IrOutputRead<'ctx> {
        self.output_read
    }
    pub fn wire(&self) -> IrWireRef<'ctx> {
        self.wire
    }
}

pub struct IrModule<'ctx> {
    ctx: ContextRef<'ctx>,
    source_location: Cell<SourceLocation<'ctx>>,
    parent: Option<IrModuleRef<'ctx>>,
    /// name is registered in parent_symbol_table
    name: IrSymbol<'ctx>,
    symbol_table: IrSymbolTable<'ctx>,
    interface_types: Vec<InOrOut<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>>,
    interface: OnceCell<Vec<InOrOut<IrModuleInputData<'ctx>, IrModuleOutputData<'ctx>>>>,
    pub(crate) wires: RefCell<Vec<IrWireRef<'ctx>>>,
    pub(crate) registers: RefCell<Vec<IrRegRef<'ctx>>>,
    debug_formatting: Cell<bool>,
}

impl<'ctx> AsContext<'ctx> for IrModule<'ctx> {
    fn ctx(&self) -> ContextRef<'ctx> {
        self.ctx
    }
}

impl Eq for IrModule<'_> {}

impl Hash for IrModule<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.parent.hash(state);
        self.name.hash(state);
    }
}

impl PartialEq for IrModule<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.parent == other.parent
    }
}

pub type IrModuleRef<'ctx> = &'ctx IrModule<'ctx>;

pub struct IrModulePath<'a, 'ctx>(&'a IrModule<'ctx>);

impl fmt::Debug for IrModulePath<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(parent) = self.0.parent {
            IrModulePath(parent).fmt(f)?;
            write!(f, ".")?;
        }
        write!(f, "{:?}", self.0.name)
    }
}

impl fmt::Display for IrModulePath<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(parent) = self.0.parent {
            IrModulePath(parent).fmt(f)?;
            write!(f, ".")?;
        }
        write!(f, "{}", self.0.name)
    }
}

impl<'ctx> IrModule<'ctx> {
    pub fn extract_interface_types<T: IO<'ctx> + ?Sized, Ctx: AsContext<'ctx>>(
        ctx: Ctx,
        external_interface: &mut T,
    ) -> Vec<InOrOut<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>> {
        let ctx = ctx.ctx();
        let mut interface_types = Vec::new();
        IOVisitor::visit(
            external_interface,
            &mut |io: IrIOMutRef<'_, 'ctx>, _path: &str| {
                interface_types.push(io.map(|v| v.value_type(ctx), |v| v.value_type()));
                Ok(())
            },
            "io",
        )
        .unwrap();
        interface_types
    }
    pub fn new_without_interface(
        ctx: impl AsContext<'ctx>,
        source_location: SourceLocation<'ctx>,
        name: Cow<'_, str>,
        parent: Option<IrModuleRef<'ctx>>,
        interface_types: Vec<InOrOut<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>>,
    ) -> IrModuleRef<'ctx> {
        let ctx = ctx.ctx();
        if let Some(parent) = parent {
            assert!(core::ptr::eq(parent.ctx(), ctx));
        }
        let parent_symbol_table = Self::parent_symbol_table_impl(ctx, parent);
        let name = parent_symbol_table.insert_uniquified(ctx, name);
        let module = ctx.modules_arena.alloc(IrModule {
            ctx,
            source_location: Cell::new(source_location),
            parent,
            name,
            symbol_table: IrSymbolTable::default(),
            interface_types,
            interface: OnceCell::new(),
            wires: RefCell::default(),
            registers: RefCell::default(),
            debug_formatting: Cell::new(false),
        });
        ctx.modules.borrow_mut().push(module);
        module
    }
    pub fn try_new_top_module<
        T: IO<'ctx> + ?Sized,
        F: FnOnce(IrModuleRef<'ctx>, &mut T) -> Result<(), E>,
        E,
        Ctx: AsContext<'ctx>,
    >(
        ctx: Ctx,
        source_location: SourceLocation<'ctx>,
        name: Cow<'_, str>,
        before_map_interface: F,
        external_interface: &mut T,
    ) -> Result<IrModuleRef<'ctx>, E> {
        let ctx = ctx.ctx();
        let module = Self::new_without_interface(
            ctx,
            source_location,
            name,
            None,
            Self::extract_interface_types(ctx, external_interface),
        );
        before_map_interface(module, external_interface)?;
        module.map_and_set_interface(external_interface);
        Ok(module)
    }
    pub fn new_top_module<T: IO<'ctx> + ?Sized>(
        ctx: impl AsContext<'ctx>,
        source_location: SourceLocation<'ctx>,
        name: Cow<'_, str>,
        external_interface: &mut T,
    ) -> IrModuleRef<'ctx> {
        let ctx = ctx.ctx();
        let retval: Result<_, Infallible> = Self::try_new_top_module(
            ctx,
            source_location,
            name,
            |_, _| Ok(()),
            external_interface,
        );
        match retval {
            Ok(module) => module,
            Err(v) => match v {},
        }
    }
    pub fn try_new_submodule<
        T: IO<'ctx> + ?Sized,
        F: FnOnce(IrModuleRef<'ctx>, &mut T) -> Result<(), E>,
        E,
    >(
        parent: IrModuleRef<'ctx>,
        source_location: SourceLocation<'ctx>,
        name: Cow<'_, str>,
        before_map_interface: F,
        external_interface: &mut T,
    ) -> Result<IrModuleRef<'ctx>, E> {
        let module = Self::new_without_interface(
            parent.ctx(),
            source_location,
            name,
            Some(parent),
            Self::extract_interface_types(parent.ctx(), external_interface),
        );
        before_map_interface(module, external_interface)?;
        module.map_and_set_interface(external_interface);
        Ok(module)
    }
    pub fn new_submodule<T: IO<'ctx> + ?Sized>(
        parent: IrModuleRef<'ctx>,
        source_location: SourceLocation<'ctx>,
        name: Cow<'_, str>,
        external_interface: &mut T,
    ) -> IrModuleRef<'ctx> {
        let retval: Result<_, Infallible> = Self::try_new_submodule(
            parent,
            source_location,
            name,
            |_, _| Ok(()),
            external_interface,
        );
        match retval {
            Ok(module) => module,
            Err(v) => match v {},
        }
    }
    pub fn map_and_set_interface<T: IO<'ctx> + ?Sized>(&'ctx self, external_interface: &mut T) {
        let mut interface = Vec::with_capacity(self.interface_types().len());
        IOVisitor::visit(
            external_interface,
            &mut |io: IrIOMutRef<'_, 'ctx>, path: &str| {
                let index = interface.len();
                assert!(index < self.interface_types().len());
                let write_end = io.map(
                    |v| v.map_to_module_internal(self.parent, self, index, path),
                    |v| v.map_to_module_internal(self.parent, self, index, path),
                );
                interface.push(write_end);
                Ok(())
            },
            "io",
        )
        .unwrap();
        assert_eq!(interface.len(), self.interface_types().len());
        let was_empty = self.interface.set(interface).is_ok();
        assert!(was_empty);
    }
    pub fn interface(
        &self,
    ) -> Option<&[InOrOut<IrModuleInputData<'ctx>, IrModuleOutputData<'ctx>>]> {
        self.interface.get().map(Deref::deref)
    }
    pub fn parent(&self) -> Option<IrModuleRef<'ctx>> {
        self.parent
    }
    fn parent_symbol_table_impl(
        ctx: impl AsContext<'ctx>,
        parent: Option<IrModuleRef<'ctx>>,
    ) -> &'ctx IrSymbolTable<'ctx> {
        match parent {
            Some(parent) => parent.symbol_table(),
            None => ctx.ctx().root_symbol_table(),
        }
    }
    pub fn parent_symbol_table(&self) -> &IrSymbolTable<'ctx> {
        Self::parent_symbol_table_impl(self, self.parent())
    }
    pub fn symbol_table(&self) -> &IrSymbolTable<'ctx> {
        &self.symbol_table
    }
    pub fn source_location(&self) -> SourceLocation<'ctx> {
        self.source_location.get()
    }
    pub fn set_source_location(&self, source_location: SourceLocation<'ctx>) {
        self.source_location.set(source_location);
    }
    /// name is registered in parent_symbol_table
    pub fn name(&self) -> IrSymbol<'ctx> {
        self.name
    }
    pub fn path(&self) -> IrModulePath<'_, 'ctx> {
        IrModulePath(self)
    }
    pub fn interface_types(&self) -> &[InOrOut<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>] {
        &self.interface_types
    }
    pub fn wires(&self) -> Vec<IrWireRef<'ctx>> {
        self.wires.borrow().clone()
    }
    pub fn registers(&self) -> Vec<IrRegRef<'ctx>> {
        self.registers.borrow().clone()
    }
}

impl<'ctx> fmt::Debug for IrModule<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nested_debug_tracking = NestedDebugTracking::new(&self.debug_formatting);
        if nested_debug_tracking.nested() {
            return f
                .debug_struct("IrModule")
                .field("path", &self.path())
                .finish_non_exhaustive();
        }
        struct DebugWires<'a, 'ctx>(&'a [IrWireRef<'ctx>]);
        impl fmt::Debug for DebugWires<'_, '_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut debug_list = f.debug_map();
                for wire in self.0 {
                    debug_list.entry(&wire.name(), &wire.debug_fmt_without_name());
                }
                debug_list.finish()
            }
        }
        struct DebugRegisters<'a, 'ctx>(&'a [IrRegRef<'ctx>]);
        impl fmt::Debug for DebugRegisters<'_, '_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut debug_list = f.debug_map();
                for reg in self.0 {
                    debug_list.entry(&reg.name(), &reg.debug_fmt_without_name());
                }
                debug_list.finish()
            }
        }
        f.debug_struct("IrModule")
            .field("path", &self.path())
            .field("source_location", &self.source_location())
            .field(
                "parent",
                debug_format_option_as_value_or_none(self.parent().map(IrModule::path).as_ref()),
            )
            .field("interface_types", &self.interface_types())
            .field(
                "interface",
                debug_format_option_as_value_or_none(self.interface().as_ref()),
            )
            .field("wires", &DebugWires(&self.wires.borrow()))
            .field("registers", &DebugRegisters(&self.registers.borrow()))
            .finish_non_exhaustive()
    }
}
