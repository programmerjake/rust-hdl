// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::ContextRef,
    fmt_utils::{debug_format_option_as_value_or_none, NestedDebugTracking},
    io::{IOVisitor, IO},
    ir::{
        io::{InOrOut, IrIOMutRef, IrInput},
        logic::{IrRegRef, IrWireRef},
        symbols::{IrSymbol, IrSymbolTable},
        types::IrValueTypeRef,
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

pub struct IrModule<'ctx> {
    ctx: ContextRef<'ctx>,
    parent: Option<IrModuleRef<'ctx>>,
    /// name is registered in parent_symbol_table
    name: IrSymbol<'ctx>,
    symbol_table: IrSymbolTable<'ctx>,
    interface_types: Vec<InOrOut<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>>,
    interface_write_ends: OnceCell<Vec<InOrOut<IrInput<'ctx>, IrWireRef<'ctx>>>>,
    pub(crate) wires: RefCell<Vec<IrWireRef<'ctx>>>,
    pub(crate) registers: RefCell<Vec<IrRegRef<'ctx>>>,
    debug_formatting: Cell<bool>,
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

impl<'ctx> IrModule<'ctx> {
    pub fn extract_interface_types<T: IO<'ctx> + ?Sized>(
        ctx: ContextRef<'ctx>,
        external_interface: &mut T,
    ) -> Vec<InOrOut<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>> {
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
        ctx: ContextRef<'ctx>,
        name: Cow<'_, str>,
        parent: Option<IrModuleRef<'ctx>>,
        interface_types: Vec<InOrOut<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>>,
    ) -> IrModuleRef<'ctx> {
        if let Some(parent) = parent {
            assert!(core::ptr::eq(parent.ctx(), ctx));
        }
        let parent_symbol_table = Self::parent_symbol_table_impl(ctx, parent);
        let name = parent_symbol_table.insert_uniquified(ctx, name);
        let module = ctx.modules_arena.alloc(IrModule {
            ctx,
            parent,
            name,
            symbol_table: IrSymbolTable::default(),
            interface_types,
            interface_write_ends: OnceCell::new(),
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
    >(
        ctx: ContextRef<'ctx>,
        name: Cow<'_, str>,
        before_map_interface: F,
        external_interface: &mut T,
    ) -> Result<IrModuleRef<'ctx>, E> {
        let module = Self::new_without_interface(
            ctx,
            name,
            None,
            Self::extract_interface_types(ctx, external_interface),
        );
        before_map_interface(module, external_interface)?;
        module.map_and_set_interface(external_interface);
        Ok(module)
    }
    pub fn new_top_module<T: IO<'ctx> + ?Sized>(
        ctx: ContextRef<'ctx>,
        name: Cow<'_, str>,
        external_interface: &mut T,
    ) -> IrModuleRef<'ctx> {
        let retval: Result<_, Infallible> =
            Self::try_new_top_module(ctx, name, |_, _| Ok(()), external_interface);
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
        name: Cow<'_, str>,
        before_map_interface: F,
        external_interface: &mut T,
    ) -> Result<IrModuleRef<'ctx>, E> {
        let module = Self::new_without_interface(
            parent.ctx(),
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
        name: Cow<'_, str>,
        external_interface: &mut T,
    ) -> IrModuleRef<'ctx> {
        let retval: Result<_, Infallible> =
            Self::try_new_submodule(parent, name, |_, _| Ok(()), external_interface);
        match retval {
            Ok(module) => module,
            Err(v) => match v {},
        }
    }
    pub fn map_and_set_interface<T: IO<'ctx> + ?Sized>(&'ctx self, external_interface: &mut T) {
        let mut interface_write_ends = Vec::with_capacity(self.interface_types().len());
        IOVisitor::visit(
            external_interface,
            &mut |io: IrIOMutRef<'_, 'ctx>, path: &str| {
                let index = interface_write_ends.len();
                assert!(index < self.interface_types().len());
                let write_end = io.map(
                    |v| v.map_to_module_internal(self.parent, self, index, path),
                    |v| v.map_to_module_internal(self.parent, self, index, path),
                );
                interface_write_ends.push(write_end);
                Ok(())
            },
            "io",
        )
        .unwrap();
        assert_eq!(interface_write_ends.len(), self.interface_types().len());
        let was_empty = self.interface_write_ends.set(interface_write_ends).is_ok();
        assert!(was_empty);
    }
    pub fn interface_write_ends(&self) -> Option<&[InOrOut<IrInput<'ctx>, IrWireRef<'ctx>>]> {
        self.interface_write_ends.get().map(Deref::deref)
    }
    pub fn ctx(&self) -> ContextRef<'ctx> {
        self.ctx
    }
    pub fn parent(&self) -> Option<IrModuleRef<'ctx>> {
        self.parent
    }
    fn parent_symbol_table_impl(
        ctx: ContextRef<'ctx>,
        parent: Option<IrModuleRef<'ctx>>,
    ) -> &'ctx IrSymbolTable<'ctx> {
        match parent {
            Some(parent) => parent.symbol_table(),
            None => ctx.root_symbol_table(),
        }
    }
    pub fn parent_symbol_table(&self) -> &IrSymbolTable<'ctx> {
        Self::parent_symbol_table_impl(self.ctx(), self.parent())
    }
    pub fn symbol_table(&self) -> &IrSymbolTable<'ctx> {
        &self.symbol_table
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
            .field(
                "parent",
                debug_format_option_as_value_or_none(self.parent().map(IrModule::path).as_ref()),
            )
            .field("interface_types", &self.interface_types())
            .field(
                "interface_write_ends",
                debug_format_option_as_value_or_none(self.interface_write_ends().as_ref()),
            )
            .field("wires", &DebugWires(&self.wires.borrow()))
            .field("registers", &DebugRegisters(&self.registers.borrow()))
            .finish_non_exhaustive()
    }
}
