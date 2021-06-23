// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::ContextRef,
    fmt_utils::{debug_format_option_as_value_or_none, NestedDebugTracking},
    io::IOTrait,
    ir::{
        io::{IrIOMutRef, IO},
        logic::IrWireRef,
        types::IrValueTypeRef,
        values::IrValueRef,
    },
};
use alloc::vec::Vec;
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
    id: usize,
    interface_types: Vec<IO<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>>,
    interface_write_ends: OnceCell<Vec<IO<IrValueRef<'ctx>, IrWireRef<'ctx>>>>,
    pub(crate) wires: RefCell<Vec<IrWireRef<'ctx>>>,
    debug_formatting: Cell<bool>,
}

impl Eq for IrModule<'_> {}

impl Hash for IrModule<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for IrModule<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub type IrModuleRef<'ctx> = &'ctx IrModule<'ctx>;

impl<'ctx> IrModule<'ctx> {
    pub fn new_without_interface(
        ctx: ContextRef<'ctx>,
        interface_types: Vec<IO<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>>,
    ) -> IrModuleRef<'ctx> {
        let module = ctx.modules_arena.alloc(IrModule {
            ctx,
            id: ctx.modules.borrow().len(),
            interface_types,
            interface_write_ends: OnceCell::new(),
            wires: RefCell::default(),
            debug_formatting: Cell::new(false),
        });
        ctx.modules.borrow_mut().push(module);
        module
    }
    pub fn try_new<
        T: IOTrait<'ctx> + ?Sized,
        F: FnOnce(IrModuleRef<'ctx>, &mut T) -> Result<(), E>,
        E,
    >(
        ctx: ContextRef<'ctx>,
        before_map_interface: F,
        external_interface: &mut T,
    ) -> Result<IrModuleRef<'ctx>, E> {
        let mut interface_types = Vec::new();
        external_interface
            .visit_ir_ports(&mut |io| {
                interface_types
                    .push(io.map(|v| v.get_wrapped_value().get_type(ctx), |v| v.value_type()));
                Ok(())
            })
            .unwrap();
        let module = Self::new_without_interface(ctx, interface_types);
        before_map_interface(module, external_interface)?;
        module.map_and_set_interface(external_interface);
        Ok(module)
    }
    pub fn new<T: IOTrait<'ctx> + ?Sized, F: FnOnce(IrModuleRef<'ctx>, &mut T)>(
        ctx: ContextRef<'ctx>,
        before_map_interface: F,
        external_interface: &mut T,
    ) -> IrModuleRef<'ctx> {
        let retval: Result<_, Infallible> = Self::try_new(
            ctx,
            |module, interface| {
                before_map_interface(module, interface);
                Ok(())
            },
            external_interface,
        );
        match retval {
            Ok(module) => module,
            Err(v) => match v {},
        }
    }
    pub fn map_and_set_interface<T: IOTrait<'ctx> + ?Sized>(
        &'ctx self,
        external_interface: &mut T,
    ) {
        let mut interface_write_ends = Vec::with_capacity(self.interface_types().len());
        external_interface
            .visit_ir_ports(&mut |io: IrIOMutRef<'_, 'ctx>| {
                let index = interface_write_ends.len();
                assert!(index < self.interface_types().len());
                let write_end = io.map(
                    |v| v.map_to_module_internal(self, index),
                    |v| v.map_to_module_internal(self, index),
                );
                interface_write_ends.push(write_end);
                Ok(())
            })
            .unwrap();
        assert_eq!(interface_write_ends.len(), self.interface_types().len());
        let was_empty = self.interface_write_ends.set(interface_write_ends).is_ok();
        assert!(was_empty);
    }
    pub fn interface_write_ends(&self) -> Option<&[IO<IrValueRef<'ctx>, IrWireRef<'ctx>>]> {
        self.interface_write_ends.get().map(Deref::deref)
    }
    pub fn ctx(&self) -> ContextRef<'ctx> {
        self.ctx
    }
    pub fn id(&self) -> impl fmt::Debug + 'static {
        self.id
    }
    pub fn interface_types(&self) -> &[IO<IrValueTypeRef<'ctx>, IrValueTypeRef<'ctx>>] {
        &self.interface_types
    }
}

impl fmt::Debug for IrModule<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nested_debug_tracking = NestedDebugTracking::new(&self.debug_formatting);
        if nested_debug_tracking.nested() {
            return f
                .debug_struct("IrModule")
                .field("id", &self.id())
                .finish_non_exhaustive();
        }
        struct DebugWires<'a, 'ctx>(&'a [IrWireRef<'ctx>]);
        impl fmt::Debug for DebugWires<'_, '_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut debug_list = f.debug_map();
                for wire in self.0 {
                    debug_list.entry(&wire.id(), &wire.debug_fmt_without_id());
                }
                debug_list.finish()
            }
        }
        f.debug_struct("IrModule")
            .field("id", &self.id())
            .field("interface_types", &self.interface_types())
            .field(
                "interface_write_ends",
                debug_format_option_as_value_or_none(self.interface_write_ends().as_ref()),
            )
            .field("wires", &DebugWires(&self.wires.borrow()))
            .finish_non_exhaustive()
    }
}
