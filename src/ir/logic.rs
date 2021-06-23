// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::Intern,
    fmt_utils::debug_format_option_as_value_or_none,
    ir::{
        module::IrModuleRef,
        types::IrValueTypeRef,
        values::{IrValue, IrValueRef},
    },
};
use core::{
    fmt,
    hash::{Hash, Hasher},
    ptr,
};
use once_cell::unsync::OnceCell;

pub struct IrWire<'ctx> {
    module: IrModuleRef<'ctx>,
    pub(crate) id: usize,
    value_type: IrValueTypeRef<'ctx>,
    assigned_value: OnceCell<IrValueRef<'ctx>>,
}

pub type IrWireRef<'ctx> = &'ctx IrWire<'ctx>;

impl fmt::Debug for IrWire<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrWire")
            .field("id", &self.id())
            .field("value_type", &self.value_type())
            .field(
                "assigned_value",
                debug_format_option_as_value_or_none(self.assigned_value.get()),
            )
            .finish_non_exhaustive()
    }
}

struct WireIdDebug<ModuleId> {
    module_id: ModuleId,
    wire_id: usize,
}

impl<ModuleId: fmt::Debug> fmt::Debug for WireIdDebug<ModuleId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}.{}", self.module_id, self.wire_id)
    }
}

impl<'ctx> IrWire<'ctx> {
    pub fn module(&self) -> IrModuleRef<'ctx> {
        self.module
    }
    pub fn id(&self) -> impl fmt::Debug + 'static {
        WireIdDebug {
            module_id: self.module.id(),
            wire_id: self.id,
        }
    }
    pub fn value_type(&self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn new(module: IrModuleRef<'ctx>, value_type: IrValueTypeRef<'ctx>) -> IrWireRef<'ctx> {
        let id = module.wires.borrow().len();
        let retval = module.ctx().wires_arena.alloc(Self {
            module,
            id,
            value_type,
            assigned_value: OnceCell::new(),
        });
        module.wires.borrow_mut().push(retval);
        retval
    }
    pub fn read(&'ctx self) -> IrValueRef<'ctx> {
        IrValue::WireRead(IrWireRead(self)).intern(self.module().ctx())
    }
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
    pub fn debug_fmt_without_id<'a: 'ctx>(&'a self) -> impl fmt::Debug + 'a {
        struct FmtWithoutId<'a, 'ctx>(&'a IrWire<'ctx>);
        impl fmt::Debug for FmtWithoutId<'_, '_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct("IrWire")
                    .field("value_type", &self.0.value_type())
                    .field(
                        "assigned_value",
                        debug_format_option_as_value_or_none(self.0.assigned_value.get()),
                    )
                    .finish_non_exhaustive()
            }
        }
        FmtWithoutId(self)
    }
}

#[derive(Clone, Copy)]
pub struct IrWireRead<'ctx>(pub IrWireRef<'ctx>);

impl fmt::Debug for IrWireRead<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrWireRead")
            .field("id", &self.0.id())
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
