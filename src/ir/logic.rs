// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{create_ir_wire_impl, ModuleRef},
    ir::{types::IrValueTypeRef, values::IrValueRef},
};
use core::{
    fmt,
    hash::{Hash, Hasher},
    ptr,
};
use once_cell::unsync::OnceCell;

pub struct IrWire<'ctx> {
    module: ModuleRef<'ctx>,
    pub(crate) id: usize,
    value_type: IrValueTypeRef<'ctx>,
    assigned_value: OnceCell<IrValueRef<'ctx>>,
}

impl fmt::Debug for IrWire<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrWire")
            .field("id", &self.id())
            .field("value_type", &self.value_type())
            .field("assigned_value", &self.assigned_value)
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
    pub fn module(&self) -> ModuleRef<'ctx> {
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
    pub fn new(module: ModuleRef<'ctx>, value_type: IrValueTypeRef<'ctx>) -> IrWireValue<'ctx> {
        create_ir_wire_impl(Self {
            module,
            id: 0,
            value_type,
            assigned_value: OnceCell::new(),
        })
    }
    pub fn assign(&self, value: IrValueRef<'ctx>) {
        let value_type = value.get_type(self.module.ctx());
        assert_eq!(self.value_type, value_type);
        if let Err(_) = self.assigned_value.set(value) {
            panic!("Wire already assigned");
        }
    }
}

#[derive(Clone, Copy)]
pub struct IrWireValue<'ctx>(pub &'ctx IrWire<'ctx>);

impl fmt::Debug for IrWireValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IrWireValue")
            .field("id", &self.0.id())
            .field("value_type", &self.0.value_type())
            .finish_non_exhaustive()
    }
}

impl<'ctx> Eq for IrWireValue<'ctx> {}

impl<'ctx> PartialEq for IrWireValue<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'ctx> Hash for IrWireValue<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const IrWire<'ctx>).hash(state)
    }
}
