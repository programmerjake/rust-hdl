// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::ModuleRef,
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
    assigned_value: OnceCell<IrValueRef<'ctx>>,
    value_type: IrValueTypeRef<'ctx>,
}

impl<'ctx> IrWire<'ctx> {
    pub fn module(&self) -> ModuleRef<'ctx> {
        self.module
    }
    pub fn value_type(&self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
}

impl fmt::Debug for IrWire<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct IrWireValue<'ctx>(pub &'ctx IrWire<'ctx>);

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
