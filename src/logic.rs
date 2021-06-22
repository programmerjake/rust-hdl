// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use core::ops::Deref;

use crate::{
    context::{Intern, ModuleRef},
    ir::{
        logic::{IrWire, IrWireValue},
        values::IrValue,
    },
    values::{Value, ValueTrait, ValueType},
};

#[must_use]
pub struct Wire<'ctx, T: ValueTrait<'ctx>> {
    value: Value<'ctx, T>,
    wire: IrWireValue<'ctx>,
}

impl<'ctx, T: ValueTrait<'ctx>> Wire<'ctx, T> {
    pub fn with_type(module: ModuleRef<'ctx>, value_type: ValueType<'ctx, T>) -> Self {
        let wire = IrWire::new(module, value_type.ir());
        let value =
            Value::from_ir_unchecked(module.ctx(), IrValue::from(wire).intern(module.ctx()));
        Self { wire, value }
    }
    pub fn new(module: ModuleRef<'ctx>) -> Self
    where
        T: Default,
    {
        Self::with_type(module, ValueTrait::default_value_type(module.ctx()))
    }
    pub fn assign(self, assigned_value: Value<'ctx, T>) {
        assert_eq!(self.value_type(), assigned_value.value_type());
        self.wire.0.assign(assigned_value.ir());
    }
    pub fn ir(&self) -> IrWireValue<'ctx> {
        self.wire
    }
}

impl<'ctx, T: ValueTrait<'ctx>> Deref for Wire<'ctx, T> {
    type Target = Value<'ctx, T>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
