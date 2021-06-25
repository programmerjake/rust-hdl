// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::Intern,
    ir::{
        logic::{IrWire, IrWireRead, IrWireRef},
        values::{IrValue, IrValueRef},
    },
    module::AsIrModule,
    values::{Val, Value, ValueType},
};
use alloc::borrow::Cow;
use core::{fmt, marker::PhantomData};

#[must_use]
pub struct Wire<'ctx, T: Value<'ctx>> {
    read_value: IrValueRef<'ctx>,
    ir: IrWireRef<'ctx>,
    _phantom: PhantomData<fn(T) -> T>,
}

impl<'ctx, T: Value<'ctx>> fmt::Debug for Wire<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Wire")
            .field("ir", self.ir)
            .finish_non_exhaustive()
    }
}

impl<'ctx, T: Value<'ctx>> Wire<'ctx, T> {
    pub fn with_type<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        value_type: ValueType<'ctx, T>,
    ) -> Self {
        let module = module.as_ir_module();
        let ir = IrWire::new(module, name.into(), value_type.ir());
        let read_value = IrValue::from(IrWireRead(ir)).intern(module.ctx());
        Self {
            ir,
            read_value,
            _phantom: PhantomData,
        }
    }
    pub fn new<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(module: M, name: N) -> Self
    where
        T: Default,
    {
        let module = module.as_ir_module();
        Self::with_type(module, name.into(), Value::default_value_type(module.ctx()))
    }
    pub fn assign(self, assigned_value: Val<'ctx, T>) {
        self.ir.assign(assigned_value.ir());
    }
    pub fn ir(&self) -> IrWireRef<'ctx> {
        self.ir
    }
    pub fn read(&self) -> Val<'ctx, T> {
        Val::from_ir_and_type_unchecked(
            self.read_value,
            ValueType::from_ir_unchecked(self.ir.module().ctx(), self.ir.value_type()),
        )
    }
}
