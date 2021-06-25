// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    clocking::ClockDomain,
    field,
    ir::{
        logic::{IrReg, IrRegRef, IrRegReset, IrWire, IrWireRef},
        values::IrValueRef,
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
        let read_value = ir.read();
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

#[must_use]
pub struct Reg<'ctx, T: Value<'ctx>> {
    output_value: IrValueRef<'ctx>,
    ir: IrRegRef<'ctx>,
    _phantom: PhantomData<fn(T) -> T>,
}

impl<'ctx, T: Value<'ctx>> fmt::Debug for Reg<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Reg")
            .field("ir", self.ir)
            .finish_non_exhaustive()
    }
}

impl<'ctx, T: Value<'ctx>> Reg<'ctx, T> {
    pub fn from_ir_unchecked(ir: IrRegRef<'ctx>) -> Self {
        let output_value = ir.output();
        Self {
            ir,
            output_value,
            _phantom: PhantomData,
        }
    }
    pub fn with_type_without_reset<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        clk: Val<'ctx, bool>,
        value_type: ValueType<'ctx, T>,
    ) -> Self {
        let module = module.as_ir_module();
        Self::from_ir_unchecked(IrReg::new(
            module,
            name.into(),
            value_type.ir(),
            clk.ir(),
            None,
        ))
    }
    pub fn without_reset<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        clk: Val<'ctx, bool>,
    ) -> Self
    where
        T: Default,
    {
        let module = module.as_ir_module();
        Self::with_type_without_reset(
            module,
            name.into(),
            clk,
            Value::default_value_type(module.ctx()),
        )
    }
    pub fn with_reset<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        clk: Val<'ctx, bool>,
        reset_enable: Val<'ctx, bool>,
        reset_value: T,
    ) -> Self {
        let module = module.as_ir_module();
        let reset_value = reset_value.get_value(module.ctx());
        Self::from_ir_unchecked(IrReg::new(
            module,
            name.into(),
            reset_value.value_type().ir(),
            clk.ir(),
            Some(IrRegReset {
                reset_enable: reset_enable.ir(),
                reset_value: reset_value.ir(),
            }),
        ))
    }
    pub fn new<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        clock_domain: Val<'ctx, ClockDomain>,
        reset_value: T,
    ) -> Self {
        Self::with_reset(
            module,
            name,
            field!((clock_domain).clk),
            field!((clock_domain).rst),
            reset_value,
        )
    }
    pub fn assign_data_in(self, assigned_value: Val<'ctx, T>) {
        self.ir.assign_data_in(assigned_value.ir());
    }
    pub fn ir(&self) -> IrRegRef<'ctx> {
        self.ir
    }
    pub fn output(&self) -> Val<'ctx, T> {
        Val::from_ir_and_type_unchecked(
            self.output_value,
            ValueType::from_ir_unchecked(self.ir.module().ctx(), self.ir.value_type()),
        )
    }
}
