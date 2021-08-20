// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    clocking::ClockDomain,
    context::AsContext,
    ir::{
        logic::{IrReg, IrRegRef, IrRegReset, IrWire, IrWireRef},
        values::IrValueRef,
        SourceLocation,
    },
    module::AsIrModule,
    values::{val, FixedTypeValue, ToVal, Val, Value, ValueType},
};
use alloc::borrow::Cow;
use core::{fmt, marker::PhantomData};

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

impl<'ctx, T: Value<'ctx>> Copy for Wire<'ctx, T> {}

impl<'ctx, T: Value<'ctx>> Clone for Wire<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'ctx, T: Value<'ctx>> Wire<'ctx, T> {
    pub fn from_ir_unchecked(ir: IrWireRef<'ctx>) -> Self {
        Self {
            ir,
            read_value: ir.read(),
            _phantom: PhantomData,
        }
    }
    pub fn ir(self) -> IrWireRef<'ctx> {
        self.ir
    }
    pub fn read(self) -> Val<'ctx, T> {
        Val::from_ir_and_type_unchecked(
            self.read_value,
            ValueType::from_ir_unchecked(self.ir.ctx(), self.ir.value_type()),
        )
    }
    #[must_use]
    #[track_caller]
    pub fn with_type<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        value_type: ValueType<'ctx, T>,
    ) -> Self {
        Self::from_ir_unchecked(IrWire::new(
            module.as_ir_module(),
            SourceLocation::caller(),
            name.into(),
            value_type.ir(),
        ))
    }
    #[must_use]
    #[track_caller]
    pub fn new<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(module: M, name: N) -> Self
    where
        T: FixedTypeValue<'ctx>,
    {
        let module = module.as_ir_module();
        Self::with_type(module, name.into(), T::static_value_type(module.ctx()))
    }
    #[track_caller]
    pub fn assign(self, assigned_value: Val<'ctx, T>) {
        self.ir.assign(assigned_value.ir());
    }
}

impl<'ctx, T: Value<'ctx>> ToVal<'ctx> for Wire<'ctx, T> {
    type ValueType = T;

    fn to_val<Ctx: AsContext<'ctx>>(&self, _ctx: Ctx) -> Val<'ctx, Self::ValueType> {
        self.read()
    }
}

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

impl<'ctx, T: Value<'ctx>> Copy for Reg<'ctx, T> {}

impl<'ctx, T: Value<'ctx>> Clone for Reg<'ctx, T> {
    fn clone(&self) -> Self {
        *self
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
    pub fn ir(self) -> IrRegRef<'ctx> {
        self.ir
    }
    pub fn output(self) -> Val<'ctx, T> {
        Val::from_ir_and_type_unchecked(
            self.output_value,
            ValueType::from_ir_unchecked(self.ir.module().ctx(), self.ir.value_type()),
        )
    }
    #[must_use]
    #[track_caller]
    pub fn with_type_without_reset<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        clk: Val<'ctx, bool>,
        value_type: ValueType<'ctx, T>,
    ) -> Self {
        let module = module.as_ir_module();
        Self::from_ir_unchecked(IrReg::new(
            module,
            SourceLocation::caller(),
            name.into(),
            value_type.ir(),
            clk.ir(),
            None,
        ))
    }
    #[must_use]
    #[track_caller]
    pub fn without_reset<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        clk: Val<'ctx, bool>,
    ) -> Self
    where
        T: FixedTypeValue<'ctx>,
    {
        let module = module.as_ir_module();
        Self::with_type_without_reset(module, name.into(), clk, T::static_value_type(module.ctx()))
    }
    #[must_use]
    #[track_caller]
    pub fn with_reset<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        clk: Val<'ctx, bool>,
        reset_enable: Val<'ctx, bool>,
        reset_value: Val<'ctx, T>,
    ) -> Self {
        let module = module.as_ir_module();
        Self::from_ir_unchecked(IrReg::new(
            module,
            SourceLocation::caller(),
            name.into(),
            reset_value.value_type().ir(),
            clk.ir(),
            Some(IrRegReset {
                reset_enable: reset_enable.ir(),
                reset_value: reset_value.ir(),
            }),
        ))
    }
    #[must_use]
    #[track_caller]
    pub fn new<'a, M: AsIrModule<'ctx>, N: Into<Cow<'a, str>>>(
        module: M,
        name: N,
        clock_domain: Val<'ctx, ClockDomain>,
        reset_value: Val<'ctx, T>,
    ) -> Self {
        let module = module.as_ir_module();
        Self::with_reset(
            module,
            name,
            val!(#![rust_hdl(crate = crate)] module, clock_domain.clk),
            val!(#![rust_hdl(crate = crate)] module, clock_domain.rst),
            reset_value,
        )
    }
    #[track_caller]
    pub fn assign_data_in(self, assigned_value: impl ToVal<'ctx, ValueType = T>) {
        self.ir
            .assign_data_in(assigned_value.to_val(self.ir.ctx()).ir());
    }
}

impl<'ctx, T: Value<'ctx>> ToVal<'ctx> for Reg<'ctx, T> {
    type ValueType = T;

    fn to_val<Ctx: AsContext<'ctx>>(&self, _ctx: Ctx) -> Val<'ctx, Self::ValueType> {
        self.output()
    }
}
