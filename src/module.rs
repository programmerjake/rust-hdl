// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    clocking::ClockDomain,
    context::{Context, ContextRef},
    export::Exporter,
    io::{Output, PlainIO, IO},
    ir::{
        module::{IrModule, IrModuleRef},
        SourceLocation,
    },
    logic::{Reg, Wire},
    values::{FixedTypeValue, Val, Value, ValueType},
};
use alloc::borrow::Cow;
use core::fmt;

pub trait AsIrModule<'ctx> {
    fn as_ir_module(&self) -> IrModuleRef<'ctx>;
}

impl<'ctx> AsIrModule<'ctx> for IrModuleRef<'ctx> {
    fn as_ir_module(&self) -> IrModuleRef<'ctx> {
        *self
    }
}

impl<'ctx, T: ?Sized + AsIrModule<'ctx>> AsIrModule<'ctx> for &'_ T {
    fn as_ir_module(&self) -> IrModuleRef<'ctx> {
        T::as_ir_module(*self)
    }
}

impl<'ctx> AsIrModule<'ctx> for Module<'ctx> {
    fn as_ir_module(&self) -> IrModuleRef<'ctx> {
        self.ir
    }
}

pub struct Module<'ctx> {
    ir: IrModuleRef<'ctx>,
}

impl<'ctx> fmt::Debug for Module<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ir.fmt(f)
    }
}

impl<'ctx> Context<'ctx> {
    #[track_caller]
    pub fn top_module<'a, N: Into<Cow<'a, str>>, T: PlainIO<'ctx>>(
        &'ctx self,
        name: N,
    ) -> (Module<'ctx>, T) {
        self.top_module_with_io(name.into(), self.external())
    }
    #[track_caller]
    pub fn top_module_with_io<'a, N: Into<Cow<'a, str>>, T: IO<'ctx>>(
        &'ctx self,
        name: N,
        mut io: T,
    ) -> (Module<'ctx>, T) {
        (
            Module {
                ir: IrModule::new_top_module(self, SourceLocation::caller(), name.into(), &mut io),
            },
            io,
        )
    }
}

impl<'ctx> Module<'ctx> {
    #[track_caller]
    pub fn submodule<'a, N: Into<Cow<'a, str>>, T: IO<'ctx>>(
        &self,
        name: N,
        mut io: T,
    ) -> (Module<'ctx>, T) {
        (
            Module {
                ir: IrModule::new_submodule(
                    self.ir,
                    SourceLocation::caller(),
                    name.into(),
                    &mut io,
                ),
            },
            io,
        )
    }
    pub fn ir(&self) -> IrModuleRef<'ctx> {
        self.ir
    }
    pub fn ctx(&self) -> ContextRef<'ctx> {
        self.ir.ctx()
    }
    #[track_caller]
    pub fn wire<'a, N: Into<Cow<'a, str>>, T: FixedTypeValue<'ctx>>(
        &self,
        name: N,
    ) -> Wire<'ctx, T> {
        Wire::new(self, name)
    }
    #[track_caller]
    pub fn wire_with_type<'a, N: Into<Cow<'a, str>>, T: Value<'ctx>>(
        &self,
        name: N,
        value_type: ValueType<'ctx, T>,
    ) -> Wire<'ctx, T> {
        Wire::with_type(self, name, value_type)
    }
    pub fn output<'a, T: FixedTypeValue<'ctx>>(&self) -> Output<'ctx, T> {
        Output::new(self)
    }
    pub fn output_with_type<'a, T: Value<'ctx>>(
        &self,
        value_type: ValueType<'ctx, T>,
    ) -> Output<'ctx, T> {
        Output::with_type(self, value_type)
    }
    #[track_caller]
    pub fn reg<'a, N: Into<Cow<'a, str>>, T: Value<'ctx>>(
        &self,
        name: N,
        clock_domain: Val<'ctx, 'ctx, ClockDomain>,
        reset_value: T,
    ) -> Reg<'ctx, T> {
        Reg::new(self, name, clock_domain, reset_value)
    }
    #[track_caller]
    pub fn reg_without_reset<'a, N: Into<Cow<'a, str>>, T: FixedTypeValue<'ctx>>(
        &self,
        name: N,
        clk: Val<'ctx, 'ctx, bool>,
    ) -> Reg<'ctx, T> {
        Reg::without_reset(self, name, clk)
    }
    #[track_caller]
    pub fn reg_with_type_without_reset<'a, N: Into<Cow<'a, str>>, T: Value<'ctx>>(
        &self,
        name: N,
        clk: Val<'ctx, 'ctx, bool>,
        value_type: ValueType<'ctx, T>,
    ) -> Reg<'ctx, T> {
        Reg::with_type_without_reset(self, name, clk, value_type)
    }
    #[track_caller]
    pub fn reg_with_reset<'a, N: Into<Cow<'a, str>>, T: Value<'ctx>>(
        &self,
        name: N,
        clk: Val<'ctx, 'ctx, bool>,
        reset_enable: Val<'ctx, 'ctx, bool>,
        reset_value: T,
    ) -> Reg<'ctx, T> {
        Reg::with_reset(self, name, clk, reset_enable, reset_value)
    }
    pub fn export<E: Exporter<'ctx>>(&self, mut exporter: E) -> Result<E, E::Error> {
        exporter.export_ir(self.ir())?;
        Ok(exporter)
    }
}
