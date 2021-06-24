// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::ContextRef,
    io::{Output, IO},
    ir::module::{IrModule, IrModuleRef},
    logic::Wire,
    values::{Value, ValueType},
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

impl<'ctx> Module<'ctx> {
    pub fn top<'a, N: Into<Cow<'a, str>>>(ctx: ContextRef<'ctx>, name: N) -> Self {
        Self {
            ir: IrModule::new_top_module(ctx, name.into()),
        }
    }
    pub fn submodule<'a, N: Into<Cow<'a, str>>, T: IO<'ctx>>(
        &self,
        name: N,
        mut io: T,
    ) -> (Module<'ctx>, T) {
        (
            Module {
                ir: IrModule::new_submodule(self.ir, name.into(), &mut io),
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
    pub fn wire<'a, N: Into<Cow<'a, str>>, T: Value<'ctx> + Default>(
        &self,
        name: N,
    ) -> Wire<'ctx, T> {
        Wire::new(self, name)
    }
    pub fn wire_with_type<'a, N: Into<Cow<'a, str>>, T: Value<'ctx>>(
        &self,
        name: N,
        value_type: ValueType<'ctx, T>,
    ) -> Wire<'ctx, T> {
        Wire::with_type(self, name, value_type)
    }
    pub fn output<'a, T: Value<'ctx> + Default>(&self) -> Output<'ctx, T> {
        Output::new(self)
    }
    pub fn output_with_type<'a, T: Value<'ctx>>(
        &self,
        value_type: ValueType<'ctx, T>,
    ) -> Output<'ctx, T> {
        Output::with_type(self, value_type)
    }
}
