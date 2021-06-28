// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::ir::module::IrModuleRef;
use alloc::boxed::Box;
use core::fmt;

pub trait Exporter<'ctx> {
    type Error: fmt::Display + fmt::Debug + Send + 'static;
    fn export_ir(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error>;
}

impl<'ctx, T: ?Sized + Exporter<'ctx>> Exporter<'ctx> for &'_ mut T {
    type Error = T::Error;

    fn export_ir(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error> {
        (**self).export_ir(module)
    }
}

impl<'ctx, T: ?Sized + Exporter<'ctx>> Exporter<'ctx> for Box<T> {
    type Error = T::Error;

    fn export_ir(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error> {
        (**self).export_ir(module)
    }
}

pub mod rtlil;
