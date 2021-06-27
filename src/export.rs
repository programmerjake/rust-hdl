// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::ir::module::IrModuleRef;
use alloc::boxed::Box;
use core::fmt;

pub trait Exporter {
    type Error: fmt::Display + fmt::Debug + Send + 'static;
    fn export_ir<'ctx>(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error>;
}

impl<T: ?Sized + Exporter> Exporter for &'_ mut T {
    type Error = T::Error;

    fn export_ir<'ctx>(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error> {
        (**self).export_ir(module)
    }
}

impl<T: ?Sized + Exporter> Exporter for Box<T> {
    type Error = T::Error;

    fn export_ir<'ctx>(&mut self, module: IrModuleRef<'ctx>) -> Result<(), Self::Error> {
        (**self).export_ir(module)
    }
}

pub mod rtlil;
