// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::IrModuleRef,
    ir::io::{IrIOTraitCallback, IrInput, IrOutput, IO},
    values::{Value, ValueTrait, ValueType},
};
use alloc::{boxed::Box, vec::Vec};
use core::{
    fmt,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

pub trait IOTrait<'ctx> {
    fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()>;
}

impl<'ctx, T: IOTrait<'ctx> + ?Sized> IOTrait<'ctx> for Box<T> {
    fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
        T::visit_ir_ports(self, callback)
    }
}

impl<'ctx, T: IOTrait<'ctx>> IOTrait<'ctx> for Option<T> {
    fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
        if let Some(v) = self {
            v.visit_ir_ports(callback)?;
        }
        Ok(())
    }
}

impl<'ctx, T: IOTrait<'ctx>> IOTrait<'ctx> for Vec<T> {
    fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
        self.iter_mut().try_for_each(|v| v.visit_ir_ports(callback))
    }
}

impl<'ctx, T: IOTrait<'ctx>> IOTrait<'ctx> for [T] {
    fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
        self.iter_mut().try_for_each(|v| v.visit_ir_ports(callback))
    }
}

impl<'ctx, T: IOTrait<'ctx>, const N: usize> IOTrait<'ctx> for [T; N] {
    fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
        self.iter_mut().try_for_each(|v| v.visit_ir_ports(callback))
    }
}

macro_rules! impl_io_trait_for_tuples {
    () => {
        impl<'ctx> IOTrait<'ctx> for () {
            fn visit_ir_ports<'a>(
                &'a mut self,
                _callback: IrIOTraitCallback<'_, 'ctx>,
            ) -> Result<(), ()> {
                Ok(())
            }
        }
    };
    ($first_v:ident: $FirstT:ident, $($v:ident: $T:ident,)*) => {
        impl_io_trait_for_tuples!($($v: $T,)*);

        impl<'ctx, $FirstT: IOTrait<'ctx>, $($T: IOTrait<'ctx>),*> IOTrait<'ctx> for ($FirstT, $($T,)*) {
            fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
                let ($first_v, $($v),*) = self;
                $first_v.visit_ir_ports(callback)?;
                $($v.visit_ir_ports(callback)?;)*
                Ok(())
            }
        }
    };
}

impl_io_trait_for_tuples!(
    v1: T1,
    v2: T2,
    v3: T3,
    v4: T4,
    v5: T5,
    v6: T6,
    v7: T7,
    v8: T8,
    v9: T9,
    v10: T10,
    v11: T11,
    v12: T12,
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NotIO<T>(pub T);

impl<T> From<T> for NotIO<T> {
    fn from(v: T) -> Self {
        NotIO(v)
    }
}

impl<T> Deref for NotIO<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for NotIO<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'ctx, T> IOTrait<'ctx> for NotIO<T> {
    fn visit_ir_ports<'a>(&'a mut self, _callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
        Ok(())
    }
}

#[derive(Debug)]
pub struct Input<'ctx, T: ValueTrait<'ctx>> {
    ir_input: IrInput<'ctx>,
    value_type: ValueType<'ctx, T>,
}

impl<'ctx, T: ValueTrait<'ctx>> IOTrait<'ctx> for Input<'ctx, T> {
    fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
        callback(IO::Input(&mut self.ir_input))
    }
}

impl<'ctx, T: ValueTrait<'ctx>> From<Value<'ctx, T>> for Input<'ctx, T> {
    fn from(value: Value<'ctx, T>) -> Self {
        Self {
            ir_input: IrInput::from(value.ir()),
            value_type: value.value_type(),
        }
    }
}

impl<'ctx, T: ValueTrait<'ctx>> Input<'ctx, T> {
    pub fn get(&self) -> Value<'ctx, T> {
        Value::from_ir_and_type_unchecked(self.ir_input.get(), self.value_type)
    }
}

pub struct Output<'ctx, T: ValueTrait<'ctx>> {
    ir: IrOutput<'ctx>,
    _phantom: PhantomData<fn(T) -> T>,
}

impl<'ctx, T: ValueTrait<'ctx>> fmt::Debug for Output<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Output")
            .field("ir", &self.ir)
            .finish_non_exhaustive()
    }
}

impl<'ctx, T: ValueTrait<'ctx>> Output<'ctx, T> {
    pub fn with_type(module: IrModuleRef<'ctx>, value_type: ValueType<'ctx, T>) -> Self {
        Output {
            ir: IrOutput::new(module, value_type.ir()),
            _phantom: PhantomData,
        }
    }
    pub fn new(module: IrModuleRef<'ctx>) -> Self
    where
        T: Default,
    {
        Self::with_type(module, ValueTrait::default_value_type(module.ctx()))
    }
    #[track_caller]
    pub fn assign(self, assigned_value: Value<'ctx, T>) {
        self.ir.assign(assigned_value.ir())
    }
    pub fn ir(&self) -> &IrOutput<'ctx> {
        &self.ir
    }
    pub fn read(&self) -> Value<'ctx, T> {
        Value::from_ir_and_type_unchecked(
            self.ir.read(),
            ValueType::from_ir_unchecked(self.ir.value_type()),
        )
    }
}

impl<'ctx, T: ValueTrait<'ctx>> IOTrait<'ctx> for Output<'ctx, T> {
    fn visit_ir_ports(&mut self, callback: IrIOTraitCallback<'_, 'ctx>) -> Result<(), ()> {
        callback(IO::Output(&mut self.ir))
    }
}
