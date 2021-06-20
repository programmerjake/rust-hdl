// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use core::ops::Deref;

use crate::{
    context::ModuleRef,
    value::{Value, ValueType, ValueTypeTrait},
};

pub struct Wire<'ctx, T: ValueTypeTrait<'ctx>> {
    value: Value<'ctx, T>,
}

impl<'ctx, T: ValueTypeTrait<'ctx>> Wire<'ctx, T> {
    pub fn new(module: ModuleRef<'ctx>, value_type: ValueType<'ctx, T>) -> Self {
        todo!()
    }
}

impl<'ctx, T: ValueTypeTrait<'ctx>> Deref for Wire<'ctx, T> {
    type Target = Value<'ctx, T>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
