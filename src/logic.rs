// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::context::Context;
use once_cell::unsync::OnceCell;

mod sealed {
    use super::*;

    pub trait ValueImplSealed {}

    impl<T: ValueType> ValueImplSealed for WireValue<'_, T> {}
}

pub trait ValueImpl<T: ValueType>: sealed::ValueImplSealed {}

#[derive(Clone)]
pub struct Value<'ctx, T: ValueType>(&'ctx dyn ValueImpl<T>);

pub(crate) trait WireTrait<'ctx> {}

pub(crate) struct WireValue<'ctx, T: ValueType> {
    assigned_value: OnceCell<Value<'ctx, T>>,
}

impl<'ctx, T: ValueType> ValueImpl<T> for WireValue<'ctx, T> {}

pub struct Wire<'ctx, T: ValueType>(&'ctx WireValue<'ctx, T>);

impl<'ctx, T: ValueType> Wire<'ctx, T> {
    pub fn new(ctx: &'ctx Context) -> Self {}
}
