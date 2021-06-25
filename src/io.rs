// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{Context, ContextRef},
    ir::io::{InOrOut, IrIOCallback, IrIOMutRef, IrInput, IrOutput},
    module::{AsIrModule, Module},
    values::{integer::IntShapeTrait, Int, SInt, UInt, Val, Value, ValueType},
};
use alloc::{boxed::Box, string::String, vec::Vec};
use core::{
    convert::{Infallible, TryInto},
    fmt::{self, Write},
    marker::PhantomData,
    ops::{Deref, DerefMut},
};
use num_bigint::{BigInt, BigUint};

#[must_use]
pub struct IOVisitor<'visitor, 'ctx> {
    callback: &'visitor mut (dyn IrIOCallback<'ctx> + 'visitor),
    status: &'visitor mut Result<(), ()>,
    path: &'visitor mut String,
}

impl<'visitor, 'ctx> IOVisitor<'visitor, 'ctx> {
    pub fn is_early_exit(&self) -> bool {
        self.status.is_err()
    }
    fn nest<T: ?Sized + IO<'ctx>>(&mut self, path_segment: impl fmt::Display, value: &mut T) {
        if self.is_early_exit() {
            return;
        }
        let Self {
            callback,
            status,
            path,
        } = self;
        let original_path_len = path.len();
        write!(path, "{}", path_segment).unwrap();
        value.visit_io(IOVisitor {
            callback: &mut **callback,
            status,
            path: &mut **path,
        });
        path.truncate(original_path_len);
    }
    pub fn visit<T: ?Sized + IO<'ctx>, P: Into<String>>(
        io: &mut T,
        callback: &'visitor mut (dyn IrIOCallback<'ctx> + 'visitor),
        path_prefix: P,
    ) -> Result<(), ()> {
        let mut path: String = path_prefix.into();
        let mut status = Ok(());
        io.visit_io(IOVisitor {
            callback,
            path: &mut path,
            status: &mut status,
        });
        status
    }
}

#[must_use]
pub struct IOVisitorList<'visitor, 'ctx> {
    visitor: IOVisitor<'visitor, 'ctx>,
    index: usize,
}

impl<'ctx> IOVisitorList<'_, 'ctx> {
    pub fn entry<T: ?Sized + IO<'ctx>>(mut self, entry: &mut T) -> Self {
        self.visitor.nest(format_args!("[{}]", self.index), entry);
        self.index += 1;
        self
    }
    pub fn entries<'a, T: ?Sized + IO<'ctx> + 'a, I: IntoIterator<Item = &'a mut T>>(
        mut self,
        entries: I,
    ) -> Self {
        if self.visitor.is_early_exit() {
            return self;
        }
        for entry in entries {
            self = self.entry(entry);
            if self.visitor.is_early_exit() {
                break;
            }
        }
        self
    }
    pub fn finish(self) {
        let _ = self;
    }
}

#[must_use]
pub struct IOVisitorStruct<'visitor, 'ctx> {
    visitor: IOVisitor<'visitor, 'ctx>,
}

impl<'ctx> IOVisitorStruct<'_, 'ctx> {
    pub fn field<'a, T: ?Sized + IO<'ctx> + 'a>(mut self, name: &str, value: &'a mut T) -> Self {
        self.visitor.nest(format_args!(".{}", name), value);
        self
    }
    pub fn finish(self) {
        let _ = self;
    }
}

impl<'visitor, 'ctx> IOVisitor<'visitor, 'ctx> {
    pub fn visit_list(self) -> IOVisitorList<'visitor, 'ctx> {
        IOVisitorList {
            visitor: self,
            index: 0,
        }
    }
    pub fn visit_struct(self) -> IOVisitorStruct<'visitor, 'ctx> {
        IOVisitorStruct { visitor: self }
    }
}

pub trait IO<'ctx> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>);
}

pub trait PlainIO<'ctx>: IO<'ctx> + Sized {
    fn external(ctx: ContextRef<'ctx>) -> Self;
}

impl<'ctx> Context<'ctx> {
    pub fn external<T: PlainIO<'ctx>>(&'ctx self) -> T {
        T::external(self)
    }
}

impl<'ctx, T: IO<'ctx> + ?Sized> IO<'ctx> for Box<T> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        T::visit_io(self, visitor)
    }
}

impl<'ctx, T: PlainIO<'ctx>> PlainIO<'ctx> for Box<T> {
    fn external(ctx: ContextRef<'ctx>) -> Self {
        Box::new(ctx.external())
    }
}

impl<'ctx, T: IO<'ctx> + ?Sized> IO<'ctx> for &'_ mut T {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        T::visit_io(self, visitor)
    }
}

impl<'ctx, T: IO<'ctx>> IO<'ctx> for Option<T> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        if let Some(v) = self {
            v.visit_io(visitor);
        }
    }
}

impl<'ctx, T: IO<'ctx>> IO<'ctx> for Vec<T> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        visitor.visit_list().entries(self).finish()
    }
}

impl<'ctx, T: IO<'ctx>> IO<'ctx> for [T] {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        visitor.visit_list().entries(self).finish()
    }
}

impl<'ctx, T: IO<'ctx>, const N: usize> IO<'ctx> for [T; N] {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        visitor.visit_list().entries(self).finish()
    }
}

impl<'ctx, T: PlainIO<'ctx>, const N: usize> PlainIO<'ctx> for [T; N] {
    fn external(ctx: ContextRef<'ctx>) -> Self {
        let mut elements = Vec::with_capacity(N);
        for _ in 0..N {
            elements.push(ctx.external());
        }
        elements.try_into().ok().unwrap()
    }
}

impl<'ctx> IO<'ctx> for IrIOMutRef<'_, 'ctx> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        match self {
            InOrOut::Input(v) => v.visit_io(visitor),
            InOrOut::Output(v) => v.visit_io(visitor),
        }
    }
}

macro_rules! no_op_impl_io {
    ([$ctx:lifetime $($generics:tt)*] $ty:ty) => {
        impl<$ctx $($generics)*> IO<$ctx> for $ty {
            fn visit_io(&mut self, visitor: IOVisitor<'_, $ctx>) {
                visitor.visit_struct().finish()
            }
        }
    };
    ($ty:ty) => {
        no_op_impl_io!(['ctx] $ty);
    }
}

no_op_impl_io!(bool);
no_op_impl_io!(char);
no_op_impl_io!(f32);
no_op_impl_io!(f64);
no_op_impl_io!(i128);
no_op_impl_io!(i16);
no_op_impl_io!(i32);
no_op_impl_io!(i64);
no_op_impl_io!(i8);
no_op_impl_io!(isize);
no_op_impl_io!(str);
no_op_impl_io!(u128);
no_op_impl_io!(u16);
no_op_impl_io!(u32);
no_op_impl_io!(u64);
no_op_impl_io!(u8);
no_op_impl_io!(usize);
no_op_impl_io!(Infallible);
no_op_impl_io!(String);
no_op_impl_io!(BigInt);
no_op_impl_io!(BigUint);
no_op_impl_io!(['ctx, Shape: IntShapeTrait] Int<Shape>);
no_op_impl_io!(ContextRef<'_>);
no_op_impl_io!(Module<'_>);
no_op_impl_io!(&'_ str);

impl<'ctx> PlainIO<'ctx> for ContextRef<'ctx> {
    fn external(ctx: ContextRef<'ctx>) -> Self {
        ctx
    }
}

impl<'ctx> PlainIO<'ctx> for SInt<0> {
    fn external(_ctx: ContextRef<'ctx>) -> Self {
        SInt::default()
    }
}

impl<'ctx> PlainIO<'ctx> for UInt<0> {
    fn external(_ctx: ContextRef<'ctx>) -> Self {
        UInt::default()
    }
}

impl<'ctx> IO<'ctx> for IrInput<'ctx> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        if !visitor.is_early_exit() {
            *visitor.status = visitor
                .callback
                .callback(InOrOut::Input(self), &visitor.path);
        }
    }
}

impl<'ctx> IO<'ctx> for IrOutput<'ctx> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        if !visitor.is_early_exit() {
            *visitor.status = visitor
                .callback
                .callback(InOrOut::Output(self), &visitor.path);
        }
    }
}

macro_rules! impl_io_trait_for_tuples {
    ($($index:tt: $T:ident,)*) => {
        impl<'ctx, $($T: IO<'ctx>),*> IO<'ctx> for ($($T,)*) {
            fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
                visitor
                    .visit_struct()
                    $(.field(stringify!($index), &mut self.$index))*
                    .finish()
            }
        }
        impl<'ctx, $($T: PlainIO<'ctx>),*> PlainIO<'ctx> for ($($T,)*) {
            #[allow(unused_variables)]
            fn external(ctx: ContextRef<'ctx>) -> Self {
                ($($T::external(ctx),)*)
            }
        }
    };
}

macro_rules! impl_io_trait_for_tuples_reversed {
    ([], [$($index:tt: $T:ident,)*]) => {
        impl_io_trait_for_tuples!($($index: $T,)*);
    };
    ([$next_index:tt: $NextT:ident, $($rev_index:tt: $RevT:ident,)*], [$($index:tt: $T:ident,)*]) => {
        impl_io_trait_for_tuples_reversed!([$($rev_index: $RevT,)*], [$next_index: $NextT, $($index: $T,)*]);
    };
    ($next_index:tt: $NextT:ident, $($rev_index:tt: $RevT:ident,)*) => {
        impl_io_trait_for_tuples_reversed!($($rev_index: $RevT,)*);
        impl_io_trait_for_tuples_reversed!([$($rev_index: $RevT,)*], [$next_index: $NextT,]);
    };
    () => {
        impl_io_trait_for_tuples!();
    };
}

impl_io_trait_for_tuples_reversed!(
    12: T12,
    11: T11,
    10: T10,
    9: T9,
    8: T8,
    7: T7,
    6: T6,
    5: T5,
    4: T4,
    3: T3,
    2: T2,
    1: T1,
    0: T0,
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

impl<'ctx, T> IO<'ctx> for NotIO<T> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        visitor.visit_struct().finish()
    }
}

#[derive(Debug)]
pub struct Input<'ctx, T: Value<'ctx>> {
    ir: IrInput<'ctx>,
    value_type: ValueType<'ctx, T>,
}

impl<'ctx, T: Value<'ctx>> IO<'ctx> for Input<'ctx, T> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        self.ir.visit_io(visitor)
    }
}

impl<'ctx, T: Value<'ctx> + Default> PlainIO<'ctx> for Input<'ctx, T> {
    fn external(ctx: ContextRef<'ctx>) -> Self {
        ctx.external_input()
    }
}

impl<'ctx, T: Value<'ctx>> From<Val<'ctx, T>> for Input<'ctx, T> {
    fn from(value: Val<'ctx, T>) -> Self {
        Self {
            ir: IrInput::from(value.ir()),
            value_type: value.value_type(),
        }
    }
}

impl<'ctx> Context<'ctx> {
    pub fn external_input_with_type<T: Value<'ctx>>(
        &'ctx self,
        value_type: ValueType<'ctx, T>,
    ) -> Input<'ctx, T> {
        Input::external_with_type(self, value_type)
    }
    pub fn external_input<T: Value<'ctx> + Default>(&'ctx self) -> Input<'ctx, T> {
        Input::external(self)
    }
    pub fn external_output_with_type<T: Value<'ctx>>(
        &'ctx self,
        value_type: ValueType<'ctx, T>,
    ) -> Output<'ctx, T> {
        Output::external_with_type(self, value_type)
    }
    pub fn external_output<T: Value<'ctx> + Default>(&'ctx self) -> Output<'ctx, T> {
        Output::external(self)
    }
}

impl<'ctx, T: Value<'ctx>> Input<'ctx, T> {
    pub fn external_with_type(ctx: ContextRef<'ctx>, value_type: ValueType<'ctx, T>) -> Self {
        Self {
            ir: IrInput::external(ctx, value_type.ir()),
            value_type,
        }
    }
    pub fn external(ctx: ContextRef<'ctx>) -> Self
    where
        T: Default,
    {
        Self::external_with_type(ctx, T::default_value_type(ctx))
    }
    pub fn get(&self) -> Val<'ctx, T> {
        Val::from_ir_and_type_unchecked(self.ir.get(), self.value_type)
    }
    pub fn ir(&self) -> &IrInput<'ctx> {
        &self.ir
    }
}

pub struct Output<'ctx, T: Value<'ctx>> {
    ir: IrOutput<'ctx>,
    _phantom: PhantomData<fn(T) -> T>,
}

impl<'ctx, T: Value<'ctx>> fmt::Debug for Output<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Output")
            .field("ir", &self.ir)
            .finish_non_exhaustive()
    }
}

impl<'ctx, T: Value<'ctx>> Output<'ctx, T> {
    pub fn with_type<M: AsIrModule<'ctx>>(module: M, value_type: ValueType<'ctx, T>) -> Self {
        Output {
            ir: IrOutput::new(module.as_ir_module(), value_type.ir()),
            _phantom: PhantomData,
        }
    }
    pub fn new<M: AsIrModule<'ctx>>(module: M) -> Self
    where
        T: Default,
    {
        let module = module.as_ir_module();
        Self::with_type(module, Value::default_value_type(module.ctx()))
    }
    pub fn external_with_type(ctx: ContextRef<'ctx>, value_type: ValueType<'ctx, T>) -> Self {
        Output {
            ir: IrOutput::external(ctx, value_type.ir()),
            _phantom: PhantomData,
        }
    }
    pub fn external(ctx: ContextRef<'ctx>) -> Self
    where
        T: Default,
    {
        Self::external_with_type(ctx, Value::default_value_type(ctx))
    }
    #[track_caller]
    pub fn assign(self, assigned_value: Val<'ctx, T>) {
        self.ir.assign(assigned_value.ir())
    }
    pub fn ir(&self) -> &IrOutput<'ctx> {
        &self.ir
    }
    pub fn read(&self) -> Val<'ctx, T> {
        Val::from_ir_and_type_unchecked(
            self.ir.read(),
            ValueType::from_ir_unchecked(self.ir.ctx(), self.ir.value_type()),
        )
    }
}

impl<'ctx, T: Value<'ctx>> IO<'ctx> for Output<'ctx, T> {
    fn visit_io(&mut self, visitor: IOVisitor<'_, 'ctx>) {
        self.ir.visit_io(visitor)
    }
}

impl<'ctx, T: Value<'ctx> + Default> PlainIO<'ctx> for Output<'ctx, T> {
    fn external(ctx: ContextRef<'ctx>) -> Self {
        ctx.external_output()
    }
}
