// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use alloc::{boxed::Box, vec::Vec};

use crate::{
    context::Intern,
    ir::values::{
        BoolOutBinOp, BoolOutBinOpKind, BoolOutUnOp, BoolOutUnOpKind, IrValue, SameSizeBinOp,
        SameSizeBinOpKind, SameSizeUnOp, SameSizeUnOpKind, SliceArray, SliceBitVector,
    },
    prelude::*,
    values::integer::IntShapeTrait,
};
use core::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Bound, Mul,
    MulAssign, Neg, Not, Range, RangeBounds, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

impl<'ctx> From<Val<'ctx, UInt1>> for Val<'ctx, bool> {
    #[must_use]
    fn from(v: Val<'ctx, UInt1>) -> Self {
        Self::from_ir_unchecked(v.ctx(), v.ir())
    }
}

impl<'ctx> From<Val<'ctx, bool>> for Val<'ctx, UInt1> {
    #[must_use]
    fn from(v: Val<'ctx, bool>) -> Self {
        Self::from_ir_unchecked(v.ctx(), v.ir())
    }
}

#[track_caller]
fn same_size_bin_op_unchecked<'ctx, T: Value<'ctx>>(
    kind: SameSizeBinOpKind,
    lhs: Val<'ctx, T>,
    rhs: Val<'ctx, T>,
) -> Val<'ctx, T> {
    Val::from_ir_unchecked(
        lhs.ctx(),
        IrValue::from(SameSizeBinOp::new(lhs.ctx(), kind, lhs.ir(), rhs.ir())).intern(lhs.ctx()),
    )
}

#[track_caller]
fn same_size_un_op_unchecked<'ctx, T: Value<'ctx>>(
    kind: SameSizeUnOpKind,
    input: Val<'ctx, T>,
) -> Val<'ctx, T> {
    Val::from_ir_unchecked(
        input.ctx(),
        IrValue::from(SameSizeUnOp::new(input.ctx(), kind, input.ir())).intern(input.ctx()),
    )
}

#[track_caller]
fn bool_out_bin_op_unchecked<'ctx, T: Value<'ctx>>(
    kind: BoolOutBinOpKind,
    lhs: Val<'ctx, T>,
    rhs: Val<'ctx, T>,
) -> Val<'ctx, bool> {
    Val::from_ir_unchecked(
        lhs.ctx(),
        IrValue::from(BoolOutBinOp::new(lhs.ctx(), kind, lhs.ir(), rhs.ir())).intern(lhs.ctx()),
    )
}

#[track_caller]
fn bool_out_un_op_unchecked<'ctx, T: Value<'ctx>>(
    kind: BoolOutUnOpKind,
    input: Val<'ctx, T>,
) -> Val<'ctx, bool> {
    Val::from_ir_unchecked(
        input.ctx(),
        IrValue::from(BoolOutUnOp::new(input.ctx(), kind, input.ir())).intern(input.ctx()),
    )
}

macro_rules! impl_same_size_bin_op {
    ([$ctx:lifetime $($generics:tt)*] $Trait:ident for $T:ty {fn $trait_fn:ident}, $AssignTrait:ident {fn $assign_trait_fn:ident}, $kind:path) => {
        impl<$ctx $($generics)*> $Trait<Val<$ctx, $T>> for Val<$ctx, $T> {
            type Output = Val<$ctx, $T>;

            #[track_caller]
            #[must_use]
            fn $trait_fn(self, rhs: Val<$ctx, $T>) -> Self::Output {
                same_size_bin_op_unchecked($kind, self, rhs)
            }
        }
        impl<$ctx $($generics)*> $Trait<$T> for Val<$ctx, $T> {
            type Output = Val<$ctx, $T>;

            #[track_caller]
            #[must_use]
            fn $trait_fn(self, rhs: $T) -> Self::Output {
                same_size_bin_op_unchecked($kind, self, rhs.get_value(self.ctx()))
            }
        }
        impl<$ctx $($generics)*> $Trait<Val<$ctx, $T>> for $T {
            type Output = Val<$ctx, $T>;

            #[track_caller]
            #[must_use]
            fn $trait_fn(self, rhs: Val<$ctx, $T>) -> Self::Output {
                same_size_bin_op_unchecked($kind, self.get_value(rhs.ctx()), rhs)
            }
        }
        impl<$ctx $($generics)*> $AssignTrait<Val<$ctx, $T>> for Val<$ctx, $T> {
            #[track_caller]
            #[must_use]
            fn $assign_trait_fn(&mut self, rhs: Val<$ctx, $T>) {
                *self = same_size_bin_op_unchecked($kind, *self, rhs);
            }
        }
        impl<$ctx $($generics)*> $AssignTrait<$T> for Val<$ctx, $T> {
            #[track_caller]
            #[must_use]
            fn $assign_trait_fn(&mut self, rhs: $T) {
                *self = same_size_bin_op_unchecked($kind, *self, rhs.get_value(self.ctx()));
            }
        }
    };
}

macro_rules! impl_same_size_un_op {
    ([$ctx:lifetime $($generics:tt)*] $Trait:ident for $T:ty {fn $trait_fn:ident}, $kind:path) => {
        impl<$ctx $($generics)*> $Trait for Val<$ctx, $T> {
            type Output = Val<$ctx, $T>;

            #[track_caller]
            #[must_use]
            fn $trait_fn(self) -> Self::Output {
                same_size_un_op_unchecked($kind, self)
            }
        }
    };
}

pub trait CompareEq<'ctx, Rhs = Self>: Sized {
    #[track_caller]
    #[must_use]
    fn eq(self, rhs: Rhs) -> Val<'ctx, bool>;
    #[track_caller]
    #[must_use]
    fn ne(self, rhs: Rhs) -> Val<'ctx, bool> {
        !self.eq(rhs)
    }
}

pub trait CompareGtLE<'ctx, Rhs = Self>: Sized {
    #[track_caller]
    #[must_use]
    fn gt(self, rhs: Rhs) -> Val<'ctx, bool>;
    #[track_caller]
    #[must_use]
    fn le(self, rhs: Rhs) -> Val<'ctx, bool> {
        !self.gt(rhs)
    }
}

pub trait CompareLtGE<'ctx, Rhs = Self>: Sized {
    #[track_caller]
    #[must_use]
    fn lt(self, rhs: Rhs) -> Val<'ctx, bool>;
    #[track_caller]
    #[must_use]
    fn ge(self, rhs: Rhs) -> Val<'ctx, bool> {
        !self.lt(rhs)
    }
}

impl<'ctx, Lhs, Rhs: CompareLtGE<'ctx, Lhs>> CompareGtLE<'ctx, Rhs> for Lhs {
    #[track_caller]
    #[must_use]
    fn gt(self, rhs: Rhs) -> Val<'ctx, bool> {
        rhs.lt(self)
    }
    #[track_caller]
    #[must_use]
    fn le(self, rhs: Rhs) -> Val<'ctx, bool> {
        rhs.ge(self)
    }
}

pub trait Compare<'ctx>: CompareEq<'ctx> + CompareGtLE<'ctx> + CompareLtGE<'ctx> {}

impl<'ctx, T: CompareEq<'ctx> + CompareGtLE<'ctx> + CompareLtGE<'ctx>> Compare<'ctx> for T {}

pub trait ReduceBitwise<'ctx>: Sized {
    #[track_caller]
    #[must_use]
    fn reduce_and(self) -> Val<'ctx, bool>;
    #[track_caller]
    #[must_use]
    fn reduce_or(self) -> Val<'ctx, bool>;
    #[track_caller]
    #[must_use]
    fn reduce_xor(self) -> Val<'ctx, bool>;
    #[track_caller]
    #[must_use]
    fn reduce_nand(self) -> Val<'ctx, bool> {
        !self.reduce_and()
    }
    #[track_caller]
    #[must_use]
    fn reduce_nor(self) -> Val<'ctx, bool> {
        !self.reduce_or()
    }
    #[track_caller]
    #[must_use]
    fn reduce_xnor(self) -> Val<'ctx, bool> {
        self.reduce_xor()
    }
}

macro_rules! impl_compare_eq {
    ([$ctx:lifetime $($generics:tt)*] $T:ty) => {
        impl<$ctx $($generics)*> CompareEq<$ctx> for Val<$ctx, $T> {
            #[track_caller]
            #[must_use]
            fn eq(self, rhs: Self) -> Val<'ctx, bool> {
                bool_out_bin_op_unchecked(BoolOutBinOpKind::CompareEq, self, rhs)
            }
        }
        impl<$ctx $($generics)*> CompareEq<$ctx, $T> for Val<$ctx, $T> {
            #[track_caller]
            #[must_use]
            fn eq(self, rhs: $T) -> Val<'ctx, bool> {
                bool_out_bin_op_unchecked(BoolOutBinOpKind::CompareEq, self, rhs.get_value(self.ctx()))
            }
        }
        impl<$ctx $($generics)*> CompareEq<$ctx, Val<$ctx, $T>> for $T {
            #[track_caller]
            #[must_use]
            fn eq(self, rhs: Val<'ctx, $T>) -> Val<'ctx, bool> {
                bool_out_bin_op_unchecked(BoolOutBinOpKind::CompareEq, self.get_value(rhs.ctx()), rhs)
            }
        }
    };
}

macro_rules! impl_compare {
    ([$ctx:lifetime $($generics:tt)*] $T:ty, $lt_kind:path) => {
        impl<$ctx $($generics)*> CompareLtGE<$ctx> for Val<$ctx, $T> {
            #[track_caller]
            #[must_use]
            fn lt(self, rhs: Self) -> Val<'ctx, bool> {
                bool_out_bin_op_unchecked($lt_kind, self, rhs)
            }
        }
        impl<$ctx $($generics)*> CompareLtGE<$ctx, $T> for Val<$ctx, $T> {
            #[track_caller]
            #[must_use]
            fn lt(self, rhs: $T) -> Val<'ctx, bool> {
                bool_out_bin_op_unchecked($lt_kind, self, rhs.get_value(self.ctx()))
            }
        }
        impl<$ctx $($generics)*> CompareLtGE<$ctx, Val<$ctx, $T>> for $T {
            #[track_caller]
            #[must_use]
            fn lt(self, rhs: Val<'ctx, $T>) -> Val<'ctx, bool> {
                bool_out_bin_op_unchecked($lt_kind, self.get_value(rhs.ctx()), rhs)
            }
        }
    };
}

macro_rules! impl_reduce_bitwise {
    ([$ctx:lifetime $($generics:tt)*] $T:ty) => {
        impl<$ctx $($generics)*> ReduceBitwise<$ctx> for Val<$ctx, $T> {
            #[track_caller]
            #[must_use]
            fn reduce_and(self) -> Val<'ctx, bool> {
                bool_out_un_op_unchecked(BoolOutUnOpKind::ReduceAnd, self)
            }
            #[track_caller]
            #[must_use]
            fn reduce_or(self) -> Val<'ctx, bool> {
                bool_out_un_op_unchecked(BoolOutUnOpKind::ReduceOr, self)
            }
            #[track_caller]
            #[must_use]
            fn reduce_xor(self) -> Val<'ctx, bool> {
                bool_out_un_op_unchecked(BoolOutUnOpKind::ReduceXor, self)
            }
        }
    };
}

impl_compare_eq!(['ctx] bool);
impl_compare!(['ctx] bool, BoolOutBinOpKind::CompareULt);
impl_same_size_bin_op!(['ctx] BitAnd for bool {fn bitand}, BitAndAssign {fn bitand_assign}, SameSizeBinOpKind::And);
impl_same_size_bin_op!(['ctx] BitOr for bool {fn bitor}, BitOrAssign {fn bitor_assign}, SameSizeBinOpKind::Or);
impl_same_size_bin_op!(['ctx] BitXor for bool {fn bitxor}, BitXorAssign {fn bitxor_assign}, SameSizeBinOpKind::Xor);
impl_same_size_un_op!(['ctx] Not for bool {fn not}, SameSizeUnOpKind::Not);
impl_reduce_bitwise!(['ctx] bool);

impl_compare_eq!(['ctx, Shape: IntShapeTrait] Int<Shape>);
impl_reduce_bitwise!(['ctx, Shape: IntShapeTrait] Int<Shape>);
impl_compare!(['ctx, const BIT_COUNT: u32] UInt<BIT_COUNT>, BoolOutBinOpKind::CompareULt);
impl_compare!(['ctx, const BIT_COUNT: u32] SInt<BIT_COUNT>, BoolOutBinOpKind::CompareSLt);
impl_same_size_bin_op!(['ctx, Shape: IntShapeTrait] BitAnd for Int<Shape> {fn bitand}, BitAndAssign {fn bitand_assign}, SameSizeBinOpKind::And);
impl_same_size_bin_op!(['ctx, Shape: IntShapeTrait] BitOr for Int<Shape> {fn bitor}, BitOrAssign {fn bitor_assign}, SameSizeBinOpKind::Or);
impl_same_size_bin_op!(['ctx, Shape: IntShapeTrait] BitXor for Int<Shape> {fn bitxor}, BitXorAssign {fn bitxor_assign}, SameSizeBinOpKind::Xor);
impl_same_size_bin_op!(['ctx, Shape: IntShapeTrait] Shl for Int<Shape> {fn shl}, ShlAssign {fn shl_assign}, SameSizeBinOpKind::ShiftLeft);
impl_same_size_bin_op!(['ctx, const BIT_COUNT: u32] Shr for UInt<BIT_COUNT> {fn shr}, ShrAssign {fn shr_assign}, SameSizeBinOpKind::UnsignedShiftRight);
impl_same_size_bin_op!(['ctx, const BIT_COUNT: u32] Shr for SInt<BIT_COUNT> {fn shr}, ShrAssign {fn shr_assign}, SameSizeBinOpKind::SignedShiftRight);
impl_same_size_bin_op!(['ctx, Shape: IntShapeTrait] Add for Int<Shape> {fn add}, AddAssign {fn add_assign}, SameSizeBinOpKind::Add);
impl_same_size_bin_op!(['ctx, Shape: IntShapeTrait] Sub for Int<Shape> {fn sub}, SubAssign {fn sub_assign}, SameSizeBinOpKind::Sub);
impl_same_size_bin_op!(['ctx, Shape: IntShapeTrait] Mul for Int<Shape> {fn mul}, MulAssign {fn mul_assign}, SameSizeBinOpKind::Mul);
impl_same_size_un_op!(['ctx, Shape: IntShapeTrait] Not for Int<Shape> {fn not}, SameSizeUnOpKind::Not);
impl_same_size_un_op!(['ctx, Shape: IntShapeTrait] Neg for Int<Shape> {fn neg}, SameSizeUnOpKind::Neg);

pub trait Len {
    #[must_use]
    fn len(&self) -> usize;
    #[must_use]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'ctx, T: Value<'ctx>, const N: usize> Len for Val<'ctx, [T; N]> {
    #[must_use]
    fn len(&self) -> usize {
        N
    }
}

impl<'ctx, T: Value<'ctx>, const N: usize> Len for ValueType<'ctx, [T; N]> {
    #[must_use]
    fn len(&self) -> usize {
        N
    }
}

impl<'ctx, T: Value<'ctx>> Len for ValueType<'ctx, Vec<T>> {
    #[must_use]
    fn len(&self) -> usize {
        self.ir().array().unwrap().length
    }
}

impl<'ctx, T: Value<'ctx>> Len for Val<'ctx, Vec<T>> {
    #[must_use]
    fn len(&self) -> usize {
        self.value_type().len()
    }
}

impl<'ctx, T: Value<'ctx>> Len for ValueType<'ctx, Box<[T]>> {
    #[must_use]
    fn len(&self) -> usize {
        self.ir().array().unwrap().length
    }
}

impl<'ctx, T: Value<'ctx>> Len for Val<'ctx, Box<[T]>> {
    #[must_use]
    fn len(&self) -> usize {
        self.value_type().len()
    }
}

pub trait Slice<Index> {
    type Output;

    #[track_caller]
    #[must_use]
    fn slice(self, index: Index) -> Self::Output;
}

impl<'ctx, R: RangeBounds<usize>, T: Value<'ctx>, const N: usize> Slice<R> for Val<'ctx, [T; N]> {
    type Output = Val<'ctx, Vec<T>>;

    #[track_caller]
    #[must_use]
    fn slice(self, index: R) -> Self::Output {
        Val::<'ctx, Vec<T>>::from(self).slice(index)
    }
}

#[track_caller]
#[must_use]
fn resolve_range_bounds<
    R: RangeBounds<T>,
    T: From<u8> + Add<Output = T> + Sub<Output = T> + Ord + Copy,
>(
    index: R,
    len: T,
) -> Range<T> {
    let start = match index.start_bound() {
        Bound::Included(&start) => {
            assert!(start <= len);
            start
        }
        Bound::Excluded(&v) => {
            assert!(v < len);
            v + T::from(1)
        }
        Bound::Unbounded => T::from(0),
    };
    let end = match index.end_bound() {
        Bound::Included(&v) => {
            assert!(v < len);
            v + T::from(1)
        }
        Bound::Excluded(&end) => {
            assert!(end <= len);
            end
        }
        Bound::Unbounded => len,
    };
    assert!(start <= end);
    start..end
}

impl<'ctx, R: RangeBounds<usize>, T: Value<'ctx>> Slice<R> for Val<'ctx, Vec<T>> {
    type Output = Val<'ctx, Vec<T>>;

    #[track_caller]
    #[must_use]
    fn slice(self, index: R) -> Self::Output {
        Val::from_ir_unchecked(
            self.ctx(),
            IrValue::from(SliceArray::new(
                self.ctx(),
                self.ir(),
                resolve_range_bounds(index, self.len()),
            ))
            .intern(self.ctx()),
        )
    }
}

impl<'ctx, R: RangeBounds<usize>, T: Value<'ctx>> Slice<R> for Val<'ctx, Box<[T]>> {
    type Output = Val<'ctx, Vec<T>>;

    #[track_caller]
    #[must_use]
    fn slice(self, index: R) -> Self::Output {
        Val::<'ctx, Vec<T>>::from(self).slice(index)
    }
}

impl<'ctx, R: RangeBounds<u32>, Shape: IntShapeTrait> Slice<R> for Val<'ctx, Int<Shape>> {
    type Output = Val<'ctx, Int>;

    #[track_caller]
    #[must_use]
    fn slice(self, index: R) -> Self::Output {
        Val::from_ir_unchecked(
            self.ctx(),
            IrValue::from(SliceBitVector::new(
                self.ctx(),
                self.ir(),
                resolve_range_bounds(
                    index,
                    self.value_type().ir().bit_vector().unwrap().bit_count,
                ),
            ))
            .intern(self.ctx()),
        )
    }
}