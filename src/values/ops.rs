// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use alloc::{boxed::Box, vec::Vec};

use crate::{
    context::Intern,
    ir::{
        types::IrBitVectorType,
        values::{
            BoolOutBinOp, BoolOutBinOpKind, BoolOutUnOp, BoolOutUnOpKind, ConvertIntWrapping,
            IrValue, SameSizeBinOp, SameSizeBinOpKind, SameSizeUnOp, SameSizeUnOpKind, SliceArray,
            SliceBitVector,
        },
    },
    prelude::*,
    values::integer::IntShapeTrait,
};
use core::ops::{
    Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Bound, Mul,
    MulAssign, Neg, Not, Range, RangeBounds, Shl, ShlAssign, Shr, ShrAssign, Sub, SubAssign,
};

impl<'ctx: 'scope, 'scope> From<Val<'ctx, 'scope, UInt1>> for Val<'ctx, 'scope, bool> {
    #[must_use]
    fn from(v: Val<'ctx, 'scope, UInt1>) -> Self {
        Self::from_ir_unchecked(v.ctx(), v.ir())
    }
}

impl<'ctx: 'scope, 'scope> From<Val<'ctx, 'scope, bool>> for Val<'ctx, 'scope, UInt1> {
    #[must_use]
    fn from(v: Val<'ctx, 'scope, bool>) -> Self {
        Self::from_ir_unchecked(v.ctx(), v.ir())
    }
}

#[track_caller]
fn same_size_bin_op_unchecked<'ctx: 'scope, 'scope, T: Value<'ctx>>(
    kind: SameSizeBinOpKind,
    lhs: Val<'ctx, 'scope, T>,
    rhs: Val<'ctx, 'scope, T>,
) -> Val<'ctx, 'scope, T> {
    Val::from_ir_unchecked(
        lhs.ctx(),
        IrValue::from(SameSizeBinOp::new(lhs.ctx(), kind, lhs.ir(), rhs.ir())).intern(lhs.ctx()),
    )
}

#[track_caller]
fn same_size_un_op_unchecked<'ctx: 'scope, 'scope, T: Value<'ctx>>(
    kind: SameSizeUnOpKind,
    input: Val<'ctx, 'scope, T>,
) -> Val<'ctx, 'scope, T> {
    Val::from_ir_unchecked(
        input.ctx(),
        IrValue::from(SameSizeUnOp::new(input.ctx(), kind, input.ir())).intern(input.ctx()),
    )
}

#[track_caller]
fn bool_out_bin_op_unchecked<'ctx: 'scope, 'scope, T: Value<'ctx>>(
    kind: BoolOutBinOpKind,
    lhs: Val<'ctx, 'scope, T>,
    rhs: Val<'ctx, 'scope, T>,
) -> Val<'ctx, 'scope, bool> {
    Val::from_ir_unchecked(
        lhs.ctx(),
        IrValue::from(BoolOutBinOp::new(lhs.ctx(), kind, lhs.ir(), rhs.ir())).intern(lhs.ctx()),
    )
}

#[track_caller]
fn bool_out_un_op_unchecked<'ctx: 'scope, 'scope, T: Value<'ctx>>(
    kind: BoolOutUnOpKind,
    input: Val<'ctx, 'scope, T>,
) -> Val<'ctx, 'scope, bool> {
    Val::from_ir_unchecked(
        input.ctx(),
        IrValue::from(BoolOutUnOp::new(input.ctx(), kind, input.ir())).intern(input.ctx()),
    )
}

macro_rules! impl_same_size_bin_op_helper {
    ([$ctx:lifetime, $scope:lifetime $($generics:tt)*] $Trait:ident for $T:ty {fn $trait_fn:ident}, $AssignTrait:ident {fn $assign_trait_fn:ident}) => {
        impl<$ctx: $scope, $scope $($generics)*> $Trait<$T> for Val<$ctx, $scope, $T> {
            type Output = Val<$ctx, $scope, $T>;

            #[track_caller]
            #[must_use]
            fn $trait_fn(self, rhs: $T) -> Self::Output {
                self.$trait_fn(rhs.get_value(self.ctx()))
            }
        }
        impl<$ctx: $scope, $scope $($generics)*> $Trait<Val<$ctx, $scope, $T>> for $T {
            type Output = Val<$ctx, $scope, $T>;

            #[track_caller]
            #[must_use]
            fn $trait_fn(self, rhs: Val<$ctx, $scope, $T>) -> Self::Output {
                self.get_value(rhs.ctx()).$trait_fn(rhs)
            }
        }
        impl<$ctx: $scope, $scope $($generics)*> $AssignTrait<Val<$ctx, $scope, $T>> for Val<$ctx, $scope, $T> {
            #[track_caller]
            #[must_use]
            fn $assign_trait_fn(&mut self, rhs: Val<$ctx, $scope, $T>) {
                *self = (*self).$trait_fn(rhs);
            }
        }
        impl<$ctx: $scope, $scope $($generics)*> $AssignTrait<$T> for Val<$ctx, $scope, $T> {
            #[track_caller]
            #[must_use]
            fn $assign_trait_fn(&mut self, rhs: $T) {
                *self = (*self).$trait_fn(rhs);
            }
        }
    };
}

macro_rules! impl_same_size_bin_op {
    ([$ctx:lifetime, $scope:lifetime $($generics:tt)*] $Trait:ident for $T:ty {fn $trait_fn:ident}, $AssignTrait:ident {fn $assign_trait_fn:ident}, $kind:path) => {
        impl<$ctx: $scope, $scope $($generics)*> $Trait<Val<$ctx, $scope, $T>> for Val<$ctx, $scope, $T> {
            type Output = Val<$ctx, $scope, $T>;

            #[track_caller]
            #[must_use]
            fn $trait_fn(self, rhs: Val<$ctx, $scope, $T>) -> Self::Output {
                same_size_bin_op_unchecked($kind, self, rhs)
            }
        }
        impl_same_size_bin_op_helper!([$ctx, $scope $($generics)*] $Trait for $T {fn $trait_fn}, $AssignTrait {fn $assign_trait_fn});
    };
}

macro_rules! impl_same_size_un_op {
    ([$ctx:lifetime, $scope:lifetime $($generics:tt)*] $Trait:ident for $T:ty {fn $trait_fn:ident}, $kind:path) => {
        impl<$ctx: $scope, $scope $($generics)*> $Trait for Val<$ctx, $scope, $T> {
            type Output = Val<$ctx, $scope, $T>;

            #[track_caller]
            #[must_use]
            fn $trait_fn(self) -> Self::Output {
                same_size_un_op_unchecked($kind, self)
            }
        }
    };
}

pub trait CompareEq<'ctx: 'scope, 'scope, Rhs = Self>: Sized {
    #[track_caller]
    #[must_use]
    fn eq(self, rhs: Rhs) -> Val<'ctx, 'scope, bool>;
    #[track_caller]
    #[must_use]
    fn ne(self, rhs: Rhs) -> Val<'ctx, 'scope, bool> {
        !self.eq(rhs)
    }
}

pub trait CompareGtLE<'ctx: 'scope, 'scope, Rhs = Self>: Sized {
    #[track_caller]
    #[must_use]
    fn gt(self, rhs: Rhs) -> Val<'ctx, 'scope, bool>;
    #[track_caller]
    #[must_use]
    fn le(self, rhs: Rhs) -> Val<'ctx, 'scope, bool> {
        !self.gt(rhs)
    }
}

pub trait CompareLtGE<'ctx: 'scope, 'scope, Rhs = Self>: Sized {
    #[track_caller]
    #[must_use]
    fn lt(self, rhs: Rhs) -> Val<'ctx, 'scope, bool>;
    #[track_caller]
    #[must_use]
    fn ge(self, rhs: Rhs) -> Val<'ctx, 'scope, bool> {
        !self.lt(rhs)
    }
}

impl<'ctx: 'scope, 'scope, Lhs, Rhs: CompareLtGE<'ctx, 'scope, Lhs>> CompareGtLE<'ctx, 'scope, Rhs>
    for Lhs
{
    #[track_caller]
    #[must_use]
    fn gt(self, rhs: Rhs) -> Val<'ctx, 'scope, bool> {
        rhs.lt(self)
    }
    #[track_caller]
    #[must_use]
    fn le(self, rhs: Rhs) -> Val<'ctx, 'scope, bool> {
        rhs.ge(self)
    }
}

pub trait Compare<'ctx: 'scope, 'scope>:
    CompareEq<'ctx, 'scope> + CompareGtLE<'ctx, 'scope> + CompareLtGE<'ctx, 'scope>
{
}

impl<
        'ctx: 'scope,
        'scope,
        T: CompareEq<'ctx, 'scope> + CompareGtLE<'ctx, 'scope> + CompareLtGE<'ctx, 'scope>,
    > Compare<'ctx, 'scope> for T
{
}

pub trait ReduceBitwise<'ctx: 'scope, 'scope>: Sized {
    #[track_caller]
    #[must_use]
    fn reduce_and(self) -> Val<'ctx, 'scope, bool>;
    #[track_caller]
    #[must_use]
    fn reduce_or(self) -> Val<'ctx, 'scope, bool>;
    #[track_caller]
    #[must_use]
    fn reduce_xor(self) -> Val<'ctx, 'scope, bool>;
    #[track_caller]
    #[must_use]
    fn reduce_nand(self) -> Val<'ctx, 'scope, bool> {
        !self.reduce_and()
    }
    #[track_caller]
    #[must_use]
    fn reduce_nor(self) -> Val<'ctx, 'scope, bool> {
        !self.reduce_or()
    }
    #[track_caller]
    #[must_use]
    fn reduce_xnor(self) -> Val<'ctx, 'scope, bool> {
        self.reduce_xor()
    }
}

macro_rules! impl_compare_eq {
    ([$ctx:lifetime, $scope:lifetime $($generics:tt)*] $T:ty) => {
        impl<$ctx: $scope, $scope $($generics)*> CompareEq<$ctx, $scope> for Val<$ctx, $scope, $T> {
            #[track_caller]
            #[must_use]
            fn eq(self, rhs: Self) -> Val<'ctx, 'scope, bool> {
                bool_out_bin_op_unchecked(BoolOutBinOpKind::CompareEq, self, rhs)
            }
        }
        impl<$ctx: $scope, $scope $($generics)*> CompareEq<$ctx, $scope, $T> for Val<$ctx, $scope, $T> {
            #[track_caller]
            #[must_use]
            fn eq(self, rhs: $T) -> Val<'ctx, 'scope, bool> {
                bool_out_bin_op_unchecked(BoolOutBinOpKind::CompareEq, self, rhs.get_value(self.ctx()))
            }
        }
        impl<$ctx: $scope, $scope $($generics)*> CompareEq<$ctx, $scope, Val<$ctx, $scope, $T>> for $T {
            #[track_caller]
            #[must_use]
            fn eq(self, rhs: Val<'ctx, 'scope, $T>) -> Val<'ctx, 'scope, bool> {
                bool_out_bin_op_unchecked(BoolOutBinOpKind::CompareEq, self.get_value(rhs.ctx()), rhs)
            }
        }
    };
}

macro_rules! impl_compare_helper {
    ([$ctx:lifetime, $scope:lifetime $($generics:tt)*] $T:ty) => {
        impl_compare_eq!([$ctx, $scope $($generics)*] $T);
        impl<$ctx: $scope, $scope $($generics)*> CompareLtGE<$ctx, $scope, $T> for Val<$ctx, $scope, $T> {
            #[track_caller]
            #[must_use]
            fn lt(self, rhs: $T) -> Val<'ctx, 'scope, bool> {
                self.lt(rhs.get_value(self.ctx()))
            }
        }
        impl<$ctx: $scope, $scope $($generics)*> CompareLtGE<$ctx, $scope, Val<$ctx, $scope, $T>> for $T {
            #[track_caller]
            #[must_use]
            fn lt(self, rhs: Val<'ctx, 'scope, $T>) -> Val<'ctx, 'scope, bool> {
                self.get_value(rhs.ctx()).lt(rhs)
            }
        }
    };
}

impl<'ctx: 'scope, 'scope> CompareLtGE<'ctx, 'scope> for Val<'ctx, 'scope, bool> {
    #[track_caller]
    #[must_use]
    fn lt(self, rhs: Self) -> Val<'ctx, 'scope, bool> {
        bool_out_bin_op_unchecked(BoolOutBinOpKind::CompareUnsignedLt, self, rhs)
    }
}

impl<'ctx: 'scope, 'scope, Shape: IntShapeTrait> Shr for Val<'ctx, 'scope, Int<Shape>> {
    type Output = Self;

    #[track_caller]
    #[must_use]
    fn shr(self, rhs: Self) -> Self::Output {
        let lhs_type = self
            .value_type()
            .ir()
            .bit_vector()
            .expect("expected bit vector");
        let rhs_type = rhs
            .value_type()
            .ir()
            .bit_vector()
            .expect("expected bit vector");
        assert_eq!(lhs_type, rhs_type);
        let kind = if lhs_type.signed {
            SameSizeBinOpKind::ArithmeticShiftRight
        } else {
            SameSizeBinOpKind::LogicalShiftRight
        };
        Val::from_ir_unchecked(
            self.ctx(),
            IrValue::from(SameSizeBinOp::new(self.ctx(), kind, self.ir(), rhs.ir()))
                .intern(self.ctx()),
        )
    }
}

impl<'ctx: 'scope, 'scope, Shape: IntShapeTrait> CompareLtGE<'ctx, 'scope>
    for Val<'ctx, 'scope, Int<Shape>>
{
    #[track_caller]
    #[must_use]
    fn lt(self, rhs: Self) -> Val<'ctx, 'scope, bool> {
        let lhs_type = self
            .value_type()
            .ir()
            .bit_vector()
            .expect("expected bit vector");
        let rhs_type = rhs
            .value_type()
            .ir()
            .bit_vector()
            .expect("expected bit vector");
        assert_eq!(lhs_type, rhs_type);
        let kind = if lhs_type.signed {
            BoolOutBinOpKind::CompareSignedLt
        } else {
            BoolOutBinOpKind::CompareUnsignedLt
        };
        Val::from_ir_unchecked(
            self.ctx(),
            IrValue::from(BoolOutBinOp::new(self.ctx(), kind, self.ir(), rhs.ir()))
                .intern(self.ctx()),
        )
    }
}

macro_rules! impl_reduce_bitwise {
    ([$ctx:lifetime, $scope:lifetime $($generics:tt)*] $T:ty) => {
        impl<$ctx: $scope, $scope $($generics)*> ReduceBitwise<$ctx, $scope> for Val<$ctx, $scope, $T> {
            #[track_caller]
            #[must_use]
            fn reduce_and(self) -> Val<'ctx, 'scope, bool> {
                bool_out_un_op_unchecked(BoolOutUnOpKind::ReduceAnd, self)
            }
            #[track_caller]
            #[must_use]
            fn reduce_or(self) -> Val<'ctx, 'scope, bool> {
                bool_out_un_op_unchecked(BoolOutUnOpKind::ReduceOr, self)
            }
            #[track_caller]
            #[must_use]
            fn reduce_xor(self) -> Val<'ctx, 'scope, bool> {
                bool_out_un_op_unchecked(BoolOutUnOpKind::ReduceXor, self)
            }
        }
    };
}

impl_compare_helper!(['ctx, 'scope] bool);
impl_same_size_bin_op!(['ctx, 'scope] BitAnd for bool {fn bitand}, BitAndAssign {fn bitand_assign}, SameSizeBinOpKind::And);
impl_same_size_bin_op!(['ctx, 'scope] BitOr for bool {fn bitor}, BitOrAssign {fn bitor_assign}, SameSizeBinOpKind::Or);
impl_same_size_bin_op!(['ctx, 'scope] BitXor for bool {fn bitxor}, BitXorAssign {fn bitxor_assign}, SameSizeBinOpKind::Xor);
impl_same_size_un_op!(['ctx, 'scope] Not for bool {fn not}, SameSizeUnOpKind::Not);
impl_reduce_bitwise!(['ctx, 'scope] bool);

impl_reduce_bitwise!(['ctx, 'scope, Shape: IntShapeTrait] Int<Shape>);
impl_compare_helper!(['ctx, 'scope, Shape: IntShapeTrait] Int<Shape>);
impl_same_size_bin_op!(['ctx, 'scope, Shape: IntShapeTrait] BitAnd for Int<Shape> {fn bitand}, BitAndAssign {fn bitand_assign}, SameSizeBinOpKind::And);
impl_same_size_bin_op!(['ctx, 'scope, Shape: IntShapeTrait] BitOr for Int<Shape> {fn bitor}, BitOrAssign {fn bitor_assign}, SameSizeBinOpKind::Or);
impl_same_size_bin_op!(['ctx, 'scope, Shape: IntShapeTrait] BitXor for Int<Shape> {fn bitxor}, BitXorAssign {fn bitxor_assign}, SameSizeBinOpKind::Xor);
impl_same_size_bin_op!(['ctx, 'scope, Shape: IntShapeTrait] Shl for Int<Shape> {fn shl}, ShlAssign {fn shl_assign}, SameSizeBinOpKind::ShiftLeft);
impl_same_size_bin_op_helper!(['ctx, 'scope, Shape: IntShapeTrait] Shr for Int<Shape> {fn shr}, ShrAssign {fn shr_assign});
impl_same_size_bin_op!(['ctx, 'scope, Shape: IntShapeTrait] Add for Int<Shape> {fn add}, AddAssign {fn add_assign}, SameSizeBinOpKind::Add);
impl_same_size_bin_op!(['ctx, 'scope, Shape: IntShapeTrait] Sub for Int<Shape> {fn sub}, SubAssign {fn sub_assign}, SameSizeBinOpKind::Sub);
impl_same_size_bin_op!(['ctx, 'scope, Shape: IntShapeTrait] Mul for Int<Shape> {fn mul}, MulAssign {fn mul_assign}, SameSizeBinOpKind::Mul);
impl_same_size_un_op!(['ctx, 'scope, Shape: IntShapeTrait] Not for Int<Shape> {fn not}, SameSizeUnOpKind::Not);
impl_same_size_un_op!(['ctx, 'scope, Shape: IntShapeTrait] Neg for Int<Shape> {fn neg}, SameSizeUnOpKind::Neg);

pub trait Len {
    #[must_use]
    fn len(&self) -> usize;
    #[must_use]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'ctx, 'scope, T: Value<'ctx>, const N: usize> Len for Val<'ctx, 'scope, [T; N]> {
    #[must_use]
    fn len(&self) -> usize {
        N
    }
}

impl<'ctx, 'scope, T: Value<'ctx>, const N: usize> Len for ValueType<'ctx, [T; N]> {
    #[must_use]
    fn len(&self) -> usize {
        N
    }
}

impl<'ctx, 'scope, T: Value<'ctx>> Len for ValueType<'ctx, Vec<T>> {
    #[must_use]
    fn len(&self) -> usize {
        self.ir().array().unwrap().length
    }
}

impl<'ctx, 'scope, T: Value<'ctx>> Len for Val<'ctx, 'scope, Vec<T>> {
    #[must_use]
    fn len(&self) -> usize {
        self.value_type().len()
    }
}

impl<'ctx, 'scope, T: Value<'ctx>> Len for ValueType<'ctx, Box<[T]>> {
    #[must_use]
    fn len(&self) -> usize {
        self.ir().array().unwrap().length
    }
}

impl<'ctx, 'scope, T: Value<'ctx>> Len for Val<'ctx, 'scope, Box<[T]>> {
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

impl<'ctx: 'scope, 'scope, R: RangeBounds<usize>, T: Value<'ctx>, const N: usize> Slice<R>
    for Val<'ctx, 'scope, [T; N]>
{
    type Output = Val<'ctx, 'scope, Vec<T>>;

    #[track_caller]
    #[must_use]
    fn slice(self, index: R) -> Self::Output {
        Val::<'ctx, 'scope, Vec<T>>::from(self).slice(index)
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

impl<'ctx: 'scope, 'scope, R: RangeBounds<usize>, T: Value<'ctx>> Slice<R>
    for Val<'ctx, 'scope, Vec<T>>
{
    type Output = Val<'ctx, 'scope, Vec<T>>;

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

impl<'ctx: 'scope, 'scope, R: RangeBounds<usize>, T: Value<'ctx>> Slice<R>
    for Val<'ctx, 'scope, Box<[T]>>
{
    type Output = Val<'ctx, 'scope, Vec<T>>;

    #[track_caller]
    #[must_use]
    fn slice(self, index: R) -> Self::Output {
        Val::<'ctx, 'scope, Vec<T>>::from(self).slice(index)
    }
}

impl<'ctx: 'scope, 'scope, R: RangeBounds<u32>, Shape: IntShapeTrait> Slice<R>
    for Val<'ctx, 'scope, Int<Shape>>
{
    type Output = Val<'ctx, 'scope, Int>;

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

#[track_caller]
#[must_use]
fn wrap_to_signed_helper<'ctx: 'scope, 'scope, Shape: IntShapeTrait, RetShape: IntShapeTrait>(
    value: Val<'ctx, 'scope, Int<Shape>>,
    signed: bool,
) -> Val<'ctx, 'scope, Int<RetShape>> {
    let IrBitVectorType {
        bit_count,
        signed: _,
    } = value
        .value_type()
        .ir()
        .bit_vector()
        .expect("expected bit vector");
    Val::from_ir_unchecked(
        value.ctx(),
        IrValue::from(ConvertIntWrapping::new(
            value.ctx(),
            IrBitVectorType { bit_count, signed },
            value.ir(),
        ))
        .intern(value.ctx()),
    )
}

impl<'ctx: 'scope, 'scope, Shape: IntShapeTrait> Val<'ctx, 'scope, Int<Shape>> {
    #[track_caller]
    #[must_use]
    pub fn wrap_into_shape<NewShape: IntShapeTrait>(
        self,
        new_shape: NewShape,
    ) -> Val<'ctx, 'scope, Int<NewShape>> {
        let value_type = new_shape.shape().into();
        Val::from_ir_unchecked(
            self.ctx(),
            IrValue::from(ConvertIntWrapping::new(self.ctx(), value_type, self.ir()))
                .intern(self.ctx()),
        )
    }
    #[track_caller]
    #[must_use]
    pub fn wrap_into<NewShape: IntShapeTrait + Default>(self) -> Val<'ctx, 'scope, Int<NewShape>> {
        self.wrap_into_shape(NewShape::default())
    }
    #[track_caller]
    #[must_use]
    pub fn wrap_to_unsigned(self) -> Val<'ctx, 'scope, Int<Shape::UnsignedShape>> {
        wrap_to_signed_helper(self, false)
    }
    #[track_caller]
    #[must_use]
    pub fn wrap_to_signed(self) -> Val<'ctx, 'scope, Int<Shape::SignedShape>> {
        wrap_to_signed_helper(self, true)
    }
}
