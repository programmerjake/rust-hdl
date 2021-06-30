// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use core::{
    convert::identity,
    fmt,
    hash::{Hash, Hasher},
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Mul,
        MulAssign, Neg, Not, Sub, SubAssign,
    },
};
use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IntShape {
    pub bit_count: u32,
    pub signed: bool,
}

impl fmt::Display for IntShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.signed {
            write!(f, "i{}", self.bit_count)
        } else {
            write!(f, "u{}", self.bit_count)
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstIntShape<const BIT_COUNT: u32, const SIGNED: bool>;

pub trait IntShapeTrait: Copy + fmt::Debug {
    type UnsignedShape: IntShapeTrait<
        UnsignedShape = Self::UnsignedShape,
        SignedShape = Self::SignedShape,
    >;
    type SignedShape: IntShapeTrait<
        UnsignedShape = Self::UnsignedShape,
        SignedShape = Self::SignedShape,
    >;
    fn shape(self) -> IntShape;
    fn static_shape() -> Option<IntShape>;
    fn to_unsigned(self) -> Self::UnsignedShape;
    fn to_signed(self) -> Self::SignedShape;
}

impl IntShapeTrait for IntShape {
    type UnsignedShape = Self;
    type SignedShape = Self;
    fn shape(self) -> IntShape {
        self
    }
    fn static_shape() -> Option<IntShape> {
        None
    }
    fn to_unsigned(self) -> Self::UnsignedShape {
        Self {
            signed: false,
            ..self
        }
    }
    fn to_signed(self) -> Self::SignedShape {
        Self {
            signed: true,
            ..self
        }
    }
}

impl<const BIT_COUNT: u32, const SIGNED: bool> IntShapeTrait for ConstIntShape<BIT_COUNT, SIGNED> {
    type UnsignedShape = ConstIntShape<BIT_COUNT, false>;
    type SignedShape = ConstIntShape<BIT_COUNT, true>;
    fn shape(self) -> IntShape {
        IntShape {
            bit_count: BIT_COUNT,
            signed: SIGNED,
        }
    }
    fn static_shape() -> Option<IntShape> {
        Some(Self::default().shape())
    }
    fn to_unsigned(self) -> Self::UnsignedShape {
        ConstIntShape
    }
    fn to_signed(self) -> Self::SignedShape {
        ConstIntShape
    }
}

pub type UIntShape<const BIT_COUNT: u32> = ConstIntShape<BIT_COUNT, false>;
pub type SIntShape<const BIT_COUNT: u32> = ConstIntShape<BIT_COUNT, true>;
pub type U1Shape = ConstIntShape<1, false>;
pub type I1Shape = ConstIntShape<1, true>;
pub type U8Shape = ConstIntShape<8, false>;
pub type I8Shape = ConstIntShape<8, true>;
pub type U16Shape = ConstIntShape<16, false>;
pub type I16Shape = ConstIntShape<16, true>;
pub type U32Shape = ConstIntShape<32, false>;
pub type I32Shape = ConstIntShape<32, true>;
pub type U64Shape = ConstIntShape<64, false>;
pub type I64Shape = ConstIntShape<64, true>;
pub type U128Shape = ConstIntShape<128, false>;
pub type I128Shape = ConstIntShape<128, true>;

#[derive(Clone, Default)]
pub struct Int<Shape: IntShapeTrait = IntShape> {
    value: BigInt,
    shape: Shape,
}

impl<Shape: IntShapeTrait> PartialEq for Int<Shape> {
    fn eq(&self, other: &Self) -> bool {
        self.shape.shape() == other.shape.shape() && self.value == other.value
    }
}

impl<Shape: IntShapeTrait> Eq for Int<Shape> {}

impl<Shape: IntShapeTrait> Hash for Int<Shape> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.shape.shape().hash(state);
        self.value.hash(state);
    }
}

impl<Shape: IntShapeTrait> fmt::Display for Int<Shape> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#X}_{}", self.value, self.shape.shape())
    }
}

impl<Shape: IntShapeTrait> fmt::Binary for Int<Shape> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#b}_{}", self.value, self.shape.shape())
    }
}

impl<Shape: IntShapeTrait> fmt::LowerHex for Int<Shape> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}_{}", self.value, self.shape.shape())
    }
}

impl<Shape: IntShapeTrait> fmt::UpperHex for Int<Shape> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#X}_{}", self.value, self.shape.shape())
    }
}

impl<Shape: IntShapeTrait> fmt::Octal for Int<Shape> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#o}_{}", self.value, self.shape.shape())
    }
}

impl<Shape: IntShapeTrait> fmt::Debug for Int<Shape> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Clone)]
pub struct DisplayDecimal<'a, Shape: IntShapeTrait>(pub &'a Int<Shape>);

impl<Shape: IntShapeTrait> fmt::Display for DisplayDecimal<'_, Shape> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.0.value, self.0.shape.shape())
    }
}

impl<Shape: IntShapeTrait> fmt::Debug for DisplayDecimal<'_, Shape> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

fn wrap(value: &mut BigInt, shape: IntShape) {
    let mask = (BigInt::one() << shape.bit_count) - 1;
    *value &= mask;
    if shape.signed && shape.bit_count > 0 && value.bit((shape.bit_count - 1).into()) {
        *value -= BigInt::one() << shape.bit_count;
    }
}

#[track_caller]
fn assert_matching_shape<Shape: IntShapeTrait>(shape1: Shape, shape2: Shape) {
    assert_eq!(shape1.shape(), shape2.shape(), "Shape mismatch");
}

impl<Shape: IntShapeTrait> Int<Shape> {
    pub fn unchecked_new_with_shape<T: Into<BigInt>>(value: T, shape: Shape) -> Self {
        Self {
            value: value.into(),
            shape,
        }
    }
    pub fn unchecked_new<T: Into<BigInt>>(value: T) -> Self
    where
        Shape: Default,
    {
        Self::unchecked_new_with_shape(value, Shape::default())
    }
    pub fn wrapping_with_shape<T: Into<BigInt>>(value: T, shape: Shape) -> Self {
        Self {
            value: value.into(),
            shape,
        }
        .wrap_move()
    }
    fn wrap_mut(&mut self) {
        wrap(&mut self.value, self.shape.shape());
    }
    fn wrap_move(mut self) -> Self {
        self.wrap_mut();
        self
    }
    pub fn wrapping_new<T: Into<BigInt>>(value: T) -> Self
    where
        Shape: Default,
    {
        Self::wrapping_with_shape(value, Shape::default())
    }
    pub fn shape(&self) -> Shape {
        self.shape
    }
    pub fn into_value(self) -> BigInt {
        self.value
    }
    pub fn value(&self) -> BigInt {
        self.value.clone()
    }
    pub fn wrap_into_shape<NewShape: IntShapeTrait>(self, new_shape: NewShape) -> Int<NewShape> {
        Int::wrapping_with_shape(self, new_shape)
    }
    pub fn wrap_into<NewShape: IntShapeTrait + Default>(self) -> Int<NewShape> {
        Int::wrapping_new(self)
    }
    pub fn into_dyn(self) -> Int {
        let Self { value, shape } = self;
        Int {
            value,
            shape: shape.shape(),
        }
    }
    pub fn display_decimal(&self) -> DisplayDecimal<'_, Shape> {
        DisplayDecimal(self)
    }
    pub fn fmt_value(
        &self,
    ) -> impl fmt::Binary + fmt::Display + fmt::LowerHex + fmt::Octal + fmt::UpperHex + '_ {
        &self.value
    }
    pub fn wrap_to_unsigned(self) -> Int<Shape::UnsignedShape> {
        Int {
            value: self.value,
            shape: self.shape.to_unsigned(),
        }
        .wrap_move()
    }
    pub fn wrap_to_signed(self) -> Int<Shape::SignedShape> {
        Int {
            value: self.value,
            shape: self.shape.to_signed(),
        }
        .wrap_move()
    }
    pub fn max_value_with_shape(shape: Shape) -> Self {
        let IntShape { bit_count, signed } = shape.shape();
        let value = if signed && bit_count > 0 {
            (BigInt::one() << (bit_count - 1)) - 1
        } else {
            -BigInt::one()
        };
        Self::wrapping_with_shape(value, shape)
    }
    pub fn max_value() -> Self
    where
        Shape: Default,
    {
        Self::max_value_with_shape(Shape::default())
    }
    pub fn min_value_with_shape(shape: Shape) -> Self {
        let IntShape { bit_count, signed } = shape.shape();
        let value = if signed && bit_count > 0 {
            -BigInt::one() << (bit_count - 1)
        } else {
            BigInt::zero()
        };
        Self::wrapping_with_shape(value, shape)
    }
    pub fn min_value() -> Self
    where
        Shape: Default,
    {
        Self::min_value_with_shape(Shape::default())
    }
}

impl<Shape: IntShapeTrait> From<Int<Shape>> for BigInt {
    fn from(v: Int<Shape>) -> Self {
        v.value
    }
}

impl<Shape: IntShapeTrait> Not for Int<Shape> {
    type Output = Int<Shape>;

    fn not(self) -> Self::Output {
        Self::wrapping_with_shape(!self.value, self.shape)
    }
}

impl<Shape: IntShapeTrait> Not for &'_ Int<Shape> {
    type Output = Int<Shape>;

    fn not(self) -> Self::Output {
        Int::wrapping_with_shape(!&self.value, self.shape)
    }
}

impl<Shape: IntShapeTrait> Neg for Int<Shape> {
    type Output = Int<Shape>;

    fn neg(self) -> Self::Output {
        Self::wrapping_with_shape(-self.value, self.shape)
    }
}

impl<Shape: IntShapeTrait> Neg for &'_ Int<Shape> {
    type Output = Int<Shape>;

    fn neg(self) -> Self::Output {
        Int::wrapping_with_shape(-&self.value, self.shape)
    }
}

macro_rules! impl_bin_op_int {
    ($Op:ident, $op:ident, $OpAssign:ident, $op_assign:ident, $fallback_prim:expr,) => {
        impl<Shape: IntShapeTrait> $OpAssign<&'_ Int<Shape>> for Int<Shape> {
            #[track_caller]
            fn $op_assign(&mut self, rhs: &'_ Int<Shape>) {
                assert_matching_shape(self.shape, rhs.shape);
                self.value.$op_assign(&rhs.value);
                self.wrap_mut();
            }
        }

        impl<Shape: IntShapeTrait> $OpAssign<Int<Shape>> for Int<Shape> {
            #[track_caller]
            fn $op_assign(&mut self, rhs: Int<Shape>) {
                assert_matching_shape(self.shape, rhs.shape);
                self.value.$op_assign(rhs.value);
                self.wrap_mut();
            }
        }

        impl<Shape: IntShapeTrait> $Op<Int<Shape>> for Int<Shape> {
            type Output = Int<Shape>;

            #[track_caller]
            fn $op(self, rhs: Int<Shape>) -> Self::Output {
                assert_matching_shape(self.shape, rhs.shape);
                Int {
                    value: self.value.$op(rhs.value),
                    shape: self.shape,
                }
                .wrap_move()
            }
        }

        impl<Shape: IntShapeTrait> $Op<&'_ Int<Shape>> for Int<Shape> {
            type Output = Int<Shape>;

            #[track_caller]
            fn $op(self, rhs: &'_ Int<Shape>) -> Self::Output {
                assert_matching_shape(self.shape, rhs.shape);
                Int {
                    value: self.value.$op(&rhs.value),
                    shape: self.shape,
                }
                .wrap_move()
            }
        }

        impl<Shape: IntShapeTrait> $Op<Int<Shape>> for &'_ Int<Shape> {
            type Output = Int<Shape>;

            #[track_caller]
            fn $op(self, rhs: Int<Shape>) -> Self::Output {
                assert_matching_shape(self.shape, rhs.shape);
                Int {
                    value: (&self.value).$op(rhs.value),
                    shape: self.shape,
                }
                .wrap_move()
            }
        }

        impl<'a, 'b, Shape: IntShapeTrait> $Op<&'a Int<Shape>> for &'b Int<Shape> {
            type Output = Int<Shape>;

            #[track_caller]
            fn $op(self, rhs: &Int<Shape>) -> Self::Output {
                assert_matching_shape(self.shape, rhs.shape);
                Int {
                    value: (&self.value).$op(&rhs.value),
                    shape: self.shape,
                }
                .wrap_move()
            }
        }
    };
}

macro_rules! impl_bin_op_prim {
    ($Op:ident, $op:ident, $OpAssign:ident, $op_assign:ident, $fallback_prim:expr, $prim:ident, $Shape:ident) => {
        impl $OpAssign<&'_ $prim> for Int<$Shape> {
            fn $op_assign(&mut self, rhs: &'_ $prim) {
                self.value.$op_assign($fallback_prim(*rhs));
                self.wrap_mut();
            }
        }

        impl $OpAssign<$prim> for Int<$Shape> {
            fn $op_assign(&mut self, rhs: $prim) {
                self.value.$op_assign($fallback_prim(rhs));
                self.wrap_mut();
            }
        }

        impl $Op<Int<$Shape>> for $prim {
            type Output = Int<$Shape>;

            fn $op(self, rhs: Int<$Shape>) -> Self::Output {
                Int {
                    value: $Op::$op($fallback_prim(self), rhs.value),
                    shape: rhs.shape,
                }
                .wrap_move()
            }
        }

        impl $Op<Int<$Shape>> for &'_ $prim {
            type Output = Int<$Shape>;

            fn $op(self, rhs: Int<$Shape>) -> Self::Output {
                Int {
                    value: $Op::$op($fallback_prim(*self), rhs.value),
                    shape: rhs.shape,
                }
                .wrap_move()
            }
        }

        impl $Op<&'_ Int<$Shape>> for $prim {
            type Output = Int<$Shape>;

            fn $op(self, rhs: &Int<$Shape>) -> Self::Output {
                Int {
                    value: $Op::$op($fallback_prim(self), &rhs.value),
                    shape: rhs.shape,
                }
                .wrap_move()
            }
        }

        impl<'a, 'b> $Op<&'a Int<$Shape>> for &'b $prim {
            type Output = Int<$Shape>;

            fn $op(self, rhs: &Int<$Shape>) -> Self::Output {
                Int {
                    value: $Op::$op($fallback_prim(*self), &rhs.value),
                    shape: rhs.shape,
                }
                .wrap_move()
            }
        }

        impl $Op<$prim> for Int<$Shape> {
            type Output = Int<$Shape>;

            fn $op(self, rhs: $prim) -> Self::Output {
                Int {
                    value: self.value.$op($fallback_prim(rhs)),
                    shape: self.shape,
                }
                .wrap_move()
            }
        }

        impl $Op<$prim> for &'_ Int<$Shape> {
            type Output = Int<$Shape>;

            fn $op(self, rhs: $prim) -> Self::Output {
                Int {
                    value: (&self.value).$op($fallback_prim(rhs)),
                    shape: self.shape,
                }
                .wrap_move()
            }
        }

        impl $Op<&'_ $prim> for Int<$Shape> {
            type Output = Int<$Shape>;

            fn $op(self, rhs: &$prim) -> Self::Output {
                Int {
                    value: self.value.$op($fallback_prim(*rhs)),
                    shape: self.shape,
                }
                .wrap_move()
            }
        }

        impl<'a, 'b> $Op<&'a $prim> for &'b Int<$Shape> {
            type Output = Int<$Shape>;

            fn $op(self, rhs: &$prim) -> Self::Output {
                Int {
                    value: (&self.value).$op($fallback_prim(*rhs)),
                    shape: self.shape,
                }
                .wrap_move()
            }
        }
    };
}

macro_rules! impl_bin_ops {
    ($m:ident!($($args:tt)*)) => {
        $m!(Add, add, AddAssign, add_assign, identity, $($args)*);
        // convert primitives to BigInt before bitwise operations as workaround for
        // https://github.com/rust-num/num-bigint/issues/205
        $m!(BitAnd, bitand, BitAndAssign, bitand_assign, BigInt::from, $($args)*);
        $m!(BitOr, bitor, BitOrAssign, bitor_assign, BigInt::from, $($args)*);
        $m!(BitXor, bitxor, BitXorAssign, bitxor_assign, BigInt::from, $($args)*);
        $m!(Mul, mul, MulAssign, mul_assign, identity, $($args)*);
        $m!(Sub, sub, SubAssign, sub_assign, identity, $($args)*);
    };
}

impl_bin_ops!(impl_bin_op_int!());

macro_rules! impl_prim {
    ($prim:ident, $Shape:ident, $to_prim:ident) => {
        impl_bin_ops!(impl_bin_op_prim!($prim, $Shape));

        impl From<Int<$Shape>> for $prim {
            fn from(v: Int<$Shape>) -> Self {
                v.value.$to_prim().unwrap()
            }
        }

        impl From<$prim> for Int<$Shape> {
            fn from(v: $prim) -> Self {
                Self::unchecked_new(v)
            }
        }
    };
}

impl_prim!(u8, U8Shape, to_u8);
impl_prim!(i8, I8Shape, to_i8);
impl_prim!(u16, U16Shape, to_u16);
impl_prim!(i16, I16Shape, to_i16);
impl_prim!(u32, U32Shape, to_u32);
impl_prim!(i32, I32Shape, to_i32);
impl_prim!(u64, U64Shape, to_u64);
impl_prim!(i64, I64Shape, to_i64);
impl_prim!(u128, U128Shape, to_u128);
impl_prim!(i128, I128Shape, to_i128);

pub type UInt<const BIT_COUNT: u32> = Int<UIntShape<BIT_COUNT>>;
pub type SInt<const BIT_COUNT: u32> = Int<SIntShape<BIT_COUNT>>;
pub type UInt1 = Int<U1Shape>;
pub type Int1 = Int<I1Shape>;
pub type UInt8 = Int<U8Shape>;
pub type Int8 = Int<I8Shape>;
pub type UInt16 = Int<U16Shape>;
pub type Int16 = Int<I16Shape>;
pub type UInt32 = Int<U32Shape>;
pub type Int32 = Int<I32Shape>;
pub type UInt64 = Int<U64Shape>;
pub type Int64 = Int<I64Shape>;
pub type UInt128 = Int<U128Shape>;
pub type Int128 = Int<I128Shape>;

#[cfg(test)]
mod tests {
    use super::*;
    use std::{format, prelude::v1::*};

    #[test]
    fn test_format() {
        assert_eq!(Int::from(0x34u8).to_string(), "0x34_u8");
        assert_eq!(Int::from(0x123456EFu32).to_string(), "0x123456EF_u32");
        assert_eq!(Int::from(-0x2934673i32).to_string(), "-0x2934673_i32");
        assert_eq!(
            Int::from(-2934673i32).display_decimal().to_string(),
            "-2934673_i32"
        );
        assert_eq!(format!("{:x}", Int::from(0x123456EFu32)), "0x123456ef_u32");
        assert_eq!(format!("{:X}", Int::from(0x123456EFu32)), "0x123456EF_u32");
        assert_eq!(format!("{:b}", Int::from(0b10101110u8)), "0b10101110_u8");
        assert_eq!(format!("{:o}", Int::from(0o257323u32)), "0o257323_u32");
    }
}
