// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information
#![no_std]

use core::{
    cmp::Ordering,
    convert::identity,
    fmt,
    hash::{Hash, Hasher},
    num::ParseIntError,
    ops::{
        Add, AddAssign, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Mul,
        MulAssign, Neg, Not, Sub, SubAssign,
    },
    str::FromStr,
};
use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

#[cfg(test)]
#[macro_use]
extern crate std;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IntShape {
    pub bit_count: u32,
    pub signed: bool,
}

impl IntShape {
    #[must_use]
    pub const fn wrap_i128(self, value: i128) -> i128 {
        if self.bit_count >= i128::BITS {
            value
        } else if self.bit_count == 0 {
            0
        } else {
            let shift_amount = i128::BITS - self.bit_count;
            let shifted_left = value.wrapping_shl(shift_amount);
            if self.signed {
                shifted_left >> shift_amount
            } else {
                (shifted_left as u128 >> shift_amount) as i128
            }
        }
    }
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

#[derive(Debug, Clone)]
pub enum IntShapeParseError {
    InvalidSignedness,
    InvalidBitCount(ParseIntError),
}

impl fmt::Display for IntShapeParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntShapeParseError::InvalidSignedness => {
                write!(f, "invalid signedness: expected `u` or `i`")
            }
            IntShapeParseError::InvalidBitCount(v) => write!(f, "invalid bit-count: {}", v),
        }
    }
}

impl FromStr for IntShape {
    type Err = IntShapeParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let signed = match s.as_bytes().get(0) {
            Some(b'i') => true,
            Some(b'u') => false,
            _ => return Err(IntShapeParseError::InvalidSignedness),
        };
        let s = &s[1..];
        if !s.bytes().all(|b| b.is_ascii_digit()) {
            return Err(IntShapeParseError::InvalidBitCount(
                "a".parse::<u32>().unwrap_err(),
            ));
        }
        let bit_count = s.parse().map_err(IntShapeParseError::InvalidBitCount)?;
        Ok(Self { bit_count, signed })
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstIntShape<const BIT_COUNT: u32, const SIGNED: bool>;

pub trait IntShapeTrait: Copy + fmt::Debug + 'static + Send + Sync {
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

pub trait FixedIntShape: IntShapeTrait + Default {}

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

impl<const BIT_COUNT: u32, const SIGNED: bool> FixedIntShape for ConstIntShape<BIT_COUNT, SIGNED> {}

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
    pub fn new_with_shape<T: Into<BigInt>>(value: T, shape: Shape) -> Option<Self> {
        let value = value.into();
        let retval = Self::wrapping_with_shape(value.clone(), shape);
        if retval.value != value {
            None
        } else {
            Some(retval)
        }
    }
    pub fn new<T: Into<BigInt>>(value: T) -> Option<Self>
    where
        Shape: Default,
    {
        Self::new_with_shape(value, Shape::default())
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
    pub fn cmp_value(&self, rhs: &Self) -> Ordering {
        self.value.cmp(&rhs.value)
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

impl From<()> for UInt<0> {
    fn from(_: ()) -> Self {
        Int::unchecked_new(0)
    }
}

impl From<()> for SInt<0> {
    fn from(_: ()) -> Self {
        Int::unchecked_new(0)
    }
}

impl From<UInt<0>> for () {
    fn from(_: UInt<0>) -> Self {
        ()
    }
}

impl From<SInt<0>> for () {
    fn from(_: SInt<0>) -> Self {
        ()
    }
}

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
    use std::prelude::v1::*;

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

    macro_rules! assert_matches {
        ($v:expr, $pat:pat $(,)?) => {{
            match $v {
                $pat => {}
                v => panic!(
                    "assertion failed: `(left matches right)`\n  left: `{:?}`,\n right: `{}`",
                    v,
                    stringify!($pat)
                ),
            }
        }};
    }

    #[test]
    fn test_int_shape() {
        assert_eq!(
            IntShape {
                bit_count: 1234,
                signed: false,
            }
            .to_string(),
            "u1234",
        );
        assert_eq!(
            IntShape {
                bit_count: 5678,
                signed: true,
            }
            .to_string(),
            "i5678",
        );
        assert_eq!(
            IntShape {
                bit_count: 0,
                signed: true,
            }
            .to_string(),
            "i0",
        );
        assert_eq!(
            IntShape {
                bit_count: 0,
                signed: false,
            }
            .to_string(),
            "u0",
        );
        assert_matches!(
            "i3".parse(),
            Ok(IntShape {
                bit_count: 3,
                signed: true,
            }),
        );
        assert_matches!(
            "i0".parse(),
            Ok(IntShape {
                bit_count: 0,
                signed: true,
            }),
        );
        assert_matches!(
            "u12345".parse(),
            Ok(IntShape {
                bit_count: 12345,
                signed: false,
            }),
        );
        assert_matches!(
            "u+1".parse::<IntShape>(),
            Err(IntShapeParseError::InvalidBitCount(_)),
        );
        assert_matches!(
            "u-1".parse::<IntShape>(),
            Err(IntShapeParseError::InvalidBitCount(_)),
        );
        assert_matches!(
            "a".parse::<IntShape>(),
            Err(IntShapeParseError::InvalidSignedness),
        );
        assert_matches!(
            "".parse::<IntShape>(),
            Err(IntShapeParseError::InvalidSignedness),
        );
        assert_matches!(
            "U45".parse::<IntShape>(),
            Err(IntShapeParseError::InvalidSignedness),
        );
        assert_matches!(
            "u".parse::<IntShape>(),
            Err(IntShapeParseError::InvalidBitCount(_)),
        );
        assert_matches!(
            "u12345678901234567890".parse::<IntShape>(),
            Err(IntShapeParseError::InvalidBitCount(_)),
        );
    }
}
