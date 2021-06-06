// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use super::ValueType;
use core::fmt;
use num_bigint::BigUint;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IntShape {
    pub bit_count: usize,
    pub signed: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct ConstIntShape<const BIT_COUNT: usize, const SIGNED: bool>;

pub trait IntShapeTrait: Clone + fmt::Debug {
    fn shape(&self) -> IntShape;
}

impl IntShapeTrait for IntShape {
    fn shape(&self) -> IntShape {
        *self
    }
}

impl<const BIT_COUNT: usize, const SIGNED: bool> IntShapeTrait
    for ConstIntShape<BIT_COUNT, SIGNED>
{
    fn shape(&self) -> IntShape {
        IntShape {
            bit_count: BIT_COUNT,
            signed: SIGNED,
        }
    }
}

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

#[derive(Clone, Debug)]
pub struct Int<Shape: IntShapeTrait = IntShape> {
    value: BigUint,
    shape: Shape,
}

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

impl<Shape: IntShapeTrait> ValueType for Int<Shape> {}
