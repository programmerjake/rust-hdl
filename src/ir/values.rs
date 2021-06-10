// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Interned},
    ir::types::IrValueType,
    types::ints::{Int, IntShape, IntShapeTrait},
};
use core::{convert::TryInto, fmt};
use num_bigint::BigUint;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralBits {
    bit_count: u32,
    value: BigUint,
}

impl fmt::Debug for LiteralBits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct DebugAsHex<'a> {
            this: &'a LiteralBits,
        }
        impl fmt::Debug for DebugAsHex<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:#x}", self.this.0)
            }
        }
        f.debug_struct("LiteralBits")
            .field("bit_count", &self.bit_count)
            .field("value", DebugAsHex { this: self })
            .finish()
    }
}

impl LiteralBits {
    pub fn to_int(self, signed: bool) -> Int {
        let LiteralBits { value, bit_count } = self;
        Int::unchecked_new_with_shape(value, IntShape { bit_count, signed })
    }
}

impl<Shape: IntShapeTrait> From<Int<Shape>> for LiteralBits {
    fn from(value: Int<Shape>) -> Self {
        let value = value.wrap_to_unsigned();
        let IntShape { bit_count, .. } = value.shape();
        let value = value.into_value().try_into().unwrap();
        Self { value, bit_count }
    }
}

impl From<LiteralBits> for IrValue {
    fn from(v: LiteralBits) -> Self {
        Self::LiteralBits(v)
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum IrValue {
    LiteralBits(LiteralBits),
}

impl fmt::Debug for IrValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValue::LiteralBits(v) => v.fmt(f),
        }
    }
}

impl IrValue {
    pub fn get_type<'ctx>(&self, ctx: ContextRef<'ctx>) -> Interned<'ctx, IrValueType> {}
}
