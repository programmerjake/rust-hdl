// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Intern, Interned},
    ir::types::{IrValueType, IrValueTypeRef},
    values::integer::{Int, IntShape, IntShapeTrait},
};
use core::{convert::TryInto, fmt};
use num_bigint::BigUint;
use num_traits::Zero;

use super::logic::IrWireValue;

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
                write!(f, "{:#x}", self.this.value)
            }
        }
        f.debug_struct("LiteralBits")
            .field("bit_count", &self.bit_count)
            .field("value", &DebugAsHex { this: self })
            .finish()
    }
}

impl LiteralBits {
    pub fn new() -> Self {
        Self {
            bit_count: 0,
            value: BigUint::zero(),
        }
    }
    pub fn to_int(self, signed: bool) -> Int {
        let LiteralBits { value, bit_count } = self;
        Int::unchecked_new_with_shape(value, IntShape { bit_count, signed })
    }
    pub fn value(&self) -> &BigUint {
        &self.value
    }
    pub fn into_value(self) -> BigUint {
        self.value
    }
    pub fn bit_count(&self) -> u32 {
        self.bit_count
    }
}

impl Default for LiteralBits {
    fn default() -> Self {
        Self::new()
    }
}

impl<Shape: IntShapeTrait> From<Int<Shape>> for LiteralBits {
    fn from(value: Int<Shape>) -> Self {
        let value = value.wrap_to_unsigned();
        let IntShape { bit_count, .. } = value.shape().shape();
        let value = value.into_value().try_into().unwrap();
        Self { value, bit_count }
    }
}

impl From<LiteralBits> for IrValue<'_> {
    fn from(v: LiteralBits) -> Self {
        Self::LiteralBits(v)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LiteralArray<'ctx> {
    element_type: IrValueTypeRef<'ctx>,
    elements: Interned<'ctx, [IrValueRef<'ctx>]>,
}

impl<'ctx> LiteralArray<'ctx> {
    pub fn new(
        ctx: ContextRef<'ctx>,
        element_type: IrValueTypeRef<'ctx>,
        elements: impl AsRef<[IrValueRef<'ctx>]>,
    ) -> Self {
        let elements = elements.as_ref();
        for element in elements {
            assert_eq!(element.get_type(ctx), element_type);
        }
        let elements = elements.intern(ctx);
        Self {
            element_type,
            elements,
        }
    }
    pub fn element_type(&self) -> IrValueTypeRef<'ctx> {
        self.element_type
    }
    pub fn len(&self) -> usize {
        self.elements.len()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn elements(&self) -> Interned<'ctx, [IrValueRef<'ctx>]> {
        self.elements
    }
}

impl<'ctx> From<LiteralArray<'ctx>> for IrValue<'ctx> {
    fn from(v: LiteralArray<'ctx>) -> Self {
        Self::LiteralArray(v)
    }
}

impl<'ctx> From<IrWireValue<'ctx>> for IrValue<'ctx> {
    fn from(v: IrWireValue<'ctx>) -> Self {
        Self::WireValue(v)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum IrValue<'ctx> {
    LiteralBits(LiteralBits),
    LiteralArray(LiteralArray<'ctx>),
    WireValue(IrWireValue<'ctx>),
}

pub type IrValueRef<'ctx> = Interned<'ctx, IrValue<'ctx>>;

fn assert_copyable<T: Copy>() {}

#[allow(dead_code)]
fn check_types() {
    assert_copyable::<IrValueRef<'static>>();
}

impl<'ctx> IrValue<'ctx> {
    pub fn get_type(&self, ctx: ContextRef<'ctx>) -> IrValueTypeRef<'ctx> {
        match self {
            &IrValue::LiteralBits(LiteralBits { bit_count, .. }) => {
                IrValueType::BitVector { bit_count }.intern(ctx)
            }
            IrValue::LiteralArray(v) => IrValueType::Array {
                element: v.element_type(),
                length: v.len(),
            }
            .intern(ctx),
            IrValue::WireValue(wire_value) => wire_value.0.value_type(),
        }
    }
}
