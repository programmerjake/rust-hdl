// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Intern, Interned},
    ir::{
        io::{IrModuleInput, IrOutputRead},
        logic::IrWireRead,
        module::IrModuleRef,
        types::{IrStructFieldType, IrStructType, IrValueType, IrValueTypeRef},
    },
    values::integer::{Int, IntShape, IntShapeTrait},
};
use alloc::vec::Vec;
use core::{convert::TryInto, fmt};
use num_bigint::BigUint;
use num_traits::Zero;

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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LiteralArray<'ctx> {
    element_type: IrValueTypeRef<'ctx>,
    owning_module: Option<IrModuleRef<'ctx>>,
    elements: Interned<'ctx, [IrValueRef<'ctx>]>,
}

impl<'ctx> LiteralArray<'ctx> {
    pub fn new(
        ctx: ContextRef<'ctx>,
        element_type: IrValueTypeRef<'ctx>,
        elements: impl AsRef<[IrValueRef<'ctx>]>,
    ) -> Self {
        let elements = elements.as_ref();
        let mut owning_module = None;
        for element in elements {
            assert_eq!(element.get_type(ctx), element_type);
            if let Some(element_owning_module) = element.owning_module() {
                match owning_module {
                    Some(owning_module) => assert_eq!(element_owning_module, owning_module),
                    None => owning_module = Some(element_owning_module),
                }
            }
        }
        let elements = elements.intern(ctx);
        Self {
            element_type,
            owning_module,
            elements,
        }
    }
    pub fn element_type(self) -> IrValueTypeRef<'ctx> {
        self.element_type
    }
    pub fn owning_module(self) -> Option<IrModuleRef<'ctx>> {
        self.owning_module
    }
    pub fn len(self) -> usize {
        self.elements.len()
    }
    pub fn is_empty(self) -> bool {
        self.len() == 0
    }
    pub fn elements(self) -> Interned<'ctx, [IrValueRef<'ctx>]> {
        self.elements
    }
}

impl<'ctx> From<LiteralArray<'ctx>> for IrValue<'ctx> {
    fn from(v: LiteralArray<'ctx>) -> Self {
        Self::LiteralArray(v)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LiteralStructField<'ctx> {
    pub name: Interned<'ctx, str>,
    pub value: IrValueRef<'ctx>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LiteralStruct<'ctx> {
    ty: IrStructType<'ctx>,
    owning_module: Option<IrModuleRef<'ctx>>,
    fields: Interned<'ctx, [LiteralStructField<'ctx>]>,
}

impl<'ctx> LiteralStruct<'ctx> {
    pub fn new(ctx: ContextRef<'ctx>, fields: impl AsRef<[LiteralStructField<'ctx>]>) -> Self {
        let fields = fields.as_ref();
        let mut field_types = Vec::with_capacity(fields.len());
        let mut owning_module = None;
        for field in fields {
            let field_type = IrStructFieldType {
                name: field.name,
                ty: field.value.get_type(ctx),
            };
            field_types.push(field_type);
            if let Some(element_owning_module) = field.value.owning_module() {
                match owning_module {
                    Some(owning_module) => assert_eq!(element_owning_module, owning_module),
                    None => owning_module = Some(element_owning_module),
                }
            }
        }
        let fields = fields.intern(ctx);
        let field_types = field_types.intern(ctx);
        Self {
            ty: IrStructType {
                fields: field_types,
            },
            owning_module,
            fields,
        }
    }
    pub fn ty(self) -> IrStructType<'ctx> {
        self.ty
    }
    pub fn owning_module(self) -> Option<IrModuleRef<'ctx>> {
        self.owning_module
    }
    pub fn fields(self) -> Interned<'ctx, [LiteralStructField<'ctx>]> {
        self.fields
    }
}

impl<'ctx> From<LiteralStruct<'ctx>> for IrValue<'ctx> {
    fn from(v: LiteralStruct<'ctx>) -> Self {
        Self::LiteralStruct(v)
    }
}

impl<'ctx> From<IrWireRead<'ctx>> for IrValue<'ctx> {
    fn from(v: IrWireRead<'ctx>) -> Self {
        Self::WireRead(v)
    }
}

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum IrValue<'ctx> {
    LiteralBits(LiteralBits),
    LiteralArray(LiteralArray<'ctx>),
    LiteralStruct(LiteralStruct<'ctx>),
    WireRead(IrWireRead<'ctx>),
    Input(IrModuleInput<'ctx>),
    OutputRead(IrOutputRead<'ctx>),
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
            IrValue::LiteralStruct(v) => IrValueType::from(v.ty()).intern(ctx),
            IrValue::WireRead(v) => v.0.value_type(),
            IrValue::Input(v) => v.value_type(),
            IrValue::OutputRead(v) => v.0.value_type(),
        }
    }
    pub fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        match self {
            IrValue::LiteralBits(_) => None,
            IrValue::LiteralArray(array) => array.owning_module(),
            IrValue::LiteralStruct(s) => s.owning_module(),
            IrValue::WireRead(wire) => Some(wire.0.module()),
            IrValue::Input(input) => Some(input.module()),
            IrValue::OutputRead(output) => Some(output.0.module()),
        }
    }
}

impl fmt::Debug for IrValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValue::LiteralBits(v) => v.fmt(f),
            IrValue::LiteralArray(v) => v.fmt(f),
            IrValue::LiteralStruct(v) => v.fmt(f),
            IrValue::WireRead(v) => v.fmt(f),
            IrValue::Input(v) => v.fmt(f),
            IrValue::OutputRead(v) => v.fmt(f),
        }
    }
}
