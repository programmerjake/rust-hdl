// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Intern, Interned},
    ir::{
        io::{IrModuleInput, IrOutputRead},
        logic::{IrRegOutput, IrWireRead},
        module::{combine_owning_modules, IrModuleRef, OwningModule},
        types::{
            IrArrayType, IrBitVectorType, IrStructFieldType, IrStructType, IrValueType,
            IrValueTypeRef,
        },
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

impl<'ctx> OwningModule<'ctx> for LiteralBits {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        None
    }
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
    pub fn value_type(&self) -> IrBitVectorType {
        IrBitVectorType {
            bit_count: self.bit_count,
        }
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

impl<'ctx> OwningModule<'ctx> for LiteralArray<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.owning_module
    }
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
        let elements: Interned<'_, [_]> = elements.intern(ctx);
        Self {
            element_type,
            owning_module: combine_owning_modules(elements.iter()),
            elements,
        }
    }
    pub fn element_type(self) -> IrValueTypeRef<'ctx> {
        self.element_type
    }
    pub fn len(self) -> usize {
        self.elements.len()
    }
    pub fn value_type(self) -> IrArrayType<'ctx> {
        IrArrayType {
            element: self.element_type,
            length: self.len(),
        }
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
    value_type: IrStructType<'ctx>,
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
            owning_module = combine_owning_modules([owning_module, field.value.owning_module()]);
        }
        let fields = fields.intern(ctx);
        let field_types = field_types.intern(ctx);
        Self {
            value_type: IrStructType {
                fields: field_types,
            },
            owning_module,
            fields,
        }
    }
    pub fn value_type(self) -> IrStructType<'ctx> {
        self.value_type
    }
    pub fn fields(self) -> Interned<'ctx, [LiteralStructField<'ctx>]> {
        self.fields
    }
}

impl<'ctx> OwningModule<'ctx> for LiteralStruct<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.owning_module
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

impl<'ctx> From<IrRegOutput<'ctx>> for IrValue<'ctx> {
    fn from(v: IrRegOutput<'ctx>) -> Self {
        Self::RegOutput(v)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct ExtractStructField<'ctx> {
    struct_value: IrValueRef<'ctx>,
    struct_type: IrStructType<'ctx>,
    field_index: usize,
}

impl<'ctx> ExtractStructField<'ctx> {
    pub fn new(ctx: ContextRef<'ctx>, struct_value: IrValueRef<'ctx>, field_index: usize) -> Self {
        let struct_type = match *struct_value.get_type(ctx) {
            IrValueType::Struct(v) => v,
            _ => panic!("value type is not a struct"),
        };
        assert!(field_index < struct_type.fields.len());
        Self {
            struct_value,
            struct_type,
            field_index,
        }
    }
    pub fn new_with_struct_type_unchecked(
        struct_value: IrValueRef<'ctx>,
        struct_type: IrValueTypeRef<'ctx>,
        field_index: usize,
    ) -> Self {
        let struct_type = match *struct_type {
            IrValueType::Struct(v) => v,
            _ => panic!("value type is not a struct"),
        };
        assert!(field_index < struct_type.fields.len());
        Self {
            struct_value,
            struct_type,
            field_index,
        }
    }
    pub fn struct_value(self) -> IrValueRef<'ctx> {
        self.struct_value
    }
    pub fn struct_field_type(self) -> IrStructFieldType<'ctx> {
        self.struct_type.fields[self.field_index]
    }
    pub fn value_type(self) -> IrValueTypeRef<'ctx> {
        self.struct_field_type().ty
    }
    pub fn field_name(self) -> Interned<'ctx, str> {
        self.struct_field_type().name
    }
    pub fn struct_type(self) -> IrStructType<'ctx> {
        self.struct_type
    }
    pub fn field_index(self) -> usize {
        self.field_index
    }
}

impl<'ctx> OwningModule<'ctx> for ExtractStructField<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.struct_value.owning_module()
    }
}

impl fmt::Debug for ExtractStructField<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExtractStructField")
            .field("struct_value", &self.struct_value())
            .field("struct_field_type", &self.struct_field_type())
            .field("field_index", &self.field_index())
            .finish_non_exhaustive()
    }
}

impl<'ctx> From<ExtractStructField<'ctx>> for IrValue<'ctx> {
    fn from(v: ExtractStructField<'ctx>) -> Self {
        Self::ExtractStructField(v)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct ExtractArrayElement<'ctx> {
    array_value: IrValueRef<'ctx>,
    array_type: IrArrayType<'ctx>,
    element_index: usize,
}

impl<'ctx> ExtractArrayElement<'ctx> {
    pub fn new(ctx: ContextRef<'ctx>, array_value: IrValueRef<'ctx>, element_index: usize) -> Self {
        let array_type = match *array_value.get_type(ctx) {
            IrValueType::Array(v) => v,
            _ => panic!("value type is not a array"),
        };
        assert!(element_index < array_type.length);
        Self {
            array_value,
            array_type,
            element_index,
        }
    }
    pub fn new_with_array_type_unchecked(
        array_value: IrValueRef<'ctx>,
        array_type: IrValueTypeRef<'ctx>,
        element_index: usize,
    ) -> Self {
        let array_type = match *array_type {
            IrValueType::Array(v) => v,
            _ => panic!("value type is not a array"),
        };
        assert!(element_index < array_type.length);
        Self {
            array_value,
            array_type,
            element_index,
        }
    }
    pub fn array_value(self) -> IrValueRef<'ctx> {
        self.array_value
    }
    pub fn value_type(self) -> IrValueTypeRef<'ctx> {
        self.array_type.element
    }
    pub fn array_type(self) -> IrArrayType<'ctx> {
        self.array_type
    }
    pub fn element_index(self) -> usize {
        self.element_index
    }
}

impl<'ctx> OwningModule<'ctx> for ExtractArrayElement<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.array_value.owning_module()
    }
}

impl fmt::Debug for ExtractArrayElement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExtractArrayElement")
            .field("array_value", &self.array_value())
            .field("value_type", &self.value_type())
            .field("element_index", &self.element_index())
            .finish_non_exhaustive()
    }
}

impl<'ctx> From<ExtractArrayElement<'ctx>> for IrValue<'ctx> {
    fn from(v: ExtractArrayElement<'ctx>) -> Self {
        Self::ExtractArrayElement(v)
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct Mux<'ctx> {
    condition: IrValueRef<'ctx>,
    value_type: IrValueTypeRef<'ctx>,
    owning_module: Option<IrModuleRef<'ctx>>,
    true_value: IrValueRef<'ctx>,
    false_value: IrValueRef<'ctx>,
}

impl<'ctx> Mux<'ctx> {
    pub fn new(
        ctx: ContextRef<'ctx>,
        condition: IrValueRef<'ctx>,
        true_value: IrValueRef<'ctx>,
        false_value: IrValueRef<'ctx>,
    ) -> Self {
        assert!(condition.get_type(ctx).is_bool());
        let value_type = true_value.get_type(ctx);
        assert_eq!(value_type, false_value.get_type(ctx));
        let owning_module = combine_owning_modules([condition, true_value, false_value]);
        Self {
            condition,
            value_type,
            owning_module,
            true_value,
            false_value,
        }
    }
    pub fn condition(self) -> IrValueRef<'ctx> {
        self.condition
    }
    pub fn value_type(self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn true_value(self) -> IrValueRef<'ctx> {
        self.true_value
    }
    pub fn false_value(self) -> IrValueRef<'ctx> {
        self.false_value
    }
}

impl<'ctx> OwningModule<'ctx> for Mux<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.owning_module
    }
}

impl<'ctx> From<Mux<'ctx>> for IrValue<'ctx> {
    fn from(v: Mux<'ctx>) -> Self {
        IrValue::Mux(v)
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
    ExtractStructField(ExtractStructField<'ctx>),
    ExtractArrayElement(ExtractArrayElement<'ctx>),
    RegOutput(IrRegOutput<'ctx>),
    Mux(Mux<'ctx>),
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
            IrValue::LiteralBits(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::LiteralArray(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::LiteralStruct(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::WireRead(v) => v.0.value_type(),
            IrValue::Input(v) => v.value_type(),
            IrValue::OutputRead(v) => v.0.value_type(),
            IrValue::ExtractStructField(v) => v.value_type(),
            IrValue::ExtractArrayElement(v) => v.value_type(),
            IrValue::RegOutput(v) => v.0.value_type(),
            IrValue::Mux(v) => v.value_type(),
        }
    }
}

impl<'ctx> OwningModule<'ctx> for IrValue<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        match self {
            IrValue::LiteralBits(v) => v.owning_module(),
            IrValue::LiteralArray(v) => v.owning_module(),
            IrValue::LiteralStruct(v) => v.owning_module(),
            IrValue::WireRead(v) => v.owning_module(),
            IrValue::Input(v) => v.owning_module(),
            IrValue::OutputRead(v) => v.owning_module(),
            IrValue::ExtractStructField(v) => v.owning_module(),
            IrValue::ExtractArrayElement(v) => v.owning_module(),
            IrValue::RegOutput(v) => v.owning_module(),
            IrValue::Mux(v) => v.owning_module(),
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
            IrValue::ExtractStructField(v) => v.fmt(f),
            IrValue::ExtractArrayElement(v) => v.fmt(f),
            IrValue::RegOutput(v) => v.fmt(f),
            IrValue::Mux(v) => v.fmt(f),
        }
    }
}
