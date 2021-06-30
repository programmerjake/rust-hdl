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
use core::{fmt, ops::Range};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralBits {
    value: Int,
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
            .field("value", &DebugAsHex { this: self })
            .finish()
    }
}

impl LiteralBits {
    pub fn new() -> Self {
        Self {
            value: Int::wrapping_with_shape(
                0,
                IntShape {
                    bit_count: 0,
                    signed: false,
                },
            ),
        }
    }
    pub fn new_bool(v: bool) -> Self {
        Self {
            value: Int::wrapping_with_shape(
                v as u8,
                IntShape {
                    bit_count: 1,
                    signed: false,
                },
            ),
        }
    }
    pub fn value(&self) -> &Int {
        &self.value
    }
    pub fn value_type(&self) -> IrBitVectorType {
        IrBitVectorType {
            bit_count: self.value.shape().bit_count,
            signed: self.value.shape().signed,
        }
    }
    pub fn into_value(self) -> Int {
        self.value
    }
    pub fn bit_count(&self) -> u32 {
        self.value.shape().bit_count
    }
    pub fn signed(&self) -> bool {
        self.value.shape().signed
    }
}

impl Default for LiteralBits {
    fn default() -> Self {
        Self::new()
    }
}

impl<Shape: IntShapeTrait> From<Int<Shape>> for LiteralBits {
    fn from(value: Int<Shape>) -> Self {
        Self {
            value: value.into_dyn(),
        }
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
        Self::new_with_struct_type_unchecked(struct_value, struct_value.get_type(ctx), field_index)
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
        Self::new_with_array_type_unchecked(array_value, array_value.get_type(ctx), element_index)
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

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct SliceArray<'ctx> {
    array_value: IrValueRef<'ctx>,
    array_type: IrArrayType<'ctx>,
    element_start_index: usize,
    element_end_index: usize,
}

impl<'ctx> SliceArray<'ctx> {
    pub fn new(
        ctx: ContextRef<'ctx>,
        array_value: IrValueRef<'ctx>,
        element_indexes: Range<usize>,
    ) -> Self {
        Self::new_with_array_type_unchecked(array_value, array_value.get_type(ctx), element_indexes)
    }
    pub fn new_with_array_type_unchecked(
        array_value: IrValueRef<'ctx>,
        array_type: IrValueTypeRef<'ctx>,
        element_indexes: Range<usize>,
    ) -> Self {
        let array_type = match *array_type {
            IrValueType::Array(v) => v,
            _ => panic!("value type is not a array"),
        };
        let Range {
            start: element_start_index,
            end: element_end_index,
        } = element_indexes;
        assert!(element_end_index <= array_type.length);
        assert!(element_start_index <= element_end_index);
        Self {
            array_value,
            array_type,
            element_start_index,
            element_end_index,
        }
    }
    pub fn array_value(self) -> IrValueRef<'ctx> {
        self.array_value
    }
    pub fn value_type(self) -> IrArrayType<'ctx> {
        IrArrayType {
            element: self.array_type.element,
            length: self.element_indexes().len(),
        }
    }
    pub fn array_type(self) -> IrArrayType<'ctx> {
        self.array_type
    }
    pub fn element_indexes(self) -> Range<usize> {
        self.element_start_index..self.element_end_index
    }
}

impl<'ctx> OwningModule<'ctx> for SliceArray<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.array_value.owning_module()
    }
}

impl fmt::Debug for SliceArray<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SliceArray")
            .field("array_value", &self.array_value())
            .field("value_type", &self.value_type())
            .field("element_indexes", &self.element_indexes())
            .finish_non_exhaustive()
    }
}

impl<'ctx> From<SliceArray<'ctx>> for IrValue<'ctx> {
    fn from(v: SliceArray<'ctx>) -> Self {
        Self::SliceArray(v)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct SliceBitVector<'ctx> {
    base_value: IrValueRef<'ctx>,
    base_type: IrBitVectorType,
    /// first bit index in LSB0 order
    bit_start_index: u32,
    /// one-after-end bit index in LSB0 order
    bit_end_index: u32,
}

impl<'ctx> SliceBitVector<'ctx> {
    /// bit_indexes is in LSB0 order
    #[track_caller]
    pub fn new(
        ctx: ContextRef<'ctx>,
        base_value: IrValueRef<'ctx>,
        bit_indexes: Range<u32>,
    ) -> Self {
        Self::new_with_bit_vector_type_unchecked(base_value, base_value.get_type(ctx), bit_indexes)
    }
    /// bit_indexes is in LSB0 order
    #[track_caller]
    pub fn new_with_bit_vector_type_unchecked(
        base_value: IrValueRef<'ctx>,
        base_type: IrValueTypeRef<'ctx>,
        bit_indexes: Range<u32>,
    ) -> Self {
        let base_type = base_type
            .bit_vector()
            .expect("base value type is not a bit vector");
        let Range {
            start: bit_start_index,
            end: bit_end_index,
        } = bit_indexes;
        assert!(bit_end_index <= base_type.bit_count);
        assert!(bit_start_index <= bit_end_index);
        Self {
            base_value,
            base_type,
            bit_start_index,
            bit_end_index,
        }
    }
    pub fn base_value(self) -> IrValueRef<'ctx> {
        self.base_value
    }
    pub fn value_type(self) -> IrBitVectorType {
        IrBitVectorType {
            bit_count: self.bit_end_index - self.bit_start_index,
            signed: false,
        }
    }
    pub fn base_type(self) -> IrBitVectorType {
        self.base_type
    }
    /// bit_indexes is in LSB0 order
    pub fn bit_indexes(self) -> Range<u32> {
        self.bit_start_index..self.bit_end_index
    }
}

impl<'ctx> OwningModule<'ctx> for SliceBitVector<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.base_value.owning_module()
    }
}

impl fmt::Debug for SliceBitVector<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SliceBitVector")
            .field("base_value", &self.base_value())
            .field("value_type", &self.value_type())
            .field("bit_indexes", &self.bit_indexes())
            .finish_non_exhaustive()
    }
}

impl<'ctx> From<SliceBitVector<'ctx>> for IrValue<'ctx> {
    fn from(v: SliceBitVector<'ctx>) -> Self {
        Self::SliceBitVector(v)
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

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct ConcatBitVectors<'ctx> {
    value_type: IrBitVectorType,
    owning_module: Option<IrModuleRef<'ctx>>,
    bit_vectors: Interned<'ctx, [IrValueRef<'ctx>]>,
}

impl<'ctx> ConcatBitVectors<'ctx> {
    #[track_caller]
    pub fn new(ctx: ContextRef<'ctx>, bit_vectors: impl AsRef<[IrValueRef<'ctx>]>) -> Self {
        let bit_vectors = bit_vectors.as_ref();
        let mut value_type = IrBitVectorType {
            bit_count: 0,
            signed: false,
        };
        for bit_vector in bit_vectors {
            let IrBitVectorType { bit_count, .. } = bit_vector
                .get_type(ctx)
                .bit_vector()
                .expect("input type must be a bit vector");
            value_type.bit_count = value_type
                .bit_count
                .checked_add(bit_count)
                .expect("too many bits in bit vector");
        }
        let owning_module = combine_owning_modules(bit_vectors);
        Self {
            value_type,
            owning_module,
            bit_vectors: bit_vectors.intern(ctx),
        }
    }
    pub fn value_type(self) -> IrBitVectorType {
        self.value_type
    }
    pub fn bit_vectors(self) -> Interned<'ctx, [IrValueRef<'ctx>]> {
        self.bit_vectors
    }
}

impl<'ctx> OwningModule<'ctx> for ConcatBitVectors<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.owning_module
    }
}

impl<'ctx> From<ConcatBitVectors<'ctx>> for IrValue<'ctx> {
    fn from(v: ConcatBitVectors<'ctx>) -> Self {
        IrValue::ConcatBitVectors(v)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SameSizeBinOpKind {
    Add,
    Sub,
    Mul,
    And,
    Or,
    Xor,
    ShiftLeft,
    LogicalShiftRight,
    ArithmeticShiftRight,
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct SameSizeBinOp<'ctx> {
    kind: SameSizeBinOpKind,
    value_type: IrBitVectorType,
    owning_module: Option<IrModuleRef<'ctx>>,
    lhs: IrValueRef<'ctx>,
    rhs: IrValueRef<'ctx>,
}

impl<'ctx> SameSizeBinOp<'ctx> {
    /// for all binary operations, both inputs and the output type must match
    #[track_caller]
    pub fn new(
        ctx: ContextRef<'ctx>,
        kind: SameSizeBinOpKind,
        lhs: IrValueRef<'ctx>,
        rhs: IrValueRef<'ctx>,
    ) -> Self {
        let value_type = lhs
            .get_type(ctx)
            .bit_vector()
            .expect("lhs type must be a bit vector");
        let rhs_value_type = rhs
            .get_type(ctx)
            .bit_vector()
            .expect("rhs type must be a bit vector");
        assert_eq!(value_type, rhs_value_type);
        let owning_module = combine_owning_modules([lhs, rhs]);
        Self {
            kind,
            value_type,
            owning_module,
            lhs,
            rhs,
        }
    }
    pub fn kind(self) -> SameSizeBinOpKind {
        self.kind
    }
    pub fn value_type(self) -> IrBitVectorType {
        self.value_type
    }
    pub fn lhs(self) -> IrValueRef<'ctx> {
        self.lhs
    }
    pub fn rhs(self) -> IrValueRef<'ctx> {
        self.rhs
    }
}

impl<'ctx> OwningModule<'ctx> for SameSizeBinOp<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.owning_module
    }
}

impl<'ctx> From<SameSizeBinOp<'ctx>> for IrValue<'ctx> {
    fn from(v: SameSizeBinOp<'ctx>) -> Self {
        IrValue::SameSizeBinOp(v)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SameSizeUnOpKind {
    Not,
    Neg,
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct SameSizeUnOp<'ctx> {
    kind: SameSizeUnOpKind,
    value_type: IrBitVectorType,
    input: IrValueRef<'ctx>,
}

impl<'ctx> SameSizeUnOp<'ctx> {
    /// for all unary operations, the input and the output type match
    #[track_caller]
    pub fn new(ctx: ContextRef<'ctx>, kind: SameSizeUnOpKind, input: IrValueRef<'ctx>) -> Self {
        let value_type = input
            .get_type(ctx)
            .bit_vector()
            .expect("input type must be a bit vector");
        Self {
            kind,
            value_type,
            input,
        }
    }
    pub fn kind(self) -> SameSizeUnOpKind {
        self.kind
    }
    pub fn value_type(self) -> IrBitVectorType {
        self.value_type
    }
    pub fn input(self) -> IrValueRef<'ctx> {
        self.input
    }
}

impl<'ctx> OwningModule<'ctx> for SameSizeUnOp<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.input.owning_module()
    }
}

impl<'ctx> From<SameSizeUnOp<'ctx>> for IrValue<'ctx> {
    fn from(v: SameSizeUnOp<'ctx>) -> Self {
        IrValue::SameSizeUnOp(v)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOutBinOpKind {
    CompareEq,
    CompareUnsignedLt,
    CompareSignedLt,
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct BoolOutBinOp<'ctx> {
    kind: BoolOutBinOpKind,
    input_type: IrBitVectorType,
    owning_module: Option<IrModuleRef<'ctx>>,
    lhs: IrValueRef<'ctx>,
    rhs: IrValueRef<'ctx>,
}

impl<'ctx> BoolOutBinOp<'ctx> {
    #[track_caller]
    pub fn new(
        ctx: ContextRef<'ctx>,
        kind: BoolOutBinOpKind,
        lhs: IrValueRef<'ctx>,
        rhs: IrValueRef<'ctx>,
    ) -> Self {
        let input_type = lhs
            .get_type(ctx)
            .bit_vector()
            .expect("lhs type must be a bit vector");
        let rhs_value_type = rhs
            .get_type(ctx)
            .bit_vector()
            .expect("rhs type must be a bit vector");
        assert_eq!(input_type, rhs_value_type);
        let owning_module = combine_owning_modules([lhs, rhs]);
        Self {
            kind,
            input_type,
            owning_module,
            lhs,
            rhs,
        }
    }
    pub fn kind(self) -> BoolOutBinOpKind {
        self.kind
    }
    pub fn input_type(self) -> IrBitVectorType {
        self.input_type
    }
    pub fn value_type(self) -> IrBitVectorType {
        IrBitVectorType {
            bit_count: 1,
            signed: false,
        }
    }
    pub fn lhs(self) -> IrValueRef<'ctx> {
        self.lhs
    }
    pub fn rhs(self) -> IrValueRef<'ctx> {
        self.rhs
    }
}

impl<'ctx> OwningModule<'ctx> for BoolOutBinOp<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.owning_module
    }
}

impl<'ctx> From<BoolOutBinOp<'ctx>> for IrValue<'ctx> {
    fn from(v: BoolOutBinOp<'ctx>) -> Self {
        IrValue::BoolOutBinOp(v)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOutUnOpKind {
    ReduceXor,
    ReduceAnd,
    ReduceOr,
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct BoolOutUnOp<'ctx> {
    kind: BoolOutUnOpKind,
    input_type: IrBitVectorType,
    input: IrValueRef<'ctx>,
}

impl<'ctx> BoolOutUnOp<'ctx> {
    #[track_caller]
    pub fn new(ctx: ContextRef<'ctx>, kind: BoolOutUnOpKind, input: IrValueRef<'ctx>) -> Self {
        let input_type = input
            .get_type(ctx)
            .bit_vector()
            .expect("input type must be a bit vector");
        Self {
            kind,
            input_type,
            input,
        }
    }
    pub fn kind(self) -> BoolOutUnOpKind {
        self.kind
    }
    pub fn input_type(self) -> IrBitVectorType {
        self.input_type
    }
    pub fn value_type(self) -> IrBitVectorType {
        IrBitVectorType {
            bit_count: 1,
            signed: false,
        }
    }
    pub fn input(self) -> IrValueRef<'ctx> {
        self.input
    }
}

impl<'ctx> OwningModule<'ctx> for BoolOutUnOp<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.input.owning_module()
    }
}

impl<'ctx> From<BoolOutUnOp<'ctx>> for IrValue<'ctx> {
    fn from(v: BoolOutUnOp<'ctx>) -> Self {
        IrValue::BoolOutUnOp(v)
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct ConvertIntWrapping<'ctx> {
    value_type: IrBitVectorType,
    input_type: IrBitVectorType,
    input: IrValueRef<'ctx>,
}

impl<'ctx> ConvertIntWrapping<'ctx> {
    #[track_caller]
    pub fn new(
        ctx: ContextRef<'ctx>,
        value_type: IrBitVectorType,
        input: IrValueRef<'ctx>,
    ) -> Self {
        let input_type = input
            .get_type(ctx)
            .bit_vector()
            .expect("input type must be a bit vector");
        Self {
            value_type,
            input_type,
            input,
        }
    }
    pub fn input_type(self) -> IrBitVectorType {
        self.input_type
    }
    pub fn value_type(self) -> IrBitVectorType {
        self.value_type
    }
    pub fn input(self) -> IrValueRef<'ctx> {
        self.input
    }
}

impl<'ctx> OwningModule<'ctx> for ConvertIntWrapping<'ctx> {
    fn owning_module(&self) -> Option<IrModuleRef<'ctx>> {
        self.input.owning_module()
    }
}

impl<'ctx> From<ConvertIntWrapping<'ctx>> for IrValue<'ctx> {
    fn from(v: ConvertIntWrapping<'ctx>) -> Self {
        IrValue::ConvertIntWrapping(v)
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
    SliceArray(SliceArray<'ctx>),
    RegOutput(IrRegOutput<'ctx>),
    Mux(Mux<'ctx>),
    ConcatBitVectors(ConcatBitVectors<'ctx>),
    SliceBitVector(SliceBitVector<'ctx>),
    SameSizeBinOp(SameSizeBinOp<'ctx>),
    SameSizeUnOp(SameSizeUnOp<'ctx>),
    BoolOutBinOp(BoolOutBinOp<'ctx>),
    BoolOutUnOp(BoolOutUnOp<'ctx>),
    ConvertIntWrapping(ConvertIntWrapping<'ctx>),
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
            IrValue::SliceArray(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::RegOutput(v) => v.0.value_type(),
            IrValue::Mux(v) => v.value_type(),
            IrValue::ConcatBitVectors(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::SliceBitVector(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::SameSizeBinOp(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::SameSizeUnOp(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::BoolOutBinOp(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::BoolOutUnOp(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::ConvertIntWrapping(v) => IrValueType::from(v.value_type()).intern(ctx),
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
            IrValue::SliceArray(v) => v.owning_module(),
            IrValue::RegOutput(v) => v.owning_module(),
            IrValue::Mux(v) => v.owning_module(),
            IrValue::ConcatBitVectors(v) => v.owning_module(),
            IrValue::SliceBitVector(v) => v.owning_module(),
            IrValue::SameSizeBinOp(v) => v.owning_module(),
            IrValue::SameSizeUnOp(v) => v.owning_module(),
            IrValue::BoolOutBinOp(v) => v.owning_module(),
            IrValue::BoolOutUnOp(v) => v.owning_module(),
            IrValue::ConvertIntWrapping(v) => v.owning_module(),
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
            IrValue::SliceArray(v) => v.fmt(f),
            IrValue::RegOutput(v) => v.fmt(f),
            IrValue::Mux(v) => v.fmt(f),
            IrValue::ConcatBitVectors(v) => v.fmt(f),
            IrValue::SliceBitVector(v) => v.fmt(f),
            IrValue::SameSizeBinOp(v) => v.fmt(f),
            IrValue::SameSizeUnOp(v) => v.fmt(f),
            IrValue::BoolOutBinOp(v) => v.fmt(f),
            IrValue::BoolOutUnOp(v) => v.fmt(f),
            IrValue::ConvertIntWrapping(v) => v.fmt(f),
        }
    }
}
