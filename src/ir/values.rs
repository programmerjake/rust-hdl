// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, Intern, Interned},
    ir::{
        io::{IrModuleInput, IrOutputRead},
        logic::{IrRegOutput, IrWireRead},
        scope::{OwningScope, Scope, ScopeRef},
        types::{
            IrArrayType, IrBitVectorType, IrEnumType, IrEnumVariantType, IrStructFieldType,
            IrStructType, IrValueType, IrValueTypeRef,
        },
        SourceLocation,
    },
    values::integer::{Int, IntShape, IntShapeTrait},
};
use alloc::{vec, vec::Vec};
use core::{fmt, ops::Range};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralBits {
    value: Int,
}

impl<'ctx> OwningScope<'ctx> for LiteralBits {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
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
        self.value.shape().into()
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
    scope: Option<ScopeRef<'ctx>>,
    elements: Interned<'ctx, [IrValueRef<'ctx>]>,
}

impl<'ctx> OwningScope<'ctx> for LiteralArray<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.scope
    }
}

impl<'ctx> LiteralArray<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        element_type: IrValueTypeRef<'ctx>,
        elements: impl AsRef<[IrValueRef<'ctx>]>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let ctx = ctx.ctx();
        let elements = elements.as_ref();
        for element in elements {
            assert_eq!(element.get_type(ctx), element_type, "at {}", caller);
        }
        let elements: Interned<'_, [_]> = elements.intern(ctx);
        Self {
            element_type,
            scope: Scope::combine_or_panic(elements.iter(), caller),
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
    scope: Option<ScopeRef<'ctx>>,
    fields: Interned<'ctx, [LiteralStructField<'ctx>]>,
}

impl<'ctx> LiteralStruct<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        fields: impl AsRef<[LiteralStructField<'ctx>]>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let fields = fields.as_ref();
        let ctx = ctx.ctx();
        let mut field_types = Vec::with_capacity(fields.len());
        let mut scope = None;
        for field in fields {
            let field_type = IrStructFieldType {
                name: field.name,
                ty: field.value.get_type(ctx),
            };
            field_types.push(field_type);
            scope = Scope::combine_or_panic([scope, field.value.owning_scope()], caller);
        }
        let fields = fields.intern(ctx);
        Self {
            value_type: IrStructType::new(ctx, field_types),
            scope,
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

impl<'ctx> OwningScope<'ctx> for LiteralStruct<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.scope
    }
}

impl<'ctx> From<LiteralStruct<'ctx>> for IrValue<'ctx> {
    fn from(v: LiteralStruct<'ctx>) -> Self {
        Self::LiteralStruct(v)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct LiteralEnumVariant<'ctx> {
    value_type: IrEnumType<'ctx>,
    scope: Option<ScopeRef<'ctx>>,
    variant_index: usize,
    fields_value: IrValueRef<'ctx>,
}

impl<'ctx> LiteralEnumVariant<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        value_type: IrEnumType<'ctx>,
        variant_index: usize,
        fields_value: IrValueRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let ctx = ctx.ctx();
        let variant = &value_type.variants()[variant_index];
        let fields_type = fields_value.get_type(ctx);
        assert_eq!(
            IrValueType::from(variant.fields),
            *fields_type,
            "enum variant fields type must match the type of the fields value\nat {}",
            caller
        );
        Self {
            value_type,
            scope: fields_value.owning_scope(),
            variant_index,
            fields_value,
        }
    }
    pub fn value_type(self) -> IrEnumType<'ctx> {
        self.value_type
    }
    pub fn variant_index(self) -> usize {
        self.variant_index
    }
    pub fn variant(self) -> &'ctx IrEnumVariantType<'ctx> {
        &self.value_type.variants().get()[self.variant_index]
    }
    pub fn fields_type(self) -> IrStructType<'ctx> {
        self.variant().fields
    }
    pub fn discriminant(self) -> &'ctx LiteralBits {
        &self.variant().discriminant
    }
    pub fn fields_value(self) -> IrValueRef<'ctx> {
        self.fields_value
    }
}

impl<'ctx> OwningScope<'ctx> for LiteralEnumVariant<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.scope
    }
}

impl<'ctx> From<LiteralEnumVariant<'ctx>> for IrValue<'ctx> {
    fn from(v: LiteralEnumVariant<'ctx>) -> Self {
        Self::LiteralEnumVariant(v)
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
    pub fn new(
        ctx: impl AsContext<'ctx>,
        struct_value: IrValueRef<'ctx>,
        field_index: usize,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let struct_type = match *struct_value.get_type(ctx.ctx()) {
            IrValueType::Struct(v) => v,
            _ => panic!("value type is not a struct\nat {}", caller),
        };
        assert!(
            field_index < struct_type.fields().len(),
            "field_index is out of bounds: field_index = {}, fields.len() = {}\nat {}",
            field_index,
            struct_type.fields().len(),
            caller,
        );
        Self::new_with_struct_type_unchecked(struct_value, struct_type, field_index)
    }
    pub fn new_with_struct_type_unchecked(
        struct_value: IrValueRef<'ctx>,
        struct_type: IrStructType<'ctx>,
        field_index: usize,
    ) -> Self {
        assert!(field_index < struct_type.fields().len());
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
        self.struct_type.fields()[self.field_index]
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

impl<'ctx> OwningScope<'ctx> for ExtractStructField<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.struct_value.owning_scope()
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
pub struct ExtractEnumVariantFields<'ctx> {
    enum_value: IrValueRef<'ctx>,
    enum_type: IrEnumType<'ctx>,
    variant_index: usize,
    match_arm_scope: ScopeRef<'ctx>,
}

impl<'ctx> ExtractEnumVariantFields<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        enum_value: IrValueRef<'ctx>,
        variant_index: usize,
        match_arm_scope: ScopeRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let enum_type = match *enum_value.get_type(ctx.ctx()) {
            IrValueType::Enum(v) => v,
            _ => panic!("value type is not a enum\nat {}", caller),
        };
        assert!(
            variant_index < enum_type.variants().len(),
            "variant_index is out of bounds: variant_index = {}, variants.len() = {}\nat {}",
            variant_index,
            enum_type.variants().len(),
            caller,
        );
        match_arm_scope.assert_scope_can_access_value(enum_value, caller);
        Self {
            enum_value,
            enum_type,
            variant_index,
            match_arm_scope,
        }
    }
    pub fn new_with_enum_type_unchecked(
        enum_value: IrValueRef<'ctx>,
        enum_type: IrEnumType<'ctx>,
        variant_index: usize,
        match_arm_scope: ScopeRef<'ctx>,
    ) -> Self {
        assert!(variant_index < enum_type.variants().len());
        match_arm_scope.assert_scope_can_access_value(enum_value, &SourceLocation::caller());
        Self {
            enum_value,
            enum_type,
            variant_index,
            match_arm_scope,
        }
    }
    pub fn enum_value(self) -> IrValueRef<'ctx> {
        self.enum_value
    }
    pub fn enum_variant_type(self) -> &'ctx IrEnumVariantType<'ctx> {
        &self.enum_type.variants().get()[self.variant_index]
    }
    pub fn value_type(self) -> IrStructType<'ctx> {
        self.enum_variant_type().fields
    }
    pub fn variant_name(self) -> Interned<'ctx, str> {
        self.enum_variant_type().name
    }
    pub fn variant_discriminant(self) -> &'ctx LiteralBits {
        &self.enum_variant_type().discriminant
    }
    pub fn enum_type(self) -> IrEnumType<'ctx> {
        self.enum_type
    }
    pub fn variant_index(self) -> usize {
        self.variant_index
    }
    pub fn match_arm_scope(self) -> ScopeRef<'ctx> {
        self.match_arm_scope
    }
}

impl<'ctx> OwningScope<'ctx> for ExtractEnumVariantFields<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        Some(self.match_arm_scope)
    }
}

impl fmt::Debug for ExtractEnumVariantFields<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExtractEnumVariantFields")
            .field("enum_value", &self.enum_value())
            .field("enum_variant_type", &self.enum_variant_type())
            .field("variant_index", &self.variant_index())
            .field("match_arm_scope", &self.match_arm_scope())
            .finish_non_exhaustive()
    }
}

impl<'ctx> From<ExtractEnumVariantFields<'ctx>> for IrValue<'ctx> {
    fn from(v: ExtractEnumVariantFields<'ctx>) -> Self {
        Self::ExtractEnumVariantFields(v)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct ShrinkScope<'ctx> {
    value: IrValueRef<'ctx>,
    value_type: IrValueTypeRef<'ctx>,
    scope: ScopeRef<'ctx>,
}

impl<'ctx> ShrinkScope<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        value: IrValueRef<'ctx>,
        scope: ScopeRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let value_type = value.get_type(ctx);
        scope.assert_scope_can_access_value(value, caller);
        Self {
            value,
            value_type,
            scope,
        }
    }
    pub fn value(self) -> IrValueRef<'ctx> {
        self.value
    }
    pub fn value_type(self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn scope(self) -> ScopeRef<'ctx> {
        self.scope
    }
}

impl<'ctx> OwningScope<'ctx> for ShrinkScope<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        Some(self.scope)
    }
}

impl<'ctx> From<ShrinkScope<'ctx>> for IrValue<'ctx> {
    fn from(v: ShrinkScope<'ctx>) -> Self {
        Self::ShrinkScope(v)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct ExpandScope<'ctx> {
    value: IrValueRef<'ctx>,
    value_type: IrValueTypeRef<'ctx>,
    input_scope: ScopeRef<'ctx>,
    result_scope: ScopeRef<'ctx>,
}

impl<'ctx> ExpandScope<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        value: IrValueRef<'ctx>,
        input_scope: ScopeRef<'ctx>,
        result_scope: ScopeRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let value_type = value.get_type(ctx);
        result_scope.assert_ancestor_of(input_scope, caller);
        input_scope.assert_scope_can_access_value(value, caller);
        Self {
            value,
            value_type,
            input_scope,
            result_scope,
        }
    }
    pub fn value(self) -> IrValueRef<'ctx> {
        self.value
    }
    pub fn value_type(self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn input_scope(self) -> ScopeRef<'ctx> {
        self.input_scope
    }
    pub fn result_scope(self) -> ScopeRef<'ctx> {
        self.result_scope
    }
}

impl<'ctx> OwningScope<'ctx> for ExpandScope<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        Some(self.result_scope)
    }
}

impl<'ctx> From<ExpandScope<'ctx>> for IrValue<'ctx> {
    fn from(v: ExpandScope<'ctx>) -> Self {
        Self::ExpandScope(v)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct IsEnumVariant<'ctx> {
    enum_value: IrValueRef<'ctx>,
    enum_type: IrEnumType<'ctx>,
    variant_index: usize,
}

impl<'ctx> IsEnumVariant<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        enum_value: IrValueRef<'ctx>,
        variant_index: usize,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let enum_type = match *enum_value.get_type(ctx.ctx()) {
            IrValueType::Enum(v) => v,
            _ => panic!("value type is not a enum\nat {}", caller),
        };
        assert!(
            variant_index < enum_type.variants().len(),
            "variant_index is out of bounds: variant_index = {}, variants.len() = {}\nat {}",
            variant_index,
            enum_type.variants().len(),
            caller,
        );
        Self::new_with_enum_type_unchecked(enum_value, enum_type, variant_index)
    }
    pub fn new_with_enum_type_unchecked(
        enum_value: IrValueRef<'ctx>,
        enum_type: IrEnumType<'ctx>,
        variant_index: usize,
    ) -> Self {
        assert!(variant_index < enum_type.variants().len());
        Self {
            enum_value,
            enum_type,
            variant_index,
        }
    }
    pub fn enum_value(self) -> IrValueRef<'ctx> {
        self.enum_value
    }
    pub fn enum_variant_type(self) -> &'ctx IrEnumVariantType<'ctx> {
        &self.enum_type.variants().get()[self.variant_index]
    }
    pub fn value_type(self) -> IrBitVectorType {
        IrBitVectorType {
            bit_count: 1,
            signed: false,
        }
    }
    pub fn variant_name(self) -> Interned<'ctx, str> {
        self.enum_variant_type().name
    }
    pub fn variant_discriminant(self) -> &'ctx LiteralBits {
        &self.enum_variant_type().discriminant
    }
    pub fn enum_type(self) -> IrEnumType<'ctx> {
        self.enum_type
    }
    pub fn variant_index(self) -> usize {
        self.variant_index
    }
}

impl<'ctx> OwningScope<'ctx> for IsEnumVariant<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.enum_value.owning_scope()
    }
}

impl fmt::Debug for IsEnumVariant<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IsEnumVariant")
            .field("enum_value", &self.enum_value())
            .field("enum_variant_type", &self.enum_variant_type())
            .field("variant_index", &self.variant_index())
            .finish_non_exhaustive()
    }
}

impl<'ctx> From<IsEnumVariant<'ctx>> for IrValue<'ctx> {
    fn from(v: IsEnumVariant<'ctx>) -> Self {
        Self::IsEnumVariant(v)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct MatchArmForEnum<'ctx> {
    pub result: IrValueRef<'ctx>,
    pub match_arm_scope: ScopeRef<'ctx>,
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct MatchEnum<'ctx> {
    enum_value: IrValueRef<'ctx>,
    enum_type: IrEnumType<'ctx>,
    value_type: IrValueTypeRef<'ctx>,
    match_arms: Interned<'ctx, [MatchArmForEnum<'ctx>]>,
    result_scope: ScopeRef<'ctx>,
}

impl<'ctx> MatchEnum<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        enum_value: IrValueRef<'ctx>,
        value_type: IrValueTypeRef<'ctx>,
        match_arms: impl IntoIterator<Item = (usize, MatchArmForEnum<'ctx>)>,
        result_scope: ScopeRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let ctx = ctx.ctx();
        let enum_type = match *enum_value.get_type(ctx) {
            IrValueType::Enum(v) => v,
            _ => panic!("value type is not a enum\nat {}", caller),
        };
        result_scope.assert_scope_can_access_value(enum_value, caller);
        let mut match_arms_opt = vec![None; enum_type.variants().len()];
        for (
            variant_index,
            MatchArmForEnum {
                result,
                match_arm_scope,
            },
        ) in match_arms
        {
            assert!(
                variant_index < enum_type.variants().len(),
                "variant_index is out of bounds: variant_index = {}, variants.len() = {}\nat {}",
                variant_index,
                enum_type.variants().len(),
                caller,
            );
            assert!(
                result.get_type(ctx) == value_type,
                "the match arm's result type doesn't match the expected type:\n\
                variant index = {}, variant name = {}, variant discriminant = {:?}\nat {}",
                variant_index,
                enum_type.variants()[variant_index].name,
                enum_type.variants()[variant_index].discriminant,
                caller,
            );
            match_arm_scope.assert_scope_can_access_value(result, caller);
            result_scope.assert_ancestor_of(match_arm_scope, caller);
            // allow duplicate variants, the first one is the one that's used
            match_arms_opt[variant_index].get_or_insert(MatchArmForEnum {
                result,
                match_arm_scope,
            });
        }
        let match_arms: Vec<_> = match_arms_opt
            .into_iter()
            .enumerate()
            .map(|(variant_index, match_arm)| match match_arm {
                Some(match_arm) => match_arm,
                None => panic!(
                    "missing match arm for variant:\n\
                    variant index = {}, variant name = {}, variant discriminant = {:?}\nat {}",
                    variant_index,
                    enum_type.variants()[variant_index].name,
                    enum_type.variants()[variant_index].discriminant,
                    caller,
                ),
            })
            .collect();
        let match_arms = match_arms.intern(ctx);
        Self {
            enum_value,
            enum_type,
            value_type,
            match_arms,
            result_scope,
        }
    }
    pub fn enum_value(self) -> IrValueRef<'ctx> {
        self.enum_value
    }
    pub fn value_type(self) -> IrValueTypeRef<'ctx> {
        self.value_type
    }
    pub fn enum_type(self) -> IrEnumType<'ctx> {
        self.enum_type
    }
    pub fn match_arms(self) -> Interned<'ctx, [MatchArmForEnum<'ctx>]> {
        self.match_arms
    }
    pub fn result_scope(self) -> ScopeRef<'ctx> {
        self.result_scope
    }
}

impl<'ctx> OwningScope<'ctx> for MatchEnum<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        Some(self.result_scope)
    }
}

impl fmt::Debug for MatchEnum<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MatchEnum")
            .field("enum_value", &self.enum_value())
            .field("enum_type", &self.enum_type())
            .field("value_type", &self.value_type())
            .field("match_arms", &self.match_arms())
            .field("result_scope", &self.result_scope())
            .finish_non_exhaustive()
    }
}

impl<'ctx> From<MatchEnum<'ctx>> for IrValue<'ctx> {
    fn from(v: MatchEnum<'ctx>) -> Self {
        Self::MatchEnum(v)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct ExtractArrayElement<'ctx> {
    array_value: IrValueRef<'ctx>,
    array_type: IrArrayType<'ctx>,
    element_index: usize,
}

impl<'ctx> ExtractArrayElement<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        array_value: IrValueRef<'ctx>,
        element_index: usize,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let array_type = match *array_value.get_type(ctx) {
            IrValueType::Array(v) => v,
            _ => panic!("value type is not a array\nat {}", caller),
        };
        assert!(
            element_index < array_type.length,
            "element_index out of bounds: element_index = {}, array_type.length = {}\nat {}",
            element_index,
            array_type.length,
            caller,
        );
        Self::new_with_array_type_unchecked(array_value, array_type, element_index)
    }
    pub fn new_with_array_type_unchecked(
        array_value: IrValueRef<'ctx>,
        array_type: IrArrayType<'ctx>,
        element_index: usize,
    ) -> Self {
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

impl<'ctx> OwningScope<'ctx> for ExtractArrayElement<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.array_value.owning_scope()
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
        ctx: impl AsContext<'ctx>,
        array_value: IrValueRef<'ctx>,
        element_indexes: Range<usize>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let array_type = match *array_value.get_type(ctx) {
            IrValueType::Array(v) => v,
            _ => panic!("value type is not a array\nat {}", caller),
        };
        assert!(
            element_indexes.end <= array_type.length
                && element_indexes.start <= element_indexes.end,
            "element_indexes is invalid: element_indexes = {}..{}, array_type.length = {}\nat {}",
            element_indexes.start,
            element_indexes.end,
            array_type.length,
            caller,
        );
        Self::new_with_array_type_unchecked(array_value, array_type, element_indexes)
    }
    pub fn new_with_array_type_unchecked(
        array_value: IrValueRef<'ctx>,
        array_type: IrArrayType<'ctx>,
        element_indexes: Range<usize>,
    ) -> Self {
        assert!(element_indexes.end <= array_type.length);
        assert!(element_indexes.start <= element_indexes.end);
        Self {
            array_value,
            array_type,
            element_start_index: element_indexes.start,
            element_end_index: element_indexes.end,
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

impl<'ctx> OwningScope<'ctx> for SliceArray<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.array_value.owning_scope()
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
    pub fn new(
        ctx: impl AsContext<'ctx>,
        base_value: IrValueRef<'ctx>,
        bit_indexes: Range<u32>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        Self::new_with_bit_vector_type_unchecked(
            base_value,
            base_value.get_type(ctx.ctx()),
            bit_indexes,
            caller,
        )
    }
    /// bit_indexes is in LSB0 order
    pub fn new_with_bit_vector_type_unchecked(
        base_value: IrValueRef<'ctx>,
        base_type: IrValueTypeRef<'ctx>,
        bit_indexes: Range<u32>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let base_type = base_type
            .bit_vector()
            .unwrap_or_else(|| panic!("base value type is not a bit vector\nat {}", caller));
        let Range {
            start: bit_start_index,
            end: bit_end_index,
        } = bit_indexes;
        if bit_end_index > base_type.bit_count || bit_start_index > bit_end_index {
            panic!(
                "bit_indexes are not a valid range: bit_indexes = {}..{}, \
                base_type.bit_count = {}\nat {}",
                bit_indexes.start, bit_indexes.end, base_type.bit_count, caller,
            );
        }
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

impl<'ctx> OwningScope<'ctx> for SliceBitVector<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.base_value.owning_scope()
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
    scope: Option<ScopeRef<'ctx>>,
    true_value: IrValueRef<'ctx>,
    false_value: IrValueRef<'ctx>,
}

impl<'ctx> Mux<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        condition: IrValueRef<'ctx>,
        true_value: IrValueRef<'ctx>,
        false_value: IrValueRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let ctx = ctx.ctx();
        assert!(
            condition.get_type(ctx).is_bool(),
            "condition must be a bool (UInt<1>)\nat {}",
            caller
        );
        let value_type = true_value.get_type(ctx);
        assert_eq!(
            value_type,
            false_value.get_type(ctx),
            "true_value and false_value must have the same type\nat {}",
            caller
        );
        let scope = Scope::combine_or_panic([condition, true_value, false_value], caller);
        Self {
            condition,
            value_type,
            scope,
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

impl<'ctx> OwningScope<'ctx> for Mux<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.scope
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
    scope: Option<ScopeRef<'ctx>>,
    bit_vectors: Interned<'ctx, [IrValueRef<'ctx>]>,
}

impl<'ctx> ConcatBitVectors<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        bit_vectors: impl AsRef<[IrValueRef<'ctx>]>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let ctx = ctx.ctx();
        let bit_vectors = bit_vectors.as_ref();
        let mut value_type = IrBitVectorType {
            bit_count: 0,
            signed: false,
        };
        for bit_vector in bit_vectors {
            let IrBitVectorType { bit_count, .. } = bit_vector
                .get_type(ctx)
                .bit_vector()
                .unwrap_or_else(|| panic!("input type must be a bit vector\nat {}", caller));
            value_type.bit_count = value_type
                .bit_count
                .checked_add(bit_count)
                .unwrap_or_else(|| panic!("too many bits in bit vector\nat {}", caller));
        }
        let scope = Scope::combine_or_panic(bit_vectors, caller);
        Self {
            value_type,
            scope,
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

impl<'ctx> OwningScope<'ctx> for ConcatBitVectors<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.scope
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
    ShiftRight,
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct SameSizeBinOp<'ctx> {
    kind: SameSizeBinOpKind,
    value_type: IrBitVectorType,
    scope: Option<ScopeRef<'ctx>>,
    lhs: IrValueRef<'ctx>,
    rhs: IrValueRef<'ctx>,
}

impl<'ctx> SameSizeBinOp<'ctx> {
    /// for all binary operations, both inputs and the output type must match
    pub fn new(
        ctx: impl AsContext<'ctx>,
        kind: SameSizeBinOpKind,
        lhs: IrValueRef<'ctx>,
        rhs: IrValueRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let ctx = ctx.ctx();
        let value_type = lhs
            .get_type(ctx)
            .bit_vector()
            .unwrap_or_else(|| panic!("lhs type must be a bit vector\nat {}", caller));
        let rhs_value_type = rhs
            .get_type(ctx)
            .bit_vector()
            .unwrap_or_else(|| panic!("rhs type must be a bit vector\nat {}", caller));
        assert_eq!(
            value_type, rhs_value_type,
            "input types must be the same\nat {}",
            caller
        );
        let scope = Scope::combine_or_panic([lhs, rhs], caller);
        Self {
            kind,
            value_type,
            scope,
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

impl<'ctx> OwningScope<'ctx> for SameSizeBinOp<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.scope
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
    pub fn new(
        ctx: impl AsContext<'ctx>,
        kind: SameSizeUnOpKind,
        input: IrValueRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let value_type = input
            .get_type(ctx.ctx())
            .bit_vector()
            .unwrap_or_else(|| panic!("input type must be a bit vector\nat {}", caller));
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

impl<'ctx> OwningScope<'ctx> for SameSizeUnOp<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.input.owning_scope()
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
    CompareLt,
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
pub struct BoolOutBinOp<'ctx> {
    kind: BoolOutBinOpKind,
    input_type: IrBitVectorType,
    scope: Option<ScopeRef<'ctx>>,
    lhs: IrValueRef<'ctx>,
    rhs: IrValueRef<'ctx>,
}

impl<'ctx> BoolOutBinOp<'ctx> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        kind: BoolOutBinOpKind,
        lhs: IrValueRef<'ctx>,
        rhs: IrValueRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let ctx = ctx.ctx();
        let input_type = lhs
            .get_type(ctx)
            .bit_vector()
            .unwrap_or_else(|| panic!("lhs type must be a bit vector\nat {}", caller));
        let rhs_value_type = rhs
            .get_type(ctx)
            .bit_vector()
            .unwrap_or_else(|| panic!("rhs type must be a bit vector\nat {}", caller));
        assert_eq!(
            input_type, rhs_value_type,
            "input types must be the same\nat {}",
            caller
        );
        let scope = Scope::combine_or_panic([lhs, rhs], caller);
        Self {
            kind,
            input_type,
            scope,
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

impl<'ctx> OwningScope<'ctx> for BoolOutBinOp<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.scope
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
    pub fn new(
        ctx: impl AsContext<'ctx>,
        kind: BoolOutUnOpKind,
        input: IrValueRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let input_type = input
            .get_type(ctx.ctx())
            .bit_vector()
            .unwrap_or_else(|| panic!("input type must be a bit vector\nat {}", caller));
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

impl<'ctx> OwningScope<'ctx> for BoolOutUnOp<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.input.owning_scope()
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
    pub fn new(
        ctx: impl AsContext<'ctx>,
        value_type: IrBitVectorType,
        input: IrValueRef<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let input_type = input
            .get_type(ctx.ctx())
            .bit_vector()
            .unwrap_or_else(|| panic!("input type must be a bit vector\nat {}", caller));
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

impl<'ctx> OwningScope<'ctx> for ConvertIntWrapping<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.input.owning_scope()
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
    LiteralEnumVariant(LiteralEnumVariant<'ctx>),
    WireRead(IrWireRead<'ctx>),
    Input(IrModuleInput<'ctx>),
    OutputRead(IrOutputRead<'ctx>),
    ExtractStructField(ExtractStructField<'ctx>),
    ExtractEnumVariantFields(ExtractEnumVariantFields<'ctx>),
    ShrinkScope(ShrinkScope<'ctx>),
    ExpandScope(ExpandScope<'ctx>),
    IsEnumVariant(IsEnumVariant<'ctx>),
    MatchEnum(MatchEnum<'ctx>),
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
    pub fn get_type(&self, ctx: impl AsContext<'ctx>) -> IrValueTypeRef<'ctx> {
        let ctx = ctx.ctx();
        match self {
            IrValue::LiteralBits(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::LiteralArray(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::LiteralStruct(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::LiteralEnumVariant(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::WireRead(v) => v.0.value_type(),
            IrValue::Input(v) => v.value_type(),
            IrValue::OutputRead(v) => v.0.value_type(),
            IrValue::ExtractStructField(v) => v.value_type(),
            IrValue::ExtractEnumVariantFields(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::ShrinkScope(v) => v.value_type(),
            IrValue::ExpandScope(v) => v.value_type(),
            IrValue::IsEnumVariant(v) => IrValueType::from(v.value_type()).intern(ctx),
            IrValue::MatchEnum(v) => v.value_type(),
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

impl<'ctx> OwningScope<'ctx> for IrValue<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        match self {
            IrValue::LiteralBits(v) => v.owning_scope(),
            IrValue::LiteralArray(v) => v.owning_scope(),
            IrValue::LiteralStruct(v) => v.owning_scope(),
            IrValue::LiteralEnumVariant(v) => v.owning_scope(),
            IrValue::WireRead(v) => v.owning_scope(),
            IrValue::Input(v) => v.owning_scope(),
            IrValue::OutputRead(v) => v.owning_scope(),
            IrValue::ExtractStructField(v) => v.owning_scope(),
            IrValue::ExtractEnumVariantFields(v) => v.owning_scope(),
            IrValue::ShrinkScope(v) => v.owning_scope(),
            IrValue::ExpandScope(v) => v.owning_scope(),
            IrValue::IsEnumVariant(v) => v.owning_scope(),
            IrValue::MatchEnum(v) => v.owning_scope(),
            IrValue::ExtractArrayElement(v) => v.owning_scope(),
            IrValue::SliceArray(v) => v.owning_scope(),
            IrValue::RegOutput(v) => v.owning_scope(),
            IrValue::Mux(v) => v.owning_scope(),
            IrValue::ConcatBitVectors(v) => v.owning_scope(),
            IrValue::SliceBitVector(v) => v.owning_scope(),
            IrValue::SameSizeBinOp(v) => v.owning_scope(),
            IrValue::SameSizeUnOp(v) => v.owning_scope(),
            IrValue::BoolOutBinOp(v) => v.owning_scope(),
            IrValue::BoolOutUnOp(v) => v.owning_scope(),
            IrValue::ConvertIntWrapping(v) => v.owning_scope(),
        }
    }
}

impl fmt::Debug for IrValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValue::LiteralBits(v) => v.fmt(f),
            IrValue::LiteralArray(v) => v.fmt(f),
            IrValue::LiteralStruct(v) => v.fmt(f),
            IrValue::LiteralEnumVariant(v) => v.fmt(f),
            IrValue::WireRead(v) => v.fmt(f),
            IrValue::Input(v) => v.fmt(f),
            IrValue::OutputRead(v) => v.fmt(f),
            IrValue::ExtractStructField(v) => v.fmt(f),
            IrValue::ExtractEnumVariantFields(v) => v.fmt(f),
            IrValue::ShrinkScope(v) => v.fmt(f),
            IrValue::ExpandScope(v) => v.fmt(f),
            IrValue::IsEnumVariant(v) => v.fmt(f),
            IrValue::MatchEnum(v) => v.fmt(f),
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
