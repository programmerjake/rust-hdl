// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{ContextRef, Intern, Interned},
    ir::values::LiteralBits,
    prelude::Int,
    values::integer::IntShape,
};
use alloc::vec::Vec;
use core::{cmp::Ordering, convert::TryInto, fmt};

fn flatten_struct_field_types<'ctx, I: IntoIterator<Item = IrValueType<'ctx>>>(
    types: I,
) -> IrBitVectorType {
    let mut retval = IrBitVectorType {
        bit_count: 0,
        signed: false,
    };
    types.into_iter().for_each(|ty| {
        let IrBitVectorType { bit_count, signed } = ty.flattened();
        retval = IrBitVectorType {
            bit_count: bit_count
                .checked_add(retval.bit_count)
                .expect("type too big"),
            signed,
        };
    });
    retval
}

fn flatten_union_field_types<'ctx, I: IntoIterator<Item = IrValueType<'ctx>>>(
    types: I,
) -> IrBitVectorType {
    let mut types = types.into_iter();
    let mut retval = match types.next() {
        Some(v) => v.flattened(),
        None => {
            return IrBitVectorType {
                bit_count: 0,
                signed: false,
            }
        }
    };
    types.for_each(|ty| {
        let ty = ty.flattened();
        match ty.bit_count.cmp(&retval.bit_count) {
            Ordering::Less => {}
            Ordering::Equal => retval.signed &= ty.signed,
            Ordering::Greater => retval = ty,
        }
    });
    retval
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrStructFieldType<'ctx> {
    pub name: Interned<'ctx, str>,
    pub ty: IrValueTypeRef<'ctx>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrStructType<'ctx> {
    fields: Interned<'ctx, [IrStructFieldType<'ctx>]>,
    flattened: IrBitVectorType,
}

impl<'ctx> IrStructType<'ctx> {
    #[track_caller]
    pub fn new(ctx: ContextRef<'ctx>, fields: impl AsRef<[IrStructFieldType<'ctx>]>) -> Self {
        let fields: Interned<'ctx, [_]> = fields.as_ref().intern(ctx);
        let flattened = flatten_struct_field_types(fields.iter().map(|v| *v.ty));
        Self { fields, flattened }
    }
    pub fn flattened_field_offsets(self) -> impl Iterator<Item = u32> + Clone + 'ctx {
        let mut offset = 0;
        self.fields.get().iter().map(move |field| {
            let retval = offset;
            offset += field.ty.flattened().bit_count;
            retval
        })
    }
    pub fn flattened(self) -> IrBitVectorType {
        self.flattened
    }
    pub fn fields(self) -> Interned<'ctx, [IrStructFieldType<'ctx>]> {
        self.fields
    }
}

impl<'ctx> From<IrStructType<'ctx>> for IrValueType<'ctx> {
    fn from(v: IrStructType<'ctx>) -> Self {
        Self::Struct(v)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IrEnumVariantType<'ctx, Discriminant = LiteralBits> {
    pub name: Interned<'ctx, str>,
    pub discriminant: Discriminant,
    pub fields: IrStructType<'ctx>,
}

/// the variants are always sorted in ascending order by their discriminant with no duplicate discriminants
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrEnumType<'ctx> {
    discriminant_type: IrBitVectorType,
    variants: Interned<'ctx, [IrEnumVariantType<'ctx>]>,
    flattened_fields: IrBitVectorType,
}

impl<'ctx> IrEnumType<'ctx> {
    /// the variants are sorted in ascending order by their discriminant, asserting that there are no duplicate discriminants
    #[track_caller]
    pub fn new<I: IntoIterator<Item = IrEnumVariantType<'ctx>>>(
        ctx: ContextRef<'ctx>,
        discriminant_type: IrBitVectorType,
        variants: I,
    ) -> Self {
        let mut variants: Vec<IrEnumVariantType<'ctx>> = variants.into_iter().collect();
        for i in &variants {
            assert_eq!(
                i.discriminant.value_type(),
                discriminant_type,
                "variant {:?} has an unexpected discriminant type",
                i.name,
            );
        }
        variants.sort_unstable_by(|a, b| a.discriminant.value().cmp_value(b.discriminant.value()));
        for i in variants.windows(2) {
            assert_ne!(
                i[0].discriminant, i[1].discriminant,
                "duplicate discriminant for variants: {:?} and {:?}",
                i[0].name, i[1].name,
            );
        }
        Self::new_unchecked(discriminant_type, variants.intern(ctx))
    }
    pub fn new_unchecked(
        discriminant_type: IrBitVectorType,
        variants: Interned<'ctx, [IrEnumVariantType<'ctx>]>,
    ) -> Self {
        let flattened_fields = flatten_union_field_types(variants.iter().map(|v| v.fields.into()));
        Self {
            discriminant_type,
            variants,
            flattened_fields,
        }
    }
    pub fn generate_discriminant_type(variant_count: usize) -> IrBitVectorType {
        if variant_count == 0 {
            return IrBitVectorType {
                bit_count: 0,
                signed: false,
            };
        }
        IrBitVectorType {
            bit_count: usize::BITS - (variant_count - 1).leading_zeros(),
            signed: false,
        }
    }
    pub fn with_generated_discriminant<I: IntoIterator<Item = IrEnumVariantType<'ctx, ()>>>(
        ctx: ContextRef<'ctx>,
        variants: I,
    ) -> Self {
        let mut variants: Vec<IrEnumVariantType<'ctx>> = variants
            .into_iter()
            .map(
                |IrEnumVariantType {
                     name,
                     discriminant: _,
                     fields,
                 }| IrEnumVariantType {
                    name,
                    discriminant: LiteralBits::new(),
                    fields,
                },
            )
            .collect();
        let discriminant_type = Self::generate_discriminant_type(variants.len());
        for (index, variant) in variants.iter_mut().enumerate() {
            variant.discriminant =
                Int::<IntShape>::wrapping_with_shape(index, discriminant_type.into()).into();
        }
        Self::new_unchecked(discriminant_type, variants.intern(ctx))
    }
    pub fn discriminant_type(self) -> IrBitVectorType {
        self.discriminant_type
    }
    pub fn contains_variant(self, discriminant: &LiteralBits) -> bool {
        self.get_variant_index(discriminant).is_some()
    }
    pub fn get_variant_index(self, discriminant: &LiteralBits) -> Option<usize> {
        self.variants
            .binary_search_by(|v| v.discriminant.value().cmp_value(discriminant.value()))
            .ok()
    }
    pub fn get_variant_and_index(
        self,
        discriminant: &LiteralBits,
    ) -> Option<(usize, &'ctx IrEnumVariantType<'ctx>)> {
        self.get_variant_index(discriminant)
            .map(|index| (index, &self.variants().get()[index]))
    }
    pub fn get_variant(self, discriminant: &LiteralBits) -> Option<&'ctx IrEnumVariantType<'ctx>> {
        self.get_variant_and_index(discriminant).map(|v| v.1)
    }
    /// the variants are always sorted in ascending order by their discriminant with no duplicate discriminants
    pub fn variants(self) -> Interned<'ctx, [IrEnumVariantType<'ctx>]> {
        self.variants
    }
    pub fn flattened(self) -> IrBitVectorType {
        flatten_struct_field_types([self.discriminant_type.into(), self.flattened_fields.into()])
    }
    pub fn flattened_fields(self) -> IrBitVectorType {
        self.flattened_fields
    }
}

impl<'ctx> From<IrEnumType<'ctx>> for IrValueType<'ctx> {
    fn from(v: IrEnumType<'ctx>) -> Self {
        Self::Enum(v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrArrayType<'ctx> {
    pub element: IrValueTypeRef<'ctx>,
    pub length: usize,
}

impl<'ctx> IrArrayType<'ctx> {
    pub fn flattened(self) -> IrBitVectorType {
        let mut retval = self.element.flattened();
        retval.bit_count = retval
            .bit_count
            .checked_mul(self.length.try_into().expect("array too big"))
            .expect("array too big");
        retval
    }
}

impl<'ctx> From<IrArrayType<'ctx>> for IrValueType<'ctx> {
    fn from(v: IrArrayType<'ctx>) -> Self {
        Self::Array(v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrBitVectorType {
    pub bit_count: u32,
    pub signed: bool,
}

impl From<IrBitVectorType> for IrValueType<'_> {
    fn from(v: IrBitVectorType) -> Self {
        Self::BitVector(v)
    }
}

impl From<IrBitVectorType> for IntShape {
    fn from(v: IrBitVectorType) -> Self {
        IntShape {
            bit_count: v.bit_count,
            signed: v.signed,
        }
    }
}

impl From<IntShape> for IrBitVectorType {
    fn from(v: IntShape) -> Self {
        IrBitVectorType {
            bit_count: v.bit_count,
            signed: v.signed,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrValueType<'ctx> {
    BitVector(IrBitVectorType),
    Array(IrArrayType<'ctx>),
    Struct(IrStructType<'ctx>),
    Enum(IrEnumType<'ctx>),
}

impl<'ctx> IrValueType<'ctx> {
    pub fn is_bool(self) -> bool {
        matches!(
            self,
            Self::BitVector(IrBitVectorType {
                bit_count: 1,
                signed: false
            })
        )
    }
    pub fn bit_vector(self) -> Option<IrBitVectorType> {
        match self {
            IrValueType::BitVector(v) => Some(v),
            _ => None,
        }
    }
    pub fn array(self) -> Option<IrArrayType<'ctx>> {
        match self {
            IrValueType::Array(v) => Some(v),
            _ => None,
        }
    }
    pub fn struct_(self) -> Option<IrStructType<'ctx>> {
        match self {
            IrValueType::Struct(v) => Some(v),
            _ => None,
        }
    }
    pub fn enum_(self) -> Option<IrEnumType<'ctx>> {
        match self {
            IrValueType::Enum(v) => Some(v),
            _ => None,
        }
    }
    pub fn flattened(self) -> IrBitVectorType {
        match self {
            IrValueType::BitVector(v) => v,
            IrValueType::Array(v) => v.flattened(),
            IrValueType::Struct(v) => v.flattened(),
            IrValueType::Enum(v) => v.flattened(),
        }
    }
}

impl fmt::Debug for IrValueType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValueType::BitVector(v) => v.fmt(f),
            IrValueType::Array(v) => v.fmt(f),
            IrValueType::Struct(v) => v.fmt(f),
            IrValueType::Enum(v) => v.fmt(f),
        }
    }
}

pub type IrValueTypeRef<'ctx> = Interned<'ctx, IrValueType<'ctx>>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{context::Context, prelude::UInt};

    #[test]
    fn test_generate_discriminant_type() {
        assert_eq!(
            IrEnumType::generate_discriminant_type(0),
            IrBitVectorType {
                bit_count: 0,
                signed: false
            }
        );
        assert_eq!(
            IrEnumType::generate_discriminant_type(1),
            IrBitVectorType {
                bit_count: 0,
                signed: false
            }
        );
        assert_eq!(
            IrEnumType::generate_discriminant_type(2),
            IrBitVectorType {
                bit_count: 1,
                signed: false
            }
        );
        assert_eq!(
            IrEnumType::generate_discriminant_type(3),
            IrBitVectorType {
                bit_count: 2,
                signed: false
            }
        );
        assert_eq!(
            IrEnumType::generate_discriminant_type(4),
            IrBitVectorType {
                bit_count: 2,
                signed: false
            }
        );
        assert_eq!(
            IrEnumType::generate_discriminant_type(5),
            IrBitVectorType {
                bit_count: 3,
                signed: false
            }
        );
        assert_eq!(
            IrEnumType::generate_discriminant_type(usize::MAX),
            IrBitVectorType {
                bit_count: usize::BITS,
                signed: false
            }
        );
    }

    #[test]
    fn test_generate_discriminant() {
        Context::with(|ctx: ContextRef<'_>| {
            let type1 = IrEnumType::new(
                ctx,
                IrBitVectorType {
                    bit_count: 2,
                    signed: false,
                },
                [
                    IrEnumVariantType {
                        name: "B".intern(ctx),
                        discriminant: UInt::<2>::wrapping_new(1).into(),
                        fields: IrStructType::new(ctx, []),
                    },
                    IrEnumVariantType {
                        name: "A".intern(ctx),
                        discriminant: UInt::<2>::wrapping_new(0).into(),
                        fields: IrStructType::new(ctx, []),
                    },
                    IrEnumVariantType {
                        name: "C".intern(ctx),
                        discriminant: UInt::<2>::wrapping_new(2).into(),
                        fields: IrStructType::new(ctx, []),
                    },
                ],
            );
            let type2 = IrEnumType::with_generated_discriminant(
                ctx,
                [
                    IrEnumVariantType {
                        name: "A".intern(ctx),
                        discriminant: (),
                        fields: IrStructType::new(ctx, []),
                    },
                    IrEnumVariantType {
                        name: "B".intern(ctx),
                        discriminant: (),
                        fields: IrStructType::new(ctx, []),
                    },
                    IrEnumVariantType {
                        name: "C".intern(ctx),
                        discriminant: (),
                        fields: IrStructType::new(ctx, []),
                    },
                ],
            );
            assert_eq!(type1, type2);
        });
    }
}
