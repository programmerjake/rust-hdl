// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, Intern, Interned},
    ir::{values::LiteralBits, SourceLocation},
    prelude::Int,
    values::integer::IntShape,
};
use alloc::vec::Vec;
use core::{cmp::Ordering, convert::TryInto, fmt};

fn flatten_type_sequence<'ctx, I: IntoIterator<Item = IrValueType<'ctx>>>(
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

fn union_bit_vectors<'ctx, I: IntoIterator<Item = IrBitVectorType>>(
    bit_vectors: I,
) -> IrBitVectorType {
    let mut bit_vectors = bit_vectors.into_iter();
    let mut retval = match bit_vectors.next() {
        Some(v) => v,
        None => {
            return IrBitVectorType {
                bit_count: 0,
                signed: false,
            }
        }
    };
    bit_vectors.for_each(|ty| match ty.bit_count.cmp(&retval.bit_count) {
        Ordering::Less => {}
        Ordering::Equal => retval.signed &= ty.signed,
        Ordering::Greater => retval = ty,
    });
    retval
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrFieldType<'ctx> {
    pub name: Interned<'ctx, str>,
    pub ty: IrValueTypeRef<'ctx>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct IrVariantType<'ctx, Discriminant = LiteralBits> {
    name: Interned<'ctx, str>,
    discriminant: Discriminant,
    fields: Interned<'ctx, [IrFieldType<'ctx>]>,
    flattened_fields: IrBitVectorType,
}

impl<'ctx, Discriminant> IrVariantType<'ctx, Discriminant> {
    pub fn new(
        ctx: impl AsContext<'ctx>,
        name: impl AsRef<str>,
        discriminant: Discriminant,
        fields: impl AsRef<[IrFieldType<'ctx>]>,
    ) -> Self {
        let ctx = ctx.ctx();
        let name = name.as_ref().intern(ctx);
        let fields: Interned<'ctx, [_]> = fields.as_ref().intern(ctx);
        let flattened_fields = flatten_type_sequence(fields.iter().map(|v| *v.ty));
        Self {
            name,
            discriminant,
            fields,
            flattened_fields,
        }
    }
    pub fn flattened_field_offsets<'a>(&'a self) -> impl Iterator<Item = u32> + Clone + 'a {
        let mut offset = 0;
        self.fields.get().iter().map(move |field| {
            let retval = offset;
            offset += field.ty.flattened().bit_count;
            retval
        })
    }
    pub fn name(&self) -> Interned<'ctx, str> {
        self.name
    }
    pub fn discriminant(&self) -> &Discriminant {
        &self.discriminant
    }
    pub fn discriminant_mut(&mut self) -> &mut Discriminant {
        &mut self.discriminant
    }
    pub fn into_discriminant(self) -> Discriminant {
        self.discriminant
    }
    pub fn map_discriminant<R>(self, f: impl FnOnce(Discriminant) -> R) -> IrVariantType<'ctx, R> {
        let Self {
            name,
            discriminant,
            fields,
            flattened_fields,
        } = self;
        IrVariantType {
            name,
            discriminant: f(discriminant),
            fields,
            flattened_fields,
        }
    }
    pub fn try_map_discriminant<R, E>(
        self,
        f: impl FnOnce(Discriminant) -> Result<R, E>,
    ) -> Result<IrVariantType<'ctx, R>, E> {
        let Self {
            name,
            discriminant,
            fields,
            flattened_fields,
        } = self;
        Ok(IrVariantType {
            name,
            discriminant: f(discriminant)?,
            fields,
            flattened_fields,
        })
    }
    pub fn flattened_fields(&self) -> IrBitVectorType {
        self.flattened_fields
    }
    pub fn fields(&self) -> Interned<'ctx, [IrFieldType<'ctx>]> {
        self.fields
    }
}

/// the variants are always sorted in ascending order by their discriminant with no duplicate discriminants
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrAggregateType<'ctx> {
    discriminant_type: IrBitVectorType,
    variants: Interned<'ctx, [IrVariantType<'ctx>]>,
    flattened_fields: IrBitVectorType,
}

impl<'ctx> IrAggregateType<'ctx> {
    pub fn new_struct(ctx: impl AsContext<'ctx>, fields: impl AsRef<[IrFieldType<'ctx>]>) -> Self {
        let ctx = ctx.ctx();
        Self::new_unchecked(
            IrBitVectorType {
                bit_count: 0,
                signed: false,
            },
            [IrVariantType::new(ctx, "", LiteralBits::new(), fields)].intern(ctx),
        )
    }
    pub fn struct_fields(self) -> Option<Interned<'ctx, [IrFieldType<'ctx>]>> {
        match (self.variants().get(), self.discriminant_type) {
            (
                [variant],
                IrBitVectorType {
                    bit_count: 0,
                    signed: false,
                },
            ) if variant.name().is_empty() => Some(variant.fields()),
            _ => None,
        }
    }
    pub fn is_struct(self) -> bool {
        self.struct_fields().is_some()
    }
    /// the variants are sorted in ascending order by their discriminant, asserting that there are no duplicate discriminants
    pub fn new<I: IntoIterator<Item = IrVariantType<'ctx>>, Ctx: AsContext<'ctx>>(
        ctx: Ctx,
        discriminant_type: IrBitVectorType,
        variants: I,
        caller: &SourceLocation<'ctx>,
    ) -> Self {
        let ctx = ctx.ctx();
        let mut variants: Vec<IrVariantType<'ctx>> = variants.into_iter().collect();
        for i in &variants {
            assert_eq!(
                i.discriminant.value_type(),
                discriminant_type,
                "variant {:?} has an unexpected discriminant type\nat {}",
                i.name,
                caller,
            );
        }
        variants.sort_unstable_by(|a, b| a.discriminant.value().cmp_value(b.discriminant.value()));
        for i in variants.windows(2) {
            assert_ne!(
                i[0].discriminant, i[1].discriminant,
                "duplicate discriminant for variants: {:?} and {:?}\nat {}",
                i[0].name, i[1].name, caller,
            );
        }
        Self::new_unchecked(discriminant_type, variants.intern(ctx))
    }
    pub fn new_unchecked(
        discriminant_type: IrBitVectorType,
        variants: Interned<'ctx, [IrVariantType<'ctx>]>,
    ) -> Self {
        let flattened_fields = union_bit_vectors(variants.iter().map(|v| v.flattened_fields()));
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
    pub fn enum_with_generated_discriminant<
        I: IntoIterator<Item = IrVariantType<'ctx, ()>>,
        Ctx: AsContext<'ctx>,
    >(
        ctx: Ctx,
        variants: I,
    ) -> Self {
        let ctx = ctx.ctx();
        let mut variants: Vec<IrVariantType<'ctx>> = variants
            .into_iter()
            .map(|variant_type| variant_type.map_discriminant(|_| LiteralBits::default()))
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
    ) -> Option<(usize, &'ctx IrVariantType<'ctx>)> {
        self.get_variant_index(discriminant)
            .map(|index| (index, &self.variants().get()[index]))
    }
    pub fn get_variant(self, discriminant: &LiteralBits) -> Option<&'ctx IrVariantType<'ctx>> {
        self.get_variant_and_index(discriminant).map(|v| v.1)
    }
    /// the variants are always sorted in ascending order by their discriminant with no duplicate discriminants
    pub fn variants(self) -> Interned<'ctx, [IrVariantType<'ctx>]> {
        self.variants
    }
    pub fn flattened(self) -> IrBitVectorType {
        flatten_type_sequence([self.discriminant_type.into(), self.flattened_fields.into()])
    }
    pub fn flattened_fields(self) -> IrBitVectorType {
        self.flattened_fields
    }
}

impl<'ctx> From<IrAggregateType<'ctx>> for IrValueType<'ctx> {
    fn from(v: IrAggregateType<'ctx>) -> Self {
        Self::Aggregate(v)
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
    Aggregate(IrAggregateType<'ctx>),
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
    pub fn aggregate(self) -> Option<IrAggregateType<'ctx>> {
        match self {
            IrValueType::Aggregate(v) => Some(v),
            _ => None,
        }
    }
    pub fn flattened(self) -> IrBitVectorType {
        match self {
            IrValueType::BitVector(v) => v,
            IrValueType::Array(v) => v.flattened(),
            IrValueType::Aggregate(v) => v.flattened(),
        }
    }
}

impl fmt::Debug for IrValueType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValueType::BitVector(v) => v.fmt(f),
            IrValueType::Array(v) => v.fmt(f),
            IrValueType::Aggregate(v) => v.fmt(f),
        }
    }
}

pub type IrValueTypeRef<'ctx> = Interned<'ctx, IrValueType<'ctx>>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        context::{Context, ContextRef},
        prelude::UInt,
    };

    #[test]
    fn test_generate_discriminant_type() {
        assert_eq!(
            IrAggregateType::generate_discriminant_type(0),
            IrBitVectorType {
                bit_count: 0,
                signed: false
            }
        );
        assert_eq!(
            IrAggregateType::generate_discriminant_type(1),
            IrBitVectorType {
                bit_count: 0,
                signed: false
            }
        );
        assert_eq!(
            IrAggregateType::generate_discriminant_type(2),
            IrBitVectorType {
                bit_count: 1,
                signed: false
            }
        );
        assert_eq!(
            IrAggregateType::generate_discriminant_type(3),
            IrBitVectorType {
                bit_count: 2,
                signed: false
            }
        );
        assert_eq!(
            IrAggregateType::generate_discriminant_type(4),
            IrBitVectorType {
                bit_count: 2,
                signed: false
            }
        );
        assert_eq!(
            IrAggregateType::generate_discriminant_type(5),
            IrBitVectorType {
                bit_count: 3,
                signed: false
            }
        );
        assert_eq!(
            IrAggregateType::generate_discriminant_type(usize::MAX),
            IrBitVectorType {
                bit_count: usize::BITS,
                signed: false
            }
        );
    }

    #[test]
    fn test_generate_discriminant() {
        Context::with(|ctx: ContextRef<'_>| {
            let type1 = IrAggregateType::new(
                ctx,
                IrBitVectorType {
                    bit_count: 2,
                    signed: false,
                },
                [
                    IrVariantType::new(ctx, "B", UInt::<2>::wrapping_new(1).into(), []),
                    IrVariantType::new(ctx, "A", UInt::<2>::wrapping_new(0).into(), []),
                    IrVariantType::new(ctx, "C", UInt::<2>::wrapping_new(2).into(), []),
                ],
                &SourceLocation::caller(),
            );
            let type2 = IrAggregateType::enum_with_generated_discriminant(
                ctx,
                [
                    IrVariantType::new(ctx, "A", (), []),
                    IrVariantType::new(ctx, "B", (), []),
                    IrVariantType::new(ctx, "C", (), []),
                ],
            );
            assert_eq!(type1, type2);
        });
    }
}
