// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::context::Interned;
use core::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrStructFieldType<'ctx> {
    pub name: Interned<'ctx, str>,
    pub ty: IrValueTypeRef<'ctx>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrStructType<'ctx> {
    pub fields: Interned<'ctx, [IrStructFieldType<'ctx>]>,
}

impl<'ctx> From<IrStructType<'ctx>> for IrValueType<'ctx> {
    fn from(v: IrStructType<'ctx>) -> Self {
        Self::Struct(v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrArrayType<'ctx> {
    pub element: IrValueTypeRef<'ctx>,
    pub length: usize,
}

impl<'ctx> From<IrArrayType<'ctx>> for IrValueType<'ctx> {
    fn from(v: IrArrayType<'ctx>) -> Self {
        Self::Array(v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IrBitVectorType {
    pub bit_count: u32,
}

impl From<IrBitVectorType> for IrValueType<'_> {
    fn from(v: IrBitVectorType) -> Self {
        Self::BitVector(v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrValueType<'ctx> {
    BitVector(IrBitVectorType),
    Array(IrArrayType<'ctx>),
    Struct(IrStructType<'ctx>),
}

impl<'ctx> IrValueType<'ctx> {
    pub fn is_bool(self) -> bool {
        matches!(self, Self::BitVector(IrBitVectorType { bit_count: 1 }))
    }
}

impl fmt::Debug for IrValueType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValueType::BitVector(v) => v.fmt(f),
            IrValueType::Array(v) => v.fmt(f),
            IrValueType::Struct(v) => v.fmt(f),
        }
    }
}

pub type IrValueTypeRef<'ctx> = Interned<'ctx, IrValueType<'ctx>>;
