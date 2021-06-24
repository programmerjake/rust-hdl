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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrValueType<'ctx> {
    BitVector {
        bit_count: u32,
    },
    Array {
        element: IrValueTypeRef<'ctx>,
        length: usize,
    },
    Struct(IrStructType<'ctx>),
}

impl fmt::Debug for IrValueType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValueType::BitVector { bit_count } => f
                .debug_struct("BitVector")
                .field("bit_count", bit_count)
                .finish(),
            IrValueType::Array { element, length } => f
                .debug_struct("Array")
                .field("element", element)
                .field("length", length)
                .finish(),
            IrValueType::Struct(v) => v.fmt(f),
        }
    }
}

pub type IrValueTypeRef<'ctx> = Interned<'ctx, IrValueType<'ctx>>;
