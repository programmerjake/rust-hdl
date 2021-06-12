// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::ir::{
    types::IrValueType,
    values::{IrValue, IrValueRef},
};
use alloc::{
    borrow::{Cow, ToOwned},
    vec::Vec,
};
use core::{
    cell::RefCell,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    ops::Deref,
    ptr,
};
use hashbrown::{hash_map::RawEntryMut, HashMap};
use typed_arena::Arena;

pub trait Internable<'ctx>: Hash + Eq + ToOwned + 'ctx {
    fn intern_clone(&self, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        Self::intern_cow(Cow::Borrowed(self), ctx)
    }
    fn intern(self, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self>
    where
        Self: Sized + ToOwned<Owned = Self>,
    {
        Self::intern_cow(Cow::Owned(self), ctx)
    }
    fn intern_cow(value: Cow<'_, Self>, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self>;
}

#[derive(Debug, Eq)]
pub struct Interned<'ctx, T: Internable<'ctx> + ?Sized>(&'ctx T);

impl<'ctx, T: Internable<'ctx> + ?Sized> Copy for Interned<'ctx, T> {}

impl<'ctx, T: Internable<'ctx> + ?Sized> Clone for Interned<'ctx, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'ctx, T: Internable<'ctx> + ?Sized> PartialEq for Interned<'ctx, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'ctx, T: Internable<'ctx> + ?Sized> Hash for Interned<'ctx, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const T).hash(state)
    }
}

impl<'ctx, T: Internable<'ctx> + ?Sized> Deref for Interned<'ctx, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

trait InternableImpl<'ctx>: Internable<'ctx> + ToOwned {
    type Arena: Default + 'ctx;
    fn allocate(arena: &'ctx Self::Arena, value: Cow<'_, Self>) -> &'ctx Self;
}

impl<'ctx> InternableImpl<'ctx> for str {
    type Arena = Arena<u8>;

    fn allocate(arena: &'ctx Self::Arena, value: Cow<'_, Self>) -> &'ctx Self {
        arena.alloc_str(&value)
    }
}

impl<'ctx, T: Internable<'ctx> + ToOwned + Sized> InternableImpl<'ctx> for T {
    type Arena = Arena<T>;

    fn allocate(arena: &'ctx Self::Arena, value: Cow<'_, Self>) -> &'ctx Self {
        arena.alloc(value.into_owned())
    }
}

impl<'ctx, T: Clone> InternableImpl<'ctx> for [T]
where
    Self: Internable<'ctx>,
{
    type Arena = Arena<T>;

    fn allocate(arena: &'ctx Self::Arena, value: Cow<'_, Self>) -> &'ctx Self {
        match value {
            Cow::Borrowed(value) => arena.alloc_extend(value.iter().cloned()),
            Cow::Owned(value) => arena.alloc_extend(value.into_iter()),
        }
    }
}

struct Interner<'ctx, T: InternableImpl<'ctx> + ?Sized> {
    arena: T::Arena,
    hash_table: RefCell<HashMap<&'ctx T, ()>>,
}

impl<'ctx, T: InternableImpl<'ctx> + ?Sized> Default for Interner<'ctx, T> {
    fn default() -> Self {
        Self {
            arena: Default::default(),
            hash_table: Default::default(),
        }
    }
}

impl<'ctx, T: InternableImpl<'ctx> + ?Sized> Interner<'ctx, T> {
    fn intern(&'ctx self, value: Cow<'_, T>) -> Interned<'ctx, T> {
        let mut hash_table = self.hash_table.borrow_mut();
        let hasher = hash_table.hasher().build_hasher();
        <&T>::hash(&value, &mut hasher);
        match hash_table
            .raw_entry_mut()
            .from_hash(hasher.finish(), |v| *v == value)
        {
            RawEntryMut::Occupied(entry) => Interned(*entry.key()),
            RawEntryMut::Vacant(entry) => {
                let allocated = T::allocate(&self.arena, value);
                entry.insert(allocated, ());
                Interned(allocated)
            }
        }
    }
}

#[derive(Default)]
pub struct Context<'ctx> {
    modules: RefCell<Vec<&'ctx Module<'ctx>>>,
    string_interner: Interner<'ctx, str>,
    value_type_interner: Interner<'ctx, IrValue<'ctx>>,
    value_interner: Interner<'ctx, IrValue<'ctx>>,
    value_ref_interner: Interner<'ctx, [IrValueRef<'ctx>]>,
    modules_arena: Arena<Module<'ctx>>,
}

impl<'ctx> Internable<'ctx> for str {
    fn intern_cow(value: Cow<'_, Self>, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.string_interner.intern(value)
    }
}

impl<'ctx> Internable<'ctx> for IrValueType<'ctx> {
    fn intern_cow(value: Cow<'_, Self>, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.value_type_interner.intern(value)
    }
}

impl<'ctx> Internable<'ctx> for IrValue<'ctx> {
    fn intern_cow(value: Cow<'_, Self>, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.value_interner.intern(value)
    }
}

impl<'ctx> Internable<'ctx> for [IrValueRef<'ctx>] {
    fn intern_cow(value: Cow<'_, Self>, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.value_ref_interner.intern(value)
    }
}

pub type ContextRef<'ctx> = &'ctx Context<'ctx>;

#[derive(Debug)]
pub struct Module<'ctx> {
    ctx: ContextRef<'ctx>,
}

pub type ModuleRef<'ctx> = &'ctx Module<'ctx>;

impl<'ctx> Module<'ctx> {
    pub fn new(ctx: ContextRef<'ctx>) -> ModuleRef<'ctx> {
        let module = ctx.modules_arena.alloc(Module { ctx });
        ctx.modules.borrow_mut().push(module);
        module
    }
    pub fn ctx(&self) -> ContextRef<'ctx> {
        self.ctx
    }
}

impl fmt::Debug for Context<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Context").finish_non_exhaustive()
    }
}
