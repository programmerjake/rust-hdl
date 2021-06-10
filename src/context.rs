// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::ir::types::IrValueType;
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

pub trait Internable: Hash + Eq + ToOwned {
    fn intern_clone<'ctx>(&self, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        Self::intern_cow(Cow::Borrowed(self), ctx)
    }
    fn intern<'ctx>(self, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self>
    where
        Self: Sized,
    {
        Self::intern_cow(Cow::Owned(self), ctx)
    }
    fn intern_cow<'ctx>(value: Cow<'_, Self>, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self>;
}

#[derive(Debug, Eq)]
pub struct Interned<'a, T: Internable + ?Sized>(&'a T);

impl<'a, T: Internable + ?Sized> Copy for Interned<'a, T> {}

impl<'a, T: Internable + ?Sized> Clone for Interned<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T: Internable + ?Sized> PartialEq for Interned<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'a, T: Internable + ?Sized> Hash for Interned<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const T).hash(state)
    }
}

impl<'a, T: Internable + ?Sized> Deref for Interned<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

trait InternableImpl: Internable + ToOwned {
    type Arena: Default;
    fn allocate<'ctx>(arena: &'ctx Self::Arena, value: Cow<'_, Self>) -> &'ctx Self;
}

impl InternableImpl for str {
    type Arena = Arena<u8>;

    fn allocate<'ctx>(arena: &'ctx Self::Arena, value: Cow<'_, Self>) -> &'ctx Self {
        arena.alloc_str(value)
    }
}

impl<T: Internable + ToOwned + Sized> InternableImpl for T {
    type Arena = Arena<T>;

    fn allocate<'ctx>(arena: &'ctx Self::Arena, value: Cow<'_, Self>) -> &'ctx Self {
        arena.alloc(value.into_owned())
    }
}

struct Interner<'ctx, T: InternableImpl + ?Sized> {
    arena: T::Arena,
    hash_table: RefCell<HashMap<&'ctx T, ()>>,
}

impl<'ctx, T: InternableImpl + ?Sized> Default for Interner<'ctx, T> {
    fn default() -> Self {
        Self {
            arena: Default::default(),
            hash_table: Default::default(),
        }
    }
}

impl<'ctx, T: InternableImpl + ?Sized> Interner<'ctx, T> {
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
    value_type_interner: Interner<'ctx, IrValueType>,
    modules_arena: Arena<Module<'ctx>>,
}

impl Internable for str {
    fn intern_cow<'ctx>(value: Cow<'_, Self>, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.string_interner.intern(value)
    }
}

impl Internable for IrValueType {
    fn intern_cow<'ctx>(value: Cow<'_, Self>, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.value_type_interner.intern(value)
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
