// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::ir::{
    logic::{IrWire, IrWireValue},
    types::IrValueType,
    values::{IrValue, IrValueRef},
};
use alloc::{string::String, vec::Vec};
use core::{
    borrow::Borrow,
    cell::RefCell,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    ops::Deref,
    ptr,
};
use hashbrown::{hash_map::RawEntryMut, HashMap};
use typed_arena::Arena;

pub trait Internable<'ctx>: ArenaAllocatable<'ctx> + HasArena<'ctx> + Hash + Eq {}

pub trait InternImpl<'ctx, T: ArenaAllocatable<'ctx, Self>>: Internable<'ctx> {
    #[must_use]
    fn intern(value: T, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self>;
}

pub trait Intern<'ctx, T: Internable<'ctx> + ?Sized = Self>: ArenaAllocatable<'ctx, T> {
    #[must_use]
    fn intern(self, ctx: ContextRef<'ctx>) -> Interned<'ctx, T>
    where
        Self: Sized;
}

impl<'ctx, T: ?Sized + InternImpl<'ctx, V>, V: ArenaAllocatable<'ctx, T>> Intern<'ctx, T> for V {
    fn intern(self, ctx: ContextRef<'ctx>) -> Interned<'ctx, T> {
        InternImpl::intern(self, ctx)
    }
}

impl Internable<'_> for str {}

impl<'ctx, T: InternImpl<'ctx, T>> Internable<'ctx> for T {}

impl<'ctx, T> Internable<'ctx> for [T] where [T]: InternImpl<'ctx, Vec<T>> {}

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

pub trait HasArena<'ctx>: 'ctx {
    type Arena: Default + 'ctx;
}

impl HasArena<'_> for str {
    type Arena = Arena<u8>;
}

impl<'ctx, T: 'ctx> HasArena<'ctx> for T {
    type Arena = Arena<T>;
}

impl<'ctx, T: 'ctx> HasArena<'ctx> for [T] {
    type Arena = Arena<T>;
}

pub trait ArenaAllocatable<'ctx, T: HasArena<'ctx> + ?Sized = Self>: Borrow<T> {
    #[must_use]
    fn allocate(arena: &'ctx T::Arena, value: Self) -> &'ctx T
    where
        Self: Sized,
    {
        let _ = arena;
        let _ = value;
        // work around https://github.com/rust-lang/rust/issues/20021
        panic!("allocate method should always be overridden when Self: Sized")
    }
}

impl<'ctx> ArenaAllocatable<'ctx> for str {}

impl<'ctx, T: 'ctx> ArenaAllocatable<'ctx> for [T] {}

impl<'ctx> ArenaAllocatable<'ctx, str> for &'_ str {
    fn allocate(arena: &'ctx Arena<u8>, value: Self) -> &'ctx str {
        arena.alloc_str(value)
    }
}

impl<'ctx, T: Clone + 'ctx> ArenaAllocatable<'ctx, T> for &'_ T {
    fn allocate(arena: &'ctx Arena<T>, value: Self) -> &'ctx T {
        arena.alloc(value.clone())
    }
}

impl<'ctx, T: 'ctx> ArenaAllocatable<'ctx> for T {
    fn allocate(arena: &'ctx Arena<T>, value: Self) -> &'ctx T {
        arena.alloc(value)
    }
}

impl<'ctx> ArenaAllocatable<'ctx, str> for String {
    fn allocate(arena: &'ctx Arena<u8>, value: Self) -> &'ctx str {
        arena.alloc_str(&value)
    }
}

impl<'ctx, T: 'ctx> ArenaAllocatable<'ctx, [T]> for Vec<T> {
    fn allocate(arena: &'ctx Arena<T>, value: Self) -> &'ctx [T] {
        arena.alloc_extend(value)
    }
}

impl<'ctx, T: 'ctx, const N: usize> ArenaAllocatable<'ctx, [T]> for [T; N] {
    fn allocate(arena: &'ctx Arena<T>, value: Self) -> &'ctx [T] {
        arena.alloc_extend(value)
    }
}

impl<'ctx, T: 'ctx + Clone> ArenaAllocatable<'ctx, [T]> for &'_ [T] {
    fn allocate(arena: &'ctx Arena<T>, value: Self) -> &'ctx [T] {
        arena.alloc_extend(value.iter().cloned())
    }
}

struct Interner<'ctx, T: Internable<'ctx> + ?Sized> {
    arena: T::Arena,
    hash_table: RefCell<HashMap<&'ctx T, ()>>,
}

impl<'ctx, T: Internable<'ctx> + ?Sized> Default for Interner<'ctx, T> {
    fn default() -> Self {
        Self {
            arena: Default::default(),
            hash_table: Default::default(),
        }
    }
}

impl<'ctx, T: Internable<'ctx> + ?Sized> Interner<'ctx, T> {
    #[must_use]
    fn intern_impl<V: ArenaAllocatable<'ctx, T>>(&'ctx self, value: V) -> Interned<'ctx, T> {
        let mut hash_table = self.hash_table.borrow_mut();
        let mut hasher = hash_table.hasher().build_hasher();
        <&T>::hash(&value.borrow(), &mut hasher);
        match hash_table
            .raw_entry_mut()
            .from_hash(hasher.finish(), |v| *v == value.borrow())
        {
            RawEntryMut::Occupied(entry) => Interned(*entry.key()),
            RawEntryMut::Vacant(entry) => {
                let allocated = V::allocate(&self.arena, value);
                entry.insert(allocated, ());
                Interned(allocated)
            }
        }
    }
}

#[derive(Default)]
pub struct Context<'ctx> {
    modules: RefCell<Vec<ModuleRef<'ctx>>>,
    string_interner: Interner<'ctx, str>,
    value_type_interner: Interner<'ctx, IrValueType<'ctx>>,
    value_interner: Interner<'ctx, IrValue<'ctx>>,
    value_ref_interner: Interner<'ctx, [IrValueRef<'ctx>]>,
    modules_arena: Arena<Module<'ctx>>,
    wires_arena: Arena<IrWire<'ctx>>,
}

impl<'ctx, T: ArenaAllocatable<'ctx, Self>> InternImpl<'ctx, T> for str {
    fn intern(value: T, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.string_interner.intern_impl(value)
    }
}

impl<'ctx, T: ArenaAllocatable<'ctx, Self>> InternImpl<'ctx, T> for IrValueType<'ctx> {
    fn intern(value: T, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.value_type_interner.intern_impl(value)
    }
}

impl<'ctx, T: ArenaAllocatable<'ctx, Self>> InternImpl<'ctx, T> for IrValue<'ctx> {
    fn intern(value: T, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.value_interner.intern_impl(value)
    }
}

impl<'ctx, T: ArenaAllocatable<'ctx, Self>> InternImpl<'ctx, T> for [IrValueRef<'ctx>] {
    fn intern(value: T, ctx: ContextRef<'ctx>) -> Interned<'ctx, Self> {
        ctx.value_ref_interner.intern_impl(value)
    }
}

pub type ContextRef<'ctx> = &'ctx Context<'ctx>;

pub struct Module<'ctx> {
    ctx: ContextRef<'ctx>,
    id: usize,
    wires: RefCell<Vec<IrWireValue<'ctx>>>,
}

pub type ModuleRef<'ctx> = &'ctx Module<'ctx>;

impl<'ctx> Module<'ctx> {
    pub fn new(ctx: ContextRef<'ctx>) -> ModuleRef<'ctx> {
        let module = ctx.modules_arena.alloc(Module {
            ctx,
            id: ctx.modules.borrow().len(),
            wires: RefCell::default(),
        });
        ctx.modules.borrow_mut().push(module);
        module
    }
    pub fn ctx(&self) -> ContextRef<'ctx> {
        self.ctx
    }
    pub fn id(&self) -> impl fmt::Debug + 'static {
        self.id
    }
}

pub(crate) fn create_ir_wire_impl<'ctx>(mut wire: IrWire<'ctx>) -> IrWireValue<'ctx> {
    let module = wire.module();
    wire.id = module.wires.borrow().len();
    let retval = IrWireValue(module.ctx().wires_arena.alloc(wire));
    module.wires.borrow_mut().push(retval);
    retval
}

impl fmt::Debug for Context<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Context").finish_non_exhaustive()
    }
}
