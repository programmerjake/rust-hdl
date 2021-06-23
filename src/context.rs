// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::ir::{
    io::IrOutputReadData,
    logic::IrWire,
    module::{IrModule, IrModuleRef},
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
    ptr::{self, NonNull},
    slice,
};
use hashbrown::{hash_map::DefaultHashBuilder, raw::RawTable};
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

#[derive(Eq)]
pub struct Interned<'ctx, T: Internable<'ctx> + ?Sized>(&'ctx T);

impl<'ctx, T: Internable<'ctx> + ?Sized + fmt::Debug> fmt::Debug for Interned<'ctx, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        T::fmt(self, f)
    }
}

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

unsafe trait HasNonNullPtr {
    type NonNullPtr: Copy + 'static;
    unsafe fn from_ptr<'a>(ptr: Self::NonNullPtr) -> &'a Self;
    fn to_ptr(&self) -> Self::NonNullPtr;
}

unsafe impl<T> HasNonNullPtr for T {
    type NonNullPtr = NonNull<()>;
    unsafe fn from_ptr<'a>(ptr: Self::NonNullPtr) -> &'a Self {
        unsafe { &*(ptr.as_ptr() as *const Self) }
    }
    fn to_ptr(&self) -> Self::NonNullPtr {
        unsafe { NonNull::new_unchecked(self as *const Self as *mut ()) }
    }
}

unsafe impl HasNonNullPtr for str {
    type NonNullPtr = NonNull<str>;
    unsafe fn from_ptr<'a>(ptr: Self::NonNullPtr) -> &'a Self {
        unsafe { &*ptr.as_ptr() }
    }
    fn to_ptr(&self) -> Self::NonNullPtr {
        NonNull::from(self)
    }
}

unsafe impl<T> HasNonNullPtr for [T] {
    type NonNullPtr = (NonNull<()>, usize);
    unsafe fn from_ptr<'a>(ptr: Self::NonNullPtr) -> &'a Self {
        unsafe { slice::from_raw_parts(ptr.0.as_ptr() as *const T, ptr.1) }
    }
    fn to_ptr(&self) -> Self::NonNullPtr {
        unsafe { (NonNull::new_unchecked(self.as_ptr() as *mut ()), self.len()) }
    }
}

struct Interner<'ctx, T: Internable<'ctx> + HasNonNullPtr + ?Sized> {
    arena: T::Arena,
    hash_table: RefCell<RawTable<T::NonNullPtr>>,
    hasher: DefaultHashBuilder,
}

impl<'ctx, T: Internable<'ctx> + HasNonNullPtr + ?Sized> Default for Interner<'ctx, T> {
    fn default() -> Self {
        Self {
            arena: Default::default(),
            hash_table: Default::default(),
            hasher: Default::default(),
        }
    }
}

impl<'ctx, T: Internable<'ctx> + HasNonNullPtr + ?Sized> Interner<'ctx, T> {
    #[must_use]
    fn intern_impl<V: ArenaAllocatable<'ctx, T>>(&'ctx self, value: V) -> Interned<'ctx, T> {
        let mut hash_table = self.hash_table.borrow_mut();
        let hash_fn = |value: &T| {
            let mut hasher = self.hasher.build_hasher();
            value.hash(&mut hasher);
            hasher.finish()
        };
        let value_ref = value.borrow();
        let hash = hash_fn(value_ref);
        if let Some(interned) = hash_table.get(hash, |v| unsafe { T::from_ptr(*v) == value_ref }) {
            Interned(unsafe { T::from_ptr(*interned) })
        } else {
            let allocated = V::allocate(&self.arena, value);
            hash_table.insert(hash, allocated.to_ptr(), |v| unsafe {
                hash_fn(T::from_ptr(*v))
            });
            Interned(allocated)
        }
    }
}

pub struct Context<'ctx> {
    pub(crate) modules: RefCell<Vec<IrModuleRef<'ctx>>>,
    string_interner: Interner<'ctx, str>,
    value_type_interner: Interner<'ctx, IrValueType<'ctx>>,
    value_interner: Interner<'ctx, IrValue<'ctx>>,
    value_ref_interner: Interner<'ctx, [IrValueRef<'ctx>]>,
    pub(crate) modules_arena: Arena<IrModule<'ctx>>,
    pub(crate) wires_arena: Arena<IrWire<'ctx>>,
    pub(crate) output_read_data_arena: Arena<IrOutputReadData<'ctx>>,
}

impl Context<'_> {
    pub fn with<F: for<'ctx> FnOnce(ContextRef<'ctx>) -> R, R>(f: F) -> R {
        let context = Context {
            modules: RefCell::default(),
            string_interner: Interner::default(),
            value_type_interner: Interner::default(),
            value_interner: Interner::default(),
            value_ref_interner: Interner::default(),
            modules_arena: Arena::default(),
            wires_arena: Arena::default(),
            output_read_data_arena: Arena::default(),
        };
        f(&context)
    }
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

impl fmt::Debug for Context<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Context").finish_non_exhaustive()
    }
}
