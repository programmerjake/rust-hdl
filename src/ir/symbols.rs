// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::context::{AsContext, Intern, Interned};
use alloc::{
    borrow::Cow,
    collections::{btree_map::Entry, BTreeMap},
    string::String,
};
use core::{
    cell::{Cell, RefCell},
    cmp::Ordering,
    fmt::{self, Write},
    ops::Deref,
};

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct IrSymbol<'ctx>(Interned<'ctx, str>);

impl fmt::Debug for IrSymbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let v: &str = self;
        write!(f, "{:?}", v)
    }
}

impl fmt::Display for IrSymbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let v: &str = self;
        write!(f, "{}", v)
    }
}

impl<'ctx> Deref for IrSymbol<'ctx> {
    type Target = Interned<'ctx, str>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct SymbolInTable<'ctx>(Interned<'ctx, str>);

impl Ord for SymbolInTable<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        let lhs: *const str = self.0.get();
        let rhs: *const str = other.0.get();
        lhs.cmp(&rhs)
    }
}

impl PartialOrd for SymbolInTable<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Debug for SymbolInTable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0.get())
    }
}

#[derive(Clone, Copy)]
struct SymbolData {
    next_suffix_to_try: usize,
}

impl Default for SymbolData {
    fn default() -> Self {
        Self {
            next_suffix_to_try: 2,
        }
    }
}

type SymbolTableImpl<'ctx> = BTreeMap<SymbolInTable<'ctx>, SymbolData>;

pub struct IrSymbolTable<'ctx> {
    next_anonymous_name_to_try: Cell<usize>,
    symbols: RefCell<SymbolTableImpl<'ctx>>,
}

impl fmt::Debug for IrSymbolTable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Symbols<T>(T);
        impl fmt::Debug for Symbols<&'_ IrSymbolTable<'_>> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_set()
                    .entries(self.0.symbols.borrow().keys())
                    .finish()
            }
        }
        f.debug_struct("IrSymbolTable")
            .field("symbols", &Symbols(self))
            .finish()
    }
}

impl<'ctx> IrSymbolTable<'ctx> {
    pub fn new() -> Self {
        Self {
            next_anonymous_name_to_try: Cell::new(0),
            symbols: RefCell::default(),
        }
    }
    pub fn find(&self, name: Interned<'ctx, str>) -> Option<IrSymbol<'ctx>> {
        if self.symbols.borrow().contains_key(&SymbolInTable(name)) {
            Some(IrSymbol(name))
        } else {
            None
        }
    }
    fn try_insert_impl(
        &self,
        symbols: &mut SymbolTableImpl<'ctx>,
        name: Interned<'ctx, str>,
    ) -> Result<IrSymbol<'ctx>, SymbolData> {
        if name.is_empty() {
            return Err(SymbolData {
                next_suffix_to_try: self.next_anonymous_name_to_try.get(),
            });
        }
        match symbols.entry(SymbolInTable(name)) {
            Entry::Vacant(entry) => {
                entry.insert(SymbolData::default());
                Ok(IrSymbol(name))
            }
            Entry::Occupied(entry) => Err(*entry.get()),
        }
    }
    pub fn try_insert_unchanged(&self, name: Interned<'ctx, str>) -> Result<IrSymbol<'ctx>, ()> {
        self.try_insert_impl(&mut self.symbols.borrow_mut(), name)
            .map_err(|_| ())
    }
    #[cold]
    fn insert_uniquified_fallback(
        &self,
        symbols: &mut SymbolTableImpl<'ctx>,
        ctx: impl AsContext<'ctx>,
        mut name: String,
        interned_original_name: Interned<'ctx, str>,
        mut next_suffix_to_try: usize,
    ) -> IrSymbol<'ctx> {
        let ctx = ctx.ctx();
        loop {
            name.truncate(interned_original_name.len());
            write!(name, "#{}", next_suffix_to_try).unwrap();
            next_suffix_to_try += 1;
            if let Ok(retval) = self.try_insert_impl(symbols, name.deref().intern(ctx)) {
                if interned_original_name.is_empty() {
                    self.next_anonymous_name_to_try.set(next_suffix_to_try);
                } else {
                    symbols
                        .get_mut(&SymbolInTable(interned_original_name))
                        .unwrap()
                        .next_suffix_to_try = next_suffix_to_try;
                }
                break retval;
            }
        }
    }
    pub fn insert_uniquified<'a, 'b, Name: Into<Cow<'b, str>>, Ctx: AsContext<'ctx>>(
        &'a self,
        ctx: Ctx,
        name: Name,
    ) -> IrSymbol<'ctx> {
        let ctx = ctx.ctx();
        let name: Cow<'b, str> = name.into();
        let mut symbols = self.symbols.borrow_mut();
        let interned_name = (*name).intern(ctx);
        match self.try_insert_impl(&mut symbols, interned_name) {
            Ok(retval) => retval,
            Err(SymbolData { next_suffix_to_try }) => self.insert_uniquified_fallback(
                &mut symbols,
                ctx,
                name.into_owned(),
                interned_name,
                next_suffix_to_try,
            ),
        }
    }
}

impl<'ctx> Default for IrSymbolTable<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{Context, ContextRef};

    #[test]
    fn test() {
        Context::with(|ctx: ContextRef<'_>| {
            let symbol_table = ctx.root_symbol_table();
            assert_eq!(symbol_table.find("".intern(ctx)), None);
            assert_eq!(symbol_table.find("#0".intern(ctx)), None);
            assert_eq!(symbol_table.find("#1".intern(ctx)), None);
            assert_eq!(symbol_table.insert_uniquified(ctx, "").get(), "#0");
            assert_eq!(symbol_table.insert_uniquified(ctx, "").get(), "#1");
            assert_eq!(symbol_table.insert_uniquified(ctx, "").get(), "#2");
            assert_eq!(symbol_table.insert_uniquified(ctx, "blah").get(), "blah");
            assert_eq!(symbol_table.insert_uniquified(ctx, "blah").get(), "blah#2");
            assert_eq!(symbol_table.insert_uniquified(ctx, "blah").get(), "blah#3");
            assert_eq!(symbol_table.insert_uniquified(ctx, "blah").get(), "blah#4");
            assert_eq!(
                symbol_table.insert_uniquified(ctx, "blah#3").get(),
                "blah#3#2"
            );
        });
    }
}
