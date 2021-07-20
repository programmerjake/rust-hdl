// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use alloc::vec::Vec;

use crate::{
    context::{AsContext, ContextRef, Intern, Internable, Interned},
    ir::{module::IrModuleRef, SourceLocation},
    module::AsIrModule,
};
use core::{
    fmt,
    hash::{Hash, Hasher},
    num::NonZeroUsize,
};

#[derive(Clone, Copy)]
struct ChildScopeData<'ctx> {
    depth: NonZeroUsize,
    source_location: SourceLocation<'ctx>,
    parent: ScopeRef<'ctx>,
    id: u64,
}

impl<'ctx> Eq for ChildScopeData<'ctx> {}

impl<'ctx> PartialEq for ChildScopeData<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'ctx> Hash for ChildScopeData<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

pub struct ScopePath<'a, 'ctx> {
    scope: &'a Scope<'ctx>,
}

impl fmt::Debug for ScopePath<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut path = Vec::with_capacity(self.scope.depth() + 1);
        let mut scope = self.scope;
        while let Some(ChildScopeData { parent, id, .. }) = scope.child_data {
            path.push(id);
            scope = parent.get();
        }
        write!(f, "{}", self.scope.module.path())?;
        for id in path {
            write!(f, "::'{}", id)?;
        }
        Ok(())
    }
}

impl fmt::Display for ScopePath<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeCombineErrorReason {
    ModulesNotSame,
    NoSubscopeRelation,
}

impl fmt::Display for ScopeCombineErrorReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScopeCombineErrorReason::ModulesNotSame => write!(
                f,
                "scopes from different modules can't be combined into a common scope"
            ),
            ScopeCombineErrorReason::NoSubscopeRelation => write!(
                f,
                "scopes without a subscope relation can't be combined into a common scope"
            ),
        }
    }
}

#[derive(Debug)]
pub struct ScopeCombineError<'ctx> {
    reason: ScopeCombineErrorReason,
    scope1: ScopeRef<'ctx>,
    scope2: ScopeRef<'ctx>,
    caller: SourceLocation<'ctx>,
}

impl fmt::Display for ScopeCombineError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{reason}:\n\
            tried to combine scopes at {caller}\n\
            scope1: {scope1} at {scope1_src}\n\
            scope2: {scope2} at {scope2_src}",
            reason = self.reason,
            caller = self.caller,
            scope1 = self.scope1.path(),
            scope1_src = self.scope1.source_location(),
            scope2 = self.scope2.path(),
            scope2_src = self.scope2.source_location(),
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SubscopeErrorReason {
    ModulesNotSame,
    NoSubscopeRelation,
}

#[derive(Debug)]
pub struct SubscopeError<'ctx> {
    reason: SubscopeErrorReason,
    ancestor_scope: Scope<'ctx>,
    expected_subscope: Scope<'ctx>,
    caller: SourceLocation<'ctx>,
}

impl fmt::Display for SubscopeError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.reason {
            SubscopeErrorReason::ModulesNotSame => write!(
                f,
                "expected a subscope of `ancestor_scope`, got a scope from a different module:\n\
                at {caller}\n\
                ancestor_scope: {ancestor_scope} at {ancestor_scope_src}\n\
                expected_subscope: {expected_subscope} at {expected_subscope_src}",
                caller = self.caller,
                ancestor_scope = self.ancestor_scope.path(),
                ancestor_scope_src = self.ancestor_scope.source_location(),
                expected_subscope = self.expected_subscope.path(),
                expected_subscope_src = self.expected_subscope.source_location(),
            ),
            SubscopeErrorReason::NoSubscopeRelation => write!(
                f,
                "`expected_subscope` is not a subscope of `ancestor_scope`:\n\
                at {caller}\n\
                ancestor_scope: {ancestor_scope} at {ancestor_scope_src}\n\
                expected_subscope: {expected_subscope} at {expected_subscope_src}",
                caller = self.caller,
                ancestor_scope = self.ancestor_scope.path(),
                ancestor_scope_src = self.ancestor_scope.source_location(),
                expected_subscope = self.expected_subscope.path(),
                expected_subscope_src = self.expected_subscope.source_location(),
            ),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Scope<'ctx> {
    child_data: Option<ChildScopeData<'ctx>>,
    module: IrModuleRef<'ctx>,
}

impl<'ctx> Scope<'ctx> {
    pub fn new<Ctx: AsContext<'ctx>>(
        parent: ScopeRef<'ctx>,
        source_location: SourceLocation<'ctx>,
    ) -> ScopeRef<'ctx> {
        let depth =
            NonZeroUsize::new(parent.depth().wrapping_add(1)).expect("too many nested scopes");
        let id = parent.module.next_scope_id.get();
        parent
            .module
            .next_scope_id
            .set(id.checked_add(1).expect("too many scopes in module"));
        Self {
            child_data: Some(ChildScopeData {
                depth,
                source_location,
                parent,
                id,
            }),
            module: parent.module,
        }
        .intern(parent.ctx())
    }
    pub fn source_location(self) -> SourceLocation<'ctx> {
        if let Some(ChildScopeData {
            source_location, ..
        }) = self.child_data
        {
            source_location
        } else {
            self.module.source_location()
        }
    }
    pub fn is_module_scope(self) -> bool {
        self.child_data.is_none()
    }
    pub fn parent(self) -> Option<ScopeRef<'ctx>> {
        self.child_data.map(|v| v.parent)
    }
    pub fn depth(self) -> usize {
        self.child_data.map_or(0, |v| v.depth.get())
    }
    /// returns true if `self` is a subscope of `potential_ancestor` --
    /// if `potential_ancestor` is either the same as `self` or is an ancestor scope of `self`.
    pub fn is_subscope_of(mut self, potential_ancestor: Self) -> bool {
        if self.module == potential_ancestor.module && self.depth() >= potential_ancestor.depth() {
            for _ in potential_ancestor.depth()..self.depth() {
                self = *self.parent().unwrap();
            }
            debug_assert_eq!(self.depth(), potential_ancestor.depth());
            self == potential_ancestor
        } else {
            false
        }
    }
    pub fn try_subscope_of(
        self,
        expected_ancestor: Scope<'ctx>,
        caller: &SourceLocation<'ctx>,
    ) -> Result<(), SubscopeError<'ctx>> {
        if self.module != expected_ancestor.module {
            Err(SubscopeError {
                reason: SubscopeErrorReason::ModulesNotSame,
                ancestor_scope: expected_ancestor,
                expected_subscope: self,
                caller: *caller,
            })
        } else if self.is_subscope_of(expected_ancestor) {
            Ok(())
        } else {
            Err(SubscopeError {
                reason: SubscopeErrorReason::ModulesNotSame,
                ancestor_scope: expected_ancestor,
                expected_subscope: self,
                caller: *caller,
            })
        }
    }
    #[track_caller]
    pub fn assert_subscope_of(self, expected_ancestor: Scope<'ctx>, caller: &SourceLocation<'ctx>) {
        if let Err(e) = self.try_subscope_of(expected_ancestor, caller) {
            panic!("{}", e);
        }
    }
    pub fn try_ancestor_of<T: OwningScope<'ctx>>(
        self,
        child: T,
        caller: &SourceLocation<'ctx>,
    ) -> Result<(), SubscopeError<'ctx>> {
        if let Some(child) = child.owning_scope() {
            child.try_subscope_of(self, caller)
        } else {
            Ok(())
        }
    }
    #[track_caller]
    pub fn assert_ancestor_of<T: OwningScope<'ctx>>(self, child: T, caller: &SourceLocation<'ctx>) {
        if let Some(child) = child.owning_scope() {
            child.assert_subscope_of(self, caller)
        }
    }
    pub fn path<'a>(&'a self) -> ScopePath<'a, 'ctx> {
        ScopePath { scope: self }
    }
    pub(crate) fn make_module_scope(module: IrModuleRef<'ctx>) -> ScopeRef<'ctx> {
        Self {
            child_data: None,
            module,
        }
        .intern(module.ctx())
    }
    pub fn try_combine<T: OwningScope<'ctx>, I: IntoIterator<Item = T>>(
        iter: I,
        caller: &SourceLocation<'ctx>,
    ) -> Result<Option<ScopeRef<'ctx>>, ScopeCombineError<'ctx>> {
        iter.into_iter()
            .try_fold(None, |retval, item| match (retval, item.owning_scope()) {
                (None, v) => Ok(v),
                (v, None) => Ok(v),
                (Some(scope1), Some(scope2)) => {
                    if scope1.module != scope2.module {
                        Err(ScopeCombineError {
                            reason: ScopeCombineErrorReason::ModulesNotSame,
                            scope1,
                            scope2,
                            caller: *caller,
                        })
                    } else if scope1.is_subscope_of(*scope2) {
                        Ok(Some(scope1))
                    } else if scope2.is_subscope_of(*scope1) {
                        Ok(Some(scope2))
                    } else {
                        Err(ScopeCombineError {
                            reason: ScopeCombineErrorReason::NoSubscopeRelation,
                            scope1,
                            scope2,
                            caller: *caller,
                        })
                    }
                }
            })
    }
    #[track_caller]
    pub fn combine_or_panic<T: OwningScope<'ctx>, I: IntoIterator<Item = T>>(
        iter: I,
        caller: &SourceLocation<'ctx>,
    ) -> Option<ScopeRef<'ctx>> {
        match Self::try_combine(iter, caller) {
            Ok(v) => v,
            Err(e) => panic!("{}", e),
        }
    }
}

impl<'ctx> AsContext<'ctx> for Scope<'ctx> {
    fn ctx(&self) -> ContextRef<'ctx> {
        self.module.ctx()
    }
}

impl<'ctx> AsIrModule<'ctx> for Scope<'ctx> {
    fn as_ir_module(&self) -> IrModuleRef<'ctx> {
        self.module
    }
}

impl<'ctx> fmt::Debug for Scope<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Scope")
            .field("source_location", &self.source_location())
            .field("path", &self.module.path())
            .finish_non_exhaustive()
    }
}

pub type ScopeRef<'ctx> = Interned<'ctx, Scope<'ctx>>;

pub trait OwningScope<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>>;
}

impl<'ctx> OwningScope<'ctx> for ScopeRef<'ctx> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        Some(*self)
    }
}

impl<'ctx, T: OwningScope<'ctx>> OwningScope<'ctx> for Option<T> {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        self.as_ref().and_then(OwningScope::owning_scope)
    }
}

impl<'ctx, T: ?Sized + OwningScope<'ctx>> OwningScope<'ctx> for &'_ T {
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        (**self).owning_scope()
    }
}

impl<'ctx, T: ?Sized + OwningScope<'ctx> + Internable<'ctx>> OwningScope<'ctx>
    for Interned<'ctx, T>
{
    fn owning_scope(&self) -> Option<ScopeRef<'ctx>> {
        (**self).owning_scope()
    }
}
