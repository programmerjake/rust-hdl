// SPDX-License-Identifier: LGPL-3.0-or-later
// See Notices.txt for copyright information

use crate::{
    context::{AsContext, ContextRef, Intern, Internable, Interned},
    ir::{module::IrModuleRef, SourceLocation},
    module::AsIrModule,
};
use alloc::vec::Vec;
use core::{
    fmt,
    hash::{Hash, Hasher},
    num::NonZeroUsize,
};

#[derive(Clone, Copy, Debug)]
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
        let mut path = Vec::with_capacity(self.scope.depth());
        let mut scope = self.scope;
        while let Some(ChildScopeData { parent, id, .. }) = scope.child_data {
            path.push(id);
            scope = parent.get();
        }
        write!(f, "{:?}", self.scope.module.path())?;
        for id in path {
            write!(f, ".'{}", id)?;
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
        f.write_str(match self {
            ScopeCombineErrorReason::ModulesNotSame => {
                "values from different modules can't be used together"
            }
            ScopeCombineErrorReason::NoSubscopeRelation => {
                "neither value is visible from the other, therefore they can't be used together"
            }
        })
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
            failed to combine scopes at {caller}\n\
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

#[derive(Debug)]
pub struct ValueNotAccessibleError<'ctx> {
    reason: SubscopeErrorReason,
    value_scope: Scope<'ctx>,
    accessing_scope: Scope<'ctx>,
    caller: SourceLocation<'ctx>,
}

impl fmt::Display for ValueNotAccessibleError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.reason {
            SubscopeErrorReason::ModulesNotSame => write!(
                f,
                "can't access value from different module:\n\
                at {caller}\n\
                value's scope: {value_scope} at {value_scope_src}\n\
                accessing scope: {accessing_scope} at {accessing_scope_src}",
                caller = self.caller,
                value_scope = self.value_scope.path(),
                value_scope_src = self.value_scope.source_location(),
                accessing_scope = self.accessing_scope.path(),
                accessing_scope_src = self.accessing_scope.source_location(),
            ),
            SubscopeErrorReason::NoSubscopeRelation => write!(
                f,
                "can't access value from outside its scope:\n\
                at {caller}\n\
                value's scope: {value_scope} at {value_scope_src}\n\
                accessing scope: {accessing_scope} at {accessing_scope_src}",
                caller = self.caller,
                value_scope = self.value_scope.path(),
                value_scope_src = self.value_scope.source_location(),
                accessing_scope = self.accessing_scope.path(),
                accessing_scope_src = self.accessing_scope.source_location(),
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
    pub fn new(parent: ScopeRef<'ctx>, source_location: SourceLocation<'ctx>) -> ScopeRef<'ctx> {
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
                reason: SubscopeErrorReason::NoSubscopeRelation,
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
    pub fn try_scope_can_access_value<T: OwningScope<'ctx>>(
        self,
        value: T,
        caller: &SourceLocation<'ctx>,
    ) -> Result<(), ValueNotAccessibleError<'ctx>> {
        if let Some(value_scope) = value.owning_scope() {
            self.try_subscope_of(*value_scope, caller).map_err(
                |SubscopeError {
                     reason,
                     ancestor_scope,
                     expected_subscope,
                     caller,
                 }| ValueNotAccessibleError {
                    reason,
                    value_scope: ancestor_scope,
                    accessing_scope: expected_subscope,
                    caller,
                },
            )
        } else {
            Ok(())
        }
    }
    #[track_caller]
    pub fn assert_scope_can_access_value<T: OwningScope<'ctx>>(
        self,
        value: T,
        caller: &SourceLocation<'ctx>,
    ) {
        if let Err(e) = self.try_scope_can_access_value(value, caller) {
            panic!("{}", e);
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
            .field("path", &self.path())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ir::module::IrModule, prelude::*};
    use alloc::{format, string::ToString};

    #[test]
    fn test_module_scope() {
        Context::with(|ctx: ContextRef<'_>| {
            let loc1 = SourceLocation::new_borrowed("file", 1, 1);
            let loc2 = SourceLocation::new_borrowed("file2", 2, 1);
            let module = IrModule::new_top_module(ctx, loc1, "top".into(), &mut ());
            let scope = module.scope();
            assert_eq!(scope.module, module);
            assert_eq!(scope.child_data, None);
            assert!(scope.is_module_scope());
            assert_eq!(scope.parent(), None);
            assert_eq!(scope.depth(), 0);
            assert!(scope.is_subscope_of(*scope));
            assert_eq!(scope.source_location(), loc1);
            assert_eq!(
                format!("{:#?}", scope),
                r#"Scope {
    source_location: SourceLocation {
        file: "file",
        line: 1,
        column: 1,
    },
    path: "top",
    ..
}"#
            );
            module.set_source_location(loc2);
            assert_eq!(scope.source_location(), loc2);
            assert_eq!(
                format!("{:#?}", scope),
                r#"Scope {
    source_location: SourceLocation {
        file: "file2",
        line: 2,
        column: 1,
    },
    path: "top",
    ..
}"#
            );
        });
    }

    #[test]
    fn test_subscope() {
        Context::with(|ctx: ContextRef<'_>| {
            let loc1 = SourceLocation::new_borrowed("file", 1, 1);
            let loc2 = SourceLocation::new_borrowed("file2", 2, 1);
            let loc3 = SourceLocation::new_borrowed("file3", 3, 1);
            let module = IrModule::new_top_module(ctx, loc1, "top".into(), &mut ());
            let module2 = IrModule::new_submodule(module, loc3, "submodule".into(), &mut ());
            let scope = module.scope();
            assert_eq!(scope.module, module);
            assert_eq!(scope.child_data, None);
            assert!(scope.is_module_scope());
            assert_eq!(scope.parent(), None);
            assert_eq!(scope.depth(), 0);
            let subscope = Scope::new(scope, loc2);
            assert_eq!(subscope.module, module);
            assert_eq!(
                subscope.child_data,
                Some(ChildScopeData {
                    depth: NonZeroUsize::new(1).unwrap(),
                    id: 1,
                    parent: scope,
                    source_location: loc2,
                })
            );
            assert_eq!(subscope.source_location(), loc2);
            assert!(!subscope.is_module_scope());
            assert_eq!(subscope.parent(), Some(scope));
            assert_eq!(subscope.depth(), 1);
            assert_eq!(
                format!("{:#?}", subscope),
                r#"Scope {
    source_location: SourceLocation {
        file: "file2",
        line: 2,
        column: 1,
    },
    path: "top".'1,
    ..
}"#
            );
            assert_eq!(
                format!("{:#?}", module2.scope()),
                r#"Scope {
    source_location: SourceLocation {
        file: "file3",
        line: 3,
        column: 1,
    },
    path: "top"."submodule",
    ..
}"#
            );
            assert!(scope.is_subscope_of(*scope));
            assert!(!scope.is_subscope_of(*subscope));
            assert!(!scope.is_subscope_of(*module2.scope()));
            assert!(subscope.is_subscope_of(*scope));
            assert!(subscope.is_subscope_of(*subscope));
            assert!(!subscope.is_subscope_of(*module2.scope()));
            assert!(!module2.scope().is_subscope_of(*scope));
            assert!(!module2.scope().is_subscope_of(*subscope));
            assert!(module2.scope().is_subscope_of(*module2.scope()));
        });
    }

    #[test]
    fn test_combine() {
        Context::with(|ctx: ContextRef<'_>| {
            #[track_caller]
            fn check<T: fmt::Debug>(v: T, expected: T) {
                struct FormatEqWrapper<T>(T);
                impl<T: fmt::Debug> fmt::Debug for FormatEqWrapper<T> {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        self.0.fmt(f)
                    }
                }
                impl<T: fmt::Debug> PartialEq for FormatEqWrapper<T> {
                    fn eq(&self, other: &Self) -> bool {
                        format!("{:?}", self) == format!("{:?}", other)
                    }
                }
                assert_eq!(FormatEqWrapper(v), FormatEqWrapper(expected));
            }
            #[track_caller]
            fn check_some<'ctx>(
                iter: impl IntoIterator<Item = ScopeRef<'ctx>>,
                caller: SourceLocation<'ctx>,
                expected: ScopeRef<'ctx>,
            ) {
                check(Scope::try_combine(iter, &caller), Ok(Some(expected)));
            }
            #[track_caller]
            fn check_err<'ctx>(
                iter: impl IntoIterator<Item = ScopeRef<'ctx>>,
                caller: SourceLocation<'ctx>,
                reason: ScopeCombineErrorReason,
                scope1: ScopeRef<'ctx>,
                scope2: ScopeRef<'ctx>,
            ) {
                check(
                    Scope::try_combine(iter, &caller),
                    Err(ScopeCombineError {
                        reason,
                        scope1,
                        scope2,
                        caller,
                    }),
                )
            }
            let loc1 = SourceLocation::new_borrowed("file", 1, 1);
            let loc2 = SourceLocation::new_borrowed("file2", 2, 1);
            let loc3 = SourceLocation::new_borrowed("file3", 3, 1);
            let loc4 = SourceLocation::new_borrowed("file4", 4, 1);
            let loc5 = SourceLocation::new_borrowed("file5", 5, 1);
            let caller = SourceLocation::new_borrowed("caller", 10, 1);
            named!(let (top, _io) = ctx.top_module_with_io(()));
            top.ir().set_source_location(loc1);
            let top_scope = top.ir().scope();
            named!(let (submodule, _io) = top.submodule(()));
            submodule.ir().set_source_location(loc2);
            let submodule_scope = submodule.ir().scope();
            let subscope = Scope::new(top_scope, loc3);
            let sub_subscope = Scope::new(subscope, loc4);
            let subscope2 = Scope::new(top_scope, loc5);
            // check 0 inputs
            check(Scope::try_combine([top_scope; 0], &caller), Ok(None));

            // check 1 input
            check_some([top_scope], caller, top_scope);
            check_some([submodule_scope], caller, submodule_scope);
            check_some([subscope], caller, subscope);
            check_some([sub_subscope], caller, sub_subscope);
            check_some([subscope2], caller, subscope2);

            // check 2 inputs
            check_some([top_scope, top_scope], caller, top_scope);
            check_err(
                [top_scope, submodule_scope],
                caller,
                ScopeCombineErrorReason::ModulesNotSame,
                top_scope,
                submodule_scope,
            );
            check_some([top_scope, subscope], caller, subscope);
            check_some([top_scope, sub_subscope], caller, sub_subscope);
            check_some([top_scope, subscope2], caller, subscope2);

            check_err(
                [submodule_scope, top_scope],
                caller,
                ScopeCombineErrorReason::ModulesNotSame,
                submodule_scope,
                top_scope,
            );
            check_some([submodule_scope, submodule_scope], caller, submodule_scope);
            check_err(
                [submodule_scope, subscope],
                caller,
                ScopeCombineErrorReason::ModulesNotSame,
                submodule_scope,
                subscope,
            );
            check_err(
                [submodule_scope, sub_subscope],
                caller,
                ScopeCombineErrorReason::ModulesNotSame,
                submodule_scope,
                sub_subscope,
            );
            check_err(
                [submodule_scope, subscope2],
                caller,
                ScopeCombineErrorReason::ModulesNotSame,
                submodule_scope,
                subscope2,
            );

            check_some([subscope, top_scope], caller, subscope);
            check_err(
                [subscope, submodule_scope],
                caller,
                ScopeCombineErrorReason::ModulesNotSame,
                subscope,
                submodule_scope,
            );
            check_some([subscope, subscope], caller, subscope);
            check_some([subscope, sub_subscope], caller, sub_subscope);
            check_err(
                [subscope, subscope2],
                caller,
                ScopeCombineErrorReason::NoSubscopeRelation,
                subscope,
                subscope2,
            );

            check_some([sub_subscope, top_scope], caller, sub_subscope);
            check_err(
                [sub_subscope, submodule_scope],
                caller,
                ScopeCombineErrorReason::ModulesNotSame,
                sub_subscope,
                submodule_scope,
            );
            check_some([sub_subscope, subscope], caller, sub_subscope);
            check_some([sub_subscope, sub_subscope], caller, sub_subscope);
            check_err(
                [sub_subscope, subscope2],
                caller,
                ScopeCombineErrorReason::NoSubscopeRelation,
                sub_subscope,
                subscope2,
            );

            check_some([subscope2, top_scope], caller, subscope2);
            check_err(
                [subscope2, submodule_scope],
                caller,
                ScopeCombineErrorReason::ModulesNotSame,
                subscope2,
                submodule_scope,
            );
            check_err(
                [subscope2, subscope],
                caller,
                ScopeCombineErrorReason::NoSubscopeRelation,
                subscope2,
                subscope,
            );
            check_err(
                [subscope2, sub_subscope],
                caller,
                ScopeCombineErrorReason::NoSubscopeRelation,
                subscope2,
                sub_subscope,
            );
            check_some([subscope2, subscope2], caller, subscope2);
        });
    }

    #[test]
    fn test_error_display() {
        Context::with(|ctx: ContextRef<'_>| {
            let loc1 = SourceLocation::new_borrowed("file", 1, 1);
            let loc2 = SourceLocation::new_borrowed("file2", 2, 1);
            let caller = SourceLocation::new_borrowed("caller", 10, 1);
            named!(let (top, _io) = ctx.top_module_with_io(()));
            top.ir().set_source_location(loc1);
            let scope1 = top.ir().scope();
            named!(let (submodule, _io) = top.submodule(()));
            submodule.ir().set_source_location(loc2);
            let scope2 = submodule.ir().scope();
            assert_eq!(
                ScopeCombineError {
                    reason: ScopeCombineErrorReason::ModulesNotSame,
                    scope1,
                    scope2,
                    caller
                }
                .to_string(),
                "values from different modules can't be used together:\n\
                failed to combine scopes at caller:10:1\n\
                scope1: \"top\" at file:1:1\n\
                scope2: \"top\".\"submodule\" at file2:2:1"
            );
            assert_eq!(
                ScopeCombineError {
                    reason: ScopeCombineErrorReason::NoSubscopeRelation,
                    scope1,
                    scope2,
                    caller
                }
                .to_string(),
                "neither value is visible from the other, therefore they can't be used together:\n\
                failed to combine scopes at caller:10:1\n\
                scope1: \"top\" at file:1:1\n\
                scope2: \"top\".\"submodule\" at file2:2:1"
            );
            let ancestor_scope = *scope1;
            let expected_subscope = *scope2;
            assert_eq!(
                SubscopeError {
                    reason: SubscopeErrorReason::ModulesNotSame,
                    ancestor_scope,
                    expected_subscope,
                    caller
                }
                .to_string(),
                "expected a subscope of `ancestor_scope`, got a scope from a different module:\n\
                at caller:10:1\n\
                ancestor_scope: \"top\" at file:1:1\n\
                expected_subscope: \"top\".\"submodule\" at file2:2:1"
            );
            assert_eq!(
                SubscopeError {
                    reason: SubscopeErrorReason::NoSubscopeRelation,
                    ancestor_scope,
                    expected_subscope,
                    caller
                }
                .to_string(),
                "`expected_subscope` is not a subscope of `ancestor_scope`:\n\
                at caller:10:1\n\
                ancestor_scope: \"top\" at file:1:1\n\
                expected_subscope: \"top\".\"submodule\" at file2:2:1"
            );
            let accessing_scope = *scope1;
            let value_scope = *scope2;
            assert_eq!(
                ValueNotAccessibleError {
                    reason: SubscopeErrorReason::ModulesNotSame,
                    value_scope,
                    accessing_scope,
                    caller
                }
                .to_string(),
                "can't access value from different module:\n\
                at caller:10:1\n\
                value's scope: \"top\".\"submodule\" at file2:2:1\n\
                accessing scope: \"top\" at file:1:1"
            );
            assert_eq!(
                ValueNotAccessibleError {
                    reason: SubscopeErrorReason::NoSubscopeRelation,
                    value_scope,
                    accessing_scope,
                    caller
                }
                .to_string(),
                "can't access value from outside its scope:\n\
                at caller:10:1\n\
                value's scope: \"top\".\"submodule\" at file2:2:1\n\
                accessing scope: \"top\" at file:1:1"
            );
        });
    }
}
