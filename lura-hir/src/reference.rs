//! Defines a walker for references and scopes in the HIR. It's intended to be used to find
//! references to a definition, and to find the scope of a definition. And to be used building a
//! call graph, or even a IDE.
//!
//! It's useful to track stuff in the HIR, and to find references to a definition. It's also useful
//! to find the scope of a definition, and to find the scope of a reference.

use std::sync::Arc;

use crate::{
    resolve::Reference,
    scope::{Scope, ScopeKind},
    source::*,
    walking::{HirListener, Walker},
};

/// Defines a walker for references and scopes in the HIR. It's intended to be used to find
/// and collect all references in the abstract syntax tree. It's also intended to be used to find
/// the scope of a definition, and to find the scope of a reference. And to be used building a
/// call graph, or even a IDE.
pub struct ReferenceWalker<'db, U: Checkable, F>
where
    F: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>) -> U,
{
    db: &'db dyn crate::HirDb,
    stack: Vec<Arc<Scope>>,
    visit_reference: F,

    /// The references that were already visited. It's used to avoid infinite loops
    visited: im::HashSet<Reference>,

    /// The references to return
    collected: im::HashSet<Reference>,
}

/// Defines a builder for a `ReferenceWalker`. It's intended to be used to build a `ReferenceWalker`
/// within multiple visit functions, to be easier to use.
pub struct ReferenceWalkerBuilder<'db, U: Checkable, VisitReference>
where
    VisitReference: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>) -> U,
{
    /// The function that will be called when a reference is visited.
    pub visit_reference: VisitReference,
    pub phantom: std::marker::PhantomData<&'db ()>,
}

impl<'db, U, VisitReference> ReferenceWalkerBuilder<'db, U, VisitReference>
where
    U: Checkable,
    VisitReference: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>) -> U,
{
    pub fn build(self, db: &'db dyn crate::HirDb) -> ReferenceWalker<'db, U, VisitReference> {
        ReferenceWalker {
            db,
            visited: im::HashSet::default(),
            stack: vec![Scope::new_ref(ScopeKind::File)],
            visit_reference: self.visit_reference,
            collected: im::HashSet::default(),
        }
    }
}

impl<'db, U, F> ReferenceWalker<'db, U, F>
where
    U: Checkable,
    F: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>) -> U,
{
    /// Creates a new `ReferenceWalker` with the given `handle` function. The `handle` function will
    /// be called when a reference is visited.
    #[allow(clippy::new_ret_no_self)]
    pub fn new(handle: F) -> ReferenceWalkerBuilder<'db, U, F> {
        ReferenceWalkerBuilder {
            visit_reference: handle,
            phantom: std::marker::PhantomData,
        }
    }

    pub fn collect(mut self, source: HirSource) -> im::HashSet<Reference> {
        source.accept(self.db, &mut self);

        // Return the collected references
        self.collected
    }
}

impl<'db, U, F> HirListener for ReferenceWalker<'db, U, F>
where
    U: Checkable,
    F: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>) -> U,
{
    fn visit_reference(&mut self, reference: Reference) {
        // If we already visited this reference, we don't need to visit it again, we can just
        // return.
        if self.visited.contains(&reference) {
            return;
        }

        let scope = self.stack.last().unwrap().clone();

        // Mark the reference as visited
        self.visited.insert(reference);

        // If the `handle` function returns true, we need to collect the reference
        if (self.visit_reference)(self.db, reference, scope).check() {
            self.collected.insert(reference);
        }
    }

    fn enter_data_top_level(&mut self, decl: top_level::DataDecl) {
        self.stack.push(decl.scope(self.db));
    }

    fn exit_data_top_level(&mut self, _: top_level::DataDecl) {
        self.stack.pop();
    }

    fn enter_class_top_level(&mut self, decl: top_level::ClassDecl) {
        self.stack.push(decl.scope(self.db));
    }

    fn exit_class_top_level(&mut self, _: top_level::ClassDecl) {
        self.stack.pop();
    }

    fn enter_trait_top_level(&mut self, decl: top_level::TraitDecl) {
        self.stack.push(decl.scope(self.db));
    }

    fn exit_trait_top_level(&mut self, _: top_level::TraitDecl) {
        self.stack.pop();
    }

    fn enter_block(&mut self, block: stmt::Block) {
        self.stack.push(block.scope(self.db));
    }

    fn exit_block(&mut self, _: stmt::Block) {
        self.stack.pop();
    }

    fn enter_abs_expr(&mut self, abs_expr: expr::AbsExpr) {
        self.stack.push(abs_expr.scope(self.db));
    }

    fn exit_abs_expr(&mut self, _: expr::AbsExpr) {
        self.stack.pop();
    }
}

/// Defines a trait that can be used to check if a value is true or not. It's a trait, to be easier
/// to implement for multiple types.
pub trait Checkable {
    fn check(self) -> bool;
}

impl Checkable for bool {
    fn check(self) -> bool {
        self
    }
}

impl Checkable for () {
    fn check(self) -> bool {
        true
    }
}
