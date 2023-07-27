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
    walking::HirListener,
};

pub struct ReferenceWalker<'db, F>
where
    F: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>),
{
    db: &'db dyn crate::HirDb,
    visited: im::HashSet<Reference>,
    stack: Vec<Arc<Scope>>,
    handle: F,
}

impl<'db, F> ReferenceWalker<'db, F>
where
    F: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>),
{
    pub fn new(db: &'db dyn crate::HirDb, handle: F) -> Self {
        Self {
            db,
            visited: im::HashSet::default(),
            stack: vec![Scope::new_ref(ScopeKind::File)],
            handle,
        }
    }
}

impl<'db, F> HirListener for ReferenceWalker<'db, F>
where
    F: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>),
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

        (self.handle)(self.db, reference, scope);
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
