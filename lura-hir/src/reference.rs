//! Defines a walker for references and scopes in the HIR. It's intended to be used to find
//! references to a definition, and to find the scope of a definition. And to be used building a
//! call graph, or even a IDE.
//!
//! It's useful to track stuff in the HIR, and to find references to a definition. It's also useful
//! to find the scope of a definition, and to find the scope of a reference.

use std::sync::Arc;

use fxhash::FxBuildHasher;

use crate::{
  scope::{Scope, ScopeKind},
  solver::Reference,
  source::*,
  walking::{HirListener, Walker},
};

/// Defines a walker for references and scopes in the HIR. It's intended to be used to find
/// and collect all references in the abstract syntax tree. It's also intended to be used to find
/// the scope of a definition, and to find the scope of a reference. And to be used building a
/// call graph, or even a IDE.
#[allow(clippy::type_complexity)]
pub struct ReferenceWalker<'db, U: Checkable> {
  db: &'db dyn crate::HirDb,
  stack: Vec<Arc<Scope>>,
  visit_reference: Box<dyn FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>) -> U>,
  enter_scope: Box<dyn FnMut(&'db dyn crate::HirDb, Location, Arc<Scope>)>,

  scopes_visited: im::HashSet<Arc<Scope>, FxBuildHasher>,

  /// The references that were already visited. It's used to avoid infinite loops
  visited: im::HashSet<Reference, FxBuildHasher>,

  /// The references to return
  collected: im::HashSet<Reference, FxBuildHasher>,
}

/// Defines a builder for a `ReferenceWalker`. It's intended to be used to build a `ReferenceWalker`
/// within multiple visit functions, to be easier to use.
#[allow(clippy::type_complexity)]
pub struct ReferenceWalkerBuilder<'db, U: Checkable> {
  /// The function that will be called when a reference is visited.
  visit_reference: Box<dyn FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>) -> U>,
  enter_scope: Box<dyn FnMut(&'db dyn crate::HirDb, Location, Arc<Scope>)>,
  phantom: std::marker::PhantomData<&'db ()>,
}

impl<'db, U: Checkable> ReferenceWalkerBuilder<'db, U> {
  pub fn enter_scope<T>(mut self, enter_scope: T) -> Self
  where
    T: FnMut(&'db dyn crate::HirDb, Location, Arc<Scope>) + 'static,
  {
    self.enter_scope = Box::new(enter_scope);
    self
  }

  pub fn build(self, db: &'db dyn crate::HirDb) -> ReferenceWalker<'db, U> {
    ReferenceWalker {
      db,
      visited: im::HashSet::default(),
      stack: vec![Scope::new_ref(ScopeKind::File)],
      visit_reference: self.visit_reference,
      enter_scope: self.enter_scope,
      collected: im::HashSet::default(),
      scopes_visited: im::HashSet::default(),
    }
  }
}

impl<'db, U: Checkable> ReferenceWalker<'db, U> {
  /// Creates a new `ReferenceWalker` with the given `handle` function. The `handle` function will
  /// be called when a reference is visited.
  #[allow(clippy::new_ret_no_self)]
  pub fn new<F>(handle: F) -> ReferenceWalkerBuilder<'db, U>
  where
    F: FnMut(&'db dyn crate::HirDb, Reference, Arc<Scope>) -> U + 'static,
  {
    ReferenceWalkerBuilder {
      visit_reference: Box::new(handle),
      enter_scope: Box::new(|_, _, _| {}),
      phantom: std::marker::PhantomData,
    }
  }

  pub fn empty() -> ReferenceWalkerBuilder<'db, ()>
  where
    U: Default,
  {
    ReferenceWalkerBuilder {
      visit_reference: Box::new(|_, _, _| ()),
      enter_scope: Box::new(|_, _, _| {}),
      phantom: std::marker::PhantomData,
    }
  }

  pub fn collect(mut self, source: HirSource) -> im::HashSet<Reference, FxBuildHasher> {
    source.accept(self.db, &mut self);

    // Return the collected references
    self.collected
  }

  fn enter_scope(&mut self, db: &'db dyn crate::HirDb, location: Location, scope: Arc<Scope>) {
    if self.scopes_visited.contains(&scope) {
      return;
    }

    (self.enter_scope)(db, location, scope.clone());

    // Mark the scope as visited
    self.scopes_visited.insert(scope);
  }
}

impl<'db, U: Checkable> HirListener for ReferenceWalker<'db, U> {
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
    self.enter_scope(self.db, decl.location(self.db), decl.scope(self.db));
    self.stack.push(decl.scope(self.db));
  }

  fn exit_data_top_level(&mut self, _: top_level::DataDecl) {
    self.stack.pop();
  }

  fn enter_class_top_level(&mut self, decl: top_level::ClassDecl) {
    self.enter_scope(self.db, decl.location(self.db), decl.scope(self.db));
    self.stack.push(decl.scope(self.db));
  }

  fn exit_class_top_level(&mut self, _: top_level::ClassDecl) {
    self.stack.pop();
  }

  fn enter_trait_top_level(&mut self, decl: top_level::TraitDecl) {
    self.enter_scope(self.db, decl.location(self.db), decl.scope(self.db));
    self.stack.push(decl.scope(self.db));
  }

  fn exit_trait_top_level(&mut self, _: top_level::TraitDecl) {
    self.stack.pop();
  }

  fn enter_block(&mut self, block: stmt::Block) {
    self.enter_scope(self.db, block.location(self.db), block.scope(self.db));
    self.stack.push(block.scope(self.db));
  }

  fn exit_block(&mut self, _: stmt::Block) {
    self.stack.pop();
  }

  fn enter_abs_expr(&mut self, abs_expr: expr::AbsExpr) {
    self.enter_scope(self.db, abs_expr.location(self.db), abs_expr.scope(self.db));
    self.stack.push(abs_expr.scope(self.db));
  }

  fn exit_abs_expr(&mut self, _: expr::AbsExpr) {
    self.stack.pop();
  }

  fn enter_arrow_type_rep(&mut self, arrow: type_rep::ArrowTypeRep) {
    self.enter_scope(self.db, arrow.location(self.db), arrow.scope(self.db));
    self.stack.push(arrow.scope(self.db));
  }

  fn exit_arrow_type_rep(&mut self, _: type_rep::ArrowTypeRep) {
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
