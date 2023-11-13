//! This module defines the lowering of the syntax tree to the high level intermediate representation
//! (HIR).
//!
//! Lowering means that it will take the syntax tree, and will transform it into a low level for
//! human readability, but high-level abstraction for the compiler. It's very useful to the future
//! steps, even of the compiler frontend.
//!
//! This implementation uses tree-sitter, and resolves into `lura-hir`

#![feature(trait_upcasting)]

extern crate salsa_2022 as salsa;

use std::{
  collections::{HashMap, HashSet},
  mem::take,
  ops::Deref,
  sync::Arc,
};

use fxhash::FxBuildHasher;
use lura_hir::{
  package::Package,
  scope::{Scope, ScopeKind},
  solver::{
    find_constructor, find_function, find_trait, find_type, query_module, Definition,
    DefinitionKind, HirLevel,
  },
  source::{
    declaration::{Attribute, DocString, Parameter, Vis},
    expr::{AbsExpr, AnnExpr, CallExpr, CallKind, Callee, Expr},
    pattern::{BindingPattern, Pattern},
    top_level::{
      BindingGroup, ClassDecl, Clause, CommandTopLevel, Constructor, ConstructorKind, DataDecl,
      InstanceDecl, Signature, TopLevel, TraitDecl, TypeDecl, UsingTopLevel,
    },
    type_rep::{AppTypeRep, ArrowKind, ArrowTypeRep, TypeRep},
    DefaultWithDb, HirPath, HirSource, Identifier, Location, OptionExt, Spanned,
  },
  HirDb,
};
use lura_syntax::{anon_unions::ExplicitArguments_ImplicitArguments, Source, SourceFile};
use salsa::{Cycle, DbWithJar};
use tree_sitter::{Node, Tree};
use type_sitter_lib::{ExtraOr, IncorrectKind, NodeResult, OptionNodeResultExt, TypedNode};

#[salsa::jar(db = HirLoweringDb)]
pub struct Jar(hir_declare, hir_lower);

/// The database that stores all the information about the source code. It is
/// implemented using the [`salsa`] crate, and it's used by the [`lura-driver`] crate.
pub trait HirLoweringDb: HirDb + DbWithJar<Jar> {}

impl<T> HirLoweringDb for T where T: HirDb + DbWithJar<Jar> {}

#[rustfmt::skip]
type SyntaxDecl<'tree> = lura_syntax::anon_unions::ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using<'tree>;

#[rustfmt::skip]
type SyntaxIdentifier<'tree> = lura_syntax::anon_unions::SimpleIdentifier_SymbolIdentifier<'tree>;

#[rustfmt::skip]
type SyntaxVariant<'tree> = lura_syntax::anon_unions::Comma_FunctionConstructor_SignatureConstructor<'tree>;

/// Defines the [`hir_declare`] query.
///
/// It will declare the source file into the scope of the
/// lowerrer. It will declare all the top level declarations, and this won't return the resolved
/// declarations within the [`HirSource`].
///
/// For solving, see [`hir_lower`].
///
/// TODO: This query duplicates resolution errors that aren't made in the Solvers, just like
/// if i report an error in the resolver, it will report the same error in the lowerer, and it
/// will duplicate the diagnostics, it should be thinked if it's sound or not.
#[salsa::tracked(recovery_fn = rec_hir_lower)]
pub fn hir_declare(db: &dyn crate::HirLoweringDb, pkg: Package, src: Source) -> HirSource {
  let parse_tree = src.syntax_node(db);

  let lower = HirLowering {
    db,
    src,
    pkg,
    txt: Arc::new(src.source_text(db).to_string()),
    decls: vec![],
    scope: Scope::new(ScopeKind::File),
    tree: parse_tree.tree.clone(),
    root_node: parse_tree.tree.root_node(),
    clauses: Default::default(),
  };

  lower.declare()
}

/// Defines the [`hir_lower`] query.
///
/// It does have a behavior just like [`hir_declare`], but it will solve the clauses too, and it
/// will return the [`HirSource`] with the declarations and the clauses solved.
///
/// For declaring, see [`hir_declare`].
#[salsa::tracked(recovery_fn = rec_hir_lower)]
pub fn hir_lower(db: &dyn crate::HirLoweringDb, pkg: Package, src: Source) -> HirSource {
  let parse_tree = src.syntax_node(db);

  let lower = HirLowering {
    db,
    src,
    pkg,
    txt: Arc::new(src.source_text(db).to_string()),
    decls: vec![],
    scope: Scope::new(ScopeKind::File),
    tree: parse_tree.tree.clone(),
    root_node: parse_tree.tree.root_node(),
    clauses: Default::default(),
  };

  lower.declare_and_solve()
}

pub fn rec_hir_lower(db: &dyn crate::HirDb, cycle: &Cycle, _: Package, _: Source) -> HirSource {
  panic!("Cyclic dependency between queries: {:#?}", cycle.debug(db))
}

/// A high-level declaration solver, that will solve the declarations, and will solve the clauses
/// too. It can either just declare the top level declarations, or solve it within the declaration.
///
/// This step is very important to be ran **AFTER** a kind of validation of the syntax tree, because
/// it will treat as the syntax tree is correct, and will not check for errors in the syntax tree.
///
/// If there are any errors in the syntax tree, it will return the [`Default`] or [`DefaultWithDb`]
/// if [`Default`] ins't specified, and it can be really bad for error handling, because it will
/// cause blindness.
///
/// The validation and the lowering to HIR are totally different steps, in this compiler.
///
/// It will return the [`HirSource`] with the declarations solved, and the clauses solved too.
struct HirLowering<'db, 'tree> {
  db: &'db dyn crate::HirDb,
  src: Source,
  txt: Arc<String>,
  tree: Arc<Tree>,
  decls: Vec<TopLevel>,
  pkg: Package,
  scope: Scope,
  root_node: Node<'tree>,
  clauses: HashMap<Definition, BindingGroup, FxBuildHasher>,
}

impl<'db, 'tree> HirLowering<'db, 'tree> {
  /// Declare the source file into the scope of the lowerrer. It will declare all the top level
  /// declarations, and this won't return the [`HirSource`] with the declarations, just an empty
  /// [`HirSource`].
  ///
  /// This should be used as a helper function for the [`hir_declare`] query. It's useful to
  /// search the functions/and other declarations in the scope, but it won't solve the clauses, so
  /// it won't cause any cycles in the queries.
  pub fn declare(mut self) -> HirSource {
    let ast = SourceFile::try_from(self.root_node).unwrap();

    for node in ast.decls(&mut self.tree.clone().walk()).flatten() {
      if let ExtraOr::Regular(node) = node {
        // Process declaration only if it is not an error, or it's not a junk
        // declaration.
        self.define(node);
      }
    }

    HirSource::new(self.db, self.src, self.pkg, self.scope, self.decls)
  }

  /// Declare the source file into the scope of the lowerrer. It will declare all the top level
  /// **AND** will solve the clauses, and this will return the [`HirSource`] with the declarations
  /// and the clauses solved.
  ///
  /// This is the main function of the lowerrer, and it will be used in the [`hir_lower`] query.
  pub fn declare_and_solve(mut self) -> HirSource {
    let ast = SourceFile::try_from(self.root_node).unwrap();

    for node in ast.decls(&mut self.tree.clone().walk()).flatten() {
      if let ExtraOr::Regular(node) = node {
        // Process declaration only if it is not an error, or it's not a junk
        // declaration.
        if let Some(solver) = self.define(node) {
          let decl = solver.run_solver(&mut self);

          // Can't let empty declarations in the declarations list, because it will cause
          // errors in the IDE, and it will cause blindness in the resolution.
          //
          // So, continue before pushing the declaration, if it is an empty declaration.
          if let TopLevel::Error(..) = decl {
            continue;
          }

          self.decls.push(decl);
        };
      }
    }

    HirSource::new(self.db, self.src, self.pkg, self.scope, self.decls)
  }

  /// Creates a new declaration solver, for the given [`TreeDecl`]. It will return `None` if the
  /// declaration is not "resolvable", just like "commands" and "using" declarations, because they
  /// are not declarations, they are just "commands" to the compiler.
  ///
  /// It will return `Some` if the declaration is "resolvable", and it will return a solver for
  /// the declaration.
  pub fn define<'a>(&mut self, decl: SyntaxDecl<'a>) -> Option<Solver<'a, TopLevel>> {
    use lura_syntax::anon_unions::ClassDecl_Clause_Command_DataDecl_InstanceDecl_Signature_TraitDecl_TypeDecl_Using::*;

    // Creates a new [`TopLevel`] instance.
    let decl = match decl {
      Command(command) => self.hir_command(command),
      ClassDecl(class_decl) => return self.hir_class(class_decl).into(),
      Clause(clause) => return self.hir_clause(clause).into(),
      DataDecl(data_decl) => return self.hir_data(data_decl).into(),
      InstanceDecl(instance_decl) => return self.hir_instance(instance_decl).into(),
      TraitDecl(trait_decl) => return self.hir_trait(trait_decl).into(),
      TypeDecl(type_decl) => return self.hir_type(type_decl).into(),
      Signature(signature) => return self.hir_signature(signature).into(),
      Using(decl) => return self.hir_using(decl).into(),
    };

    self.decls.push(decl);

    None // Not solving a "not resolvable" declaration
  }

  /// Creates a new high level command top level [`CommandTopLevel`] solver, for the given
  /// concrete syntax tree [`lura_syntax::Command`].
  pub fn hir_command(&mut self, tree: lura_syntax::Command) -> TopLevel {
    let path = tree.command().solve(self, |this, node| this.path(node));
    let arguments = tree
      .arguments(&mut tree.walk())
      .flatten()
      .filter_map(|node| node.regular())
      .map(|node| self.expr(node, HirLevel::Expr))
      .collect();

    let location = self.range(tree.range());

    // Search definition of command in the scope, and if it is not found, it will create a new
    // definition with the given name, and the given location.
    let name = self.qualify(path, DefinitionKind::Command);

    TopLevel::Command(CommandTopLevel::new(self.db, name, arguments, location))
  }

  /// Creates a new high level type declaration [`UsingTopLevel`] solver, for the given
  /// concrete syntax tree [`lura_syntax::Using`].
  ///
  /// It will return a [`Solver`] for the [`Signature`], and it will solve the [`Signature`] in
  /// the [`hir_lower`] query.
  pub fn hir_using<'a>(&mut self, tree: lura_syntax::Using<'a>) -> Solver<'a, TopLevel> {
    let range = self.range(tree.range());
    let path = tree.path().solve(self, |this, node| this.path(node));

    Solver::new(move |db, this| {
      // TODO: search for functions or anything too.
      let (scope, def) = query_module(db, path);
      let reference = this.scope.using(db, def, path.location(db));

      // Extends the scope with the new scope.
      this.scope.extend(scope);

      TopLevel::Using(UsingTopLevel::new(db, reference, range))
    })
  }

  /// Creates a new high level type declaration [`TypeDecl`] solver, for the given
  /// concrete syntax tree [`lura_syntax::TypeDecl`].
  ///
  /// It will return a [`Solver`] for the [`Signature`], and it will solve the [`Signature`] in
  /// the [`hir_lower`] query.
  pub fn hir_type<'a>(&mut self, tree: lura_syntax::TypeDecl<'a>) -> Solver<'a, TopLevel> {
    let range = self.range(tree.range());
    let path = tree.name().solve(self, |this, node| this.path(node));

    let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
    let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));

    // Converts the visibility to default visibility, if it is not specified.
    let vis = tree
      .visibility()
      .map(|vis| vis.solve(self, |this, node| this.hir_visibility(node)))
      .unwrap_or(Spanned::on_call_site(Vis::Public));

    // Defines the node on the scope
    let node = self
      .scope
      .define(self.db, path, range.clone(), DefinitionKind::Type);

    Solver::new(move |db, this| {
      // Creates a new scope for the function, and it will be used to store the parameters,
      // and the variables.
      this.scope = this.scope.fork(ScopeKind::Type);

      let parameters = this.parameters(tree.arguments(&mut tree.walk()));

      let type_rep = tree
        .clause_type()
        .flatten()
        .map(|node| this.clause_type(node))
        .unwrap_or_default_with_db(db);

      let type_decl = TypeDecl::new(
        this.db,
        /* attributes  = */ attrs,
        /* docs        = */ docs,
        /* visibility  = */ vis,
        /* name        = */ node,
        /* parameters  = */ parameters,
        /* return_type = */ type_rep,
        /* location    = */ range.clone(),
        /* scope       = */ this.pop_scope(),
      );

      // It's not needed to solve the clause, because it is already solved in the next steps.
      //
      // The entire next step, is getting the clauses from the scope, and transforms into
      // declarations, so it is not needed to solve the clause here.
      TopLevel::TypeDecl(type_decl)
    })
  }

  /// Creates a new high level class declaration [`ClassDecl`] solver, for the given
  /// concrete syntax tree [`lura_syntax::ClassDecl`].
  ///
  /// It will return a [`Solver`] for the [`Signature`], and it will solve the [`Signature`] in
  /// the [`hir_lower`] query.
  pub fn hir_class<'a>(&mut self, tree: lura_syntax::ClassDecl<'a>) -> Solver<'a, TopLevel> {
    let range = self.range(tree.range());
    let path = tree.name().solve(self, |this, node| this.path(node));

    let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
    let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));

    // Converts the visibility to default visibility, if it is not specified.
    let vis = tree
      .visibility()
      .map(|vis| vis.solve(self, |this, node| this.hir_visibility(node)))
      .unwrap_or(Spanned::on_call_site(Vis::Public));

    // Defines the node on the scope
    let node = self
      .scope
      .define(self.db, path, range.clone(), DefinitionKind::Function);

    let methods = tree
      .fields(&mut tree.walk())
      .flatten()
      .filter_map(|method| method.regular())
      .map(|method| self.hir_signature(method))
      .collect::<Vec<_>>();

    Solver::new(move |db, this| {
      // Creates a new scope for the function, and it will be used to store the parameters,
      // and the variables.
      this.scope = this.scope.fork(ScopeKind::Class);

      let parameters = this.parameters(tree.arguments(&mut tree.walk()));

      let methods = methods
        .into_iter()
        .map(|method| method.run_solver(this))
        .filter_map(|top_level| match top_level {
          TopLevel::BindingGroup(binding_group) => Some(binding_group),
          _ => None,
        })
        .collect::<Vec<_>>();

      let type_rep = tree
        .clause_type()
        .flatten()
        .map(|node| this.clause_type(node))
        .unwrap_or_default_with_db(db);

      // Publish all definitions to parent scope
      this.scope.publish_all_definitions(this.db, node);

      let class_decl = ClassDecl::new(
        this.db,
        /* attributes  = */ attrs,
        /* docs        = */ docs,
        /* visibility  = */ vis,
        /* name        = */ node,
        /* parameters  = */ parameters,
        /* return_type = */ type_rep,
        /* fields      = */ vec![], // TODO
        /* methods     = */ methods,
        /* location    = */ range.clone(),
        /* scope       = */ this.pop_scope(),
      );

      // It's not needed to solve the clause, because it is already solved in the next steps.
      //
      // The entire next step, is getting the clauses from the scope, and transforms into
      // declarations, so it is not needed to solve the clause here.
      TopLevel::ClassDecl(class_decl)
    })
  }

  /// Creates a new high instance trait declaration [`InstanceDecl`] solver, for the given
  /// concrete syntax tree [`lura_syntax::TraitDecl`].
  ///
  /// It will return a [`Solver`] for the [`Signature`], and it will solve the [`Signature`] in
  /// the [`hir_lower`] query.
  pub fn hir_instance<'a>(&mut self, tree: lura_syntax::InstanceDecl<'a>) -> Solver<'a, TopLevel> {
    let range = self.range(tree.range());
    let path = tree.name().solve(self, |this, path| this.path(path));

    let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
    let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));

    // Converts the visibility to default visibility, if it is not specified.
    let vis = tree
      .visibility()
      .map(|vis| vis.solve(self, |this, node| this.hir_visibility(node)))
      .unwrap_or(Spanned::on_call_site(Vis::Public));

    let methods = tree
      .fields(&mut tree.walk())
      .flatten()
      .filter_map(|method| method.regular())
      .map(|method| self.hir_signature(method))
      .collect::<Vec<_>>();

    Solver::new(move |db, this| {
      // Creates a new scope for the function, and it will be used to store the parameters,
      // and the variables.
      this.scope = this.scope.fork(ScopeKind::Class);

      // Defines the node on the scope
      let node = this.qualify(path, DefinitionKind::Trait);

      let parameters = this.parameters(tree.arguments(&mut tree.walk()));

      let methods = methods
        .into_iter()
        .map(|method| method.run_solver(this))
        .filter_map(|top_level| match top_level {
          TopLevel::BindingGroup(binding_group) => Some(binding_group),
          _ => None,
        })
        .collect::<Vec<_>>();

      let types = tree
        .items(&mut tree.walk())
        .flatten()
        .filter_map(|node| node.regular())
        .map(|node| this.primary(node, HirLevel::Type).upgrade(db))
        .collect();

      // Publish all definitions to parent scope
      this.scope.publish_all_definitions(this.db, node);

      let instance_decl = InstanceDecl::new(
        this.db,
        /* attributes  = */ attrs,
        /* docs        = */ docs,
        /* visibility  = */ vis,
        /* name        = */ node,
        /* parameters  = */ parameters,
        /* types       = */ types,
        /* methods     = */ methods,
        /* location    = */ range.clone(),
        /* scope       = */ this.pop_scope(),
      );

      // It's not needed to solve the clause, because it is already solved in the next steps.
      //
      // The entire next step, is getting the clauses from the scope, and transforms into
      // declarations, so it is not needed to solve the clause here.
      TopLevel::InstanceDecl(instance_decl)
    })
  }

  /// Creates a new high level trait declaration [`TraitDecl`] solver, for the given
  /// concrete syntax tree [`lura_syntax::TraitDecl`].
  ///
  /// It will return a [`Solver`] for the [`Signature`], and it will solve the [`Signature`] in
  /// the [`hir_lower`] query.
  pub fn hir_trait<'a>(&mut self, tree: lura_syntax::TraitDecl<'a>) -> Solver<'a, TopLevel> {
    let range = self.range(tree.range());
    let path = tree.name().solve(self, |this, path| this.path(path));

    let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
    let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));

    // Converts the visibility to default visibility, if it is not specified.
    let vis = tree
      .visibility()
      .map(|vis| vis.solve(self, |this, node| this.hir_visibility(node)))
      .unwrap_or(Spanned::on_call_site(Vis::Public));

    // Defines the node on the scope
    let node = self
      .scope
      .define(self.db, path, range.clone(), DefinitionKind::Type);

    // Defines a trait for the trait too, so we can destruct it later.
    self
      .scope
      .define(self.db, path, range.clone(), DefinitionKind::Trait);

    let methods = tree
      .fields(&mut tree.walk())
      .flatten()
      .filter_map(|method| method.regular())
      .map(|method| self.hir_signature(method))
      .collect::<Vec<_>>();

    Solver::new(move |db, this| {
      // Creates a new scope for the function, and it will be used to store the parameters,
      // and the variables.
      this.scope = this.scope.fork(ScopeKind::Class);

      let parameters = this.parameters(tree.arguments(&mut tree.walk()));

      let methods = methods
        .into_iter()
        .map(|method| method.run_solver(this))
        .filter_map(|top_level| match top_level {
          TopLevel::BindingGroup(binding_group) => Some(binding_group),
          _ => None,
        })
        .collect::<Vec<_>>();

      let type_rep = tree
        .clause_type()
        .flatten()
        .map(|node| this.clause_type(node))
        .unwrap_or_default_with_db(db);

      // Publish all definitions to parent scope
      this.scope.publish_all_definitions(this.db, node);

      let trait_decl = TraitDecl::new(
        this.db,
        /* attributes  = */ attrs,
        /* docs        = */ docs,
        /* visibility  = */ vis,
        /* name        = */ node,
        /* parameters  = */ parameters,
        /* return_type = */ type_rep,
        /* methods     = */ methods,
        /* location    = */ range.clone(),
        /* scope       = */ this.pop_scope(),
      );

      // It's not needed to solve the clause, because it is already solved in the next steps.
      //
      // The entire next step, is getting the clauses from the scope, and transforms into
      // declarations, so it is not needed to solve the clause here.
      TopLevel::TraitDecl(trait_decl)
    })
  }

  /// Creates a new high level data declaration [`DataDecl`] solver, for the given
  /// concrete syntax tree [`lura_syntax::DataDecl`].
  ///
  /// It will return a [`Solver`] for the [`Signature`], and it will solve the [`Signature`] in
  /// the [`hir_lower`] query.
  pub fn hir_data<'a>(&mut self, tree: lura_syntax::DataDecl<'a>) -> Solver<'a, TopLevel> {
    let range = self.range(tree.range());
    let path = tree.name().solve(self, |this, path| this.path(path));

    let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
    let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));

    // Converts the visibility to default visibility, if it is not specified.
    let vis = tree
      .visibility()
      .map(|vis| vis.solve(self, |this, node| this.hir_visibility(node)))
      .unwrap_or(Spanned::on_call_site(Vis::Public));

    // Defines the node on the scope
    let node = self
      .scope
      .define(self.db, path, range.clone(), DefinitionKind::Type);

    let variants = tree
      .constructors(&mut tree.walk())
      .flatten()
      .filter_map(|constructor| constructor.regular())
      .filter_map(|constructor| self.hir_variant(constructor))
      .collect::<Vec<_>>();

    let methods = tree
      .methods(&mut tree.walk())
      .flatten()
      .filter_map(|method| method.regular())
      .map(|method| self.hir_signature(method))
      .collect::<Vec<_>>();

    Solver::new(move |db, this| {
      // Creates a new scope for the function, and it will be used to store the parameters,
      // and the variables.
      this.scope = this.scope.fork(ScopeKind::Data);

      let parameters = this.parameters(tree.arguments(&mut tree.walk()));

      let methods = methods
        .into_iter()
        .map(|method| method.run_solver(this))
        .filter_map(|top_level| match top_level {
          TopLevel::BindingGroup(binding_group) => Some(binding_group),
          _ => None,
        })
        .collect::<Vec<_>>();

      let type_rep = tree
        .clause_type()
        .flatten()
        .map(|node| this.clause_type(node))
        .unwrap_or_default_with_db(db);

      // Solve the variants within the current scope
      let variants = variants
        .into_iter()
        .map(|variant| variant.run_solver(this))
        .collect();

      // Publish all definitions to parent scope
      this.scope.publish_all_definitions(this.db, node);

      let data_decl = DataDecl::new(
        this.db,
        /* attributes  = */ attrs,
        /* docs        = */ docs,
        /* visibility  = */ vis,
        /* name        = */ node,
        /* parameters  = */ parameters,
        /* return_type = */ type_rep,
        /* variants    = */ variants,
        /* methods     = */ methods,
        /* location    = */ range.clone(),
        /* scope       = */ this.pop_scope(),
      );

      // It's not needed to solve the clause, because it is already solved in the next steps.
      //
      // The entire next step, is getting the clauses from the scope, and transforms into
      // declarations, so it is not needed to solve the clause here.
      TopLevel::DataDecl(data_decl)
    })
  }

  /// Creates a new high level constructor declaration [`Constructor`] solver, for the given
  /// concrete syntax tree [`lura_syntax::Constructor`].
  ///
  /// It will return [`None`] if the constructor is a comma, because it is not a constructor, it
  /// is just a separator.
  pub fn hir_variant<'a>(&mut self, tree: SyntaxVariant<'a>) -> Option<Solver<'a, Constructor>> {
    use lura_syntax::anon_unions::Comma_FunctionConstructor_SignatureConstructor::*;

    // Creates constructor declarations only if it is not a comma
    match tree {
      Comma(_) => None,
      FunctionConstructor(tree) => {
        let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
        let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));
        let name = tree.name().solve(self, |this, path| this.path(path));

        let location = self.range(tree.range());

        // Defines the node on the scope
        let name = self
          .scope
          .define(self.db, name, location.clone(), DefinitionKind::Constructor);

        Some(Solver::new(move |db, this| {
          this.scope = this.scope.fork(ScopeKind::Pi);

          let parameters = tree
            .parameters(&mut tree.walk())
            .flatten()
            .filter_map(|node| node.regular())
            .map(|type_rep| Parameter::unnamed(this.db, this.type_expr(type_rep)))
            .collect::<Vec<_>>();

          let scope = this.pop_scope();

          // As the function isn't a data constructor, it will be a function constructor, and
          // it's needed to create a local type representing the function.
          //
          // The Self type is used here, to avoid confusion in the resolution.
          let type_rep = TypeRep::Arrow(ArrowTypeRep::new(
            db,
            /* kind       = */ ArrowKind::Fun,
            /* parameters = */ parameters,
            /* value      = */ TypeRep::SelfType,
            /* location   = */ Location::CallSite,
            /* scope      = */ scope,
          ));

          Constructor::new(
            this.db,
            /* kind        = */ ConstructorKind::Function,
            /* attributes  = */ attrs,
            /* docs        = */ docs,
            /* name        = */ name,
            /* return_type = */ type_rep,
            /* location    = */ location,
          )
        }))
      }
      SignatureConstructor(tree) => {
        let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
        let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));
        let name = tree.name().solve(self, |this, path| this.path(path));

        let location = self.range(tree.range());

        // Defines the node on the scope
        let name = self
          .scope
          .define(self.db, name, location.clone(), DefinitionKind::Constructor);

        Some(Solver::new(move |_db, this| {
          // As it's a GADT constructor, it's already defined the type of the constructor, so
          // it's not needed to create a local type representing the function.
          let type_rep = tree
            .field_type()
            .solve(this, |this, expr| this.type_expr(expr));

          Constructor::new(
            this.db,
            /* kind        = */ ConstructorKind::Function,
            /* attributes  = */ attrs,
            /* docs        = */ docs,
            /* name        = */ name,
            /* return_type = */ type_rep,
            /* location    = */ location,
          )
        }))
      }
    }
  }

  /// Creates a new high level clause declaration within [`BindingGroup`] for the given concrete
  /// syntax tree [`lura_syntax::Clause`].
  ///
  /// It will return a [`Solver`] for the [`Clause`], and it will solve the [`Clause`] in the
  /// [`hir_lower`] query.
  pub fn hir_clause<'a>(&mut self, tree: lura_syntax::Clause<'a>) -> Solver<'a, TopLevel> {
    let path = tree.name().solve(self, |this, path| this.path(path));
    let location = self.range(tree.range());

    // Defines in the scope if it is not defined yet, and it will return the definition.
    let name = self
      .scope
      .search(self.db, path, DefinitionKind::Function)
      .unwrap_or_else(|| {
        // Defines the node on the scope
        self
          .scope
          .define(self.db, path, location.clone(), DefinitionKind::Function)
      });

    Solver::new(move |db, this| {
      //
      // Creates a new scope for the function, and it will be used to store the parameters,
      // and the variables.
      this.scope = this.scope.fork(ScopeKind::Function);

      let patterns = this.patterns(tree.patterns(&mut tree.walk()));

      // Transforms the patterns into bindings, to be used in the scope.
      let value = tree
        .value()
        .map(|value| value.solve(this, |this, node| this.expr(node, HirLevel::Expr)))
        .unwrap_or_default_with_db(db);

      let clause = Clause::new(db, name, patterns, value, location.clone());

      let binding_group = this.clauses.entry(name).or_insert_with(|| {
        // Creates a dummy signature implementation, to be used in the clause.
        let signature = Signature::new(
          db,
          /* attributes  = */ HashSet::default(),
          /* docs        = */ vec![],
          /* visibility  = */ Spanned::on_call_site(Vis::Public),
          /* name        = */ name,
          /* parameters  = */ vec![],
          /* return_type = */ TypeRep::Hole,
          /* location    = */ location,
        );

        BindingGroup::new(db, signature, HashSet::default())
      });

      let signature = binding_group.signature(db);

      // Inserts the current clause in the clauses, and solve it
      let mut clauses = binding_group.clauses(db);
      clauses.insert(clause);

      let group = BindingGroup::new(db, signature, clauses);

      // Adds the clause to the scope, and solve it
      this.clauses.insert(name, group);
      this.pop_scope();

      TopLevel::BindingGroup(group)
    })
  }

  /// Creates a new high level signature declaration [`Signature`] solver, for the given
  /// concrete syntax tree [`TreeSignature`].
  ///
  /// It will return a [`Solver`] for the [`Signature`], and it will solve the [`Signature`] in
  /// the [`hir_lower`] query.
  pub fn hir_signature<'a>(&mut self, tree: lura_syntax::Signature<'a>) -> Solver<'a, TopLevel> {
    let range = self.range(tree.range());
    let path = tree.name().solve(self, |this, node| this.path(node));

    let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
    let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));

    // Converts the visibility to default visibility, if it is not specified.
    let vis = tree
      .visibility()
      .map(|vis| vis.solve(self, |this, node| this.hir_visibility(node)))
      .unwrap_or(Spanned::on_call_site(Vis::Public));

    // Defines the node on the scope
    let node = self
      .scope
      .define(self.db, path, range.clone(), DefinitionKind::Function);

    Solver::new(move |db, this| {
      // Creates a new scope for the function, and it will be used to store the parameters,
      // and the variables.
      this.scope = this.scope.fork(ScopeKind::Function);

      let parameters = this.parameters(tree.arguments(&mut tree.walk()));

      // Transforms the parameters into bindings, to be used in the scope.
      let arguments = parameters
        .iter()
        .map(|parameter| parameter.binding(this.db))
        .collect::<Vec<_>>();

      let type_rep = tree
        .clause_type()
        .flatten()
        .map(|node| this.clause_type(node))
        .unwrap_or_default_with_db(db);

      let signature = Signature::new(
        this.db,
        /* attributes  = */ attrs,
        /* docs        = */ docs,
        /* visibility  = */ vis,
        /* name        = */ node,
        /* parameters  = */ parameters,
        /* return_type = */ type_rep,
        /* location    = */ range.clone(),
      );

      let clause = this
        .clauses
        .entry(node)
        .or_insert_with(|| BindingGroup::new(db, signature, HashSet::default()));

      // Adds the current body to the clause, and solve it
      let mut clauses = clause.clauses(this.db);

      let value = tree.value().map(|value| {
        value.solve(this, |this, node| {
          // Uses a new scope for the function, and it will be used to store the
          // parameters, and the variables.
          this.scoped(node, HirLevel::Expr)
        })
      });

      if let Some(value) = value {
        clauses.insert(Clause::new(
          this.db,
          /* name       = */ node,
          /* arguments  = */ arguments,
          /* value      = */ Expr::block(this.db, value),
          /* location   = */ range,
        ));
      }

      // Adds the clause to the scope, and solve it
      this
        .clauses
        .insert(node, BindingGroup::new(db, signature, clauses));

      this.pop_scope();

      // It's not needed to solve the clause, because it is already solved in the next steps.
      //
      // The entire next step, is getting the clauses from the scope, and transforms into
      // declarations, so it is not needed to solve the clause here.
      TopLevel::BindingGroup(*this.clauses.get(&node).unwrap())
    })
  }

  /// Takes an list of syntatic arguments, and returns a list of parameters, handled if it is
  /// either implicit or explicit, and if it is a named or unnamed parameter.
  ///
  /// It will transform `[a: b] => c -> d` into a list of `a: b`, `_: b`, `_: c` parameters, for
  /// example.
  pub fn parameters<'a, I>(&mut self, arguments: I) -> Vec<Parameter>
  where
    I: Iterator<Item = NodeResult<'a, ExtraOr<'a, ExplicitArguments_ImplicitArguments<'a>>>>,
  {
    arguments
      .flatten()
      .filter_map(|parameter| parameter.regular())
      .flat_map(|parameter| {
        use lura_syntax::anon_unions::ExplicitArguments_ImplicitArguments::*;

        match parameter {
          ExplicitArguments(explicits) => explicits
            .parameters(&mut explicits.walk())
            .flatten()
            .filter_map(|node| node.regular())
            .map(|parameter| self.any_parameter(false, true, parameter))
            .collect::<Vec<_>>(),

          ImplicitArguments(implicits) => implicits
            .parameters(&mut implicits.walk())
            .flatten()
            .filter_map(|node| node.regular())
            .map(|parameter| self.any_parameter(true, true, parameter))
            .collect::<Vec<_>>(),
        }
      })
      .collect::<Vec<_>>()
  }

  /// Takes a raw parameter, and returns a high level parameter, to be handled by the resolution.
  /// It will return a [`Parameter`].
  ///
  /// It will return a [`Parameter`] because it is possible to have multiple parameters in the
  /// same declaration, and it will be handled as a list of [`Parameter`].
  ///
  /// It does takes rigid and implicit parameters, and it will return a [`Parameter`] with the
  /// given parameters.
  pub fn parameter(
    &mut self,
    implicit: bool,
    rigid: bool,
    tree: lura_syntax::Parameter,
  ) -> Parameter {
    let binding = tree
      .pattern()
      .map(|node| node.solve(self, |this, pattern| this.pattern(pattern)))
      .unwrap_or_default_with_db(self.db);

    let type_rep = tree
      .parameter_type()
      .solve(self, |this, expr| this.type_expr(expr));

    let location = self.range(tree.range());

    Parameter::new(
      self.db,
      /* binding     = */ binding,
      /* type_rep    = */ type_rep,
      /* is_implicit = */ implicit,
      /* rigid       = */ rigid,
      /* level       = */ HirLevel::Expr,
      /* location    = */ location,
    )
  }

  /// Takes a raw parameter, and returns a high level parameter, to be handled by the resolution.
  /// It will return a [`Parameter`].
  ///
  /// It will return a [`Parameter`] because it is possible to have multiple parameters in the
  /// same declaration, and it will be handled as a list of [`Parameter`].
  ///
  /// It does takes rigid and implicit parameters, and it will return a [`Parameter`] with the
  /// given parameters.
  pub fn any_parameter(
    &mut self,
    implicit: bool,
    rigid: bool,
    tree: lura_syntax::anon_unions::ForallParameter_Parameter,
  ) -> Parameter {
    use lura_syntax::anon_unions::ForallParameter_Parameter::*;

    match tree {
      ForallParameter(forall_parameter) => self.forall_parameter(forall_parameter),
      Parameter(parameter) => {
        let binding = parameter
          .pattern()
          .map(|node| node.solve(self, |this, pattern| this.pattern(pattern)))
          .unwrap_or_default_with_db(self.db);

        let type_rep = parameter
          .parameter_type()
          .solve(self, |this, expr| this.type_expr(expr));

        let location = self.range(parameter.range());

        self::Parameter::new(
          self.db,
          /* binding     = */ binding,
          /* type_rep    = */ type_rep,
          /* is_implicit = */ implicit,
          /* rigid       = */ rigid,
          /* level       = */ HirLevel::Expr,
          /* location    = */ location,
        )
      }
    }
  }

  /// Takes a raw parameter, and returns a high level parameter, to be handled by the resolution.
  /// It will return a [`Parameter`].
  ///
  /// It will return a [`Parameter`] because it is possible to have multiple parameters in the
  /// same declaration, and it will be handled as a list of [`Parameter`].
  ///
  /// It does takes rigid and implicit parameters, and it will return a [`Parameter`] with the
  /// given parameters.
  pub fn forall_parameter(&mut self, tree: lura_syntax::ForallParameter) -> Parameter {
    let name = tree.identifier().with_db(self, |this, segment| {
      let range = this.range(segment.range());
      let identifer = segment.child().ok()?;
      let txt = this.txt.clone();

      // Creates an unique identifier, as it is not possible to
      // have two parameters with the same name.
      let unique = match identifer {
        SyntaxIdentifier::SimpleIdentifier(value) => {
          let string = value.utf8_text(txt.as_bytes()).ok().unwrap_or_default();

          Identifier::new(this.db, string.into(), false, range.clone())
        }
        SyntaxIdentifier::SymbolIdentifier(value) => {
          let string = value
            .child()
            .with_db(this, |_, node| node.utf8_text(txt.as_bytes()).ok());

          Identifier::new(this.db, string.into(), true, range.clone())
        }
      };

      Some(HirPath::new(this.db, range, vec![unique]))
    });
    let location = self.range(tree.range());

    let definition = self.scope.define(
      self.db,
      /* name     = */ name,
      /* location = */ name.location(self.db),
      /* kind     = */ DefinitionKind::Type,
    );
    let binding = BindingPattern::new(self.db, definition, name.location(self.db));
    let pattern = Pattern::Binding(binding);

    Parameter::new(
      self.db,
      /* binding     = */ pattern,
      /* type_rep    = */ TypeRep::Hole,
      /* is_implicit = */ true,
      /* rigid       = */ true,
      /* level       = */ HirLevel::Type,
      /* location    = */ location,
    )
  }

  /// Handles a list of raw documentation items in to a list of high level documentation items
  /// to be shown in the IDE, or handled by resolution. It will return a list of [`DocString`].
  ///
  /// It will return a list of [`DocString`] because it is possible to have multiple documentation
  /// items in the same declaration, and it will be handled as a list of [`DocString`].
  pub fn hir_docs<'a, I>(&mut self, attributes: I) -> Vec<DocString>
  where
    I: Iterator<Item = NodeResult<'a, ExtraOr<'a, lura_syntax::DocString<'a>>>>,
  {
    attributes
      .flatten()
      .filter_map(|attr| {
        let value = attr.regular()?;
        let range = self.range(value.range());

        Some(DocString::new(self.db, range))
      })
      .collect()
  }

  /// Handles a list of raw attributes in to a list of high level attributes to be shown in the
  /// IDE, or handled by resolution. It will return a list of [`Attribute`].
  ///
  /// It will return a list of [`Attribute`] because it is possible to have multiple attributes
  /// in the same declaration, and it will be handled as a list of [`Attribute`].
  pub fn hir_attributes<'a, I>(&mut self, attributes: I) -> HashSet<Attribute, FxBuildHasher>
  where
    I: Iterator<Item = NodeResult<'a, ExtraOr<'a, lura_syntax::Attribute<'a>>>>,
  {
    attributes
      .flatten()
      .filter_map(|attribute| {
        let value = attribute.regular()?;
        let name = value.name().solve(self, |this, path| this.path(path));
        let arguments = vec![];
        let range = self.range(attribute.range());

        Some(Attribute::new(self.db, name, arguments, range))
      })
      .collect()
  }

  /// Handles a raw visibility in to a high level visibility to be shown in the IDE, or handled by
  /// resolution. It will return a [`Vis`]. Defaulting to [`Vis::Public`] if the visibility is
  /// not specified, or bad specified.
  ///
  /// It does return a [`Spanned`] wrapped visibility, to be possible to get it's location.
  pub fn hir_visibility(&mut self, visibility: lura_syntax::Visibility) -> Spanned<Vis> {
    let value = match visibility.utf8_text(self.src.source_text(self.db).as_bytes()) {
      Ok("public") => Vis::Public,
      Ok("sealed") => Vis::Sealed,
      Ok("private") => Vis::Private,
      Ok("internal") => Vis::Internal,
      _ => Vis::Public,
    };
    let location = self.range(visibility.range());
    Spanned::new(value, location)
  }

  /// Creates a new [`HirPath`] from the given [`Path`]. It does transform the raw path, in a
  /// high level path, to be handled within [`Self::qualify`] function.
  ///
  /// The [`HirPath`] is a high level path, and it is used in the resolution.
  pub fn path(&mut self, path: lura_syntax::Path) -> HirPath {
    let mut new_segments = vec![];
    let source_text = self.src.source_text(self.db).as_bytes();

    let range = self.range(path.range());

    for segment in path.segments(&mut path.walk()) {
      segment.or_default_error(self, |this, segment: lura_syntax::Identifier| {
        let range = this.range(segment.range());

        let identifer = match segment.child() {
          Ok(name) => name,
          Err(_) => return,
        };

        new_segments.push(match identifer {
          SyntaxIdentifier::SimpleIdentifier(value) => {
            let string = value.utf8_text(source_text).ok().unwrap_or_default();

            Identifier::new(this.db, string.into(), false, range)
          }
          SyntaxIdentifier::SymbolIdentifier(value) => {
            let string = value
              .child()
              .with_db(this, |_, node| node.utf8_text(source_text).ok());

            Identifier::new(this.db, string.into(), true, range)
          }
        });
      });
    }

    HirPath::new(self.db, range, new_segments)
  }

  /// Qualifies the given [`HirPath`] with the current package, and returns a new [`Definition`]
  /// with the qualified [`HirPath`]. It does search the definition in the scope, and if it is
  /// not present in the scope, it will invoke a compiler query to search in the entire package.
  ///
  /// It is useful to search for definitions in the scope, and it will be used in the resolution.
  pub fn qualify(&self, path: HirPath, kind: DefinitionKind) -> Definition {
    let definition = self.scope.search(self.db, path, kind);

    // Seach in the scope, or query the compiler query-system to search in the entire package.
    //
    // If the definition is not found, it will return a [`Definition::no`].
    definition.unwrap_or_else(|| match kind {
      DefinitionKind::Function => find_function(self.db, path),
      DefinitionKind::Constructor => find_constructor(self.db, path),
      DefinitionKind::Type => find_type(self.db, path),
      DefinitionKind::Trait => find_trait(self.db, path),
      DefinitionKind::Variable => Definition::no(self.db, kind, path),
      DefinitionKind::Module => Definition::no(self.db, kind, path),
      DefinitionKind::Command => Definition::no(self.db, kind, path),
      DefinitionKind::Unresolved => Definition::no(self.db, kind, path),
    })
  }

  /// Creates a new [`Location`] from the given [`tree_sitter::Range`]. It does transforms the
  /// raw location, in a high level location, to be handled within resolution.
  pub fn range(&self, range: tree_sitter::Range) -> Location {
    Location::new(
      self.db,
      self.src,
      self.txt.clone(),
      range.start_byte,
      range.end_byte,
    )
  }

  pub fn pop_scope(&mut self) -> Arc<Scope> {
    let scope = take(&mut self.scope);
    self.scope = scope.root().deref().clone();
    Arc::new(scope)
  }
}

/// Defines a solver function over the [`LowerHir`] struct, that will solve the clauses, and will
/// return the [`HirSource`] with the declarations and the clauses solved.
#[allow(clippy::type_complexity)]
struct Solver<'a, T> {
  f: Box<dyn FnOnce(&dyn crate::HirDb, &mut HirLowering<'_, '_>) -> T + 'a>,
}

impl<'a, T> Solver<'a, T> {
  /// Creates a new solver function over the [`LowerHir`] struct.
  fn new<F>(f: F) -> Self
  where
    F: FnOnce(&dyn crate::HirDb, &mut HirLowering<'_, '_>) -> T + 'a,
  {
    Self { f: Box::new(f) }
  }

  /// Runs the solver function over the [`LowerHir`] struct, and returns the result.
  fn run_solver(self, lower: &mut HirLowering<'_, '_>) -> T {
    (self.f)(lower.db, lower)
  }
}

trait NodeResultExt<'tree, N, T: Default> {
  fn or_error<F>(self, f: F) -> T
  where
    F: FnOnce(N) -> Option<T>;
}

/// Defines a trait that will extend the [`DbNodeResult`] with the [`LowerHir`] struct.
trait DbNodeResultExt<'tree, N> {
  #[inline]
  fn solve<F, T>(self, db: &mut HirLowering, f: F) -> T
  where
    Self: Sized,
    T: DefaultWithDb,
    F: FnOnce(&mut HirLowering, N) -> T,
  {
    self.with_db(db, |db, node| Some(f(db, node)))
  }

  #[inline]
  fn or_default_error<F>(self, db: &mut HirLowering, f: F)
  where
    Self: Sized,
    F: FnOnce(&mut HirLowering, N),
  {
    self.with_db(db, |db, node| {
      f(db, node);
      Some(())
    })
  }

  fn with_db<F, T>(self, db: &mut HirLowering, f: F) -> T
  where
    T: DefaultWithDb,
    F: FnOnce(&mut HirLowering, N) -> Option<T>;
}

/// Defines implementations of util traits to the lower.
///
/// This module defines implementations of util traits to the lower, that are used to simplify the
/// code.
mod util {
  use super::*;

  impl<'tree, N, T: Default> NodeResultExt<'tree, N, T> for Result<N, IncorrectKind<'tree>> {
    #[inline]
    fn or_error<F>(self, f: F) -> T
    where
      F: FnOnce(N) -> Option<T>,
    {
      match self {
        Ok(node) => f(node).unwrap_or_default(),
        Err(..) => T::default(),
      }
    }
  }

  impl<'tree, N, T: Default> NodeResultExt<'tree, N, T>
    for Result<ExtraOr<'tree, N>, IncorrectKind<'tree>>
  {
    #[inline]
    fn or_error<F>(self, f: F) -> T
    where
      F: FnOnce(N) -> Option<T>,
    {
      match self {
        Ok(ExtraOr::Extra(..)) => T::default(),
        Ok(ExtraOr::Regular(node)) => f(node).unwrap_or_default(),
        Err(..) => T::default(),
      }
    }
  }

  impl<'tree, N> DbNodeResultExt<'tree, N> for Result<N, IncorrectKind<'tree>> {
    #[inline]
    fn with_db<F, T>(self, lower: &mut HirLowering, f: F) -> T
    where
      T: DefaultWithDb,
      F: FnOnce(&mut HirLowering, N) -> Option<T>,
    {
      match self {
        Ok(node) => f(lower, node).unwrap_or_else(|| T::default_with_db(lower.db)),
        Err(err) => {
          let location = lower.range(err.node.range());

          T::incorrect_kind(lower.db, err.kind, location)
        }
      }
    }
  }

  impl<'tree, N> DbNodeResultExt<'tree, N> for Result<ExtraOr<'tree, N>, IncorrectKind<'tree>> {
    #[inline]
    fn with_db<F, T>(self, lower: &mut HirLowering, f: F) -> T
    where
      T: DefaultWithDb,
      F: FnOnce(&mut HirLowering, N) -> Option<T>,
    {
      match self {
        Ok(ExtraOr::Regular(node)) => {
          f(lower, node).unwrap_or_else(|| T::default_with_db(lower.db))
        }
        Ok(ExtraOr::Extra(extra)) => {
          let location = lower.range(extra.range());

          T::extra_data(lower.db, location)
        }
        Err(err) => {
          let location = lower.range(err.node.range());

          T::incorrect_kind(lower.db, err.kind, location)
        }
      }
    }
  }
}

mod literal;
mod pattern;
mod stmt;
mod term;
