//! This module defines the lowering of the syntax tree to the high level intermediate representation
//! (HIR).
//!
//! Lowering means that it will take the syntax tree, and will transform it into a low level for
//! human readability, but high-level abstraction for the compiler. It's very useful to the future
//! steps, even of the compiler frontend.

use std::{
    collections::{HashMap, HashSet},
    mem::take,
    ops::Deref,
    sync::Arc,
};

use salsa::Cycle;
use tree_sitter::{Node, Tree};
use type_sitter_lib::{ExtraOr, IncorrectKind, NodeResult, OptionNodeResultExt, TypedNode};

use lura_syntax::{
    anon_unions::ExplicitArguments_ImplicitArguments, generated::lura::SourceFile, Source,
};

use crate::source::{
    pattern::{BindingPattern, Pattern},
    top_level::{ClassDecl, CommandTopLevel, Constructor, ConstructorKind, DataDecl, TraitDecl},
    type_rep::{ArrowKind, ArrowTypeRep},
};
use crate::{
    package::Package,
    resolve::{find_function, Definition, DefinitionKind, HirLevel},
    scope::{Scope, ScopeKind},
    source::{
        declaration::{Attribute, DocString, Parameter, Vis},
        expr::{AbsExpr, AnnExpr, CallExpr, CallKind, Callee, Expr},
        top_level::{BindingGroup, Clause, Signature, TopLevel, UsingTopLevel},
        type_rep::{AppTypeRep, TypeRep},
        DefaultWithDb, HirPath, HirSource, Identifier, Location, OptionExt, Spanned,
    },
};

#[rustfmt::skip]
type SyntaxDecl<'tree> = lura_syntax::anon_unions::ClassDecl_Clause_Command_DataDecl_Signature_TraitDecl_Using<'tree>;

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
pub fn hir_declare(db: &dyn crate::HirDb, pkg: Package, src: Source) -> HirSource {
    let parse_tree = src.syntax_node(db);

    let lower = LowerHir {
        db,
        src,
        pkg,
        txt: Arc::new(src.source_text(db).to_string()),
        decls: vec![],
        scope: Scope::new(ScopeKind::File),
        tree: parse_tree.tree.clone(),
        root_node: parse_tree.tree.root_node(),
        clauses: HashMap::new(),
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
pub fn hir_lower(db: &dyn crate::HirDb, pkg: Package, src: Source) -> HirSource {
    let parse_tree = src.syntax_node(db);

    let lower = LowerHir {
        db,
        src,
        pkg,
        txt: Arc::new(src.source_text(db).to_string()),
        decls: vec![],
        scope: Scope::new(ScopeKind::File),
        tree: parse_tree.tree.clone(),
        root_node: parse_tree.tree.root_node(),
        clauses: HashMap::new(),
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
struct LowerHir<'db, 'tree> {
    db: &'db dyn crate::HirDb,
    src: Source,
    txt: Arc<String>,
    tree: Arc<Tree>,
    decls: Vec<TopLevel>,
    pkg: Package,
    scope: Scope,
    root_node: Node<'tree>,
    clauses: HashMap<HirPath, BindingGroup>,
}

impl<'db, 'tree> LowerHir<'db, 'tree> {
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
                    if let TopLevel::Empty = decl {
                        continue;
                    }

                    self.decls.push(decl);
                };
            }
        }

        // Pulls the clauses from the scope, and transforms into new declarations, to be put in the
        // instance of [`HirSource`].
        for group in self.clauses.values() {
            self.decls.push(TopLevel::BindingGroup(*group))
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
        use lura_syntax::anon_unions::ClassDecl_Clause_Command_DataDecl_Signature_TraitDecl_Using::*;

        // Creates a new [`TopLevel`] instance.
        let decl = match decl {
            Command(command) => self.hir_command(command),
            ClassDecl(class_decl) => return self.hir_class(class_decl).into(),
            Clause(clause) => return self.hir_clause(clause).into(),
            DataDecl(data_decl) => return self.hir_data(data_decl).into(),
            TraitDecl(trait_decl) => return self.hir_trait(trait_decl).into(),
            Signature(signature) => return self.hir_signature(signature).into(),
            Using(decl) => {
                let range = self.range(decl.range());
                let path = decl.path().solve(self, |this, node| this.path(node));

                // TODO: search for functions or anything too.
                let def = self.qualify(path, DefinitionKind::Module);
                let reference = self.scope.using(self.db, def, path.location(self.db));

                TopLevel::Using(UsingTopLevel::new(self.db, reference, range))
            }
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
                let name =
                    self.scope
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
                        /* kind       = */ ArrowKind::Pi,
                        /* parameters = */ parameters,
                        /* value      = */ TypeRep::This,
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
                let name =
                    self.scope
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
                self.scope
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

            let binding_group = this.clauses.entry(path).or_insert_with(|| {
                // Creates a dummy signature implementation, to be used in the clause.
                let signature = Signature::new(
                    db,
                    /* attributes  = */ HashSet::default(),
                    /* docs        = */ vec![],
                    /* visibility  = */ Spanned::on_call_site(Vis::Public),
                    /* name        = */ name,
                    /* parameters  = */ vec![],
                    /* return_type = */ TypeRep::Empty,
                    /* location    = */ location,
                );

                BindingGroup::new(db, signature, HashSet::new())
            });

            let signature = binding_group.signature(db);

            // Inserts the current clause in the clauses, and solve it
            let mut clauses = binding_group.clauses(db);
            clauses.insert(clause);

            let group = BindingGroup::new(db, signature, clauses);

            // Adds the clause to the scope, and solve it
            this.clauses.insert(path, group);
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
                .entry(path)
                .or_insert_with(|| BindingGroup::new(db, signature, HashSet::new()));

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
            this.clauses
                .insert(path, BindingGroup::new(db, signature, clauses));

            this.pop_scope();

            // It's not needed to solve the clause, because it is already solved in the next steps.
            //
            // The entire next step, is getting the clauses from the scope, and transforms into
            // declarations, so it is not needed to solve the clause here.
            TopLevel::BindingGroup(*this.clauses.get(&path).unwrap())
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

        Parameter::new(self.db, binding, type_rep, implicit, rigid, location)
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

                self::Parameter::new(self.db, binding, type_rep, implicit, rigid, location)
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
        let name = tree.path().solve(self, |this, path| this.path(path));
        let location = self.range(tree.range());

        let definition = self.scope.define(
            self.db,
            /* name     = */ name,
            /* location = */ name.location(self.db),
            /* kind     = */ DefinitionKind::Type,
        );
        let binding = BindingPattern::new(self.db, definition, name.location(self.db));
        let pattern = Pattern::Binding(binding);

        Parameter::new(self.db, pattern, TypeRep::Tt, true, true, location)
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
    pub fn hir_attributes<'a, I>(&mut self, attributes: I) -> HashSet<Attribute>
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
        Definition::no(self.db, kind, path)
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

/// Defines a module for resolving language "patterns", that matches agains't values and another
/// things. It will be used in the resolution, and it's a helper module for the [`LowerHir`] struct.
///
/// It's only a module, to organization purposes.
mod pattern_solver {
    use crate::source::pattern::{BindingPattern, Constructor, ConstructorPattern, Pattern};

    use super::*;

    #[rustfmt::skip]
    type SyntaxPattern<'tree> = lura_syntax::anon_unions::ConsPattern_GroupPattern_Literal_RestPattern<'tree>;

    impl LowerHir<'_, '_> {
        pub fn pattern(&mut self, tree: SyntaxPattern) -> Pattern {
            use lura_syntax::anon_unions::ConsPattern_GroupPattern_Literal_RestPattern::*;

            let location = self.range(tree.range());

            match tree {
                ConsPattern(cons_pattern) => self.cons_pattern(cons_pattern),
                GroupPattern(group_pattern) => self.group_pattern(group_pattern),
                Literal(literal) => self.literal(literal).upgrade_pattern(location, self.db),
                RestPattern(_) => Pattern::Rest(location),
            }
        }

        pub fn group_pattern(&mut self, group: lura_syntax::GroupPattern) -> Pattern {
            // There's no AST for groups, so we need to solve it directly
            return group.pattern().solve(self, |this, node| this.pattern(node));
        }

        pub fn cons_pattern(&mut self, pattern: lura_syntax::ConsPattern) -> Pattern {
            let name = pattern.name().solve(self, |this, node| this.path(node));
            let patterns = self.patterns(pattern.patterns(&mut pattern.walk()));
            let location = self.range(pattern.range());

            // If the patterns are empty, it's a binding pattern, otherwise, it's a constructor
            // pattern.
            if patterns.is_empty() {
                // Defines the node on the scope
                let name =
                    self.scope
                        .define(self.db, name, location.clone(), DefinitionKind::Variable);

                Pattern::Binding(BindingPattern::new(self.db, name, location))
            } else {
                let def = self.qualify(name, DefinitionKind::Constructor);
                let reference = self.scope.using(self.db, def, name.location(self.db));
                let name = Constructor::Path(reference);

                Pattern::Constructor(ConstructorPattern::new(self.db, name, patterns, location))
            }
        }

        pub fn patterns<'a, I>(&mut self, patterns: I) -> Vec<Pattern>
        where
            I: Iterator<Item = NodeResult<'a, ExtraOr<'a, SyntaxPattern<'a>>>>,
        {
            patterns
                .flatten()
                .filter_map(|pattern| pattern.regular())
                .map(|pattern| self.pattern(pattern))
                .collect()
        }
    }
}

/// Defines a module for resolving language "literals", that are simple values, like numbers,
/// strings, and other things. It will be used in the resolution, and it's a helper module for the
/// [`LowerHir`] struct.
///
/// It's only a module, to organization purposes.
mod literal_solver {
    use crate::source::literal::Literal;

    use super::*;

    impl LowerHir<'_, '_> {
        pub fn literal(&mut self, tree: lura_syntax::Literal) -> Literal {
            use lura_syntax::anon_unions::Char_F32_F64_I128_I16_I64_I8_Nat_String_U1_U128_U16_U32_U64_U8::*;

            let text = tree
                .utf8_text(self.src.source_text(self.db).as_bytes())
                .unwrap_or_default();

            tree.child().with_db(self, |_, node| match node {
                Char(..) => todo!("Not implemented Char literal"),
                F32(..) => todo!("Not implemented F32 literal"),
                F64(..) => todo!("Not implemented F64 literal"),
                I8(..) => text.parse::<i8>().ok().map(Literal::Int8),
                I16(..) => text.parse::<i16>().ok().map(Literal::Int16),
                I64(..) => text.parse::<i64>().ok().map(Literal::Int64),
                I128(..) => todo!("Not implemented I128 literal"),
                U1(..) => text.parse::<bool>().ok().map(Literal::Boolean),
                U8(..) => text.parse::<u8>().ok().map(Literal::UInt8),
                U16(..) => text.parse::<u16>().ok().map(Literal::UInt16),
                U32(..) => text.parse::<u32>().ok().map(Literal::UInt32),
                U64(..) => text.parse::<u64>().ok().map(Literal::UInt64),
                U128(..) => todo!("Not implemented U128 literal"),
                Nat(..) => todo!("Not implemented Nat literal"),
                String(..) => Some(Literal::String((&text[1..text.len() - 1]).into())),
            })
        }
    }
}

/// Defines a module for resolving language "statements", that are the statements, like `let`,
/// `ask`, and other things. It will be used in the resolution, and it's a helper module for the
/// [`LowerHir`] struct.
///
/// It's only a module, to organization purposes.
mod stmt_solver {
    use crate::{
        resolve::HirLevel,
        source::{
            expr::{MatchArm, MatchExpr, MatchKind},
            literal::Literal,
            pattern::Pattern,
            stmt::{AskStmt, Block, LetStmt, Stmt},
            HirElement,
        },
    };

    use super::*;

    type SyntaxStmt<'tree> = lura_syntax::anon_unions::AskStmt_ExprStmt_IfStmt_LetStmt<'tree>;

    impl LowerHir<'_, '_> {
        pub fn stmt(&mut self, stmt: SyntaxStmt, level: HirLevel) -> Stmt {
            use lura_syntax::anon_unions::AskStmt_ExprStmt_IfStmt_LetStmt::*;

            match stmt {
                AskStmt(ask_stmt) => self.ask_stmt(ask_stmt, level),
                ExprStmt(expr_stmt) => self.expr_stmt(expr_stmt, level),
                IfStmt(if_stmt) => self.if_stmt(if_stmt, level),
                LetStmt(let_stmt) => self.let_stmt(let_stmt, level),
            }
        }

        pub fn ask_stmt(&mut self, stmt: lura_syntax::AskStmt, level: HirLevel) -> Stmt {
            let pattern = stmt.pattern().solve(self, |this, node| this.pattern(node));
            let expr = stmt
                .value()
                .solve(self, |this, node| this.expr(node, level));

            let location = self.range(stmt.range());

            Stmt::Ask(AskStmt::new(self.db, pattern, expr, location))
        }

        pub fn expr_stmt(&mut self, stmt: lura_syntax::ExprStmt, level: HirLevel) -> Stmt {
            let expr = stmt
                .child()
                .solve(self, |this, node| this.expr(node, level));

            Stmt::Downgrade(expr)
        }

        pub fn if_stmt(&mut self, stmt: lura_syntax::IfStmt, level: HirLevel) -> Stmt {
            let scrutinee = stmt
                .condition()
                .solve(self, |this, node| this.expr(node, level));

            let then = stmt.then().solve(self, |this, node| {
                use lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr::*;

                node.child().solve(this, |this, node| match node {
                    Block(block) => Expr::block(this.db, this.block(block, level)),
                    _ => this.expr(node.into_node().try_into().unwrap(), level),
                })
            });

            let otherwise = stmt.otherwise().map(|then| {
                then.solve(self, |this, node| {
                    use lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr::*;

                    node.value().solve(this, |this, node| match node {
                        Block(block) => Expr::block(this.db, this.block(block, level)),
                        _ => this.expr(node.into_node().try_into().unwrap(), level),
                    })
                })
            })
            .unwrap_or_else(|| Expr::call_unit_expr(Location::CallSite, self.db));

            let clauses = vec![
                MatchArm {
                    pattern: Pattern::Literal(Spanned::on_call_site(Literal::TRUE)),
                    location: then.location(self.db),
                    value: then,
                },
                MatchArm {
                    pattern: Pattern::Literal(Spanned::on_call_site(Literal::FALSE)),
                    location: otherwise.location(self.db),
                    value: otherwise,
                },
            ];

            let location = self.range(stmt.range());

            Stmt::Downgrade(Expr::Match(MatchExpr::new(
                self.db,
                /* kind      = */ MatchKind::StmtLevel(Box::new(MatchKind::If)),
                /* scrutinee = */ scrutinee,
                /* clauses   = */ clauses,
                /* location  = */ location,
            )))
        }

        pub fn let_stmt(&mut self, stmt: lura_syntax::LetStmt, level: HirLevel) -> Stmt {
            let pattern = stmt.pattern().solve(self, |this, node| this.pattern(node));
            let expr = stmt
                .value()
                .solve(self, |this, node| this.expr(node, level));

            let location = self.range(stmt.range());

            Stmt::Let(LetStmt::new(self.db, pattern, expr, location))
        }

        pub fn scoped(&mut self, block: lura_syntax::Block, level: HirLevel) -> Block {
            let stmts = block
                .statements(&mut block.walk())
                .flatten()
                .filter_map(|stmt| stmt.regular())
                .map(|stmt| self.stmt(stmt, level))
                .collect();

            Block::new(
                self.db,
                /* statements = */ stmts,
                /* location   = */ self.range(block.range()),
                /* scope      = */ self.scope.clone().into(),
            )
        }

        pub fn block(&mut self, block: lura_syntax::Block, level: HirLevel) -> Block {
            self.scope = self.scope.fork(ScopeKind::Block);
            let value = self.scoped(block, level);
            self.pop_scope();
            value
        }
    }
}

/// Defines a module for resolving language "terms", that are the expressions, types, and other
/// things that are used in the language, like primaries. It will be used in the resolution, and
/// it's a helper module for the [`LowerHir`] struct.
///
/// It's only a module, to organization purposes.
mod term_solver {
    use crate::{
        resolve::{find_type, HirLevel},
        source::{
            expr::{MatchArm, MatchExpr, MatchKind},
            literal::Literal,
            pattern::Pattern,
            type_rep::ArrowKind,
            HirElement,
        },
    };

    use super::*;

    #[rustfmt::skip]
    type SyntaxExpr<'tree> = lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<'tree>;

    #[rustfmt::skip]
    type SyntaxTypeRep<'tree> = lura_syntax::anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr<'tree>;

    impl LowerHir<'_, '_> {
        pub fn clause_type(&mut self, clause: lura_syntax::ClauseType) -> TypeRep {
            clause
                .clause_type()
                .solve(self, |this, node| this.type_expr(node))
        }

        pub fn type_expr(&mut self, tree: SyntaxTypeRep) -> TypeRep {
            use lura_syntax::anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr::*;

            match tree {
                // SECTION: type_expr
                //
                // Upgrades the expressions to type level expressions, to be easier to handle errors
                // in the type system, and still keeps the diagnostics in the IDE.
                Primary(primary) => self.primary(primary, HirLevel::Type).upgrade(self.db),
                AnnExpr(ann_expr) => self.ann_expr(ann_expr, HirLevel::Type).upgrade(self.db),
                LamExpr(lam_expr) => self.lam_expr(lam_expr, HirLevel::Type).upgrade(self.db),
                MatchExpr(match_expr) => {
                    self.match_expr(match_expr, HirLevel::Type).upgrade(self.db)
                }
                BinaryExpr(binary_expr) => self
                    .binary_expr(binary_expr, HirLevel::Type)
                    .upgrade(self.db),

                // Type level expressions
                PiExpr(pi) => self.pi_expr(pi),
                SigmaExpr(sigma) => self.sigma_expr(sigma),
                TypeAppExpr(type_app) => self.type_app_expr(type_app),
                ForallExpr(forall) => self.forall_expr(forall),
            }
        }

        pub fn expr(&mut self, tree: SyntaxExpr, level: HirLevel) -> Expr {
            use lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr::*;

            match tree {
                // SECTION: expr
                Primary(primary) => self.primary(primary, level),
                AnnExpr(ann_expr) => self.ann_expr(ann_expr, level),
                LamExpr(lam_expr) => self.lam_expr(lam_expr, level),
                MatchExpr(match_expr) => self.match_expr(match_expr, level),
                BinaryExpr(binary_expr) => self.binary_expr(binary_expr, level),
                AppExpr(app) => self.app_expr(app, level),

                // Type level expressions
                PiExpr(pi) => self.pi_expr(pi).downgrade(self.db),
                SigmaExpr(sigma) => self.sigma_expr(sigma).downgrade(self.db),
                ForallExpr(forall) => self.forall_expr(forall).downgrade(self.db),
            }
        }

        pub fn ann_expr(&mut self, tree: lura_syntax::AnnExpr, level: HirLevel) -> Expr {
            let value = tree
                .value()
                .solve(self, |this, node| this.expr(node, level));
            let type_rep = tree
                .against()
                .solve(self, |this, expr| this.type_expr(expr));
            let location = self.range(tree.range());

            Expr::Ann(AnnExpr::new(self.db, value, type_rep, location))
        }

        pub fn binary_expr(&mut self, tree: lura_syntax::BinaryExpr, level: HirLevel) -> Expr {
            let lhs = tree.lhs().solve(self, |this, node| this.expr(node, level));
            let rhs = tree.rhs().solve(self, |this, node| this.expr(node, level));
            let op = tree.op().solve(self, |this, node| {
                let location = this.range(node.range());
                let identifier = node
                    .utf8_text(this.src.source_text(this.db).as_bytes())
                    .unwrap_or_default();

                let identifier = Identifier::symbol(this.db, identifier, location.clone());

                HirPath::new(this.db, location, vec![identifier])
            });
            let location = self.range(tree.range());

            let op = self
                .scope
                .search(self.db, op, DefinitionKind::Function)
                .unwrap_or_else(|| {
                    // Queries [`self.db`] for the definition of the operator, and returns it, otherwise it
                    // will report an error. It's made for doing global lookups, and not local lookups.
                    find_function(self.db, op)
                });

            let reference = self.scope.using(self.db, op, location.clone());

            Expr::Call(CallExpr::new(
                self.db,
                /* kind        = */ CallKind::Infix,
                /* callee      = */ Callee::Reference(reference),
                /* arguments   = */ vec![lhs, rhs],
                /* do_notation = */ None,
                /* location    = */ location,
            ))
        }

        pub fn lam_expr(&mut self, tree: lura_syntax::LamExpr, level: HirLevel) -> Expr {
            self.scope = self.scope.fork(ScopeKind::Lambda);

            let parameters = tree
                .parameters(&mut tree.walk())
                .flatten()
                .filter_map(|node| node.regular())
                .filter_map(|node| node.parameter())
                .map(|parameter| self.parameter(false, false, parameter))
                .collect::<Vec<_>>();

            let value = tree
                .value()
                .solve(self, |this, node| this.expr(node, level));
            let location = self.range(tree.range());

            let scope = self.pop_scope();

            Expr::Abs(AbsExpr::new(self.db, parameters, value, location, scope))
        }

        pub fn app_expr(&mut self, tree: lura_syntax::AppExpr, level: HirLevel) -> Expr {
            let callee = tree
                .callee()
                .solve(self, |this, node| this.primary(node, level));

            let arguments = tree
                .arguments(&mut tree.walk())
                .flatten()
                .flat_map(|node| node.regular())
                .map(|node| self.primary(node, level))
                .collect::<Vec<_>>();

            let do_notation = tree
                .children(&mut tree.walk())
                .flatten()
                .filter_map(|node| node.regular())
                .filter_map(|node| node.block())
                .map(|node| self.block(node, level))
                .last();

            let location = self.range(tree.range());

            Expr::Call(CallExpr::new(
                self.db,
                /* kind        = */ CallKind::Infix,
                /* callee      = */ Callee::Expr(callee),
                /* arguments   = */ arguments,
                /* do_notation = */ do_notation,
                /* location    = */ location,
            ))
        }

        pub fn type_app_expr(&mut self, tree: lura_syntax::TypeAppExpr) -> TypeRep {
            let callee = tree.callee().solve(self, |this, node| {
                this.primary(node, HirLevel::Type).upgrade(this.db)
            });

            let arguments = tree
                .arguments(&mut tree.walk())
                .flatten()
                .flat_map(|node| node.regular())
                .map(|node| self.primary(node, HirLevel::Type).upgrade(self.db))
                .collect::<Vec<_>>();

            let location = self.range(tree.range());

            TypeRep::App(AppTypeRep::new(self.db, callee, arguments, location))
        }

        pub fn pi_expr(&mut self, tree: lura_syntax::PiExpr) -> TypeRep {
            use lura_syntax::anon_unions::AnnExpr_BinaryExpr_ForallExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr::*;

            self.scope = self.scope.fork(ScopeKind::Pi);

            let parameters = tree.parameter().solve(self, |this, node| {
                let type_rep = match node {
                    PiNamedParameterSet(tree) => {
                        return tree
                            .parameters(&mut tree.walk())
                            .flatten()
                            .filter_map(|node| node.regular())
                            .filter_map(|node| node.parameter())
                            .map(|parameter| this.parameter(false, true, parameter))
                            .collect::<Vec<_>>();
                    }
                    _ => this.type_expr(node.into_node().try_into().unwrap()),
                };

                // This handles the case where the parameter is unnamed, and only haves a type. The name
                // should not be shown in the IDE in this case.
                vec![Parameter::unnamed(this.db, type_rep)]
            });

            let value = tree.value().solve(self, |this, expr| this.type_expr(expr));

            let scope = self.pop_scope();

            TypeRep::Arrow(ArrowTypeRep::new(
                self.db,
                /* kind       = */ ArrowKind::Pi,
                /* parameters = */ parameters,
                /* value      = */ value,
                /* location   = */ self.range(tree.range()),
                /* scope      = */ scope,
            ))
        }

        pub fn sigma_expr(&mut self, tree: lura_syntax::SigmaExpr) -> TypeRep {
            self.scope = self.scope.fork(ScopeKind::Sigma);

            let parameters = tree
                .parameters(&mut tree.walk())
                .flatten()
                .filter_map(|parameter| parameter.regular())
                .filter_map(|parameter| parameter.parameter())
                .map(|parameter| self.parameter(false, true, parameter))
                .collect::<Vec<_>>();

            let value = tree.value().solve(self, |this, expr| this.type_expr(expr));

            let scope = self.pop_scope();

            TypeRep::Arrow(ArrowTypeRep::new(
                self.db,
                /* kind       = */ ArrowKind::Sigma,
                /* parameters = */ parameters,
                /* value      = */ value,
                /* location   = */ self.range(tree.range()),
                /* scope      = */ scope,
            ))
        }

        pub fn forall_expr(&mut self, tree: lura_syntax::ForallExpr) -> TypeRep {
            self.scope = self.scope.fork(ScopeKind::Sigma);

            let parameters = tree
                .parameters(&mut tree.walk())
                .flatten()
                .filter_map(|parameter| parameter.regular())
                .map(|parameter| self.forall_parameter(parameter))
                .collect::<Vec<_>>();

            let value = tree.value().solve(self, |this, expr| this.type_expr(expr));

            let scope = self.pop_scope();

            TypeRep::Arrow(ArrowTypeRep::new(
                self.db,
                /* kind       = */ ArrowKind::Forall,
                /* parameters = */ parameters,
                /* value      = */ value,
                /* location   = */ self.range(tree.range()),
                /* scope      = */ scope,
            ))
        }

        pub fn match_expr(&mut self, tree: lura_syntax::MatchExpr, level: HirLevel) -> Expr {
            let scrutinee = tree
                .scrutinee()
                .solve(self, |this, node| this.expr(node, level));

            let location = self.range(tree.range());

            let clauses = tree.arms(&mut tree.walk())
                .flatten()
                .filter_map(|node| node.regular())
                .map(|node| {
                    let pattern = node.pattern().solve(self, |this, pattern| this.pattern(pattern));
                    let body = node.body().solve(self, |this, node| {
                        use lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr::*;

                        match node {
                            Block(block) => Expr::block(this.db, this.block(block, level)),
                            _ => this.expr(node.into_node().try_into().unwrap(), level)
                        }
                    });

                    MatchArm {
                        pattern,
                        value: body,
                        location: self.range(node.range()),
                    }
                })
                .collect();

            Expr::Match(MatchExpr::new(
                self.db,
                /* kind      = */ MatchKind::Match,
                /* scrutinee = */ scrutinee,
                /* clauses   = */ clauses,
                /* location  = */ location,
            ))
        }

        pub fn if_expr(&mut self, tree: lura_syntax::IfExpr, level: HirLevel) -> Expr {
            let scrutinee = tree
                .condition()
                .solve(self, |this, node| this.expr(node, level));

            let then = tree.then().solve(self, |this, node| {
                use lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr::*;

                node.child().solve(this, |this, node| match node {
                    Block(block) => Expr::block(this.db, this.block(block, level)),
                    _ => this.expr(node.into_node().try_into().unwrap(), level),
                })
            });

            let otherwise = tree.otherwise().solve(self, |this, node| {
                use lura_syntax::anon_unions::AnnExpr_AppExpr_BinaryExpr_Block_ForallExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr::*;

                node.value().solve(this, |this, node| match node {
                    Block(block) => Expr::block(this.db, this.block(block, level)),
                    _ => this.expr(node.into_node().try_into().unwrap(), level),
                })
            });

            let clauses = vec![
                MatchArm {
                    pattern: Pattern::Literal(Spanned::on_call_site(Literal::TRUE)),
                    location: then.location(self.db),
                    value: then,
                },
                MatchArm {
                    pattern: Pattern::Literal(Spanned::on_call_site(Literal::FALSE)),
                    location: otherwise.location(self.db),
                    value: otherwise,
                },
            ];

            let location = self.range(tree.range());

            Expr::Match(MatchExpr::new(
                self.db,
                /* kind      = */ MatchKind::If,
                /* scrutinee = */ scrutinee,
                /* clauses   = */ clauses,
                /* location  = */ location,
            ))
        }

        pub fn array_expr(&mut self, tree: lura_syntax::ArrayExpr, level: HirLevel) -> Expr {
            let location = self.range(tree.range());

            let items = tree
                .items(&mut tree.walk())
                .map(|item| item.solve(self, |this, node| this.expr(node, level)))
                .collect::<Vec<_>>();

            Expr::Call(CallExpr::new(
                self.db,
                /* kind        = */ CallKind::Prefix,
                /* callee      = */ Callee::Array,
                /* arguments   = */ items,
                /* do_notation = */ None,
                /* location    = */ location,
            ))
        }

        pub fn tuple_expr(&mut self, tree: lura_syntax::TupleExpr, level: HirLevel) -> Expr {
            let location = self.range(tree.range());

            let items = tree
                .children(&mut tree.walk())
                .map(|item| item.solve(self, |this, node| this.expr(node, level)))
                .collect::<Vec<_>>();

            Expr::Call(CallExpr::new(
                self.db,
                /* kind        = */ CallKind::Prefix,
                /* callee      = */ Callee::Tuple,
                /* arguments   = */ items,
                /* do_notation = */ None,
                /* location    = */ location,
            ))
        }

        pub fn return_expr(&mut self, tree: lura_syntax::ReturnExpr, level: HirLevel) -> Expr {
            if !self.scope.is_do_notation_scope() {
                // TODO: report error if it's outside of a do notation
            }

            let location = self.range(tree.range());

            // If it's a return expression, it will return the value of the expression, otherwise it
            // will return a default value.
            let value = tree
                .value()
                .map(|node| node.solve(self, |this, node| this.expr(node, level)))
                .unwrap_or_else(|| Expr::call_unit_expr(location.clone(), self.db));

            Expr::Call(CallExpr::new(
                self.db,
                /* kind        = */ CallKind::Prefix,
                /* callee      = */ Callee::Pure,
                /* arguments   = */ vec![value],
                /* do_notation = */ None,
                /* location    = */ location,
            ))
        }

        pub fn primary(&mut self, tree: lura_syntax::Primary, level: HirLevel) -> Expr {
            use lura_syntax::anon_unions::ArrayExpr_Identifier_IfExpr_Literal_MatchExpr_ReturnExpr_TupleExpr::*;
            use lura_syntax::anon_unions::SimpleIdentifier_SymbolIdentifier::*;

            let location = self.range(tree.range());

            tree.child().solve(self, |this, node| match node {
                // SECTION: primary
                ArrayExpr(array_expr) => this.array_expr(array_expr, level),
                IfExpr(if_expr) => this.if_expr(if_expr, level),
                Literal(literal) => this.literal(literal).upgrade_expr(location, this.db),
                MatchExpr(match_expr) => this.match_expr(match_expr, level),
                ReturnExpr(return_expr) => this.return_expr(return_expr, level),
                TupleExpr(tuple_expr) => this.tuple_expr(tuple_expr, level),

                // SECTION: identifier
                // It will match agains't the identifier, and it will create a new [`Expr::Path`]
                // expression, with the [`Definition`] as the callee, from the given identifier.
                //
                // It will search for the definition in the scope, and if it is not present in the
                // it will query the compiler.
                Identifier(identifier) => identifier.child().solve(this, |this, node| {
                    let source_text = this.src.source_text(this.db).as_bytes();

                    let location = this.range(identifier.range());

                    // Matches agains't node to check if it is a symbol or a simple identifier to
                    // create proper identifier.
                    let identifier = match node {
                        SimpleIdentifier(value) => {
                            let string = value.utf8_text(source_text).ok().unwrap_or_default();

                            self::Identifier::new(this.db, string.into(), false, location.clone())
                        }
                        SymbolIdentifier(value) => {
                            let string = value
                                .child()
                                .with_db(this, |_, node| node.utf8_text(source_text).ok());

                            self::Identifier::new(this.db, string.into(), true, location.clone())
                        }
                    };

                    // Create a new path with the identifier, and search for the definition in the
                    // scope, and if it is not present in the scope, it will invoke a compiler query
                    // to search in the entire package.
                    let path = HirPath::new(this.db, location.clone(), vec![identifier]);

                    let def = match level {
                        HirLevel::Expr => {
                            this.scope
                                .search(this.db, path, DefinitionKind::Function)
                                .unwrap_or_else(|| {
                                    // Queries [`self.db`] for the definition of the operator, and returns it, otherwise it
                                    // will report an error. It's made for doing global lookups, and not local lookups.
                                    find_function(this.db, path)
                                })
                        }
                        HirLevel::Type => {
                            this.scope
                                .search(this.db, path, DefinitionKind::Type)
                                .unwrap_or_else(|| {
                                    // Queries [`self.db`] for the definition of the operator, and returns it, otherwise it
                                    // will report an error. It's made for doing global lookups, and not local lookups.
                                    find_type(this.db, path)
                                })
                        }
                    };

                    // Creates a new [`Reference`] from the [`Definition`] and the location.
                    let reference = this.scope.using(this.db, def, location);

                    // Creates a new [`Expr`] with the [`Definition`] as the callee.
                    Expr::Path(reference)
                }),
            })
        }
    }
}

/// Defines a solver function over the [`LowerHir`] struct, that will solve the clauses, and will
/// return the [`HirSource`] with the declarations and the clauses solved.
#[allow(clippy::type_complexity)]
struct Solver<'a, T> {
    f: Box<dyn FnOnce(&dyn crate::HirDb, &mut LowerHir<'_, '_>) -> T + 'a>,
}

impl<'a, T> Solver<'a, T> {
    /// Creates a new solver function over the [`LowerHir`] struct.
    fn new<F>(f: F) -> Self
    where
        F: FnOnce(&dyn crate::HirDb, &mut LowerHir<'_, '_>) -> T + 'a,
    {
        Self { f: Box::new(f) }
    }

    /// Runs the solver function over the [`LowerHir`] struct, and returns the result.
    fn run_solver(self, lower: &mut LowerHir<'_, '_>) -> T {
        (self.f)(lower.db, lower)
    }
}

trait NodeResultExt<'tree, N, T: Default> {
    fn or_error<F>(self, f: F) -> T
    where
        F: FnOnce(N) -> Option<T>;
}

trait DbNodeResultExt<'tree, N> {
    #[inline]
    fn solve<F, T>(self, db: &mut LowerHir, f: F) -> T
    where
        Self: Sized,
        T: DefaultWithDb,
        F: FnOnce(&mut LowerHir, N) -> T,
    {
        self.with_db(db, |db, node| Some(f(db, node)))
    }

    #[inline]
    fn or_default_error<F>(self, db: &mut LowerHir, f: F)
    where
        Self: Sized,
        F: FnOnce(&mut LowerHir, N),
    {
        self.with_db(db, |db, node| {
            f(db, node);
            Some(())
        })
    }

    fn with_db<F, T>(self, db: &mut LowerHir, f: F) -> T
    where
        T: DefaultWithDb,
        F: FnOnce(&mut LowerHir, N) -> Option<T>;
}

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
    fn with_db<F, T>(self, lower: &mut LowerHir, f: F) -> T
    where
        T: DefaultWithDb,
        F: FnOnce(&mut LowerHir, N) -> Option<T>,
    {
        match self {
            Ok(node) => f(lower, node).unwrap_or_else(|| T::default_with_db(lower.db)),
            Err(err) => {
                let location = lower.range(err.node.range());

                T::incorrect_kind(lower.db, err.node, err.kind, location)
            }
        }
    }
}

impl<'tree, N> DbNodeResultExt<'tree, N> for Result<ExtraOr<'tree, N>, IncorrectKind<'tree>> {
    #[inline]
    fn with_db<F, T>(self, lower: &mut LowerHir, f: F) -> T
    where
        T: DefaultWithDb,
        F: FnOnce(&mut LowerHir, N) -> Option<T>,
    {
        match self {
            Ok(ExtraOr::Regular(node)) => {
                f(lower, node).unwrap_or_else(|| T::default_with_db(lower.db))
            }
            Ok(ExtraOr::Extra(extra)) => {
                let location = lower.range(extra.range());

                T::extra_data(lower.db, extra, location)
            }
            Err(err) => {
                let location = lower.range(err.node.range());

                T::incorrect_kind(lower.db, err.node, err.kind, location)
            }
        }
    }
}
