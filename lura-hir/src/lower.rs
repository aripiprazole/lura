use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use salsa::Cycle;
use tree_sitter::{Node, Tree};
use type_sitter_lib::{ExtraOr, IncorrectKind, NodeResult, OptionNodeResultExt, TypedNode};

use lura_syntax::{
    anon_unions::ExplicitArguments_ImplicitArguments, generated::lura::SourceFile, Source,
    TreeDecl, TreeIdentifier,
};

use crate::{
    package::Package,
    resolve::{Definition, DefinitionKind},
    scope::{Scope, ScopeKind},
    source::{
        declaration::{Attribute, DocString, Parameter, Vis},
        expr::Expr,
        top_level::{BindingGroup, Signature, TopLevel, Using},
        type_rep::TypeRep,
        DefaultWithDb, HirPath, HirSource, Identifier, Location, OptionExt, Spanned,
    },
};

#[salsa::tracked(recovery_fn = rec_hir_lower)]
pub fn hir_declare(db: &dyn crate::HirDb, pkg: Package, src: Source) -> HirSource {
    let parse_tree = src.syntax_node(db);

    let lower = LowerHir {
        db,
        src,
        pkg,
        decls: vec![],
        scope: Scope::new(ScopeKind::File),
        tree: parse_tree.tree.clone(),
        root_node: parse_tree.tree.root_node(),
        clauses: HashMap::new(),
    };

    lower.declare()
}

#[salsa::tracked(recovery_fn = rec_hir_lower)]
pub fn hir_lower(db: &dyn crate::HirDb, pkg: Package, src: Source) -> HirSource {
    let parse_tree = src.syntax_node(db);

    let lower = LowerHir {
        db,
        src,
        pkg,
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

struct LowerHir<'db, 'tree> {
    db: &'db dyn crate::HirDb,
    src: Source,
    tree: Arc<Tree>,
    decls: Vec<TopLevel>,
    pkg: Package,
    scope: Scope,
    root_node: Node<'tree>,
    clauses: HashMap<HirPath, BindingGroup>,
}

impl<'db, 'tree> LowerHir<'db, 'tree> {
    pub fn declare(mut self) -> HirSource {
        let ast = SourceFile::try_from(self.root_node).unwrap();

        for node in ast.decls(&mut self.tree.clone().walk()).flatten() {
            if let ExtraOr::Regular(node) = node {
                // Process declaration only if it is not an error, or it's not a junk
                // declaration.
                self.define(node);
            }
        }

        for group in self.clauses.values() {
            self.decls.push(TopLevel::BindingGroup(*group))
        }

        HirSource::new(self.db, self.src, self.pkg, self.scope, self.decls)
    }

    pub fn declare_and_solve(mut self) -> HirSource {
        let ast = SourceFile::try_from(self.root_node).unwrap();

        for node in ast.decls(&mut self.tree.clone().walk()).flatten() {
            if let ExtraOr::Regular(node) = node {
                // Process declaration only if it is not an error, or it's not a junk
                // declaration.
                if let Some(solver) = self.define(node) {
                    let decl = solver.run_solver(&mut self);
                    self.decls.push(decl);
                };
            }
        }

        for group in self.clauses.values() {
            self.decls.push(TopLevel::BindingGroup(*group))
        }

        HirSource::new(self.db, self.src, self.pkg, self.scope, self.decls)
    }

    pub fn define<'a>(&mut self, decl: TreeDecl<'a>) -> Option<Solver<'a, TopLevel>> {
        let value = match decl {
            TreeDecl::ClassDecl(_) => todo!(),
            TreeDecl::Clause(_) => todo!(),
            TreeDecl::Command(_) => todo!(),
            TreeDecl::DataDecl(_) => todo!(),
            TreeDecl::Signature(signature) => return Some(self.hir_signature(signature)),
            TreeDecl::TraitDecl(_) => todo!(),
            TreeDecl::Using(decl) => {
                let range = self.range(decl.range());
                let path = decl.path().solve(self.db, |node| self.path(node));

                TopLevel::Using(Using::new(self.db, self.qualify(path), range))
            }
        };

        self.decls.push(value);
        None // not solving
    }

    pub fn hir_signature<'a>(&mut self, tree: lura_syntax::Signature<'a>) -> Solver<'a, TopLevel> {
        let range = self.range(tree.range());
        let path = tree.name().solve(self.db, |node| self.path(node));

        let attrs = self.hir_attributes(tree.attributes(&mut tree.walk()));
        let docs = self.hir_docs(tree.doc_strings(&mut tree.walk()));

        let vis = tree
            .visibility()
            .map(|vis| vis.solve(self.db, |node| self.hir_visibility(node)))
            .unwrap_or(Spanned::on_call_site(Vis::Public));

        // TODO: define the node on the scope
        let node = Definition::no(self.db, DefinitionKind::Function, path);

        Solver::new(move |db, this| {
            let parameters = this.parameters(tree.arguments(&mut tree.walk()));

            let type_rep = tree
                .clause_type()
                .flatten()
                .map(|node| this.clause_type(node))
                .unwrap_or_default_with_db(db);

            let signature =
                Signature::new(this.db, attrs, docs, vis, node, parameters, type_rep, range);

            let clause = this
                .clauses
                .entry(path)
                .or_insert_with(|| BindingGroup::new(db, signature, HashSet::new()));

            // TODO: add the current body to the clause, and solve it
            let _ = clause;

            // It's not needed to solve the clause, because it is already solved in the next steps.
            //
            // The entire next step, is getting the clauses from the scope, and transforms into
            // declarations, so it is not needed to solve the clause here.
            TopLevel::Empty
        })
    }

    /// Takes an list of syntatic arguments, and returns a list of parameters, handled if it is
    /// either implicit or explicit, and if it is a named or unnamed parameter.
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
                        .map(|parameter| self.parameter(parameter))
                        .collect::<Vec<_>>(),

                    ImplicitArguments(implicits) => implicits
                        .parameters(&mut implicits.walk())
                        .flatten()
                        .filter_map(|node| node.regular())
                        .map(|parameter| self.parameter(parameter))
                        .collect::<Vec<_>>(),
                }
            })
            .collect::<Vec<_>>()
    }

    pub fn clause_type(&mut self, clause: lura_syntax::ClauseType) -> TypeRep {
        use lura_syntax::anon_unions::AnnExpr_BinaryExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr::*;

        clause.clause_type().solve(self.db, |node| match node {
            // SECTION: type_expr
            //
            // Upgrades the expressions to type level expressions, to be easier to handle errors
            // in the type system, and still keeps the diagnostics in the IDE.
            Primary(primary) => self.primary(primary).upgrade(self.db),
            AnnExpr(ann_expr) => self.ann_expr(ann_expr).upgrade(self.db),
            LamExpr(lam_expr) => self.lam_expr(lam_expr).upgrade(self.db),
            MatchExpr(match_expr) => self.match_expr(match_expr).upgrade(self.db),
            BinaryExpr(binary_expr) => self.binary_expr(binary_expr).upgrade(self.db),

            // Type level expressions
            PiExpr(pi) => self.pi_expr(pi),
            SigmaExpr(sigma) => self.sigma_expr(sigma),
            TypeAppExpr(type_app) => self.type_app_expr(type_app),
        })
    }

    pub fn type_expr(&mut self, tree: lura_syntax::TreeTypeRep) -> TypeRep {
        use lura_syntax::anon_unions::AnnExpr_BinaryExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr::*;

        match tree {
            // SECTION: type_expr
            //
            // Upgrades the expressions to type level expressions, to be easier to handle errors
            // in the type system, and still keeps the diagnostics in the IDE.
            Primary(primary) => self.primary(primary).upgrade(self.db),
            AnnExpr(ann_expr) => self.ann_expr(ann_expr).upgrade(self.db),
            LamExpr(lam_expr) => self.lam_expr(lam_expr).upgrade(self.db),
            MatchExpr(match_expr) => self.match_expr(match_expr).upgrade(self.db),
            BinaryExpr(binary_expr) => self.binary_expr(binary_expr).upgrade(self.db),

            // Type level expressions
            PiExpr(pi) => self.pi_expr(pi),
            SigmaExpr(sigma) => self.sigma_expr(sigma),
            TypeAppExpr(type_app) => self.type_app_expr(type_app),
        }
    }

    pub fn ann_expr(&mut self, tree: lura_syntax::AnnExpr) -> Expr {
        todo!()
    }

    pub fn binary_expr(&mut self, tree: lura_syntax::BinaryExpr) -> Expr {
        todo!()
    }

    pub fn lam_expr(&mut self, tree: lura_syntax::LamExpr) -> Expr {
        todo!()
    }

    pub fn match_expr(&mut self, tree: lura_syntax::MatchExpr) -> Expr {
        todo!()
    }

    pub fn type_app_expr(&mut self, tree: lura_syntax::TypeAppExpr) -> TypeRep {
        todo!()
    }

    pub fn pi_expr(&mut self, tree: lura_syntax::PiExpr) -> TypeRep {
        use lura_syntax::anon_unions::AnnExpr_BinaryExpr_LamExpr_MatchExpr_PiExpr_PiNamedParameterSet_Primary_SigmaExpr_TypeAppExpr::*;

        let parameters = tree.parameter().solve(self.db, |node| {
            let type_rep = match node {
                Primary(primary) => self.primary(primary).upgrade(self.db),
                AnnExpr(ann_expr) => self.ann_expr(ann_expr).upgrade(self.db),
                LamExpr(lam_expr) => self.lam_expr(lam_expr).upgrade(self.db),
                MatchExpr(match_expr) => self.match_expr(match_expr).upgrade(self.db),
                BinaryExpr(binary_expr) => self.binary_expr(binary_expr).upgrade(self.db),

                // Type level expressions
                PiExpr(pi) => self.pi_expr(pi),
                SigmaExpr(sigma) => self.sigma_expr(sigma),
                TypeAppExpr(type_app) => self.type_app_expr(type_app),
                PiNamedParameterSet(tree) => {
                    return tree
                        .parameters(&mut tree.walk())
                        .flatten()
                        .filter_map(|node| node.regular())
                        .filter_map(|node| node.parameter())
                        .map(|parameter| self.parameter(parameter))
                        .collect::<Vec<_>>();
                }
            };

            // This handles the case where the parameter is unnamed, and only haves a type. The name
            // should not be shown in the IDE in this case.
            vec![Parameter::unnamed(self.db, type_rep)]
        });

        TypeRep::Pi {
            parameters,
            value: Box::new(tree.value().solve(self.db, |node| self.type_expr(node))),
            location: self.range(tree.range()),
        }
    }

    pub fn primary(&mut self, tree: lura_syntax::Primary) -> Expr {
        todo!()
    }

    pub fn parameter(&mut self, tree: lura_syntax::Parameter) -> Parameter {
        todo!()
    }

    pub fn sigma_expr(&mut self, tree: lura_syntax::SigmaExpr) -> TypeRep {
        TypeRep::Sigma {
            parameters: vec![],
            value: Box::new(tree.value().solve(self.db, |node| self.type_expr(node))),
            location: self.range(tree.range()),
        }
    }

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

    pub fn hir_attributes<'a, I>(&mut self, attributes: I) -> HashSet<Attribute>
    where
        I: Iterator<Item = NodeResult<'a, ExtraOr<'a, lura_syntax::Attribute<'a>>>>,
    {
        attributes
            .flatten()
            .filter_map(|attribute| {
                let value = attribute.regular()?;
                let name = value.name().solve(self.db, |path| self.path(path));
                let arguments = vec![];
                let range = self.range(attribute.range());

                Some(Attribute::new(self.db, name, arguments, range))
            })
            .collect()
    }

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

    pub fn path(&mut self, path: lura_syntax::Path) -> HirPath {
        let mut new_segments = vec![];
        let source_text = self.src.source_text(self.db).as_bytes();

        let range = self.range(path.range());
        for segment in path.segments(&mut self.tree.walk()) {
            segment.or_default_error(self.db, |segment: lura_syntax::Identifier| {
                let range = self.range(segment.range());

                let identifer = match segment.child() {
                    Ok(name) => name,
                    Err(_) => return,
                };

                new_segments.push(match identifer {
                    TreeIdentifier::SimpleIdentifier(value) => {
                        let string = value.utf8_text(source_text).ok().unwrap_or_default();

                        Identifier::new(self.db, string.into(), false, range)
                    }
                    TreeIdentifier::SymbolIdentifier(value) => {
                        let string = value
                            .child()
                            .with_db(self.db, |_, node| node.utf8_text(source_text).ok());

                        Identifier::new(self.db, string.into(), true, range)
                    }
                });
            });
        }

        HirPath::new(self.db, range, new_segments)
    }

    pub fn qualify(&self, path: HirPath) -> Definition {
        Definition::no(self.db, DefinitionKind::Unresolved, path)
    }

    pub fn range(&self, range: tree_sitter::Range) -> Location {
        Location::new(self.db, self.src, range.start_byte, range.end_byte)
    }
}

pub struct Solver<'a, T> {
    f: Box<dyn FnOnce(&dyn crate::HirDb, &mut LowerHir<'_, '_>) -> T + 'a>,
}

impl<'a, T> Solver<'a, T> {
    fn new<F>(f: F) -> Self
    where
        F: FnOnce(&dyn crate::HirDb, &mut LowerHir<'_, '_>) -> T + 'a,
    {
        let a = Box::new(f);

        Self { f: a }
    }

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
    fn solve<F, T>(self, db: &dyn crate::HirDb, f: F) -> T
    where
        Self: Sized,
        T: DefaultWithDb,
        F: FnOnce(N) -> T,
    {
        self.with_db(db, |_, node| Some(f(node)))
    }

    fn or_default_error<F>(self, db: &dyn crate::HirDb, f: F)
    where
        Self: Sized,
        F: FnOnce(N),
    {
        self.with_db(db, |db, node| {
            f(node);
            Some(())
        })
    }

    fn with_db<F, T>(self, db: &dyn crate::HirDb, f: F) -> T
    where
        T: DefaultWithDb,
        F: FnOnce(&dyn crate::HirDb, N) -> Option<T>;
}

impl<'tree, N, T: Default> NodeResultExt<'tree, N, T> for Result<N, IncorrectKind<'tree>> {
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
    fn with_db<F, T>(self, db: &dyn crate::HirDb, f: F) -> T
    where
        T: DefaultWithDb,
        F: FnOnce(&dyn crate::HirDb, N) -> Option<T>,
    {
        match self {
            Ok(node) => f(db, node).unwrap_or(T::default_with_db(db)),
            Err(..) => T::default_with_db(db),
        }
    }
}

impl<'tree, N> DbNodeResultExt<'tree, N> for Result<ExtraOr<'tree, N>, IncorrectKind<'tree>> {
    fn with_db<F, T>(self, db: &dyn crate::HirDb, f: F) -> T
    where
        T: DefaultWithDb,
        F: FnOnce(&dyn crate::HirDb, N) -> Option<T>,
    {
        match self {
            Ok(ExtraOr::Extra(..)) => T::default_with_db(db),
            Ok(ExtraOr::Regular(node)) => f(db, node).unwrap_or(T::default_with_db(db)),
            Err(..) => T::default_with_db(db),
        }
    }
}
