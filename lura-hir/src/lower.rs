use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use if_chain::if_chain;
use salsa::Cycle;
use tree_sitter::{Node, Tree};
use type_sitter_lib::{ExtraOr, IncorrectKind, NodeResult, TypedNode};

use lura_syntax::{generated::lura::SourceFile, Source, TreeDecl, TreeIdentifier};

use crate::{
    package::Package,
    resolve::{Definition, DefinitionKind},
    scope::{Scope, ScopeKind},
    source::{
        declaration::{Attribute, DocString, Visibility},
        top_level::{BindingGroup, Signature, TopLevel, Using},
        type_rep::TypeRep,
        DefaultWithDb, HirError, HirPath, HirSource, Identifier, Location, Spanned,
    },
};

#[salsa::tracked(recovery_fn = rec_hir_lower)]
pub fn hir_lower(db: &dyn crate::HirDb, pkg: Package, src: Source) -> HirSource {
    let parse_tree = src.syntax_node(db);

    let mut lower = LowerHir {
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
    pub fn declare_and_solve(mut self) -> HirSource {
        let ast = SourceFile::try_from(self.root_node).unwrap();

        let scope = Scope::new(ScopeKind::File);
        for node in ast.decls(&mut self.tree.clone().walk()).flatten() {
            if let ExtraOr::Regular(node) = node {
                // Process declaration only if it is not an error, or it's not a junk
                // declaration.
                if let Some(solver) = self.define(node) {
                    solver.run_solver(&mut self);
                };
            }
        }

        for group in self.clauses.values() {
            self.decls.push(TopLevel::BindingGroup(*group))
        }

        HirSource::new(self.db, self.src, self.pkg, scope, self.decls)
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
            .unwrap_or(Spanned::on_call_site(Visibility::Public));

        // TODO: define the node on the scope
        let node = Definition::no(self.db, DefinitionKind::Function, path);

        Solver::new(move |this| {
            let parameters = vec![];

            let type_rep = TypeRep::Unit;

            let signature =
                Signature::new(this.db, attrs, docs, vis, node, parameters, type_rep, range);

            let clause = this
                .clauses
                .entry(path)
                .or_insert_with(|| BindingGroup::new(this.db, signature, HashSet::new()));

            todo!()
        })
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

    pub fn hir_visibility(&mut self, visibility: lura_syntax::Visibility) -> Spanned<Visibility> {
        let value = match visibility.utf8_text(self.src.source_text(self.db).as_bytes()) {
            Ok("public") => Visibility::Public,
            Ok("sealed") => Visibility::Sealed,
            Ok("private") => Visibility::Private,
            Ok("internal") => Visibility::Internal,
            _ => Visibility::Public,
        };
        let location = self.range(visibility.range());
        Spanned::new(value, location)
    }

    pub fn path(&mut self, path: lura_syntax::Path<'_>) -> HirPath {
        let mut new_segments = vec![];
        let source_text = self.src.source_text(self.db).as_bytes();

        let range = self.range(path.range());
        for segment in path.segments(&mut self.tree.walk()) {
            segment.or_default_error(self.db, |_, segment: lura_syntax::Identifier| {
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

    pub fn range(&self, range: tree_sitter::Range) -> Location {
        Location::new(self.db, self.src, range.start_byte, range.end_byte)
    }

    pub fn qualify(&self, path: HirPath) -> Definition {
        Definition::no(self.db, DefinitionKind::Unresolved, path)
    }
}

pub struct Solver<'a, T> {
    f: Box<dyn FnOnce(&mut LowerHir<'_, '_>) -> T + 'a>,
}

impl<'a, T> Solver<'a, T> {
    fn new<F>(f: F) -> Self
    where
        F: FnOnce(&mut LowerHir<'_, '_>) -> T + 'a,
    {
        let a = Box::new(f);

        Self { f: a }
    }

    fn run_solver(mut self, lower: &mut LowerHir<'_, '_>) -> T {
        (self.f)(lower)
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
        F: FnOnce(&dyn crate::HirDb, N),
    {
        self.with_db(db, |db, node| {
            f(db, node);
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
