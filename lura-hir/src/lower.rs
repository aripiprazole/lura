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
        tree: parse_tree.tree.clone(),
        root_node: parse_tree.tree.root_node(),
        clauses: HashMap::new(),
    };

    lower.hir_source()
}

pub fn rec_hir_lower(db: &dyn crate::HirDb, cycle: &Cycle, _: Package, _: Source) -> HirSource {
    panic!("Cyclic dependency between queries: {:#?}", cycle.debug(db))
}

struct LowerHir<'db, 'tree> {
    db: &'db dyn crate::HirDb,
    src: Source,
    tree: Arc<Tree>,
    pkg: Package,
    root_node: Node<'tree>,
    clauses: HashMap<HirPath, BindingGroup>,
}

impl<'db, 'tree> LowerHir<'db, 'tree> {
    pub fn hir_source(&mut self) -> HirSource {
        let ast = SourceFile::try_from(self.root_node).unwrap();

        let scope = Scope::new(ScopeKind::File);
        let mut decls: Vec<_> = vec![];
        for node in ast.decls(&mut self.tree.clone().walk()).flatten() {
            if let ExtraOr::Regular(node) = node {
                // Process declaration only if it is not an error, or it's not a junk
                // declaration.
                if let Some(decl) = self.hir_decl(node) {
                    decls.push(decl);
                }
            }
        }

        for group in self.clauses.values() {
            decls.push(TopLevel::BindingGroup(*group))
        }

        HirSource::new(self.db, self.src, self.pkg, scope, decls)
    }

    pub fn hir_decl(&mut self, decl: lura_syntax::TreeDecl) -> Option<TopLevel> {
        Some(match decl {
            TreeDecl::ClassDecl(_) => todo!(),
            TreeDecl::Clause(_) => todo!(),
            TreeDecl::Command(_) => todo!(),
            TreeDecl::DataDecl(_) => todo!(),
            TreeDecl::Signature(signature) => return self.hir_signature(signature),
            TreeDecl::TraitDecl(_) => todo!(),
            TreeDecl::Using(decl) => {
                let range = self.range(decl.range());
                let path = self.path(decl.path().unwrap_on(self.db));

                TopLevel::Using(Using::new(self.db, self.qualify(path), range))
            }
        })
    }

    pub fn hir_signature(&mut self, signature: lura_syntax::Signature) -> Option<TopLevel> {
        let range = self.range(signature.range());
        let path = self.path(signature.name().unwrap_on(self.db));

        let attributes = self.hir_attributes(signature.attributes(&mut signature.walk()));
        let docs = self.hir_docs(signature.doc_strings(&mut signature.walk()));
        let definition = Definition::no(self.db, DefinitionKind::Function, path);
        let visibility = signature
            .visibility()
            .map(|vis| self.hir_visibility(vis.unwrap_on(self.db)))
            .unwrap_or(Spanned::on_call_site(Visibility::Public));

        let signature = Signature::new(
            /* db          = */ self.db,
            /* attributes  = */ attributes,
            /* docs        = */ docs,
            /* visibility  = */ visibility,
            /* name        = */ definition,
            /* parameters  = */ vec![],
            /* return_type = */ TypeRep::Unit,
            /* location    = */ range,
        );

        let clause = self
            .clauses
            .entry(path)
            .or_insert_with(|| BindingGroup::new(self.db, signature, HashSet::new()));

        None
    }

    pub fn hir_docs<'a, I>(&mut self, attributes: I) -> Vec<DocString>
    where
        I: Iterator<Item = NodeResult<'a, ExtraOr<'a, lura_syntax::DocString<'a>>>>,
    {
        attributes
            .filter_map(|attribute| {
                if let ExtraOr::Regular(attribute) = attribute.unwrap_on(self.db) {
                    Some(DocString::new(self.db, self.range(attribute.range())))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn hir_attributes<'a, I>(&mut self, attributes: I) -> HashSet<Attribute>
    where
        I: Iterator<Item = NodeResult<'a, ExtraOr<'a, lura_syntax::Attribute<'a>>>>,
    {
        attributes
            .filter_map(|attribute| {
                if let ExtraOr::Regular(attribute) = attribute.unwrap_on(self.db) {
                    Some(self.hir_attribute(attribute))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn hir_attribute(&mut self, attribute: lura_syntax::Attribute) -> Attribute {
        let name = self.path(attribute.name().unwrap_on(self.db));
        let arguments = vec![];

        Attribute::new(self.db, name, arguments, self.range(attribute.range()))
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
            segment.or_default_error(self.db, |db, segment: lura_syntax::Identifier| {
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
                            .or_db_error(self.db, |db, node| node.utf8_text(source_text).ok());

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

trait NodeResultExt<'tree, N, T: Default> {
    fn or_error<F>(self, f: F) -> T
    where
        F: FnOnce(N) -> Option<T>;
}

trait DbNodeResultExt<'tree, N> {
    fn or_default_error<F>(self, db: &dyn crate::HirDb, f: F)
    where
        Self: Sized,
        F: FnOnce(&dyn crate::HirDb, N),
    {
        self.or_db_error(db, |db, node| {
            f(db, node);
            Some(())
        })
    }

    fn or_db_error<F, T>(self, db: &dyn crate::HirDb, f: F) -> T
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
    fn or_db_error<F, T>(self, db: &dyn crate::HirDb, f: F) -> T
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
    fn or_db_error<F, T>(self, db: &dyn crate::HirDb, f: F) -> T
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
