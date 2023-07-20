use std::{cell::RefCell, collections::HashMap, sync::Arc};

use if_chain::if_chain;
use lura_syntax::{generated::lura::SourceFile, Source};
use salsa::Cycle;
use tree_sitter::{Node, Tree};
use type_sitter_lib::{ExtraOr, IncorrectKind, TypedNode};

use crate::{
    package::Package,
    scope::{Scope, ScopeKind},
    source::{
        top_level::{BindingGroup, TopLevel, Using},
        HirError, HirPath, HirSource, HirSourceId, Identifier, Location, Offset, TextRange,
    },
};

#[salsa::tracked(recovery_fn = rec_hir_lower)]
pub fn hir_lower(db: &dyn crate::HirDb, pkg: Package, src: Source) -> HirSource {
    let id = HirSourceId::new(db, src.file_path(db).clone());
    let parse_tree = src.syntax_node(db);

    let mut lower = LowerHir {
        db,
        source: src,
        package: pkg,
        tree: parse_tree.tree.clone(),
        root_node: parse_tree.tree.root_node(),
        clauses: HashMap::new(),
        source_id: id,
    };

    lower.hir_source()
}

pub fn rec_hir_lower(db: &dyn crate::HirDb, cycle: &Cycle, pkg: Package, src: Source) -> HirSource {
    todo!()
}

struct LowerHir<'db, 'tree> {
    db: &'db dyn crate::HirDb,
    source: Source,
    tree: Arc<Tree>,
    package: Package,
    root_node: Node<'tree>,
    clauses: HashMap<HirPath, BindingGroup>,
    source_id: HirSourceId,
}

#[rustfmt::skip]
mod concrete {
    pub type Decl<'tree> = lura_syntax::anon_unions::ClassDecl_Clause_Command_DataDecl_Signature_TraitDecl_Using<'tree>;
    pub type Identifier<'tree> = lura_syntax::anon_unions::SimpleIdentifier_SymbolIdentifier<'tree>;
}

impl<'db, 'tree> LowerHir<'db, 'tree> {
    pub fn hir_source(&mut self) -> HirSource {
        let ast = SourceFile::try_from(self.root_node).unwrap();

        let scope = Scope::new(ScopeKind::File);
        let mut decls: Vec<_> = vec![];
        for node in ast.decls(&mut self.tree.clone().walk()) {
            let node: TopLevel = if_chain! {
                if let Ok(node) = node;
                if let ExtraOr::Regular(node) = node;
                then {
                    // Process declaration only if it is not an error, or it's not a junk
                    // declaration.
                    self.hir_decl(node)
                }
                else {
                    let range = TextRange {
                        start: Offset(0),
                        end: Offset(0),
                    };
                    TopLevel::Error(HirError::new(self.db, range))
                }
            };

            decls.push(node);
        }

        let location = Location::start();

        HirSource::new(
            self.db,
            self.source_id,
            self.source,
            location,
            self.package,
            scope,
            decls,
        )
    }

    pub fn hir_decl(&mut self, decl: concrete::Decl) -> TopLevel {
        match decl {
            concrete::Decl::ClassDecl(_) => todo!(),
            concrete::Decl::Clause(_) => todo!(),
            concrete::Decl::Command(_) => todo!(),
            concrete::Decl::DataDecl(_) => todo!(),
            concrete::Decl::Signature(_) => todo!(),
            concrete::Decl::TraitDecl(_) => todo!(),
            concrete::Decl::Using(decl) => {
                let path = decl.path().unwrap_db(self.db);
                let range = self.hir_range(decl.range());
                let hir_path = self.hir_path(path);

                TopLevel::Using(Using::new(self.db, hir_path, range))
            }
        }
    }

    pub fn hir_path(&mut self, path: lura_syntax::Path<'_>) -> HirPath {
        let mut new_segments = vec![];

        let range = self.hir_range(path.range());
        for segment in path.segmentss(&mut self.tree.walk()) {
            let segment: lura_syntax::Identifier<'_> = segment.unwrap_db(self.db);
            let identifer = segment.child().unwrap_db(self.db);
            let refers_symbol = matches!(identifer, concrete::Identifier::SymbolIdentifier(_));
            let result = match identifer {
                concrete::Identifier::SimpleIdentifier(identifier) => {
                    identifier.utf8_text(self.source.source_text(self.db).as_bytes())
                }
                concrete::Identifier::SymbolIdentifier(identifier) => {
                    identifier.utf8_text(self.source.source_text(self.db).as_bytes())
                }
            };
            let range = self.hir_range(segment.range());

            match result {
                Ok(value) => {
                    let value = value.into();
                    let hir_identifier = Identifier::new(self.db, value, refers_symbol, range);
                    new_segments.push(hir_identifier)
                }
                Err(_) => {
                    println!("todo")
                    // push error
                }
            }
        }

        HirPath::new(self.db, range, new_segments)
    }

    pub fn hir_range(&self, range: tree_sitter::Range) -> TextRange {
        TextRange {
            start: Offset(range.start_byte),
            end: Offset(range.end_byte),
        }
    }
}

pub trait DbNodeResult<'tree, T> {
    fn unwrap_db(self, db: &dyn crate::HirDb) -> T;
}

impl<'tree, T> DbNodeResult<'tree, T> for Result<T, IncorrectKind<'tree>> {
    fn unwrap_db(self, db: &dyn crate::HirDb) -> T {
        todo!()
    }
}

impl<'tree, T> DbNodeResult<'tree, T> for Result<ExtraOr<'tree, T>, IncorrectKind<'tree>> {
    fn unwrap_db(self, db: &dyn crate::HirDb) -> T {
        todo!()
    }
}
