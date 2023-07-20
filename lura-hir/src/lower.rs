use std::sync::Arc;

use if_chain::if_chain;
use lura_syntax::{generated::lura::SourceFile, Source};
use salsa::Cycle;
use tree_sitter::{Node, Tree};
use type_sitter_lib::ExtraOr;

use crate::{
    package::Package,
    scope::{Scope, ScopeKind},
    source::{top_level::TopLevel, HirError, HirSource, HirSourceId, Location, Offset, TextRange},
};

#[salsa::tracked(recovery_fn = rec_hir_lower)]
pub fn hir_lower(db: &dyn crate::HirDb, pkg: Package, src: Source) -> HirSource {
    let id = HirSourceId::new(db, src.file_path(db).clone());
    let parse_tree = src.syntax_node(db);

    let lower = LowerHir {
        db,
        source: src,
        package: pkg,
        tree: parse_tree.tree.clone(),
        root_node: parse_tree.tree.root_node(),
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
    source_id: HirSourceId,
}

#[rustfmt::skip]
mod concrete {
    pub type Decl<'tree> = lura_syntax::anon_unions::ClassDecl_Clause_Command_DataDecl_Signature_TraitDecl_Using<'tree>;
}

impl<'db, 'tree> LowerHir<'db, 'tree> {
    pub fn hir_source(&self) -> HirSource {
        let ast = SourceFile::try_from(self.root_node).unwrap();

        let scope = Scope::new(ScopeKind::File);
        let decls: Vec<_> = ast
            .decls(&mut self.tree.walk())
            .map(|node| {
                if_chain! {
                    if let Ok(node) = node;
                    if let ExtraOr::Regular(node) = node;
                    then {
                        // Process declaration only if it is not an error, or it's not a junk
                        // declaration.
                        return self.hir_decl(node);
                    }
                }

                let range = TextRange {
                    start: Offset(0),
                    end: Offset(0),
                };

                TopLevel::Error(HirError::new(self.db, range))
            })
            .collect();

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

    pub fn hir_decl(&self, decl: concrete::Decl) -> TopLevel {
        match decl {
            concrete::Decl::ClassDecl(_) => todo!(),
            concrete::Decl::Clause(_) => todo!(),
            concrete::Decl::Command(_) => todo!(),
            concrete::Decl::DataDecl(_) => todo!(),
            concrete::Decl::Signature(_) => todo!(),
            concrete::Decl::TraitDecl(_) => todo!(),
            concrete::Decl::Using(_) => todo!(),
        }
    }
}
