use std::sync::Arc;

use if_chain::if_chain;
use lura_syntax::{generated::lura::SourceFile, Source};
use tree_sitter::{Node, Tree};
use type_sitter_lib::ExtraOr;

use crate::source::{top_level::TopLevel, HirSource, HirSourceId, Location};

#[salsa::tracked]
pub fn hir_lower(db: &dyn crate::HirDb, source: Source) -> HirSource {
    let id = HirSourceId::new(db, source.file_path(db).clone());
    let parse_tree = source.syntax_node(db);

    let lower = LowerHir {
        db,
        source,
        tree: parse_tree.tree.clone(),
        root_node: parse_tree.tree.root_node(),
        source_id: id,
    };

    lower.hir_source()
}

struct LowerHir<'db, 'tree> {
    db: &'db dyn crate::HirDb,
    source: Source,
    tree: Arc<Tree>,
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

                TopLevel::Error
            })
            .collect();

        let location = Location::start();
        let package = todo!();

        HirSource::new(
            self.db,
            self.source_id,
            self.source,
            location,
            package,
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
