#![feature(trait_upcasting)]
#[allow(clippy::all)]
#[allow(unused_variables)]
#[allow(dead_code)]
#[allow(non_snake_case)]
#[allow(unused_macros)]
#[allow(unused_parens)]
#[allow(unused_labels)]
#[allow(non_upper_case_globals)]
#[rustfmt::skip]
pub mod generated {
    pub mod lura;

    pub type TreeTypeRep<'tree> = lura::anon_unions::AnnExpr_BinaryExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr_TypeAppExpr<'tree>;
    pub type TreeDecl<'tree> = lura::anon_unions::ClassDecl_Clause_Command_DataDecl_Signature_TraitDecl_Using<'tree>;
    pub type TreeArguments<'tree> = lura::anon_unions::ExplicitArguments_ImplicitArguments<'tree>;
    pub type TreeExpr<'tree> = lura::anon_unions::AnnExpr_AppExpr_BinaryExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<'tree>;
    pub type TreeTypeExpr<'tree> = lura::anon_unions::AnnExpr_AppExpr_BinaryExpr_LamExpr_MatchExpr_PiExpr_Primary_SigmaExpr<'tree>;
    pub type TreeIdentifier<'tree> = lura::anon_unions::SimpleIdentifier_SymbolIdentifier<'tree>;
}

pub use generated::lura::*;
pub use generated::*;

use std::{hash::Hash, sync::Arc};

use lura_diagnostic::DiagnosticDb;
use lura_vfs::{SourceFile, VfsDb};
use tree_sitter::{Parser, Tree};

extern crate salsa_2022 as salsa;

#[salsa::jar(db = ParseDb)]
pub struct Jar(crate::Source, crate::parse);

pub trait ParseDb: VfsDb + DiagnosticDb + salsa::DbWithJar<Jar> {}

impl<DB> ParseDb for DB where DB: ?Sized + VfsDb + DiagnosticDb + salsa::DbWithJar<Jar> {}

#[salsa::interned]
pub struct Source {
    #[return_ref]
    pub file_path: std::path::PathBuf,

    #[return_ref]
    pub source_text: std::string::String,

    #[return_ref]
    pub syntax_node: ParseTree,
}

#[salsa::tracked]
pub fn parse(db: &dyn ParseDb, program: SourceFile) -> Source {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_lura::language())
        .expect("Error loading lura language");

    let text = program.source_text(db).clone();

    let tree = ParseTree {
        tree: parser.parse(&text, None).unwrap().into(),
    };

    Source::new(db, program.file_path(db).clone(), text, tree)
}

#[derive(Clone, Debug)]
pub struct ParseTree {
    pub tree: Arc<Tree>,
}

impl Eq for ParseTree {}

impl PartialEq for ParseTree {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.tree, &other.tree)
    }
}

impl Hash for ParseTree {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.tree.root_node().hash(state);
    }
}

#[cfg(test)]
mod tests {
    use tree_sitter::Parser;

    #[test]
    fn test() {
        let mut parser = Parser::new();
        parser
            .set_language(tree_sitter_lura::language())
            .expect("Error loading lura language");

        let _tree = parser
            .parse("Main { IO.println \"Hello, world\" }", None)
            .unwrap();
    }
}
