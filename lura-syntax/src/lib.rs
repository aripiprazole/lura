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
}

/// Contains error handling stuff for tree-sitter. This is a separate module because it is
/// generated by tree-sitter.
pub mod error_handling;

pub use generated::lura::*;
pub use generated::*;

use std::{hash::Hash, ops::Deref, sync::Arc};

use lura_diagnostic::DiagnosticDb;
use lura_vfs::VfsDb;
use tree_sitter::{Parser, Tree};

extern crate salsa_2022 as salsa;

#[salsa::jar(db = ParseDb)]
pub struct Jar(
    Source,
    parse,
    imp_parse,
    error_handling::Source_errors,
    error_handling::Source_validated,
    error_handling::SyntaxError,
);

/// A database that contains all the information needed to parse a Lura program. This includes
/// the source code, the syntax tree, and any diagnostics that were generated during parsing.
pub trait ParseDb: VfsDb + DiagnosticDb + salsa::DbWithJar<Jar> {}

impl<DB> ParseDb for DB where DB: ?Sized + VfsDb + DiagnosticDb + salsa::DbWithJar<Jar> {}

/// Represents a Lura program that has been parsed into a syntax tree. This is the main entry
/// point for interacting with the Lura syntax tree. It is created by calling the `parse` method
/// on a `SourceFile`.
#[salsa::interned]
pub struct Source {
    /// The path to the file that this program is in.
    #[return_ref]
    pub file_path: std::path::PathBuf,

    /// The name of the module that this program is in.
    #[return_ref]
    pub module_name: std::string::String,

    /// The source code of the program.
    #[return_ref]
    pub source_text: std::string::String,

    /// The syntax tree of the program.
    #[return_ref]
    pub syntax_node: ParseTree,
}

/// Defines the [`parse`] query.
///
/// Parses a Lura program into a syntax tree. This query is memoized, so it will only be executed
/// once for each program.
#[salsa::tracked]
pub fn parse(db: &dyn ParseDb, program: lura_vfs::SourceFile) -> Source {
    imp_parse(db, program).validated(db)
}

/// Defines the [`parse`] query.
///
/// Parses a Lura program into a syntax tree. This query is memoized, so it will only be executed
/// once for each program.
#[salsa::tracked]
pub fn imp_parse(db: &dyn ParseDb, program: lura_vfs::SourceFile) -> Source {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_lura::language())
        .expect("Error loading lura language");

    let name = program.module_name(db).clone();
    let text = program.source_text(db).clone();

    let tree = ParseTree {
        tree: parser.parse(&text, None).unwrap().into(),
    };

    Source::new(db, program.file_path(db).clone(), name, text, tree)
}

/// Represents a Lura syntax tree. This is a wrapper around a tree-sitter syntax tree that
/// implements the `Eq` and `Hash` traits so that it can be used as a key in a `HashMap`.
///
/// This is necessary because tree-sitter syntax trees are not `Eq` or `Hash`.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct ParseTree {
    pub tree: Arc<Tree>,
}

impl Deref for ParseTree {
    type Target = Arc<Tree>;

    fn deref(&self) -> &Self::Target {
        &self.tree
    }
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
