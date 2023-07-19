#![feature(trait_upcasting)]

use lura_diagnostic::DiagnosticDb;
use lura_syntax::ParseDb;
use package::HasManifest;
use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = HirDb)]
pub struct Jar(
    crate::package::Package,
    crate::package::all_package_files,
    crate::resolve::find_constructor,
    crate::resolve::find_function,
    crate::resolve::find_type,
    crate::source::HirSource,
    crate::source::HirSourceId,
    crate::source::HirError,
    crate::source::QualifiedPath,
    crate::source::Identifier,
    crate::source::top_level::Clause,
    crate::source::top_level::Signature,
    crate::source::pattern::ConstructorPattern,
    crate::source::pattern::BindingPattern,
    crate::source::declaration::Parameter,
    crate::source::declaration::DocString,
    crate::source::declaration::Attribute,
    crate::source::stmt::LetStmt,
    crate::source::stmt::Block,
    crate::source::stmt::AskStmt,
    crate::source::expr::AbsExpr,
    crate::source::expr::AnnExpr,
    crate::source::expr::CallExpr,
    crate::source::expr::MatchExpr,
    crate::source::type_rep::QPath,
    crate::lower::hir_lower,
);

pub trait HirDb: HasManifest + ParseDb + DiagnosticDb + DbWithJar<Jar> {}

impl<DB: HasManifest> HirDb for DB where DB: ?Sized + ParseDb + DiagnosticDb + salsa::DbWithJar<Jar> {}

pub mod lower;
pub mod package;
pub mod resolve;
pub mod source;
