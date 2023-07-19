#![feature(trait_upcasting)]

use lura_diagnostic::DiagnosticDb;
use lura_syntax::ParseDb;
use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = HirDb)]
pub struct Jar(
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

pub trait HirDb: ParseDb + DiagnosticDb + DbWithJar<Jar> {}

impl<DB> HirDb for DB where DB: ?Sized + ParseDb + DiagnosticDb + salsa::DbWithJar<Jar> {}

pub mod lower;
pub mod source;
