#![feature(trait_upcasting)]

use lura_diagnostic::DiagnosticDb;
use lura_syntax::ParseDb;
use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = HirDb)]
pub struct Jar(
    crate::source::HirSource,
    crate::source::HirSourceId,
    crate::lower::hir_lower,
);

pub trait HirDb: ParseDb + DiagnosticDb + DbWithJar<Jar> {}

impl<DB> HirDb for DB where DB: ?Sized + ParseDb + DiagnosticDb + salsa::DbWithJar<Jar> {}

pub mod lower;
pub mod source;
