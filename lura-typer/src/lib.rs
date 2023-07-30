#![feature(trait_upcasting)]

use lura_hir::HirDb;

use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = TyperDb)]
pub struct Jar(ty::ThirTy);

/// The database that Typer uses internally. This is a trait so that we can
/// mock it during testing.
pub trait TyperDb: DbWithJar<Jar> {}

impl<DB> TyperDb for DB where DB: ?Sized + HirDb + salsa::DbWithJar<Jar> {}

pub mod coverage;
pub mod infer;
pub mod thir;
pub mod ty;
