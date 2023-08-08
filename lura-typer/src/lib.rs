#![feature(trait_upcasting)]
#![feature(stmt_expr_attributes)]

use lura_hir::HirDb;

use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = TyperDb)]
pub struct Jar(
    type_rep::ThirTy,
    table::TypeTable,
    table::infer_type_table,
    elaboration::thir_elab,
    thir::ThirSource,
);

/// The database that Typer uses internally. This is a trait so that we can
/// mock it during testing.
pub trait TyperDb: HirDb + DbWithJar<Jar> {}

impl<DB> TyperDb for DB where DB: ?Sized + HirDb + salsa::DbWithJar<Jar> {}

pub mod coverage;
pub mod declaration;
pub mod elaboration;
pub mod infer;
pub mod table;
pub mod thir;
pub mod type_rep;
