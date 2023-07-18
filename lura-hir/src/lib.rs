use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = HirDb)]
pub struct Jar(crate::program::SourceProgram);

pub trait HirDb: DbWithJar<Jar> {}

impl<DB> HirDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

pub mod program;
