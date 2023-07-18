use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::jar(db = DiagnosticDb)]
pub struct Jar(crate::Diagnostics);

#[salsa::accumulator]
pub struct Diagnostics(pub DiagnosticData);

#[derive(Clone)]
pub struct DiagnosticData {}

pub trait DiagnosticDb: DbWithJar<Jar> {}

impl<DB> DiagnosticDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}
