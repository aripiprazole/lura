use std::path::PathBuf;

use salsa::DbWithJar;

extern crate salsa_2022 as salsa;

#[salsa::input]
pub struct SourceFile {
  #[return_ref]
  pub file_path: PathBuf,

  #[return_ref]
  pub module_name: String,

  #[return_ref]
  pub source_text: String,
}

#[salsa::jar(db = VfsDb)]
pub struct Jar(crate::SourceFile);

pub trait VfsDb: DbWithJar<Jar> {}

impl<DB> VfsDb for DB where DB: DbWithJar<Jar> {}
