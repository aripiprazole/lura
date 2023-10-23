use crate::source::{HirLocation, Identifier};

/// Defines the [`reparse_hir_path`] query.
///
/// It does reparse a new [`Vec<Identifier>`] from a [`HirPath`]. It's used to
/// rename a [`HirPath`] in the source code.
#[salsa::tracked]
pub fn reparse_hir_path(
  db: &dyn crate::HirDb,
  base: HirLocation,
  new_name: String,
) -> Vec<Identifier> {
  new_name
    .split('.')
    .map(|name| Identifier::new(db, name.to_string(), false, base.location(db)))
    .collect()
}
