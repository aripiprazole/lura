use crate::source::{Identifier, Location};

/// Defines the [`reparse_hir_path`] query.
///
/// It does reparse a new [`Vec<Identifier>`] from a [`HirPath`]. It's used to
/// rename a [`HirPath`] in the source code.
pub fn reparse_hir_path(db: &dyn crate::HirDb, new_name: String) -> Vec<Identifier> {
    let mut new_path = Vec::new();
    let mut new_name = new_name.chars().peekable();
    while let Some(c) = new_name.next() {
        if c == '.' {
            new_path.push(Identifier::new(
                db,
                /* contents      = */ new_name.peek().unwrap().to_string(),
                /* refers_symbol = */ false,
                /* location      = */ Location::CallSite,
            ));
            new_name.next();
        } else {
            new_path.push(Identifier::new(
                db,
                /* contents      = */ c.to_string(),
                /* refers_symbol = */ false,
                /* location      = */ Location::CallSite,
            ));
        }
    }
    new_path
}
