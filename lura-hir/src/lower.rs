use lura_syntax::Source;

use crate::source::{HirSource, HirSourceId};

#[salsa::tracked]
pub fn hir_lower(db: &dyn crate::HirDb, source: Source) -> HirSource {
    let id = HirSourceId::new(db, source.file_path(db).clone());

    HirSource::new(db, id, source, vec![])
}
