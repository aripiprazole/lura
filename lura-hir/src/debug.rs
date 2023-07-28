use std::fmt::Formatter;

use salsa_2022::DebugWithDb;

use crate::source::HirSource;

pub struct HirSourceDebug {
    source: HirSource,
}

impl HirSource {
    /// NOTE: Do not execute after a revision, as it will be invalidated. And it
    /// will be invalidated by the time you get it back from the debug, and it
    /// will panic.
    pub fn hir_debug(self) -> HirSourceDebug {
        HirSourceDebug { source: self }
    }
}

impl DebugWithDb<<crate::Jar as salsa::jar::Jar<'_>>::DynDb> for HirSourceDebug {
    fn fmt(&self, f: &mut Formatter<'_>, db: &dyn crate::HirDb, _: bool) -> std::fmt::Result {
        f.debug_struct("HirSource")
            .field("package", &self.source.package(db).debug_all(db))
            .field("scope", &self.source.scope(db))
            .field(
                "contents",
                &self
                    .source
                    .contents(db)
                    .iter()
                    .map(|top_level| top_level.debug_all(db))
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}
