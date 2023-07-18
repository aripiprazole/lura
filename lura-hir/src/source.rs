use std::path::PathBuf;

use lura_syntax::Source;

#[salsa::tracked]
pub struct HirSource {
    #[id]
    pub id: HirSourceId,
    pub source: Source,

    #[return_ref]
    pub contents: Vec<TopLevel>,
}

#[salsa::interned]
pub struct HirSourceId {
    #[return_ref]
    pub path: PathBuf,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TopLevel {}
