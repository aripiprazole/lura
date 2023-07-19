use crate::source::{HirSource, Location, QualifiedPath, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefinitionKind {
    Function,
    Constructor,
    Type,
    Variable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Definition {
    pub kind: DefinitionKind,
    pub name: QualifiedPath,
    pub file: HirSource,
    pub location: Location,
}

#[salsa::tracked]
pub fn find_function(db: &dyn crate::HirDb, name: QualifiedPath) -> QualifiedPath {
    todo!()
}

#[salsa::tracked]
pub fn find_constructor(db: &dyn crate::HirDb, name: QualifiedPath) -> QualifiedPath {
    todo!()
}

#[salsa::tracked]
pub fn find_type(db: &dyn crate::HirDb, name: QualifiedPath) -> QualifiedPath {
    todo!()
}
