use crate::{
    lower::hir_lower,
    package::package_files,
    source::{declaration::Declaration, HirSource, Location, QualifiedPath},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefinitionKind {
    Function,
    Constructor,
    Type,
    Variable,
}

#[salsa::tracked]
pub struct Definition {
    pub kind: DefinitionKind,
    pub name: QualifiedPath,
    pub file: HirSource,
    pub location: Location,
}

#[salsa::tracked]
pub fn find_function(db: &dyn crate::HirDb, name: QualifiedPath) -> Definition {
    for package in db.all_packages() {
        for file in package_files(db, *package) {
            let source = hir_lower(db, *package, file);
            let scope = source.scope(db);

            if let Some(function) = scope.search(name, DefinitionKind::Function) {
                return function;
            }
        }
    }

    todo!()
}

#[salsa::tracked]
pub fn find_constructor(db: &dyn crate::HirDb, name: QualifiedPath) -> Definition {
    for package in db.all_packages() {
        for file in package_files(db, *package) {
            let source = hir_lower(db, *package, file);
            let scope = source.scope(db);

            if let Some(function) = scope.search(name, DefinitionKind::Constructor) {
                return function;
            }
        }
    }

    todo!()
}

#[salsa::tracked]
pub fn find_type(db: &dyn crate::HirDb, name: QualifiedPath) -> Definition {
    for package in db.all_packages() {
        for file in package_files(db, *package) {
            let source = hir_lower(db, *package, file);
            let scope = source.scope(db);

            if let Some(function) = scope.search(name, DefinitionKind::Constructor) {
                return function;
            }
        }
    }

    todo!()
}
