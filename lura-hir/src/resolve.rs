use lura_diagnostic::{Diagnostic, Diagnostics, ErrorKind, ErrorText, Report};

use crate::{
    lower::hir_declare,
    source::{DefaultWithDb, HirPath, Location},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefinitionKind {
    Function,
    Constructor,
    Type,
    Variable,
    Module,

    /// This is a temporary state that should never be returned by the resolver. Only if there's
    /// an unresolved name.
    Unresolved,
}

/// Defines a definition in the High-Level Intermediate Representation. It's intended to be used
/// as a resolved path or definition, and to be used to create the HIR.
#[salsa::tracked]
pub struct Definition {
    pub kind: DefinitionKind,
    pub name: HirPath,
    pub location: Location,
}

/// Represents the diagnostic for High-Level Intermediate Representation. It's intended to be used
/// to report errors to the diagnostic database, by this crate, only.
#[derive(Debug)]
pub struct HirDiagnostic {
    pub location: Location,
    pub message: String,
}

impl Definition {
    /// Creates a new `Definition` with the given `kind`, `name`, and `location`, and reports
    /// an error to the diagnostic database.
    pub fn no(db: &dyn crate::HirDb, kind: DefinitionKind, name: HirPath) -> Self {
        // Reports an error to the diagnostic database.
        Diagnostics::push(
            db,
            Report::new(HirDiagnostic {
                location: name.location(db),
                message: format!("Unresolved {kind:?}: {:?}", name.to_string(db)),
            }),
        );

        // Creates a new `Definition` with the given `kind`, `name`, and `location`.
        Self::new(db, DefinitionKind::Unresolved, name, name.location(db))
    }
}

impl DefaultWithDb for Definition {
    fn default_with_db(db: &dyn crate::HirDb) -> Self {
        let name = HirPath::new(db, Location::call_site(db), vec![]);

        Self::no(db, DefinitionKind::Unresolved, name)
    }
}

impl Diagnostic for HirDiagnostic {
    type TextRange = Location;

    const KIND: ErrorKind = ErrorKind::ResolutionError;

    fn text(&self) -> Vec<lura_diagnostic::ErrorText> {
        vec![ErrorText::Text(self.message.clone())]
    }

    fn location(&self) -> Option<Self::TextRange> {
        Some(self.location.clone())
    }
}

/// Defines the [`find_function`] query.
///
/// It does search for a constructor with the given `name` in all packages, and returns it as a
/// [`Definition`].
///
/// If it can't find a constructor with the given `name`, it returns a [`Definition`] with the
/// [`DefinitionKind::Functions`] and [`DefinitionKind::Unresolved`] kind. And will report an
/// error to the revision diagnostic database.
#[salsa::tracked]
pub fn find_function(db: &dyn crate::HirDb, name: HirPath) -> Definition {
    for package in db.all_packages() {
        for file in package.all_files(db) {
            let source = hir_declare(db, *package, file);
            let scope = source.scope(db);

            if let Some(function) = scope.search(name, DefinitionKind::Function) {
                return function;
            }
        }
    }

    // TODO: report error
    Definition::no(db, DefinitionKind::Function, name)
}

/// Defines the [`find_constructor`] query.
///
/// It does search for a constructor with the given `name` in all packages, and returns it as a
/// [`Definition`].
///
/// If it can't find a constructor with the given `name`, it returns a [`Definition`] with the
/// [`DefinitionKind::Constructor`] and [`DefinitionKind::Unresolved`] kind. And will report an
/// error to the revision diagnostic database.
#[salsa::tracked]
pub fn find_constructor(db: &dyn crate::HirDb, name: HirPath) -> Definition {
    for package in db.all_packages() {
        for file in package.all_files(db) {
            let source = hir_declare(db, *package, file);
            let scope = source.scope(db);

            if let Some(function) = scope.search(name, DefinitionKind::Constructor) {
                return function;
            }
        }
    }

    // TODO: report error
    Definition::no(db, DefinitionKind::Constructor, name)
}

/// Defines the [`find_type`] query.
///
/// It does search for a type with the given `name` in all packages, and returns it as a
/// [`Definition`].
///
/// If it can't find a type with the given `name`, it returns a [`Definition`] with the
/// [`DefinitionKind::Type`] and [`DefinitionKind::Unresolved`] kind. And will report an error to
/// the revision diagnostic database.
#[salsa::tracked]
pub fn find_type(db: &dyn crate::HirDb, name: HirPath) -> Definition {
    for package in db.all_packages() {
        for file in package.all_files(db) {
            let source = hir_declare(db, *package, file);
            let scope = source.scope(db);

            if let Some(function) = scope.search(name, DefinitionKind::Constructor) {
                return function;
            }
        }
    }

    // TODO: report error
    Definition::no(db, DefinitionKind::Type, name)
}
