use im::OrdSet;
use lura_diagnostic::{code, message, Diagnostics, ErrorId, Report};

// Re-export the diagnostics. The diagnostics are used to report errors, warnings, and other kinds
// of diagnostics.
//
// They were defined here, so we are rexporting it to avoid confusion!
pub use crate::errors::HirDiagnostic;
use crate::{
  debruijin::{Index, Level},
  primitives::{initialize_primitive_bag, primitive_type_definition},
  reference::ReferenceWalker,
  reparse::reparse_hir_path,
  scope::{Scope, ScopeKind},
  source::{DefaultWithDb, HirLocation, HirPath, Location, VirtualPath},
};

/// Represents the kind of a definition in the High-Level Intermediate Representation. It's
/// intended to be used to store the kind of the definition, and to be used to create the HIR.
///
/// The `Unresolved` variant is a temporary state that should never be returned by the resolver.
/// Only if there's an unresolved name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum DefinitionKind {
  Function = 1,
  Constructor = 2,
  Type = 3,
  Variable = 4,
  Module = 5,
  Command = 6,
  Trait = 7,

  /// This is a temporary state that should never be returned by the resolver. Only if there's
  /// an unresolved name.
  Unresolved = 8,
}

/// Represents the level of the expression in the High-Level Intermediate Representation. It's
/// intended to be used to store the level of the expression, and to be used to create the HIR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum HirLevel {
  Expr = 1,
  Type = 2,
}

#[salsa::interned]
pub struct DefinitionId {
  #[return_ref]
  pub location: Location,

  /// Optional synthetic identifier for the definition.
  #[return_ref]
  pub name: Option<String>,
}

/// Defines a definition in the High-Level Intermediate Representation. It's intended to be used
/// as a resolved path or definition, and to be used to create the HIR.
#[salsa::tracked]
pub struct Definition {
  pub id: DefinitionId,
  pub kind: DefinitionKind,
  pub name: HirPath,
  pub defined_at: Level,
}

#[salsa::tracked]
impl Definition {
  #[salsa::tracked]
  pub fn location(self, db: &dyn crate::HirDb) -> Location {
    self.name(db).location(db)
  }

  #[salsa::tracked]
  pub fn to_string(self, db: &dyn crate::HirDb) -> String {
    self
      .name(db)
      .to_string(db)
      .unwrap_or("~INTERNAL ERROR~".into())
  }
}

impl crate::walking::Walker for Definition {
  fn accept<T: crate::walking::HirListener>(self, _db: &dyn crate::HirDb, _listener: &mut T) {}
}

/// It's a reference to a definition in the High-Level Intermediate Representation. It's intended
/// to be used as a resolved path or definition, and to be used to create the HIR.
#[salsa::input]
pub struct Reference {
  pub definition: Definition,
  pub location: Location,

  /// The index of the reference with debruijin indices.
  pub idx: Index,
}

#[salsa::tracked]
impl Reference {
  /// Checks if the reference is a type level. It's intended to be used to check if the reference
  /// is defined as a type.
  #[salsa::tracked]
  pub fn is_type_level(self, db: &dyn crate::HirDb) -> bool {
    // This is splited in two lines to avoid a salsa bug.
    let v = self;

    matches!(v.definition(db).kind(db), DefinitionKind::Type)
  }

  /// Gets the definition of the reference. It's intended to be used to get the definition of the
  /// reference.
  #[salsa::tracked]
  pub fn name(self, db: &dyn crate::HirDb) -> HirPath {
    self.definition(db).name(db)
  }
}

impl crate::walking::Walker for Reference {
  fn accept<T: crate::walking::HirListener>(self, _db: &dyn crate::HirDb, listener: &mut T) {
    listener.visit_reference(self)
  }
}

impl Definition {
  /// Creates a new `Definition` with the given `kind`, `name`, and `location`, and reports
  /// an error to the diagnostic database.
  pub fn no(db: &dyn crate::HirDb, kind: DefinitionKind, name: HirPath) -> Self {
    let kind_str = format!("{:?}", kind).to_lowercase();
    let name_str = name.to_string(db).unwrap_or("~INTERNAL ERROR~".into());

    // Reports an error to the diagnostic database.
    Diagnostics::push(
      db,
      Report::new(HirDiagnostic {
        location: name.location(db),
        message: message!("unresolved", code!(kind_str), code!(name_str)),
        id: ErrorId("unresolved-definition"),
      }),
    );

    let id = DefinitionId::new(db, name.location(db), None);

    // Creates a new `Definition` with the given `kind`, `name`, and `location`.
    Self::new(db, id, DefinitionKind::Unresolved, name, Level::default())
  }
}

impl DefaultWithDb for Definition {
  fn default_with_db(db: &dyn crate::HirDb) -> Self {
    let name = HirPath::new(db, Location::call_site(db), vec![]);

    Self::no(db, DefinitionKind::Unresolved, name)
  }
}

/// Defines the [`unresolved`] query.
///
/// Creates a new `Definition` with the given `kind`, `name`, and `location`, and reports
/// an error to the diagnostic database.
#[salsa::tracked]
pub fn unresolved(db: &dyn crate::HirDb, location: HirLocation) -> Definition {
  let input = VirtualPath::new(db, "_".into());
  let segments = reparse_hir_path(db, location, input.path(db));
  let path = HirPath::new(db, Location::call_site(db), segments);
  let id = DefinitionId::new(db, path.location(db), None);

  // Creates a new `Definition` with the given `kind`, `name`, and `location`.
  Definition::new(db, id, DefinitionKind::Unresolved, path, Level::default())
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
      let hir = db.hir_declare(package, file);
      let mut target = Scope::new(ScopeKind::InternalFile);

      hir.scope(db).publish_all_definitions_to(
        db,
        /* prefix = */ hir.source(db).module_name(db),
        /* scope  = */ &mut target,
      );

      if let Some(function) = target.search(db, name, DefinitionKind::Function) {
        return function;
      }
    }
  }

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
      let hir = db.hir_declare(package, file);
      let mut target = Scope::new(ScopeKind::InternalFile);

      hir.scope(db).publish_all_definitions_to(
        db,
        /* prefix = */ hir.source(db).module_name(db),
        /* scope  = */ &mut target,
      );

      if let Some(function) = target.search(db, name, DefinitionKind::Constructor) {
        return function;
      }
    }
  }

  Definition::no(db, DefinitionKind::Constructor, name)
}

/// Defines the [`find_trait`] query.
///
/// It does search for a trait with the given `name` in all packages, and returns it as a
/// [`Definition`].
///
/// If it can't find a type with the given `name`, it returns a [`Definition`] with the
/// [`DefinitionKind::Trait`] and [`DefinitionKind::Unresolved`] kind. And will report an error to
/// the revision diagnostic database.
#[salsa::tracked]
pub fn find_trait(db: &dyn crate::HirDb, name: HirPath) -> Definition {
  for package in db.all_packages() {
    for file in package.all_files(db) {
      let hir = db.hir_declare(package, file);
      let mut target = Scope::new(ScopeKind::InternalFile);

      hir.scope(db).publish_all_definitions_to(
        db,
        /* prefix = */ hir.source(db).module_name(db),
        /* scope  = */ &mut target,
      );

      if let Some(function) = target.search(db, name, DefinitionKind::Trait) {
        return function;
      }
    }
  }

  Definition::no(db, DefinitionKind::Trait, name)
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
      let hir = db.hir_declare(package, file);
      let mut target = Scope::new(ScopeKind::InternalFile);

      hir.scope(db).publish_all_definitions_to(
        db,
        /* prefix = */ hir.source(db).module_name(db),
        /* scope  = */ &mut target,
      );

      if let Some(function) = target.search(db, name, DefinitionKind::Type) {
        return function;
      }
    }
  }

  // Tries to initialize the default primitive types.
  //
  // NOTE: If they was already defined, it will just return
  // the [`Definition`] of the primitive type.
  initialize_primitive_bag(db);

  // Tries to find a primitive type bound to the name of
  // the [`HirPath`]. If it can't find a primitive type
  // bound to the name of the [`HirPath`], it returns a
  // [`Definition`] with the [`DefinitionKind::Type`] and
  // [`DefinitionKind::Unresolved`] kind.
  //
  // And will report an error to the revision diagnostic database.
  primitive_type_definition(db, name)
    .unwrap_or_else(|| Definition::no(db, DefinitionKind::Type, name))
}

/// Defines the [`query_module`] query. It's defined as "query", because it's returning a scope
/// instead of a single definition.
///
/// It does search for a type with the given `name` in all packages, and returns it as a
/// [`Definition`].
///
/// If it can't find a type with the given `name`, it returns a [`Definition`] with the
/// [`DefinitionKind::Type`] and [`DefinitionKind::Unresolved`] kind. And will report an error to
/// the revision diagnostic database.
#[salsa::tracked]
pub fn query_module(db: &dyn crate::HirDb, name: HirPath) -> (Scope, Definition) {
  let path = name.to_string(db).unwrap_or("~INTERNAL ERROR~".into());

  for package in db.all_packages() {
    for file in package.all_files(db) {
      let hir = db.hir_declare(package, file);
      let name = file.module_name(db);

      // If the name of the file is the same as the name of the module, then it's the
      // module we're looking for.
      if &path == name {
        let txt = file.source_text(db).to_string();
        let id = DefinitionId::new(db, Location::new(db, file, txt.into(), 0, 0), None);
        let kind = DefinitionKind::Module;
        let path = HirPath::create(db, name);

        return (
          hir.scope(db),
          Definition::new(db, id, kind, path, Level::default()),
        );
      }
    }
  }

  let file = Scope::new(ScopeKind::File);

  (file, Definition::no(db, DefinitionKind::Type, name))
}

/// Defines the [`references`] query.
///
/// It does search for all references to the given `definition` in all packages, and returns it as
/// a [`im::OrdSet<Reference>`].
#[salsa::tracked]
pub fn references(db: &dyn crate::HirDb, definition: Definition) -> OrdSet<Reference> {
  let mut references = OrdSet::new();

  for package in db.all_packages() {
    for file in package.all_files(db) {
      let hir_source = db.hir_lower(package, file);
      // TODO: fixme, for some reason it's not working with same references for the
      // definitions, the lower is duplicating the references, and creating new instances
      let local_references = ReferenceWalker::new(move |db, reference, _| {
        reference.definition(db).id(db) == definition.id(db)
      })
      .build(db)
      .collect(hir_source);

      references.extend(local_references);
    }
  }

  references
}
