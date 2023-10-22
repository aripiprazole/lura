use std::sync::Arc;

use dashmap::DashMap;
use fxhash::FxBuildHasher;

use crate::debruijin::Level;
use crate::{
  solver::{Definition, DefinitionId, DefinitionKind},
  source::{
    type_rep::{TypeReference, TypeRep},
    HirPath, Location,
  },
};

/// The primitive map, that stores all the primitives that are available in the
/// language.
pub type PrimitiveMap = im::HashMap<String, Definition, FxBuildHasher>;

/// The primitive bag, that stores all the primitive maps.
#[derive(Default, Clone)]
pub struct PrimitiveBag {
  type_representations: DashMap<String, Definition>,
  type_definitions: DashMap<Definition, TypeRep>,
}

pub trait PrimitiveProvider {
  /// Gets a primitive map for the given definition kind.
  ///
  /// It does it lazily
  fn primitives(&self) -> Arc<PrimitiveBag>;
}

/// Defines the [`initialize_primitive_bag`] query.
///
/// Initializes the primitive bag, with the default builtins.
pub fn initialize_primitive_bag(db: &dyn crate::HirDb) {
  /// Overrides the [`self::new_type_rep`] query, to make it simplier to
  /// use.
  pub fn new_type_rep(db: &dyn crate::HirDb, name: &str, refr: TypeReference) {
    let type_rep = TypeRep::Path(refr, Location::CallSite);

    self::new_type_rep(db, HirPath::create(db, name), type_rep);
  }

  // Defines string types
  new_type_rep(db, "String", TypeReference::String);
  new_type_rep(db, "Unit", TypeReference::Unit);

  // Defines bool types
  new_type_rep(db, "Bool", TypeReference::Bool);

  // Defines integer types
  new_type_rep(db, "Int", TypeReference::Int32);
  new_type_rep(db, "Int8", TypeReference::Int8);
  new_type_rep(db, "UInt8", TypeReference::UInt8);
  new_type_rep(db, "Int16", TypeReference::Int16);
  new_type_rep(db, "UInt16", TypeReference::UInt16);
  new_type_rep(db, "Int32", TypeReference::Int32);
  new_type_rep(db, "UInt32", TypeReference::UInt32);
  new_type_rep(db, "Int64", TypeReference::Int64);
  new_type_rep(db, "UInt64", TypeReference::UInt64);
  new_type_rep(db, "Nat", TypeReference::Nat);
}

/// Defines the [`new_type_rep`] query.
///
/// Creates a new type representation primitive in the current context.
///
/// # Parameters
/// - `db`: The database
/// - `path`: The path of the type representation
#[salsa::tracked]
pub fn new_type_rep(db: &dyn crate::HirDb, path: HirPath, repr: TypeRep) {
  // Get the database for primitives
  let primitives = db.primitives();

  // Create a definition
  let text = path.to_string(db);
  let definition = *primitives
    .type_representations
    .entry(text.clone().unwrap())
    .or_insert_with(move || {
      let id = DefinitionId::new(db, Location::CallSite, text.clone());
      Definition::new(db, id, DefinitionKind::Type, path, Level::default())
    });

  // Define the type if it is not defined
  if !primitives.type_definitions.contains_key(&definition) {
    primitives.type_definitions.insert(definition, repr);
  }
}

/// Defines the [`primitive_type_rep`] query.
///
/// Gets the type representation of a primitive type.
#[salsa::tracked]
pub fn primitive_type_rep(db: &dyn crate::HirDb, path: HirPath) -> Option<TypeRep> {
  let primitives = db.primitives();
  let definition = primitives.type_representations.get(&path.to_string(db)?)?;
  primitives
    .type_definitions
    .get(&definition)
    .map(|value| value.clone())
}

/// Defines the [`primitive_type_definition`] query.
///
/// Gets the type definition of a primitive type.
#[salsa::tracked]
pub fn primitive_type_definition(db: &dyn crate::HirDb, path: HirPath) -> Option<Definition> {
  let primitives = db.primitives();
  let definition = primitives.type_representations.get(&path.to_string(db)?)?;

  Some(*definition)
}
