use std::{fmt::Debug, sync::Arc};

use fxhash::FxBuildHasher;

use crate::{
  debruijin::Level,
  solver::{Definition, DefinitionId, DefinitionKind, Reference},
  source::{HirPath, HirSource, Location},
  HirDb,
};

/// Represents the kind of the scope
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum ScopeKind {
  Function,
  Method,
  Lambda,
  Data,
  Class,
  Trait,
  Block,
  Pi,
  Sigma,
  Type,
  InternalFile,

  #[default]
  File,
}

impl ScopeKind {
  /// If the current scope kind should increase the scope level, or
  /// in other words, create a new environment.
  pub fn should_increase_scope_level(&self) -> bool {
    match self {
      Self::Function => true,
      Self::Method => true,
      Self::Lambda => true,
      Self::Data => false,
      Self::Class => false,
      Self::Trait => false,
      Self::Block => false,
      Self::Pi => true,
      Self::Sigma => true,
      Self::Type => false,
      Self::InternalFile => false,
      Self::File => false,
    }
  }
}

/// Represents a import in HIR, and it's intended to be used to store imports in a scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
  pub file: HirSource,
  pub name: Option<HirPath>,
}

/// Defines the entrypoint for a scope in the High-Level Intermediate Representation. It's intended
/// to store context-sensitive information, like imports, and definitions.
///
/// It's also used to store parameters, variables, functions, types and more.
#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct Scope {
  pub lvl: Level,
  pub kind: ScopeKind,
  pub parent: Option<Arc<Scope>>,
  pub free_variables: im::OrdSet<Definition>,

  /// The imports informations
  pub imports: im::HashSet<Import, FxBuildHasher>,

  pub references: im::HashMap<Definition, im::OrdSet<Reference>, FxBuildHasher>,

  // The values informations
  pub constructors: im::HashMap<String, Definition, FxBuildHasher>,
  pub traits: im::HashMap<String, Definition, FxBuildHasher>,
  pub values: im::HashMap<String, Definition, FxBuildHasher>,
  pub variables: im::HashMap<String, Definition, FxBuildHasher>,
  pub types: im::HashMap<String, Definition, FxBuildHasher>,
}

impl Scope {
  /// Creates a new `Scope` with the given `kind`.
  pub fn new(kind: ScopeKind) -> Self {
    Self {
      kind,
      lvl: Level::default(),
      parent: None,
      references: im::HashMap::default(),
      constructors: im::HashMap::default(),
      types: im::HashMap::default(),
      values: im::HashMap::default(),
      variables: im::HashMap::default(),
      traits: im::HashMap::default(),
      imports: im::HashSet::default(),
      free_variables: im::OrdSet::new(),
    }
  }

  pub fn new_ref(kind: ScopeKind) -> Arc<Self> {
    Arc::new(Self::new(kind))
  }

  pub fn root(&self) -> Arc<Self> {
    match self.parent {
      Some(ref parent) => parent.clone(),
      None => Arc::new(self.clone()),
    }
  }

  /// Returns the references to the given `definition` in the current scope. It will search in the
  /// current scope, and in the parent scope.
  pub fn references(&mut self, definition: Definition) -> im::OrdSet<Reference> {
    self.references.entry(definition).or_default().clone()
  }

  /// Adds a reference to the given `definition` in the current scope.
  pub fn using(&mut self, db: &dyn crate::HirDb, it: Definition, loc: Location) -> Reference {
    // Create a new reference to [it] and insert it in the current scope
    // let idx = self.lvl.as_idx(it.defined_at(db)).unwrap_or_else(|error| {
    //   panic!("failed to create a reference to {:?}: {}", it.to_string(db), error)
    // });
    let idx = it.defined_at(db).as_idx(self.lvl).unwrap_or_default();
    let reference = Reference::new(db, it, loc, idx);
    self.references.entry(it).or_default().insert(reference);

    // Return the reference
    reference
  }

  /// Creates a new `Scope` with the given `kind`, `parent`, `constructors`, `types`, `values`,
  /// forking the current environment.
  ///
  /// The `parent` scope will be used in searches
  pub fn fork(&self, kind: ScopeKind) -> Self {
    Self {
      kind,
      lvl: if kind.should_increase_scope_level() {
        self.lvl + 1
      } else {
        self.lvl
      },
      parent: Some(Arc::new(self.clone())),
      references: im::HashMap::default(),
      constructors: im::HashMap::default(),
      types: im::HashMap::default(),
      traits: im::HashMap::default(),
      values: im::HashMap::default(),
      variables: im::HashMap::default(),
      imports: im::HashSet::default(),
      free_variables: im::OrdSet::new(),
    }
  }

  /// Defines a name for the given `kind` in the current scope, and returns the definition.
  ///
  /// If the name is already defined, it will return the existing definition.
  pub fn define(
    &mut self,
    db: &dyn crate::HirDb,
    name: HirPath,
    location: Location,
    kind: DefinitionKind,
  ) -> Definition {
    let id = DefinitionId::new(db, location, None);
    let definition = Definition::new(db, id, kind, name, self.lvl);
    let Some(name) = definition.name(db).to_string(db) else {
      // TODO: report error
      return definition;
    };

    // Increase the scope level, as debruijin indexes only
    // works in linear environments: let x = 10; let y = x; let z = y;
    // when let defines a new scope.
    self.lvl += 1;
    self.create(db, name, definition)
  }

  /// Defines a name for the given `kind` in the current scope, and returns the definition.
  ///
  /// If the name is already defined, it will return the existing definition.
  pub fn create(&mut self, db: &dyn crate::HirDb, name: String, def: Definition) -> Definition {
    let kind = def.kind(db);

    match kind {
      DefinitionKind::Function => self.values.insert(name, def),
      DefinitionKind::Constructor => self.constructors.insert(name, def),
      DefinitionKind::Type => self.types.insert(name, def),
      DefinitionKind::Variable => self.variables.insert(name, def),
      DefinitionKind::Trait => self.traits.insert(name, def),
      DefinitionKind::Module => todo!("Nested modules are not supported yet"),
      DefinitionKind::Command => todo!("Nested commands are not supported yet"),
      DefinitionKind::Unresolved => panic!("Illegal definition kind: Unresolved"),
    };

    def
  }

  /// Searches a name for the given `kind` in the current scope, and returns the definition if
  /// found.
  pub fn search(
    &self,
    db: &dyn crate::HirDb,
    path: HirPath,
    kind: DefinitionKind,
  ) -> Option<Definition> {
    let name = path.to_string(db).unwrap_or_default();

    match kind {
      DefinitionKind::Function => self
        .values
        .get(&name)
        .copied() // Searches in the current scope variables and functions
        .or_else(|| self.search(db, path, DefinitionKind::Variable))
        .or_else(|| self.search(db, path, DefinitionKind::Constructor))
        .or_else(|| match self.parent.as_ref() {
          Some(root) => root.search(db, path, DefinitionKind::Function),
          None => None,
        }),
      DefinitionKind::Constructor => {
        self
          .constructors
          .get(&name)
          .copied()
          .or_else(|| match self.parent.as_ref() {
            Some(root) => root.search(db, path, DefinitionKind::Constructor),
            None => None,
          })
      }
      DefinitionKind::Trait => {
        self
          .traits
          .get(&name)
          .copied()
          .or_else(|| match self.parent.as_ref() {
            Some(root) => root.search(db, path, DefinitionKind::Trait),
            None => None,
          })
      }
      DefinitionKind::Type => {
        self
          .types
          .get(&name)
          .copied()
          .or_else(|| match self.parent.as_ref() {
            Some(root) => root.search(db, path, DefinitionKind::Type),
            None => None,
          })
      }
      DefinitionKind::Variable => {
        self
          .variables
          .get(&name)
          .copied()
          .or_else(|| match self.parent.as_ref() {
            Some(root) => root.search(db, path, DefinitionKind::Variable),
            None => None,
          })
      }
      DefinitionKind::Module => None,
      DefinitionKind::Command => None,
      DefinitionKind::Unresolved => None,
    }
  }

  /// Create free variable in the current scope.
  pub fn insert_free_variable(&mut self, db: &dyn HirDb, path: HirPath) -> Reference {
    match self.kind {
      ScopeKind::Block | ScopeKind::Sigma | ScopeKind::Pi => match self.parent {
        Some(ref parent) => {
          let mut parent = parent.clone();
          let parent = Arc::make_mut(&mut parent);
          parent.insert_free_variable(db, path)
        }
        None => todo!("report unresolved or panic unreachable"),
      },
      _ => {
        if let Some(definition) = self
          .free_variables
          .iter()
          .find(|it| it.name(db).to_string(db) == path.to_string(db))
        {
          return self.using(db, *definition, path.location(db));
        }
        let definition = self.define(db, path, path.location(db), DefinitionKind::Type);
        self.free_variables.insert(definition);
        self.using(db, definition, definition.location(db))
      }
    }
  }

  /// Publishes all definitions in the current scope to the parent scope.
  pub fn publish_all_definitions_to(
    &mut self,
    db: &dyn crate::HirDb,
    prefix: &str,
    another: &mut Self,
  ) {
    for (name, definition) in self.all_definitions() {
      let text = format!("{prefix}.{name}");

      another.create(db, text, definition);
    }
  }

  /// Publishes all definitions in the current scope to the parent scope.
  pub fn publish_all_definitions(&mut self, db: &dyn crate::HirDb, prefix: Definition) {
    let prefix = prefix
      .name(db)
      .to_string(db)
      .unwrap_or("~INTERNAL ERROR~".into());

    self.publish_all_definitions_to(db, &prefix, Arc::make_mut(&mut self.root()));
  }

  /// Checks if the current scope is a do-notation scope. If it's a do-notation scope, the
  /// return expr is allowed.
  pub fn is_do_notation_scope(&self) -> bool {
    let mut current = Some(self);

    // If it reaches the root scope, it's not a do-notation scope. Otherwise, it's a
    // do-notation scope.
    while let Some(parent) = current {
      current = parent.parent.as_ref().map(|parent| parent.as_ref());

      // It does match not exactly what's a do-notation, because match if it's a do-notation
      // is the work of the type checker, but it does make impossible to use `return` in type
      // level, for example.
      match parent.kind {
        ScopeKind::Function => return true,
        ScopeKind::Method => return true,
        ScopeKind::Lambda => return true,
        ScopeKind::Data => break,
        ScopeKind::Class => break,
        ScopeKind::Trait => break,
        ScopeKind::Block => return true,
        ScopeKind::Pi => {}
        ScopeKind::Sigma => {}
        ScopeKind::Type => {}
        ScopeKind::InternalFile => break,
        ScopeKind::File => break,
      };
    }

    false
  }

  pub fn all_definitions(&self) -> im::HashMap<String, Definition, FxBuildHasher> {
    let mut definitions = im::HashMap::default();

    for (name, definition) in self.constructors.iter() {
      definitions.insert(name.clone(), *definition);
    }

    for (name, definition) in self.types.iter() {
      definitions.insert(name.clone(), *definition);
    }

    for (name, definition) in self.values.iter() {
      definitions.insert(name.clone(), *definition);
    }

    for (name, definition) in self.variables.iter() {
      definitions.insert(name.clone(), *definition);
    }

    definitions
  }

  pub fn extend(&mut self, scope: Scope) {
    for (name, definition) in scope.constructors {
      self.constructors.insert(name, definition);
    }

    for (name, definition) in scope.types {
      self.types.insert(name, definition);
    }

    for (name, definition) in scope.values {
      self.values.insert(name, definition);
    }

    for (name, definition) in scope.variables {
      self.variables.insert(name, definition);
    }
  }
}

impl Debug for Scope {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Scope({:?})", self.kind)
  }
}
