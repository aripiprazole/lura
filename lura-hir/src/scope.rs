use std::{fmt::Debug, sync::Arc};

use fxhash::FxBuildHasher;

use crate::{
    resolve::{Definition, DefinitionId, DefinitionKind, Reference},
    source::{HirPath, HirSource, Location},
};

/// Represents the kind of the scope
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum ScopeKind {
    Function = 1,
    Method = 2,
    Lambda = 3,
    Match = 4,
    Call = 5,
    Data = 6,
    Class = 7,
    Trait = 8,
    Block = 9,

    #[default]
    File = 10,
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
    pub kind: ScopeKind,
    pub parent: Option<Arc<Scope>>,

    /// The imports informations
    pub imports: im::HashSet<Import>,

    pub references: im::HashMap<Definition, im::OrdSet<Reference>, FxBuildHasher>,

    // The values informations
    pub constructors: im::HashMap<String, Definition, FxBuildHasher>,
    pub values: im::HashMap<String, Definition, FxBuildHasher>,
    pub variables: im::HashMap<String, Definition, FxBuildHasher>,
    pub types: im::HashMap<String, Definition, FxBuildHasher>,
}

impl Scope {
    /// Creates a new `Scope` with the given `kind`.
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            parent: None,
            references: im::HashMap::default(),
            constructors: im::HashMap::default(),
            types: im::HashMap::default(),
            values: im::HashMap::default(),
            variables: im::HashMap::default(),
            imports: im::HashSet::default(),
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
        self.references
            .entry(definition)
            .or_insert_with(im::OrdSet::new)
            .clone()
    }

    /// Adds a reference to the given `definition` in the current scope.
    pub fn using(&mut self, db: &dyn crate::HirDb, it: Definition, loc: Location) -> Reference {
        // Create a new reference to [it] and insert it in the current scope
        let reference = Reference::new(db, it, loc);
        self.references
            .entry(it)
            .or_insert_with(im::OrdSet::new)
            .insert(reference);

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
            parent: Some(Arc::new(self.clone())),
            references: im::HashMap::default(),
            constructors: im::HashMap::default(),
            types: im::HashMap::default(),
            values: im::HashMap::default(),
            variables: im::HashMap::default(),
            imports: im::HashSet::new(),
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
        let id = DefinitionId::new(db, location);
        let definition = Definition::new(db, id, kind, name);
        let Some(name) = definition.name(db).to_string(db) else {
            // TODO: report error
            return definition;
        };

        match kind {
            DefinitionKind::Function => self.values.insert(name, definition),
            DefinitionKind::Constructor => self.constructors.insert(name, definition),
            DefinitionKind::Type => self.types.insert(name, definition),
            DefinitionKind::Variable => self.variables.insert(name, definition),
            DefinitionKind::Module => todo!("Nested modules are not supported yet"),
            DefinitionKind::Command => todo!("Nested commands are not supported yet"),
            DefinitionKind::Unresolved => panic!("Illegal definition kind: Unresolved"),
        };

        definition
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
            DefinitionKind::Constructor => self.constructors.get(&name).copied(),
            DefinitionKind::Type => self.types.get(&name).copied(),
            DefinitionKind::Variable => self.variables.get(&name).copied(),
            DefinitionKind::Module => todo!("Nested modules are not supported yet"),
            DefinitionKind::Command => todo!("Nested commands are not supported yet"),
            DefinitionKind::Unresolved => None,
        }
    }

    /// Publishes all definitions in the current scope to the parent scope.
    ///
    /// TODO: This is not implemented yet.
    pub fn publish_all_definitions(&mut self, _db: &dyn crate::HirDb, _prefix: Definition) {}

    /// Checks if the current scope is a do-notation scope. If it's a do-notation scope, the
    /// return expr is allowed.
    pub fn is_do_notation_scope(&self) -> bool {
        todo!()
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

        if let Some(parent) = self.parent.as_ref() {
            for (name, definition) in parent.all_definitions().iter() {
                definitions.insert(name.clone(), *definition);
            }
        }

        definitions
    }
}

impl Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Scope({:?})", self.kind)
    }
}
