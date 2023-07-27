use std::collections::HashMap;

use fxhash::FxBuildHasher;

use crate::{
    resolve::{Definition, DefinitionKind, Reference},
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

    #[default]
    File = 9,
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
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<Box<Scope>>,

    /// The imports informations
    pub imports: im::HashSet<Import>,

    pub references: im::HashMap<Definition, im::OrdSet<Reference>, FxBuildHasher>,

    // The values informations
    pub constructors: HashMap<HirPath, Definition, FxBuildHasher>,
    pub values: HashMap<HirPath, Definition, FxBuildHasher>,
    pub variables: HashMap<HirPath, Definition, FxBuildHasher>,
    pub types: HashMap<HirPath, Definition, FxBuildHasher>,
}

impl Scope {
    /// Creates a new `Scope` with the given `kind`.
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            parent: None,
            references: im::HashMap::default(),
            constructors: HashMap::default(),
            types: HashMap::default(),
            values: HashMap::default(),
            variables: HashMap::default(),
            imports: im::HashSet::default(),
        }
    }

    pub fn root(self) -> Box<Self> {
        match self.parent {
            Some(parent) => parent,
            None => Box::new(self),
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
            parent: Some(Box::new(self.clone())),
            references: im::HashMap::default(),
            constructors: HashMap::default(),
            types: HashMap::default(),
            values: HashMap::default(),
            variables: HashMap::default(),
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
        let definition = Definition::new(db, kind, name, location);

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
    pub fn search(&self, name: HirPath, kind: DefinitionKind) -> Option<Definition> {
        match kind {
            DefinitionKind::Function => self
                .values
                .get(&name)
                .copied() // Searches in the current scope variables and functions
                .or_else(|| self.search(name, DefinitionKind::Variable))
                .or_else(|| self.search(name, DefinitionKind::Constructor))
                .or_else(|| match self.parent.as_ref() {
                    Some(root) => root.search(name, DefinitionKind::Function),
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
}
