use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::{
    resolve::{Definition, DefinitionKind},
    source::{HirPath, HirSource},
};

/// Represents the kind of the scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum ScopeKind {
    Function = 1,
    Method = 2,
    Lambda = 3,
    Match = 4,
    Call = 5,
    File = 6,
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<Arc<Scope>>,

    /// The imports informations
    pub imports: HashSet<Import>,

    // The values informations
    pub constructors: HashMap<HirPath, Definition>,
    pub values: HashMap<HirPath, Definition>,
    pub types: HashMap<HirPath, Definition>,
}

impl Scope {
    /// Creates a new `Scope` with the given `kind`.
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            parent: None,
            constructors: HashMap::new(),
            types: HashMap::new(),
            values: HashMap::new(),
            imports: HashSet::new(),
        }
    }

    /// Creates a new `Scope` with the given `kind`, `parent`, `constructors`, `types`, `values`,
    /// forking the current environment.
    ///
    /// The `parent` scope will be used in searches
    pub fn fork(&self, kind: ScopeKind) -> Self {
        Self {
            kind,
            parent: Some(Arc::new(self.clone())),
            constructors: HashMap::new(),
            types: HashMap::new(),
            values: HashMap::new(),
            imports: HashSet::new(),
        }
    }

    /// Searches a name for the given `kind` in the current scope, and returns the definition if
    /// found.
    pub fn search(&self, name: HirPath, kind: DefinitionKind) -> Option<Definition> {
        match kind {
            DefinitionKind::Function => self.values.get(&name).copied(),
            DefinitionKind::Constructor => todo!(),
            DefinitionKind::Type => todo!(),
            DefinitionKind::Variable => todo!(),
            DefinitionKind::Module => todo!(),
            DefinitionKind::Unresolved => None,
        }
    }
}
