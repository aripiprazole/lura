use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::{
    resolve::{Definition, DefinitionKind},
    source::{HirPath, HirSource},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Function,
    Method,
    Lambda,
    Match,
    Call,
    File,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub file: HirSource,
    pub name: Option<HirPath>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<Arc<Scope>>,

    pub imports: HashSet<Import>,

    pub constructors: HashMap<HirPath, Definition>,
    pub values: HashMap<HirPath, Definition>,
    pub types: HashMap<HirPath, Definition>,
}

impl Scope {
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

    pub fn search(&self, _name: HirPath, kind: DefinitionKind) -> Option<Definition> {
        match kind {
            DefinitionKind::Function => todo!(),
            DefinitionKind::Constructor => todo!(),
            DefinitionKind::Type => todo!(),
            DefinitionKind::Variable => todo!(),
            DefinitionKind::Module => todo!(),
            DefinitionKind::Unresolved => None,
        }
    }
}
