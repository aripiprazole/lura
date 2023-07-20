use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Arc,
};

use crate::{
    resolve::{Definition, DefinitionKind},
    source::{HirSource, QualifiedPath},
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
    pub name: Option<QualifiedPath>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub kind: ScopeKind,
    pub parent: Option<Arc<Scope>>,

    pub imports: HashSet<Import>,

    pub constructors: HashMap<QualifiedPath, Definition>,
    pub values: HashMap<QualifiedPath, Definition>,
    pub types: HashMap<QualifiedPath, Definition>,
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

    pub fn search(&self, name: QualifiedPath, kind: DefinitionKind) -> Option<Definition> {
        match kind {
            DefinitionKind::Function => todo!(),
            DefinitionKind::Constructor => todo!(),
            DefinitionKind::Type => todo!(),
            DefinitionKind::Variable => todo!(),
        }
    }
}
