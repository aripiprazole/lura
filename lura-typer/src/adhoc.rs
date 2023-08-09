use std::{fmt::Display, ops::Deref};

use lura_hir::resolve::Definition;

use crate::type_rep::{self, Quote, Type};

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassEnv {}

/// Represents a class, i.e. a collection of types that share a common
/// interface.
pub struct Trait<M: type_rep::state::TypeState> {
    pub superclasses: Vec<String>,
    pub instances: Vec<Instance<M>>,
}

pub type Instance<M> = Qual<M, Pred<M>>;

/// Represents a qualified type, i.e. a type with predicates.
///
/// For example, `Eq a => a` is a qualified type, where `Eq a` is the predicate
/// and `a` is the type value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qual<M: type_rep::state::TypeState, T> {
    pub predicates: Vec<Pred<M>>,
    pub data: T,
}

impl<M: type_rep::state::TypeState, T> Qual<M, T> {
    /// Creates a new qualified type.
    pub fn new(value: T) -> Self {
        Self {
            predicates: vec![],
            data: value,
        }
    }
}

impl<T: Quote> Quote for Qual<type_rep::state::Hoas, T> {
    type Sealed = Qual<type_rep::state::Quoted, T::Sealed>;

    fn seal(self) -> Self::Sealed {
        Qual {
            predicates: self.predicates.into_iter().map(|p| p.seal()).collect(),
            data: self.data.seal(),
        }
    }
}

impl<M: type_rep::state::TypeState, T> Deref for Qual<M, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<M: type_rep::state::TypeState, T: Display> Display for Qual<M, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for pred in &self.predicates {
            write!(f, "{pred}")?;
        }
        write!(f, " => ")?;
        write!(f, "{}", self.data)
    }
}

/// Represents a predicate, i.e. a constraint on a type.
///
/// For example, `Eq a` is a predicate, where `Eq` is the class and `a` is the
/// type value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pred<M: type_rep::state::TypeState> {
    IsIn(Type<M>, Definition, String),
}

impl Quote for Pred<type_rep::state::Hoas> {
    type Sealed = Pred<type_rep::state::Quoted>;

    fn seal(self) -> Self::Sealed {
        match self {
            Pred::IsIn(ty, class, name) => Pred::IsIn(ty.seal(), class, name),
        }
    }
}

impl<M: type_rep::state::TypeState> Display for Pred<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pred::IsIn(ty, _, name) => write!(f, "{ty} {name}"),
        }
    }
}