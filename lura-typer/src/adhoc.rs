use crate::type_rep::{self, Ty};

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassEnv {}

/// Represents a class, i.e. a collection of types that share a common
/// interface.
pub struct Trait<M: type_rep::modes::TypeMode> {
    pub superclasses: Vec<String>,
    pub instances: Vec<Instance<M>>,
}

pub type Instance<M> = Qual<M, Pred<M>>;

/// Represents a qualified type, i.e. a type with predicates.
///
/// For example, `Eq a => a` is a qualified type, where `Eq a` is the predicate
/// and `a` is the type value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qual<M: type_rep::modes::TypeMode, T> {
    pub predicates: Vec<Pred<M>>,
    pub value: T,
}

/// Represents a predicate, i.e. a constraint on a type.
///
/// For example, `Eq a` is a predicate, where `Eq` is the class and `a` is the
/// type value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pred<M: type_rep::modes::TypeMode> {
    IsIn(Ty<M>, String),
}
