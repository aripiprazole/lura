use crate::type_rep::{state, Type};

/// The kind of a type. It's basically the type of types.
/// 
/// It's used to implement higher-kinded types, and type families.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind<S: state::TypeState> {
    /// The type of types.
    Star,

    /// Wraps a type, to be easier to deal with 
    /// type families.
    Type(Type<S>),

    /// The kind of a function.
    Fun(Box<Kind<S>>, Box<Kind<S>>),
}
