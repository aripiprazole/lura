use crate::type_rep::state;

/// The kind of a type. It's basically the type of types.
/// 
/// It's used to implement higher-kinded types, and type families.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind<M: state::TypeState> {
    /// The type of types.
    Star,

    /// Wraps a type, to be easier to deal with 
    /// type families.
    Type(M::Ty),

    /// The kind of a function.
    Fun(Box<Kind<M>>, Box<Kind<M>>),
}
