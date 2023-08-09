use std::fmt::{Display, Formatter};
use crate::type_rep::{state, Type};
use crate::type_rep::state::Quoted;

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

impl<S: state::TypeState> Display for Kind<S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::Type(_) => todo!(),
            Kind::Fun(domain, codomain) => write!(f, "{} -> {}", domain, codomain),
        }
    }
}
