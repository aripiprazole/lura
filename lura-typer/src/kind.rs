use std::fmt::{Display, Formatter};
use crate::type_rep::{state, Type};

/// The kind of a type. It's basically the type of types.
/// 
/// It's used to implement higher-kinded types, and type families.
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind<S: state::TypeState> {
    /// The type of types.
    #[default]
    Star,

    /// Wraps a type, to be easier to deal with
    /// type families.
    Type(Box<Type<S>>),

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
