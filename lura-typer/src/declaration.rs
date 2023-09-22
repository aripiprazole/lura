use crate::type_rep::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedDeclaration {
  pub type_rep: Type,
}
