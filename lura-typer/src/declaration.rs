use crate::type_rep::TypeRep;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedDeclaration {
    pub type_rep: TypeRep,
}
