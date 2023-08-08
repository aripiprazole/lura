use crate::ty::TypeRep;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedDeclaration {
    pub type_rep: TypeRep,
}
