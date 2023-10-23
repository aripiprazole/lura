//! Type terms to type infer and check the lura programs, they are the simpliest
//! normal-form terms.

use lura_hir::debruijin::{Index, Level};
use lura_hir::solver::{Definition, Reference};
use lura_hir::source::expr::{Expr, Meta};
use lura_hir::source::Location;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ConstructorKind {
  True,
  False,
  Int(isize),
  String(String),
}

/// Constant, or primitive value that has no subterms
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Constructor {
  pub kind: ConstructorKind,
  pub location: Location,
}

pub type Type = Term;
pub type Env = Vec<Value>;

/// Value that can have a type associated with it.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value {
  Type(Term),
  Runtime(Term, Type),
}

/// It does represent a type level function stores the environment and can
/// take environments to evaluate the quoted expression.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Closure {
  pub env: Env,
  pub expr: Expr,
}

/// Implicitness of a term.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Implicitness {
  Impl,
  Expl,
}

/// Dependent function type, it's a type-level function
/// that depends on a value.
///
/// It allows we to construct every dependent-type features.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Pi {
  pub name: Definition,
  pub implicitness: Implicitness,
  pub type_rep: Box<Type>,
  pub closure: Closure,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Object {
  Tuple(Vec<Value>),
  Record(im::HashMap<Definition, Value>),
}

/// Basic normalized expression, it has the term's NFE.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Term {
  Var(Index, Option<Reference>),
  Object(Object),
  Constructor(Constructor),
  Flexible(Meta, Vec<Value>),
  Rigid(Level, Vec<Value>),
  Pi(Pi),
  Lam(Closure),
  Type,
}
