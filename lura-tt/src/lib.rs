//! Type terms to type infer and check the lura programs, they are the simpliest
//! normal-form terms.

use lura_hir::debruijin::Idx;
use lura_hir::solver::{Definition, Reference};
use lura_hir::source::expr::Expr;
use lura_hir::source::Location;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ConstructorKind {
  True,
  False,
  Universe,
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
  pub environment: Vec<Value>,
  pub expr: Expr,
}

/// Implicitness of a term.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Icit {
  Impl,
  Expl,
}

/// Basic normalized expression, it has the term's NFE.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Term {
  Var(Idx, Option<Reference>),
  Tuple(Vec<Value>),
  Constructor(Constructor),
  Flexible(Hole, Vec<Value>),
  Rigid(Hole, Vec<Value>),
  Pi(Definition, Icit, Box<Type>, Closure),
  Lam(Closure),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Hole {}
