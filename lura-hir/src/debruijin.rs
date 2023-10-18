//! Debruijin indexes and levels. It's used for type checker's efficient indexing
//! and substitution.

use crate::source::expr::Expr;

/// Defines a debruijin level. It does represent the level of the context/environment
///
/// It can be transformed into a debruijin index by using the [`Lvl::as_idx`] method.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lvl(pub usize);

impl Lvl {
  /// Transforms a level into a debruijin index.
  pub fn as_idx(&self, Lvl(x): Lvl) -> Idx {
    let Lvl(l) = *self;
    assert!(l > x, "l > x, but {l} < {x}");
    assert!(l > 0, "l should be greater than 0");

    Idx(l - x - 1)
  }
}

impl std::ops::Add<usize> for Lvl {
  type Output = Self;

  fn add(self, rhs: usize) -> Self::Output {
    Self(self.0 + rhs)
  }
}

impl std::ops::AddAssign<usize> for Lvl {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs
  }
}

/// Defines a debruijin index. That can be converted by two levels.
///
/// It's used to represent a variable in the syntax tree.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Idx(usize);

impl From<usize> for Idx {
  fn from(value: usize) -> Self {
    Self(value)
  }
}

impl std::ops::Add<usize> for Idx {
  type Output = Self;

  fn add(self, rhs: usize) -> Self::Output {
    Self(self.0 + rhs)
  }
}

impl std::ops::AddAssign<usize> for Idx {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs
  }
}

/// Debruijin indices construction, that can be used to get the names of the
/// variables.
#[salsa::accumulator]
pub struct Indices((String, Expr));
