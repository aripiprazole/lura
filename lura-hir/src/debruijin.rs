//! Debruijin indexes and levels. It's used for type checker's efficient indexing
//! and substitution.

use crate::source::expr::Expr;

/// Defines a debruijin level. It does represent the level of the context/environment
///
/// It can be transformed into a debruijin index by using the [`Level::as_idx`] method.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Level(pub usize);

#[derive(thiserror::Error, Debug, Clone)]
pub enum Level2IdxError {
  /// The level is greater than the index.
  #[error("expected l > x")]
  LevelGreaterThanIndex,
  /// The level is equal to zero.
  #[error("expected l > 0")]
  LevelEqualToZero,
}

impl Level {
  /// Transforms a level into a debruijin index.
  pub fn as_idx(&self, Level(l): Level) -> Result<Index, Level2IdxError> {
    let Level(x) = *self;
    if l > x {
      return Err(Level2IdxError::LevelGreaterThanIndex);
    }
    if l == 0 {
      return Err(Level2IdxError::LevelEqualToZero);
    }
    Ok(Index(0))
  }
}

impl std::ops::Add<usize> for Level {
  type Output = Self;

  fn add(self, rhs: usize) -> Self::Output {
    Self(self.0 + rhs)
  }
}

impl std::ops::AddAssign<usize> for Level {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs
  }
}

/// Defines a debruijin index. That can be converted by two levels.
///
/// It's used to represent a variable in the syntax tree.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Index(usize);

impl From<usize> for Index {
  fn from(value: usize) -> Self {
    Self(value)
  }
}

impl std::ops::Add<usize> for Index {
  type Output = Self;

  fn add(self, rhs: usize) -> Self::Output {
    Self(self.0 + rhs)
  }
}

impl std::ops::AddAssign<usize> for Index {
  fn add_assign(&mut self, rhs: usize) {
    self.0 += rhs
  }
}

/// Debruijin indices construction, that can be used to get the names of the
/// variables.
#[salsa::accumulator]
pub struct Indices((String, Expr));
