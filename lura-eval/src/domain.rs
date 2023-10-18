//! This file holds the domain

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
  pub message: Option<String>,
  pub stacktrace: crate::stack::Stack,
}
