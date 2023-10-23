//! This file holds the domain

use std::fmt::Debug;
use std::ops::Deref;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::sync::Arc;

use lura_tt::Value;

use crate::stack::Stack;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
  pub message: Option<String>,
  pub stacktrace: Stack,
}

pub type Arguments = Vec<Value>;

/// A foreign function interface
#[derive(Clone)]
pub struct ForeignFunction {
  pub name: String,
  pub ffi: Arc<dyn Fn(Stack, Arguments) -> Value + Sync + Send + UnwindSafe + RefUnwindSafe>,
}

impl Debug for ForeignFunction {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ForeignFunction").finish_non_exhaustive()
  }
}

impl PartialEq for ForeignFunction {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl Eq for ForeignFunction {}

impl Deref for ForeignFunction {
  type Target = Arc<dyn Fn(Stack, Arguments) -> Value + Sync + Send + UnwindSafe + RefUnwindSafe>;

  fn deref(&self) -> &Self::Target {
    &self.ffi
  }
}
