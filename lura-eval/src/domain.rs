//! This file holds the domain

use std::collections::HashMap;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::sync::Arc;
use lura_tt::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
  pub message: Option<String>,
  pub stacktrace: crate::stack::Stack,
}

pub type Arguments = Vec<Value>;
pub type FFI = Arc<dyn Fn(crate::stack::Stack, Arguments) -> Value + Sync + Send + UnwindSafe + RefUnwindSafe>;

/// The context that holds external functions or ffi
pub struct Ctx {
  pub functions: HashMap<String, FFI>
}
