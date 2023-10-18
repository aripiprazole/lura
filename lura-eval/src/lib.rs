//! This crate performs simple evaluation using debruijin indices, it's intended
//! to be used by the type system, as it does performs evaluation to allow
//! dependent-types.
//!
//! But we can use it as an engine to the language, for macros etc.

use std::panic::AssertUnwindSafe;
use lura_hir::source::expr::Expr;
use lura_tt::Value;

pub mod domain;
pub mod logic;
pub mod stack;

/// Evaluates an expression into a value
pub fn eval(db: &dyn lura_hir::HirDb, expr: Expr) -> Result<Value, domain::RuntimeError> {
  let db = AssertUnwindSafe(db);
  let internal_stack = stack::Stack::default();
  std::panic::catch_unwind(|| normalize(*db, internal_stack, expr))
    .map_err(|err| {
      match err.downcast::<domain::RuntimeError>() {
        Ok(runtime_error) => *runtime_error.clone(),

        // Continue panicking if the error isn't generated
        // by our application
        Err(message) => std::panic::panic_any(message)
      }
    })
}

pub fn normalize(db: &dyn lura_hir::HirDb, stack: stack::Stack, expr: Expr) -> Value {
  todo!()
}
