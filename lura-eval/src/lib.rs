//! This crate performs simple evaluation using debruijin indices, it's intended
//! to be used by the type system, as it does performs evaluation to allow
//! dependent-types.
//!
//! But we can use it as an engine to the language, for macros etc.

use std::panic::AssertUnwindSafe;

use lura_hir::source::{
  expr::Expr,
  stmt::{Block, Stmt},
  type_rep::TypeRep,
};
use lura_tt::{Env, Value};

pub mod domain;
pub mod logic;
pub mod stack;

enum StmtResult {
  Value(Value),
  Ok,
}

/// Evaluates an expression into a value
pub fn eval(db: &dyn lura_hir::HirDb, env: Env, expr: Expr) -> Result<Value, domain::RuntimeError> {
  let db = AssertUnwindSafe(db);
  let stack = stack::Stack::default();
  std::panic::catch_unwind(|| evaluate(*db, stack, env, expr)).map_err(|err| {
    match err.downcast::<domain::RuntimeError>() {
      Ok(runtime_error) => *runtime_error.clone(),

      // Continue panicking if the error isn't generated
      // by our application
      Err(message) => std::panic::panic_any(message),
    }
  })
}

pub fn evaluate(db: &dyn lura_hir::HirDb, stack: stack::Stack, env: Env, expr: Expr) -> Value {
  match expr {
    Expr::Empty | Expr::Error(_) => stack.unwind("empty expressions can't be evaluated"),
    Expr::Path(_) => todo!(),
    Expr::Meta(_) => todo!(),
    Expr::Literal(_) => todo!(),
    Expr::Call(_) => todo!(),
    Expr::Ann(_) => todo!(),
    Expr::Abs(_) => todo!(),
    Expr::Match(_) => todo!(),
    Expr::Upgrade(_) => todo!(),
  }
}

fn evaluate_type(
  db: &dyn lura_hir::HirDb, stack: stack::Stack, env: Env, type_rep: TypeRep,
) -> Value {
  match type_rep {
    TypeRep::Unit => todo!(),
    TypeRep::Hole => todo!(),
    TypeRep::SelfType => todo!(),
    TypeRep::Type => todo!(),
    TypeRep::Error(_) => todo!(),
    TypeRep::Path(_, _) => todo!(),
    TypeRep::QPath(_) => todo!(),
    TypeRep::App(_) => todo!(),
    TypeRep::Arrow(_) => todo!(),
    TypeRep::Downgrade(_) => todo!(),
  }
}

fn evaluate_block(db: &dyn lura_hir::HirDb, stack: stack::Stack, env: Env, block: Block) -> Value {
  todo!()
}

fn evaluate_stmt(
  db: &dyn lura_hir::HirDb, stack: stack::Stack, env: Env, stmt: Stmt,
) -> StmtResult {
  todo!()
}
