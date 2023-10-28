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
  HirSource,
};
use lura_tt::{Env, Value};

pub mod domain;
pub mod logic;
pub mod stack;

pub enum StmtResult {
  Value(Value),
  Ok,
}

/// Evaluates an expression into a value
pub fn eval(
  db: &dyn lura_hir::HirDb,
  stack: stack::Stack,
  env: Env,
  expr: Expr,
) -> Result<Value, domain::RuntimeError> {
  let db = AssertUnwindSafe(db);
  std::panic::catch_unwind(|| eval_expr(*db, stack, env, expr)).map_err(|err| {
    match err.downcast::<domain::RuntimeError>() {
      Ok(runtime_error) => *runtime_error.clone(),

      // Continue panicking if the error isn't generated
      // by our application
      Err(message) => std::panic::panic_any(message),
    }
  })
}

/// Evaluates hir file in the given environment
pub fn hir_eval(
  db: &dyn lura_hir::HirDb,
  _stack: stack::Stack,
  _env: Env,
  src: HirSource,
) -> Result<Value, domain::RuntimeError> {
  for top_level in src.contents(db) {
    use lura_hir::source::top_level::TopLevel::*;
    match top_level {
      Error(_) => todo!(),
      Using(_) => todo!(),
      Command(_) => todo!(),
      BindingGroup(_) => todo!(),
      ClassDecl(_) => todo!(),
      InstanceDecl(_) => todo!(),
      TraitDecl(_) => todo!(),
      DataDecl(_) => todo!(),
      TypeDecl(_) => todo!(),
    }
  }
  todo!()
}

pub fn eval_expr(_db: &dyn lura_hir::HirDb, stack: stack::Stack, _env: Env, expr: Expr) -> Value {
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

pub fn eval_type(
  _db: &dyn lura_hir::HirDb,
  _stack: stack::Stack,
  _env: Env,
  type_rep: TypeRep,
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

pub fn eval_block(
  _db: &dyn lura_hir::HirDb,
  _stack: stack::Stack,
  _env: Env,
  _block: Block,
) -> Value {
  todo!()
}

pub fn eval_stmt(
  _db: &dyn lura_hir::HirDb,
  _stack: stack::Stack,
  _env: Env,
  _stmt: Stmt,
) -> StmtResult {
  todo!()
}
