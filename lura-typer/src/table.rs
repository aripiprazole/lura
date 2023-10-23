use fxhash::FxBuildHasher;
use lura_hir::source::{declaration::Parameter, expr::Expr, top_level::TopLevel, HirSource};

use crate::{declaration::TypedDeclaration, type_rep::Type};

#[salsa::tracked]
pub struct TypeTable {
  pub declarations: im::HashMap<TopLevel, TypedDeclaration, FxBuildHasher>,
  pub parameters: im::HashMap<Parameter, Type, FxBuildHasher>,
  pub expressions: im::HashMap<Expr, Type, FxBuildHasher>,
}

/// Defines the [`infer_type_table`] query.
///
/// [`infer_type_table`]: crate::table::infer_type_table
#[salsa::tracked]
pub fn infer_type_table(db: &dyn crate::TyperDb, _src: HirSource) -> TypeTable {
  // let mut ctx = InferCtx {
  //   db,
  //   pkg: source.package(db),
  //   self_type: None,
  //   return_type: None,
  //   location: lura_hir::source::Location::CallSite,
  //   debruijin_index: Default::default(),
  //   expressions: Default::default(),
  //   diagnostics: Default::default(),
  //   parameters: Default::default(),
  //   declarations: Default::default(),
  //   env: Default::default(),
  //   adhoc_env: Default::default(),
  //   type_env: Default::default(),
  //   eval_env: Default::default(),
  //   options: Default::default(),
  //   type_rep: Default::default(),
  // };
  //
  // // Register the errors that already were reported, to avoid
  // // duplicated errors
  // let mut already_reported = HashSet::new();
  //
  // // Infer the types of all expressions.
  // for top_level in source.contents(db).iter().cloned() {
  //   top_level.infer(&mut ctx);
  // }
  //
  // // We push the diagnostic to the diagnostics
  // for diagnostic in ctx.diagnostics.borrow().iter().cloned() {
  //   // If the diagnostic was already reported, we skip it
  //   if already_reported.contains(&diagnostic) {
  //     continue;
  //   }
  //   already_reported.insert(diagnostic.clone());
  //
  //   Diagnostics::push(db, Report::new(diagnostic));
  // }
  //
  // let debrujin_index = ctx.debruijin_index.into_iter().collect();
  //
  // // Transforms the expressions into their sealed versions.
  // let expressions = ctx.expressions.into_iter().map(|(k, v)| (k, v.seal())).collect();
  //
  // // Transforms the declarations into their sealed versions.
  // let declarations = ctx
  //   .declarations
  //   .into_iter()
  //   .map(|(k, v)| (k, TypedDeclaration { type_rep: v.seal() }))
  //   .collect();
  //
  // // Transforms the parameters into their sealed versions.
  // let parameters = ctx.parameters.into_iter().map(|(k, v)| (k, v.seal())).collect();
  let declarations = Default::default();
  let parameters = Default::default();
  let expressions = Default::default();

  TypeTable::new(db, declarations, parameters, expressions)
}
