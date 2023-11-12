use std::{path::Path, rc::Rc};

use deno_core::{error::AnyError, FastString};
use lura_hir::{
  solver::Definition,
  source::{expr::Expr, HirSource},
  HirDb,
};
use resast::prelude as js;

pub async fn run_js(file_path: &str) -> Result<(), AnyError> {
  let main_module = deno_core::resolve_path(file_path, Path::new("."))?;
  let mut js_runtime = deno_core::JsRuntime::new(deno_core::RuntimeOptions {
    module_loader: Some(Rc::new(deno_core::FsModuleLoader)),
    ..Default::default()
  });
  js_runtime.execute_script(
    "[lura:runtime.js]",
    FastString::Static(include_str!("runtime.js")),
  )?;
  let mod_id = js_runtime.load_main_module(&main_module, None).await?;
  let result = js_runtime.mod_evaluate(mod_id);
  js_runtime.run_event_loop(false).await?;
  result.await?
}

pub fn definition_mangle_name(db: &dyn HirDb, definition: Definition) -> String {
  let mut name = String::new();
  for segment in definition.name(db).segments(db) {
    name.push_str(&segment.contents(db));
    name.push_str("_");
  }
  name
}

pub fn walk_on_expr<'a>(db: &dyn HirDb, expr: Expr) -> js::Expr<'a> {
  match expr {
    Expr::Empty => todo!(),
    Expr::Error(_) => todo!(),
    Expr::Path(reference) => {
      let definition = reference.definition(db);
      let mangled_name = definition_mangle_name(db, definition);
      js::Expr::Ident(js::Ident::new(mangled_name))
    }
    Expr::Meta(_) => todo!(),
    Expr::Literal(_) => todo!(),
    Expr::Call(_) => todo!(),
    Expr::Ann(_) => todo!(),
    Expr::Abs(_) => todo!(),
    Expr::Match(_) => todo!(),
    Expr::Upgrade(_) => todo!(),
  }
}

pub fn dump_into_string(db: &dyn HirDb, source: HirSource) -> eyre::Result<String> {
  let _ = (db, source);
  todo!()
}
