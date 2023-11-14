use std::{path::Path, rc::Rc};

use deno_core::{error::AnyError, FastString};
use lura_eyre::bail;
use lura_hir::source::declaration::Declaration;
use lura_hir::source::top_level::TopLevel;
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
    name.push_str("__");
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

pub fn transform_top_level(
  db: &dyn HirDb,
  top_level: TopLevel,
) -> lura_eyre::Result<js::ProgramPart> {
  match top_level {
    TopLevel::Error(_) => bail!("errors are not supported"),
    TopLevel::Using(_) => bail!("using are not supported"),
    TopLevel::Command(_) => bail!("commands are not supported"),
    TopLevel::BindingGroup(group) => Ok(js::ProgramPart::decl(js::Decl::Func(js::Func {
      id: Some(js::Ident::new(group.name(db).to_string(db))),
      params: vec![],
      body: js::FuncBody(vec![]),
      generator: false,
      is_async: false,
    }))),
    TopLevel::ClassDecl(_) => bail!("class declarations are not supported"),
    TopLevel::InstanceDecl(_) => bail!("instance declarations are not supported"),
    TopLevel::TraitDecl(_) => bail!("trait declarations are not supported"),
    TopLevel::DataDecl(_) => bail!("data declarations are not supported"),
    TopLevel::TypeDecl(_) => bail!("type declarations are not supported"),
  }
}

pub fn dump_into_string<W: std::io::Write>(
  db: &dyn HirDb,
  source: HirSource,
  w: W,
) -> lura_eyre::Result<()> {
  let mut writer = resw::Writer::new(w);
  for top_level in source.contents(db) {
    let Ok(program_part) = transform_top_level(db, *top_level) else {
      continue;
    };

    writer.write_part(&program_part)?;
  }
  Ok(())
}
