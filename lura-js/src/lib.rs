use std::{path::Path, rc::Rc};

use deno_core::{error::AnyError, FastString};
use eyre::bail;
use lura_hir::solver::Reference;
use lura_hir::source::declaration::Declaration;
use lura_hir::source::expr::Callee;
use lura_hir::source::pattern::Pattern;
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
    name.push_str("_");
  }
  name
}

pub fn compile_path(db: &dyn HirDb, path: Reference) -> js::Ident {
  let definition = path.definition(db);
  let mangled_name = definition_mangle_name(db, definition);
  js::Ident::new(mangled_name)
}

pub fn compile_callee<'a>(db: &dyn HirDb, callee: Callee) -> js::Expr<'a> {
  match callee {
    Callee::Array => todo!(),
    Callee::Tuple => todo!(),
    Callee::Unit => todo!(),
    Callee::Pure => todo!(),
    Callee::Do => todo!(),
    Callee::Reference(path) => js::Expr::Ident(compile_path(db, path)),
    Callee::Expr(expr) => compile_expr(db, *expr),
  }
}

pub fn compile_expr<'a>(db: &dyn HirDb, expr: Expr) -> js::Expr<'a> {
  match expr {
    Expr::Empty => todo!(),
    Expr::Error(_) => todo!(),
    Expr::Path(reference) => js::Expr::Ident(compile_path(db, reference)),
    Expr::Meta(_) => todo!(),
    Expr::Literal(_) => todo!(),
    Expr::Call(call) => js::Expr::Call(js::CallExpr {
      callee: compile_callee(db, call.callee).into(),
      arguments: call
        .arguments
        .into_iter()
        .map(|argument| compile_expr(db, argument))
        .collect(),
    }),
    Expr::Ann(_) => todo!(),
    Expr::Abs(_) => todo!(),
    Expr::Match(_) => todo!(),
    Expr::Upgrade(_) => todo!(),
  }
}

pub struct PatternMatching {
  pub scrutinee: Vec<Expr>,
  pub patterns: im::HashMap<Vec<Pattern>, Expr>,
}

pub fn compile_pattern_matching<'a>(db: &dyn HirDb, matching: PatternMatching) -> js::Expr<'a> {
  todo!()
}

pub fn compile_top_level(db: &dyn HirDb, top_level: TopLevel) -> eyre::Result<js::ProgramPart> {
  match top_level {
    TopLevel::Error(_) => bail!("errors are not supported"),
    TopLevel::Using(_) => bail!("using are not supported"),
    TopLevel::Command(_) => bail!("commands are not supported"),
    TopLevel::BindingGroup(group) => Ok(js::ProgramPart::Decl(js::Decl::Func(js::Func {
      id: Some(js::Ident::new(group.name(db).to_string(db))),
      params: vec![],
      body: js::FuncBody(vec![js::ProgramPart::Stmt(js::Stmt::Return(Some(
        compile_pattern_matching(db, PatternMatching {
          scrutinee: group
            .clauses(db)
            .into_iter()
            .map(|clause| clause.arguments(db))
            .collect(),
          patterns: group
            .clauses(db)
            .into_iter()
            .map(|clause| (clause.arguments(db), clause.value(db)))
            .collect(),
        }),
      )))]),
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
) -> eyre::Result<()> {
  let mut writer = resw::Writer::new(w);
  for top_level in source.contents(db) {
    let Ok(program_part) = compile_top_level(db, *top_level) else {
      continue;
    };

    writer.write_part(&program_part)?;
  }
  Ok(())
}
