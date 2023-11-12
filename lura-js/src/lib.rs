use std::{path::Path, rc::Rc};

use deno_core::{error::AnyError, FastString};

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
