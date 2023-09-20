use lura_diagnostic::Diagnostics;
use lura_driver::{make_test, suite::*};
use lura_hir::lower::hir_lower;
use lura_syntax::parse;
use lura_typer::table::infer_type_table;
use lura_vfs::SourceFile;
use utils::create_package;

pub mod utils;

make_test!(typeclasses, |db, source, output| {
  let file = SourceFile::new(&db, "repl".into(), "Repl".into(), source);

  let src = parse(&db, file);
  let local = create_package(&db, src, "local");
  let hir = hir_lower(&db, local, src);
  let table = infer_type_table(&db, hir);

  debug_type_table(output, &db, table)?;

  // Concats the diagnostics of the various passes and prints them.
  //
  // Using the aridane crate, we can print the diagnostics in a nice way,
  // with colors and all.
  push_ariadne_errors(output, &[
    lura_syntax::parse::accumulated::<Diagnostics>(&db, file),
    hir_lower::accumulated::<Diagnostics>(&db, local, src),
    infer_type_table::accumulated::<Diagnostics>(&db, hir),
  ])?;

  Ok(())
});
