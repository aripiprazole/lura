use std::{
  path::PathBuf,
  sync::{Arc, Mutex},
};

use dashmap::{DashMap, DashSet};
use lura_hir::{
  package::{HasManifest, Package},
  primitives::{PrimitiveBag, PrimitiveProvider},
};
use salsa::DebugWithDb;

/// Defines watcher strategies for [`RootDb`].
pub mod watcher;

pub mod rename;

extern crate salsa_2022 as salsa;

/// The root database implementation for the Lura frontend, after the frontend, there's the
/// codegen backend, and it's not mean't to use incremental compilation, so it's not part of the
/// database.
///
/// The database is used to store the results of the various passes of the compiler, and to
/// invalidate them when the input changes.
///
/// The database is also used to store the logs of the compiler, so that they can be printed.
#[salsa::db(
  lura_hir::Jar,
  lura_vfs::Jar,
  lura_syntax::Jar,
  lura_diagnostic::Jar,
  lura_typer::Jar
)]
#[derive(Default)]
pub struct RootDb {
  /// Salsa storage, used to store the results of the various passes of the compiler.
  storage: salsa::Storage<RootDb>,
  packages: Arc<DashSet<Package>>,

  primitives: Arc<PrimitiveBag>,
  files: DashMap<PathBuf, lura_vfs::SourceFile>,
  logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl RootDb {
  /// Registers a package in the database.
  pub fn register_package(&self, package: Package) -> Package {
    self.packages.insert(package);
    package
  }
}

impl PrimitiveProvider for RootDb {
  /// Gets primitives lazily
  fn primitives(&self) -> Arc<PrimitiveBag> {
    self.primitives.clone()
  }
}

impl HasManifest for RootDb {
  fn all_packages(&self) -> Vec<Package> {
    self.packages.iter().map(|p| *p).collect::<Vec<_>>()
  }
}

impl salsa::Database for RootDb {
  fn salsa_event(&self, event: salsa::Event) {
    // Log interesting events, if logging is enabled
    if let Some(logs) = &self.logs {
      // don't log boring events
      if let salsa::EventKind::WillExecute { .. } = event.kind {
        logs.lock().unwrap().push(format!("Event: {:?}", event.debug(self)));
      }
    }
  }
}

impl salsa::ParallelDatabase for RootDb {
  fn snapshot(&self) -> salsa::Snapshot<Self> {
    salsa::Snapshot::new(Self {
      primitives: self.primitives.clone(),
      storage: self.storage.snapshot(),
      logs: self.logs.clone(),
      files: self.files.clone(),
      packages: self.packages.clone(),
    })
  }
}

#[cfg(test)]
#[allow(clippy::unnecessary_mut_passed)]
mod tests {
  use std::collections::HashMap;

  use ariadne::{Color, Fmt};
  use itertools::Itertools;
  use lura_ariadne::AriadneReport;
  use lura_diagnostic::{Diagnostics, TextRange};
  use lura_hir::{
    fmt::HirFormatter,
    lower::hir_lower,
    package::{Package, PackageKind, Version},
    source::HirElement,
  };
  use lura_syntax::Source;
  use lura_typer::table::{infer_type_table, TypeTable};
  use lura_vfs::SourceFile;
  use salsa_2022::DebugWithDb;

  use crate::RootDb;

  const EXAMPLE: &[&str] = &[
    // Generalised functions
    "public id : ^a. a -> a",
    // Data type tests
    "public data List (^a) {}",
    // Type class tests
    "public trait Show (^a) {}",
    "println : [Show a] => a -> ()",
    "putStrLn : [Show a] => a -> ()",
    "putStrLn = println",
    // Instances
    "instance Show of String {}",
    "instance Show (^a) of (List a) {}",
    // Defines test functions to test
    // instantiation.
    "fa (args: List String)",
    "fb (args: List Int)",
    // Defines a main function
    "main (args: List String) {",
    // -- Concrete application tests
    "  let a = fa args",
    "  let b = fb args",
    // -- Id tests
    "  let x = id 10",
    "  let y = id \"hello\"",
    // -- Trait tests
    "  let z = println \"string\"",
    "  let g = println 10",
    "  let f = println args",
    // -- Return tests
    "  y",
    "}",
  ];

  /// This is an end-to-end test of the pipeline, from parsing to type checking/compiling, etc,
  /// it's not a unit test.
  ///
  /// This test is meant to be run with `cargo test -- --nocapture` so that the logs are printed.
  #[test]
  fn pipeline_tests() {
    let db = RootDb::default();
    let source = EXAMPLE.join("\n");

    let file = SourceFile::new(&db, "repl".into(), "Repl".into(), source);
    let src = lura_syntax::parse(&db, file);

    let local = create_package(&db, src, "local");
    let hir = hir_lower(&db, local, src);
    let table = infer_type_table(&db, hir);

    debug_type_table_report(&db, table);

    // Concats the diagnostics of the various passes and prints them.
    //
    // Using the aridane crate, we can print the diagnostics in a nice way,
    // with colors and all.
    AriadneReport::default()
      .expand(lura_syntax::parse::accumulated::<Diagnostics>(&db, file))
      .expand(hir_lower::accumulated::<Diagnostics>(&db, local, src))
      .expand(infer_type_table::accumulated::<Diagnostics>(&db, hir))
      .eprint()
      .unwrap();
  }

  fn create_package(db: &RootDb, source: Source, name: &str) -> Package {
    let version = Version(0, 0, 1);
    let kind = PackageKind::Binary;

    // Creates a new package with the given `name`, `version`, `source` and `kind`.
    let package = Package::new(db, name.into(), version, source, kind, vec![]);

    // Registers the package in the database.
    db.register_package(package)
  }

  fn debug_type_table_report(db: &RootDb, type_table: TypeTable) {
    let expressions = type_table.expressions(db);

    let mut parameters = HashMap::new();
    for (parameter, type_rep) in type_table.parameters(db) {
      let location = parameter.location(db);
      let file_name = location.file_name().to_string();

      parameters
        .entry(file_name)
        .or_insert_with(Vec::new)
        .push((parameter, type_rep));
    }

    let file_type_tables = expressions.iter().group_by(|(expression, _)| {
      let location = expression.location(db);
      let file_name = location.file_name().to_string();
      let contents = location.source().to_string();
      (file_name, contents)
    });

    for ((file, contents), type_table) in file_type_tables.into_iter() {
      // Get all the parameters contained in the current file,
      // to be able to print them in the report.
      let parameters = parameters.get(&file).cloned().unwrap_or_default();

      type Span = (String, std::ops::Range<usize>);
      ariadne::Report::<Span>::build(ariadne::ReportKind::Advice, file.clone(), 0)
        .with_message("type table information")
        .with_note("These are generated types, they are not part of the source code.")
        .with_config(
          ariadne::Config::default()
            .with_cross_gap(true)
            .with_char_set(ariadne::CharSet::Unicode)
            .with_label_attach(ariadne::LabelAttach::Start),
        )
        .with_labels(parameters.into_iter().map(|(parameter, type_rep)| {
          let location = parameter.location(db);
          let range = location.start().0..location.end().0;

          // TODO: use a better representation for types
          let type_rep = type_rep.to_string();

          // Build pattern string
          let pattern = parameter.binding(db);
          let pattern = pattern.formatter();
          let pattern = format!("{:?}", pattern.debug_all(db));

          ariadne::Label::new((file.clone(), range))
            .with_color(Color::Yellow)
            .with_message(format!("parameter {} has type {}", pattern.fg(Color::Yellow), type_rep.fg(Color::Red)))
        }))
        .with_labels(type_table.into_iter().map(|(expr, type_rep)| {
          let location = expr.location(db);
          let range = location.start().0..location.end().0;

          // TODO: use a better representation for types
          let type_rep = type_rep.to_string();

          ariadne::Label::new((file.clone(), range))
            .with_color(Color::Green)
            .with_message(format!("has type {}", type_rep.fg(Color::Green)))
        }))
        .finish()
        .print((file.clone(), ariadne::Source::from(&contents)))
        .unwrap();
    }
  }
}
