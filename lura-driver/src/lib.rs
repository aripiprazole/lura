use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

use dashmap::{DashMap, DashSet};
use lura_hir::package::{HasManifest, Package};
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
                logs.lock()
                    .unwrap()
                    .push(format!("Event: {:?}", event.debug(self)));
            }
        }
    }
}

impl salsa::ParallelDatabase for RootDb {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
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
    use ariadne::Fmt;
    use itertools::Itertools;
    use lura_diagnostic::{Diagnostics, Report, TextRange};
    use lura_hir::{
        lower::hir_lower,
        package::{Package, PackageKind, Version},
        source::HirElement,
    };
    use lura_syntax::Source;
    use lura_typer::table::{infer_type_table, TypeTable};
    use lura_vfs::SourceFile;

    use crate::RootDb;

    const EXAMPLE: &[&str] = &[
        "public id : ^a. a -> a",
        "public data String {}",
        "public data List (^a) {}",
        "main (args: List String) {",
        "  id 10",
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

        // NOTE: We don't use the `create_diagnostic_report` function here, because we want to
        // concat the lowering errors and the type checking errors.
        let mut errors = hir_lower::accumulated::<Diagnostics>(&db, local, src);
        errors.extend(infer_type_table::accumulated::<Diagnostics>(&db, hir));

        create_diagnostic_report(errors);
        create_type_table_report(&db, table)
    }

    fn create_package(db: &RootDb, source: Source, name: &str) -> Package {
        let version = Version(0, 0, 1);
        let kind = PackageKind::Binary;

        // Creates a new package with the given `name`, `version`, `source` and `kind`.
        let package = Package::new(db, name.into(), version, source, kind, vec![]);

        // Registers the package in the database.
        db.register_package(package)
    }

    fn create_diagnostic_report(errors: Vec<Report>) {
        /// NOTE: For some reason, rust-analyzer catches [`content`] as
        /// not used, but it's used when displaying the errors.
        ///
        /// So we suppress it, by using `#[allow(dead_code)]`
        #[derive(Debug, Clone)]
        #[allow(dead_code)]
        struct FileDescriptor {
            path: String,
            content: String,
        }

        impl Eq for FileDescriptor {}

        impl PartialEq for FileDescriptor {
            fn eq(&self, other: &Self) -> bool {
                self.path == other.path
            }
        }

        impl std::hash::Hash for FileDescriptor {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.path.hash(state);
            }
        }

        let mut diagnostics = std::collections::HashMap::new();
        for report in errors {
            diagnostics
                .entry(FileDescriptor {
                    path: report.file_name().to_string(),
                    content: report.location().unwrap().source().to_string(),
                })
                .or_insert_with(std::collections::HashSet::new)
                .insert(report.clone());
        }

        for (file, diagnostics) in diagnostics {
            use ariadne::ReportKind::*;

            type Span = (String, std::ops::Range<usize>);
            ariadne::Report::<Span>::build(Error, file.path.clone(), 0)
                .with_code("E0001")
                .with_message(format!("found {} errors", diagnostics.len()))
                .with_config(
                    ariadne::Config::default()
                        .with_char_set(ariadne::CharSet::Unicode)
                        .with_label_attach(ariadne::LabelAttach::Start),
                )
                .with_labels(diagnostics.into_iter().map(|d| {
                    let kind = match d.error_kind() {
                        lura_diagnostic::ErrorKind::ParseError => "parse error",
                        lura_diagnostic::ErrorKind::TypeError => "type error",
                        lura_diagnostic::ErrorKind::ResolutionError => "resolution error",
                        lura_diagnostic::ErrorKind::RuntimeError => "runtime error",
                        lura_diagnostic::ErrorKind::InternalError(_) => "internal error",
                    };
                    let message = d.markdown_text();
                    ariadne::Label::new((d.file_name(), d.range().unwrap()))
                        .with_color(ariadne::Color::Red)
                        .with_message(format!("{kind}: {message}").fg(ariadne::Color::Red))
                }))
                .finish()
                .eprint((file.path, ariadne::Source::from(&file.content)))
                .unwrap();
        }
    }

    fn create_type_table_report(db: &RootDb, type_table: TypeTable) {
        let expressions = type_table.expressions(db);
        let file_type_tables = expressions.iter().group_by(|(expression, _)| {
            let location = expression.location(db);
            let file_name = location.file_name().to_string();
            let contents = location.source().to_string();
            (file_name, contents)
        });

        let mut colors = ariadne::ColorGenerator::new();

        for ((file, contents), type_table) in file_type_tables.into_iter() {
            type Span = (String, std::ops::Range<usize>);
            ariadne::Report::<Span>::build(ariadne::ReportKind::Advice, file.clone(), 0)
                .with_code("E0001")
                .with_message("type table information")
                .with_config(
                    ariadne::Config::default()
                        .with_char_set(ariadne::CharSet::Unicode)
                        .with_label_attach(ariadne::LabelAttach::Start),
                )
                .with_labels(type_table.into_iter().map(|(expr, type_rep)| {
                    let location = expr.location(db);
                    let range = location.start().0..location.end().0;

                    ariadne::Label::new((file.clone(), range))
                        .with_color(colors.next())
                        .with_message(format!("has type {type_rep}").fg(ariadne::Color::White))
                }))
                .finish()
                .print((file.clone(), ariadne::Source::from(&contents)))
                .unwrap();
        }
    }
}
