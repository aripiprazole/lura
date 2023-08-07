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
    use lura_hir::{
        lower::hir_lower,
        package::{Package, PackageKind, Version}, fmt::HirFormatter,
    };
    use lura_syntax::Source;
    use lura_typer::table::infer_type_table;
    use lura_vfs::SourceFile;
    use salsa_2022::DebugWithDb;

    use crate::RootDb;

    const EXAMPLE: &[&str] = &[
        "data String",
        "data List (^a)",
        "Id : ^a. a -> a",
        "Main (args: List String) { Id args }",
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
        let source = lura_syntax::parse(&db, file);

        let local = create_package(&db, source, "local");
        let hir = hir_lower(&db, local, source);
        let table = infer_type_table(&db, hir);

        println!("Type Table:");
        for (expr, type_rep) in table.expressions(&db) {
            print!("  ");
            let expr = format!("{:#?}", expr.formatter().debug_all(&db))
                .split('\n')
                .collect::<Vec<_>>()
                .join("\n  ");
            println!("Expr: {expr}");
            print!("  ");
            println!("Type: {type_rep}");
            println!();
        }
    }

    fn create_package(db: &RootDb, source: Source, name: &str) -> Package {
        let version = Version(0, 0, 1);
        let kind = PackageKind::Binary;

        // Creates a new package with the given `name`, `version`, `source` and `kind`.
        let package = Package::new(db, name.into(), version, source, kind, vec![]);

        // Registers the package in the database.
        db.register_package(package)
    }
}
