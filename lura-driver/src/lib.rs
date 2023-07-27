use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
    time::Duration,
};

use crossbeam_channel::Sender;
use dashmap::{mapref::entry::Entry, DashMap, DashSet};
use eyre::Context;
use lura_hir::package::{HasManifest, Package};
use notify_debouncer_mini::{
    new_debouncer,
    notify::{RecommendedWatcher, RecursiveMode},
    DebounceEventResult, Debouncer,
};
use salsa::DebugWithDb;

/// Defines watcher strategies for [`RootDb`].
pub mod watcher;

extern crate salsa_2022 as salsa;

/// The root database implementation for the Lura frontend, after the frontend, there's the
/// codegen backend, and it's not mean't to use incremental compilation, so it's not part of the
/// database.
///
/// The database is used to store the results of the various passes of the compiler, and to
/// invalidate them when the input changes.
///
/// The database is also used to store the logs of the compiler, so that they can be printed.
#[salsa::db(lura_hir::Jar, lura_vfs::Jar, lura_syntax::Jar, lura_diagnostic::Jar)]
pub struct RootDb {
    /// Salsa storage, used to store the results of the various passes of the compiler.
    storage: salsa::Storage<RootDb>,
    packages: Arc<DashSet<Package>>,

    files: DashMap<PathBuf, lura_vfs::SourceFile>,
    watcher: Arc<Mutex<Debouncer<RecommendedWatcher>>>,
    logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl RootDb {
    /// Creates a new [`RootDb`].
    pub fn new(tx: Sender<DebounceEventResult>) -> Self {
        let storage = Default::default();

        Self {
            storage,
            packages: Default::default(),
            logs: Default::default(),
            files: DashMap::new(),
            watcher: Arc::new(Mutex::new(
                new_debouncer(Duration::from_secs(1), None, tx).unwrap(),
            )),
        }
    }

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
            watcher: self.watcher.clone(),
            packages: self.packages.clone(),
        })
    }
}

impl lura_vfs::VfsDb for RootDb {
    fn input(&self, path: PathBuf) -> eyre::Result<lura_vfs::SourceFile> {
        let path = path
            .canonicalize()
            .wrap_err_with(|| format!("Failed to read {}", path.display()))?;

        Ok(match self.files.entry(path.clone()) {
            // If the file already exists in our cache then just return it.
            Entry::Occupied(entry) => *entry.get(),
            // If we haven't read this file yet set up the watch, read the
            // contents, store it in the cache, and return it.
            Entry::Vacant(entry) => {
                // Set up the watch before reading the contents to try to avoid
                // race conditions.
                let watcher = &mut *self.watcher.lock().unwrap();
                watcher
                    .watcher()
                    .watch(&path, RecursiveMode::NonRecursive)
                    .wrap_err_with(|| {
                        format!("Failed to create a watcher on {}", path.display())
                    })?;

                let contents = std::fs::read_to_string(&path)
                    .wrap_err_with(|| format!("Failed to read {}", path.display()))?;

                *entry.insert(lura_vfs::SourceFile::new(self, path, contents))
            }
        })
    }
}

#[cfg(test)]
#[allow(clippy::unnecessary_mut_passed)]
mod tests {
    use std::path::PathBuf;

    use lura_diagnostic::{Diagnostics, Offset};
    use lura_hir::{
        completions::{completions, Position},
        lower::hir_lower,
        package::{Package, PackageKind, Version},
        reference::ReferenceWalker,
    };
    use lura_syntax::Source;
    use lura_vfs::SourceFile;

    use crate::RootDb;

    const EXAMPLE: &str = "Main (args: List String) { args }";

    /// This is an end-to-end test of the pipeline, from parsing to type checking/compiling, etc,
    /// it's not a unit test.
    ///
    /// This test is meant to be run with `cargo test -- --nocapture` so that the logs are printed.
    #[test]
    fn pipeline_tests() {
        let (tx, _) = crossbeam_channel::unbounded();
        let db = RootDb::new(tx);

        let file = SourceFile::new(&db, PathBuf::from("repl"), EXAMPLE.into());
        let source = lura_syntax::parse(&db, file);

        let local = create_package(&db, source, "local");
        let hir = hir_lower(&db, local, source);

        let diagnostics = hir_lower::accumulated::<Diagnostics>(&db, local, source);

        println!(
            "{:#?}",
            completions(&db, hir, "ar".into(), Position { offset: Offset(29) })
        );
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
