use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
    time::Duration,
};

use crossbeam_channel::Sender;
use dashmap::{mapref::entry::Entry, DashMap};
use eyre::Context;
use lura_hir::package::HasManifest;
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

    files: DashMap<PathBuf, lura_vfs::SourceFile>,
    watcher: Arc<Mutex<Debouncer<RecommendedWatcher>>>,
    logs: Option<Arc<Mutex<Vec<String>>>>,
}

impl RootDb {
    pub fn new(tx: Sender<DebounceEventResult>) -> Self {
        let storage = Default::default();
        Self {
            storage,
            logs: Default::default(),
            files: DashMap::new(),
            watcher: Arc::new(Mutex::new(
                new_debouncer(Duration::from_secs(1), None, tx).unwrap(),
            )),
        }
    }
}

impl HasManifest for RootDb {
    fn all_packages(&self) -> &[lura_hir::package::Package] {
        todo!()
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

    use lura_hir::package::{Package, PackageKind};
    use lura_syntax::Source;
    use lura_vfs::SourceFile;
    use salsa_2022::DebugWithDb;

    use crate::RootDb;

    const EXAMPLE: &str = "using Std.IO";

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
        let hir = lura_hir::lower::hir_lower(&db, local, source);

        println!("{:#?}", hir.debug_all(&db));
    }

    fn create_package(db: &RootDb, source: Source, name: &str) -> Package {
        Package::new(
            db,
            name.into(),
            (0, 0, 1).into(),
            source,
            PackageKind::Binary,
            vec![],
        )
    }
}
