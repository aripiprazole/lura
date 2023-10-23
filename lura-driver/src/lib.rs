use std::{
  path::PathBuf,
  sync::{Arc, Mutex},
};

use dashmap::{DashMap, DashSet};
use lura_hir::{
  lowering::HirLowering,
  package::{HasManifest, Package},
  primitives::{PrimitiveBag, PrimitiveProvider},
};
use salsa::DebugWithDb;

/// Defines watcher strategies for [`RootDb`].
pub mod watcher;

pub mod rename;

#[allow(unused)]
pub mod suite;

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
  lura_typer::Jar,
  lura_hir_lowering::Jar
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

/// Bridges the [`RootDb`] with the [`lura_hir_lowering::HirLowering`] trait.
impl HirLowering for RootDb {
  fn hir_declare(&self, pkg: Package, src: lura_syntax::Source) -> lura_hir::source::HirSource {
    lura_hir_lowering::hir_declare(self, pkg, src)
  }

  fn hir_lower(&self, pkg: Package, src: lura_syntax::Source) -> lura_hir::source::HirSource {
    lura_hir_lowering::hir_lower(self, pkg, src)
  }
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
        logs
          .lock()
          .unwrap()
          .push(format!("Event: {:?}", event.debug(self)));
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
