use std::{
  path::PathBuf,
  sync::{atomic::AtomicBool, Arc, RwLock},
};

use dashmap::DashMap;
use lura_driver::RootDb;
use lura_hir::{
  package::{Package, PackageKind, Version},
  source::HirSource,
};
use lura_hir_lowering::hir_lower;
use lura_syntax::Source;
use lura_vfs::SourceFile;
use ropey::Rope;
use tower_lsp::lsp_types::Url;

use crate::backend::{Backend, TextDocumentItem};

/// This struct represents the workspace. It is used to store the files that are
/// opened in the editor.
#[derive(Default)]
pub struct Workspace {
  pub package: RwLock<Option<Package>>,

  pub file_map: Arc<DashMap<String, Rope>>,

  /// Tree map is used to store the syntax tree for each file. This is used
  /// to get the syntax tree for a file when the client requests it.
  pub tree_map: Arc<DashMap<String, Source>>,

  /// Hir map is used to store the hir for each file. This is used to get the
  /// hir for a file when the client requests it.
  pub hir_map: Arc<DashMap<String, HirSource>>,

  /// This is used to prevent the server from sending diagnostics before the
  /// workspace is ready, and this is needed because the server will send
  /// diagnostics for all files when it starts up.
  pub ready: AtomicBool,
}

impl Backend {
  pub fn get_or_create_file(&self, item: TextDocumentItem) -> HirSource {
    self
      .workspace
      .file_map
      .insert(item.uri.to_string(), item.text.clone().into());

    let path = PathBuf::from(item.uri.to_string());

    // TODO:
    let file = SourceFile::new(&*self.db(), path, item.uri.to_string(), item.text.clone());
    let source = lura_syntax::parse(&*self.db(), file);

    let package = create_default_package(&self.db(), source, "main");

    let hir = hir_lower(&*self.db(), package, source);
    self.workspace.hir_map.insert(item.uri.to_string(), hir);
    hir
  }

  pub fn hir_source(&self, uri: Url) -> Option<HirSource> {
    self
      .workspace
      .hir_map
      .get(&uri.to_string())
      .map(|value| *value)
  }
}

fn create_default_package(db: &RootDb, source: Source, name: &str) -> Package {
  let version = Version(0, 0, 1);
  let kind = PackageKind::Binary;

  // Creates a new package with the given `name`, `version`, `source` and `kind`.
  let package = Package::new(db, name.into(), version, source, kind, vec![]);

  // Registers the package in the database.
  db.register_package(package)
}
