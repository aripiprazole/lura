use std::{collections::HashMap, fmt::Display, path::PathBuf};

use fxhash::FxBuildHasher;
use itertools::Itertools;
use lura_diagnostic::{Diagnostics, Report};
use lura_driver::RootDb;
use lura_eyre::Context;
use lura_hir::{
  package::{HasManifest, Package, Version},
  source::HirSource,
};
use lura_hir_lowering::hir_lower;
use lura_syntax::Source;
use lura_vfs::SourceFile;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Config {
  pub name: String,
  pub version: String,
  #[serde(default = "source_folder_default")]
  pub source: String,
  pub dependencies: HashMap<String, Dependency, FxBuildHasher>,
}

#[derive(Clone)]
pub struct Manifest<'db> {
  pub db: &'db RootDb,
  pub root_folder: PathBuf,
  pub soruce_folder: PathBuf,
  pub config: Config,
  pub diagnostics: im::HashSet<Report, FxBuildHasher>,
}

impl<'db> Manifest<'db> {
  pub const FILE_NAME: &'static str = "lura.toml";

  pub fn load_in_folder(db: &'db RootDb, folder: PathBuf) -> lura_eyre::Result<Self> {
    let manifest_path = folder.join(Self::FILE_NAME);
    let manifest_content = std::fs::read_to_string(manifest_path.clone())
      .wrap_err_with(|| format!("Unable to find manifest file for folder {folder:?}"))?;
    let manifest: Config = toml::from_str(&manifest_content)?;
    let root_folder = manifest_path.parent().unwrap().to_path_buf();

    Ok(Self {
      db,
      soruce_folder: root_folder.join(&manifest.source),
      root_folder,
      config: manifest,
      diagnostics: Default::default(),
    })
  }

  pub fn read_file(&mut self, folder: PathBuf, path: PathBuf) -> lura_eyre::Result<Source> {
    let path = folder.join(path);
    let contents = std::fs::read_to_string(&path)
      .wrap_err_with(|| format!("Failed to read {}", path.display()))?;

    let name = folder.strip_prefix(&self.soruce_folder)?;
    let mut name = name
      .components()
      .map(|c| c.as_os_str().to_string_lossy().into_owned())
      .chain(std::iter::once(
        path
          .with_extension(String::default())
          .file_name()
          .unwrap()
          .to_string_lossy()
          .into_owned(),
      ))
      .join(".");

    // WORKAROUND: If it's the main file, simple use the package name. This
    // is a workaround for the fact that the main file is not in a folder
    // and thus the name would be empty.
    if name == "Main" {
      name = self.config.name.clone();
    }

    let file = SourceFile::new(self.db, path, name, contents);
    let cst = lura_syntax::parse(self.db, file);

    Ok(cst)
  }

  pub fn as_package(&mut self) -> lura_eyre::Result<Package> {
    let version = parse_version(&self.config.version)?;
    let source = self.root_folder.join(&self.config.source);

    let source = self.read_file(source, PathBuf::from("Main.lura"))?;

    Ok(Package::new(
      self.db,
      /* name    = */ self.config.name.clone(),
      /* version = */ version,
      /* sources = */ source,
      /* kind    = */ lura_hir::package::PackageKind::Binary,
      /* files   = */ Default::default(),
    ))
  }

  pub fn register_packages(&mut self) -> lura_eyre::Result<()> {
    for dependency in self.config.dependencies.values() {
      let folder = self.root_folder.join(&dependency.path).canonicalize()?;
      let mut manifest = Manifest::load_in_folder(self.db, folder)?;
      let package = manifest.as_package()?;

      self.db.register_package(package);
    }
    // Self-registering

    let package = self.as_package()?;
    self.db.register_package(package);

    Ok(())
  }

  pub fn resolve_all_files(&mut self) -> lura_eyre::Result<SourceMap> {
    // Clear diagnostics for new revision
    self.diagnostics = Default::default();

    let mut files = im::HashMap::default();
    for package in self.db.all_packages() {
      for file in package.all_files(self.db) {
        // Gets the diagnostics from the CST
        let diagnostics = file
          .errors(self.db)
          .iter()
          .map(|error| error.diagnostic(self.db))
          .map(Report::new)
          .collect_vec();

        // Add syntax errors' diagnostics to the manifest
        self.diagnostics.extend(diagnostics);

        let hir = hir_lower(self.db, package, file);
        let diagnostics = hir_lower::accumulated::<Diagnostics>(self.db, package, file);

        // Add HIR errors' diagnostics to the manifest
        self.diagnostics.extend(diagnostics);

        files.insert(package, hir);
      }
    }

    Ok(SourceMap(files))
  }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Dependency {
  pub path: String,
}

fn source_folder_default() -> String {
  "src".to_string()
}

fn parse_version(version: &str) -> lura_eyre::Result<Version> {
  let mut split = version.split('.');
  let major = split.next().unwrap();
  let minor = split.next().unwrap();
  let patch = split.next().unwrap();
  Ok(Version(major.parse()?, minor.parse()?, patch.parse()?))
}

/// A map of packages to their HIR sources
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceMap(im::HashMap<Package, HirSource, FxBuildHasher>);

impl SourceMap {
  /// Gets the HIR source for a given package
  pub fn get_in_db(&self, db: &RootDb, name: impl Display) -> Option<HirSource> {
    self.iter().find_map(|(package, hir_source)| {
      if package.name(db) == &name.to_string() {
        Some(*hir_source)
      } else {
        None
      }
    })
  }
}

impl std::ops::Deref for SourceMap {
  type Target = im::HashMap<Package, HirSource, FxBuildHasher>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
