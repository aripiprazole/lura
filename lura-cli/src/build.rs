use std::{collections::HashMap, path::PathBuf};

use eyre::Context;
use lura_diagnostic::{Diagnostics, Report};
use lura_driver::RootDb;
use lura_hir::{
    lower::hir_lower,
    package::{HasManifest, Package, Version},
    source::HirSource,
};
use lura_syntax::Source;
use lura_vfs::SourceFile;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Config {
    pub name: String,
    pub version: String,
    #[serde(default = "source_folder_default")]
    pub source: String,
    pub dependencies: HashMap<String, Dependency>,
}

#[derive(Clone)]
pub struct Manifest<'db> {
    pub db: &'db RootDb,
    pub root_folder: PathBuf,
    pub config: Config,
    pub diagnostics: im::HashSet<Report>,
}

impl<'db> Manifest<'db> {
    pub const FILE_NAME: &str = "lura.toml";

    pub fn load_in_folder(db: &'db RootDb, folder: PathBuf) -> eyre::Result<Self> {
        let manifest_path = folder.join(Self::FILE_NAME);
        let manifest_content = std::fs::read_to_string(manifest_path.clone())
            .wrap_err_with(|| format!("Unable to find manifest file for folder {folder:?}"))?;
        let manifest: Config = toml::from_str(&manifest_content)?;

        Ok(Self {
            db,
            root_folder: manifest_path.parent().unwrap().to_path_buf(),
            config: manifest,
            diagnostics: Default::default(),
        })
    }

    pub fn read_file(&self, folder: PathBuf, path: PathBuf) -> eyre::Result<Source> {
        let path = folder.join(path);
        let contents = std::fs::read_to_string(&path)
            .wrap_err_with(|| format!("Failed to read {}", path.display()))?;

        let name = path
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();

        let file = SourceFile::new(self.db, path, name, contents);
        let cst = lura_syntax::parse(self.db, file);
        Ok(cst)
    }

    pub fn manifest(&self, folder: PathBuf, manifest: Manifest) -> eyre::Result<Package> {
        let version = parse_version(&manifest.config.version)?;
        let source = folder.join(manifest.config.source);

        let source = self.read_file(source, PathBuf::from("Main.lura"))?;

        Ok(Package::new(
            self.db,
            manifest.config.name,
            version,
            source,
            lura_hir::package::PackageKind::Binary,
            Default::default(),
        ))
    }

    pub fn register_packages(&self) -> eyre::Result<()> {
        for dependency in self.config.dependencies.values() {
            let folder = self.root_folder.join(&dependency.path);
            let manifest = Manifest::load_in_folder(self.db, folder.clone())?;
            let package = self.manifest(folder, manifest)?;

            self.db.register_package(package);
        }
        // Self-registering

        let package = self.manifest(self.root_folder.clone(), self.clone())?;
        self.db.register_package(package);

        Ok(())
    }

    pub fn resolve_all_files(&mut self) -> eyre::Result<im::HashMap<Package, HirSource>> {
        // Clear diagnostics for new revision
        self.diagnostics = Default::default();

        let mut files = im::HashMap::new();
        for package in self.db.all_packages() {
            for file in package.all_files(self.db) {
                let hir = hir_lower(self.db, package, file);
                let diagnostics = hir_lower::accumulated::<Diagnostics>(self.db, package, file);
                self.diagnostics.extend(diagnostics.into_iter());

                files.insert(package, hir);
            }
        }

        Ok(files)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Dependency {
    pub path: String,
}

fn source_folder_default() -> String {
    "src".to_string()
}

fn parse_version(version: &str) -> eyre::Result<Version> {
    let mut split = version.split('.');
    let major = split.next().unwrap();
    let minor = split.next().unwrap();
    let patch = split.next().unwrap();
    Ok(Version(major.parse()?, minor.parse()?, patch.parse()?))
}
