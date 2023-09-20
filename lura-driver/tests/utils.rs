use lura_driver::RootDb;
use lura_hir::package::{Package, PackageKind, Version};
use lura_syntax::Source;

pub fn create_package(db: &RootDb, source: Source, name: &str) -> Package {
  let version = Version(0, 0, 1);
  let kind = PackageKind::Binary;

  // Creates a new package with the given `name`, `version`, `source` and `kind`.
  let package = Package::new(db, name.into(), version, source, kind, vec![]);

  // Registers the package in the database.
  db.register_package(package)
}
