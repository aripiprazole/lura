use std::fmt::Debug;

use lura_syntax::Source;

/// Defines a version for a package. That can receive a tuple of `(major, minor, patch)`.
///
/// # Examples
/// ```
/// use lura_hir::package::Version;
///
/// let version = Version::from((0, 1, 0));
/// assert_eq!(version, Version(0, 1, 0));
///
/// let version = Version::from((1, 0, 0));
/// assert_eq!(version, Version(1, 0, 0));
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Version(pub u8, pub u8, pub u8);

impl Debug for Version {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let Version(major, minor, patch) = self;

    write!(f, "{}.{}.{}", major, minor, patch)
  }
}

/// Defines a kind of package, that can be either a library or a binary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PackageKind {
  /// Libraries can be imported by other packages, and can be used to create binaries. It's
  /// intended to enable options to export libraries as `.js`, object files or either static or
  /// dynamic libraries.
  Library,

  /// Binaries are packages that can be executed by the user. It's intended to be used to create
  /// executables.
  Binary,
}

/// Defines the main entry point for a package in the *Lura Build System*. It's intended to be
/// used to create a package, and then, add files to it, incrementally.
#[salsa::input]
pub struct Package {
  #[return_ref]
  pub name: String,
  pub version: Version,
  pub main: Source,
  pub kind: PackageKind,
  pub files: Vec<Source>,
}

#[salsa::tracked]
impl Package {
  /// Defines the [`Package::all_files`] query.
  ///
  /// It does takes all files from the package, including the main file, and return it as a
  /// vector.
  #[salsa::tracked]
  pub fn all_files(self, db: &dyn crate::HirDb) -> Vec<Source> {
    // This is spplited into two functions because of a bug in salsa, that causes the `self`
    // keyword being replaced for some kind of `__salsa_self` variable, that is not recognized
    // in macro invocations.
    //
    // And the `vec![]` is a macro invocation.
    let main_file = self.main(db);

    let mut files = vec![main_file];
    files.extend(self.files(db));
    files
  }
}

impl Package {
  /// Makes [`self`] depend on [`another`] package troughough a dependency graph. This is
  /// necessary to make the resolver work.
  pub fn depends_on(&self, graph: &mut DepGraph, another: Package) {
    let another_idx = graph.create_package(another);
    let this_idx = graph.create_package(*self);

    // Validate if the package isn't a binary package
    if let PackageKind::Binary = another.kind(graph.db) {
      panic!("Cannot depend on a binary package");
    }

    graph.relations.add_edge(this_idx, another_idx, *self);
  }
}

/// Defines a not incremental clause for searching all packages, as it's not intendeed to be
/// "incremented", or be hot. Every change on this can trigger a full rebuild.
pub trait HasManifest {
  /// Finds all the packages in the current workspace.
  fn all_packages(&self) -> Vec<Package>;
}

pub struct DepGraph<'db> {
  pub db: &'db dyn crate::HirDb,
  pub relations: petgraph::Graph<Package, Package>,
}

impl<'db> DepGraph<'db> {
  pub fn create_package(&mut self, package: Package) -> petgraph::graph::NodeIndex {
    self.relations.add_node(package)
  }
}

impl From<(u8, u8, u8)> for Version {
  fn from((major, minor, patch): (u8, u8, u8)) -> Self {
    Self(major, minor, patch)
  }
}
