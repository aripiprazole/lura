use lura_syntax::Source;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Version(u8, u8, u8);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PackageKind {
    Library,
    Binary,
}

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
pub fn all_package_files(db: &dyn crate::HirDb, package: Package) -> Vec<Source> {
    let mut files = vec![package.main(db)];
    files.extend(package.files(db));
    files
}

impl Package {
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

pub trait HasManifest {
    fn all_packages(&self) -> &[Package];
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
