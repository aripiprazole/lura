use lura_syntax::Source;

use crate::{package::Package, source::HirSource};

/// Controls the lowering of a source file into the HIR. It's a trait, so we can
/// have this separated from this crate.
///
/// This trait is implemented by the `lura-hir-lowering` crate, and it's used by the
/// `lura-driver` crate.
pub trait HirLowering {
  /// Declares all the definitions in the source file.
  fn hir_declare(&self, pkg: Package, src: Source) -> HirSource;

  /// Lower the source file into the HIR.
  fn hir_lower(&self, pkg: Package, src: Source) -> HirSource;
}
