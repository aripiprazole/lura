use lura_hir::source::HirSource;

/// Defines the [`thir_elab`] query.
///
/// This query is used to elaborate the HIR into the THIR. The
/// typed IR, is the THIR, and it does have semantic information,
/// containing the following:
///
/// - Inference/Typing information
/// - Coverage information
/// - Elaborated Case trees
///
/// [`thir_elab`]: typer::elaboration::thir_elab
#[salsa::tracked]
pub fn thir_elab(_db: &dyn crate::TyperDb, _source: HirSource) -> crate::thir::ThirSource {
  todo!()
}
