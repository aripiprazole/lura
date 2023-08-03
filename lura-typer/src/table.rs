use lura_hir::source::{expr::Expr, HirSource};

use crate::{
    infer::{Infer, InferCtx},
    ty::{modes, Ty},
};

#[salsa::tracked]
pub struct TypeTable {
    pub expressions: im::HashMap<Expr, Ty<modes::Ready>>,
}

/// Defines the [`infer_type_table`] query.
/// 
/// [`infer_type_table`]: crate::table::infer_type_table
#[salsa::tracked]
pub fn infer_type_table(db: &dyn crate::TyperDb, source: HirSource) -> TypeTable {
    let mut ctx = InferCtx {
        db,
        pkg: source.package(db),
        self_type: None,
        location: lura_hir::source::Location::CallSite,
        expressions: Default::default(),
        env: Default::default(),
    };

    // Infer the types of all expressions.
    for top_level in source.contents(db).iter().cloned() {
        top_level.infer(&mut ctx);
    }

    // Transforms the expressions into their sealed versions.
    let expressions = ctx
        .expressions
        .into_iter()
        .map(|(k, v)| (k, v.seal()))
        .collect();

    TypeTable::new(db, expressions)
}
