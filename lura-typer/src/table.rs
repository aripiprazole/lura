use lura_hir::source::{declaration::Parameter, expr::Expr, top_level::TopLevel, HirSource};

use crate::{
    declaration::TypedDeclaration,
    infer::{Infer, InferCtx},
    ty::TypeRep,
};

#[salsa::tracked]
pub struct TypeTable {
    pub declarations: im::HashMap<TopLevel, TypedDeclaration>,
    pub parameters: im::HashMap<Parameter, TypeRep>,
    pub expressions: im::HashMap<Expr, TypeRep>,
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
        parameters: Default::default(),
        declarations: Default::default(),
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

    // Transforms the declarations into their sealed versions.
    let declarations = ctx
        .declarations
        .into_iter()
        .map(|(k, v)| (k, TypedDeclaration { type_rep: v.seal() }))
        .collect();

    // Transforms the parameters into their sealed versions.
    let parameters = ctx
        .parameters
        .into_iter()
        .map(|(k, v)| (k, v.seal()))
        .collect();

    TypeTable::new(db, declarations, parameters, expressions)
}
