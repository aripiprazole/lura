#![allow(unused_variables)]
#![allow(dead_code)]

use lura_hir::{resolve::Definition, source::expr::Expr};

use crate::ty::*;

/// Represents the rigidness of the type variable. This is used to represent the rigidness of the
/// type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rigidness {
    Rigid,
    Flexible,
}

pub struct TyName {
    pub ty: String,
    pub rigidness: Rigidness,
}

pub struct TyCtxt {
    pub level: Level,
    pub type_names: im_rc::HashMap<String, TyName>,
    pub type_variables: Vec<(String, Ty<modes::Mut>)>,
}

type Sigma = Ty<modes::Mut>;
type Rho = Ty<modes::Mut>;
type Tau = Ty<modes::Mut>;

trait Infer {
    type Output;

    fn infer(self, ctxt: &mut InferCtxt) -> Self::Output;
}

struct MetaTv {}

struct InferCtxt<'tctx> {
    pub db: &'tctx dyn crate::TyperDb,
    pub ty_ctxt: TyCtxt,
}

impl TyCtxt {
    fn lookup(name: Definition) -> Sigma {
        todo!()
    }

    fn extend_env(&self, name: Definition, ty: Sigma) -> TyCtxt {
        todo!()
    }
}

impl<'tctx> InferCtxt<'tctx> {
    fn instantiate(&self, ty: Sigma) -> Rho {
        todo!()
    }

    fn quantify(&self, names: Vec<MetaTv>, ty: Rho) -> Sigma {
        todo!()
    }

    fn new_meta(&mut self) -> Tau {
        todo!()
    }

    fn new_skolem(&mut self) -> TyVar {
        todo!()
    }

    fn unify(&mut self, a: Tau, b: Tau) {
        todo!()
    }

    fn extend<F, U>(&mut self, name: Definition, ty: Sigma, f: F) -> U
    where
        F: for<'tctxn> FnOnce(&mut InferCtxt<'tctxn>) -> U,
    {
        let new_env = self.ty_ctxt.extend_env(name, ty);
        let old_env = std::mem::replace(&mut self.ty_ctxt, new_env);
        let result = f(self);
        self.ty_ctxt = old_env;
        result
    }
}

impl Ty<modes::Mut> {
    fn check(other: &Self, ctx: &mut InferCtxt) {
        todo!()
    }

    fn expr_check(expr: Expr, ctx: &mut InferCtxt) {
        todo!()
    }
}

impl InferCtxt<'_> {}

impl Infer for Expr {
    type Output = Ty<modes::Mut>;

    fn infer(self, ctxt: &mut InferCtxt) -> Self::Output {
        match self {
            Expr::Empty => todo!(),
            Expr::Error(_) => todo!(),
            Expr::Path(_) => todo!(),
            Expr::Literal(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::Ann(_) => todo!(),
            Expr::Abs(_) => todo!(),
            Expr::Match(_) => todo!(),
            Expr::Upgrade(_) => todo!(),
        }
    }
}
