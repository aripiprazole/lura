#![allow(unused_variables)]
#![allow(dead_code)]

use lura_hir::{
    resolve::{Definition, Reference},
    source::{
        expr::{Callee, Expr},
        literal::Literal,
        pattern::Pattern,
        stmt::{Block, Stmt},
        top_level::TopLevel,
        type_rep::TypeRep,
        Spanned,
    },
};

use crate::ty::*;

/// Represents the rigidness of the type variable. This is used to represent the rigidness of the
/// type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rigidness {
    Rigid,
    Flexible,
}

#[derive(Clone)]
pub struct TyName {
    pub ty: String,
    pub rigidness: Rigidness,
}

#[derive(Clone)]
pub struct TyCtxt {
    pub level: Level,
    pub type_names: im_rc::HashMap<String, TyName>,
    pub type_variables: im_rc::HashMap<Definition, Sigma>,
}

type Sigma = Ty<modes::Mut>;
type Rho = Ty<modes::Mut>;
type Tau = Ty<modes::Mut>;

trait Infer {
    type Output;

    fn infer(self, ctxt: &mut InferCtxt) -> Self::Output;
}

trait Check {
    type Output;

    fn check(self, ty: Tau, ctxt: &mut InferCtxt) -> Self::Output;
}

impl Infer for Pattern {
    type Output = Ty<modes::Mut>;

    fn infer(self, ctxt: &mut InferCtxt) -> Self::Output {
        match self {
            Pattern::Empty => todo!(),
            Pattern::Literal(_) => todo!(),
            Pattern::Wildcard(_) => todo!(),
            Pattern::Rest(_) => todo!(),
            Pattern::Error(_) => todo!(),
            Pattern::Constructor(_) => todo!(),
            Pattern::Binding(_) => todo!(),
        }
    }
}

impl Infer for Callee {
    type Output = Ty<modes::Mut>;

    fn infer(self, ctxt: &mut InferCtxt) -> Self::Output {
        match self {
            Callee::Array => todo!(),
            Callee::Tuple => todo!(),
            Callee::Unit => todo!(),
            Callee::Pure => todo!(),
            Callee::Do => todo!(),
            Callee::Reference(_) => todo!(),
            Callee::Expr(_) => todo!(),
        }
    }
}

impl Infer for Expr {
    type Output = Ty<modes::Mut>;

    fn infer(self, ctxt: &mut InferCtxt) -> Self::Output {
        match self {
            Expr::Empty => Tau::Primary(Primary::Error),
            Expr::Error(_) => Tau::Primary(Primary::Error),
            Expr::Path(reference) => ctxt.reference(reference),
            Expr::Literal(literal) => literal.infer(ctxt),
            Expr::Call(call) => {
                let parameters = call
                    .arguments(ctxt.db)
                    .into_iter()
                    .map(|argument| argument.infer(ctxt))
                    .collect::<Vec<_>>();

                let ty = ctxt.new_meta();
                let pi = Ty::from_pi(parameters.into_iter(), ty.clone());

                pi.unify(call.callee(ctxt.db).infer(ctxt));

                ty
            }
            Expr::Ann(ann) => {
                let ty = ctxt.eval(ann.type_rep(ctxt.db));
                let value = ann.value(ctxt.db);

                value.check(ty, ctxt)
            }
            Expr::Abs(abs) => {
                let ty_ctxt = abs
                    .parameters(ctxt.db)
                    .iter()
                    .fold(ctxt.ty_ctxt.clone(), |acc, parameter| acc); // TODO

                ctxt.with_env(ty_ctxt, |local| abs.value(local.db).infer(local))
            }
            Expr::Match(_) => todo!(),
            Expr::Upgrade(type_rep) => ctxt.eval(*type_rep),
        }
    }
}

impl Infer for Spanned<Literal> {
    type Output = Ty<modes::Mut>;

    fn infer(self, _: &mut InferCtxt) -> Self::Output {
        match self.value {
            Literal::Empty => Tau::Primary(Primary::Error),
            Literal::Int8(_) => Tau::Primary(Primary::I8),
            Literal::UInt8(_) => Tau::Primary(Primary::U8),
            Literal::Int16(_) => Tau::Primary(Primary::I16),
            Literal::UInt16(_) => Tau::Primary(Primary::U16),
            Literal::Int32(_) => Tau::Primary(Primary::I32),
            Literal::UInt32(_) => Tau::Primary(Primary::U32),
            Literal::Int64(_) => Tau::Primary(Primary::I64),
            Literal::UInt64(_) => Tau::Primary(Primary::U64),
            Literal::String(_) => Tau::Primary(Primary::String),
            Literal::Boolean(_) => Tau::Primary(Primary::Bool),
            Literal::Char(_) => Tau::Primary(Primary::Char),
        }
    }
}

impl Infer for TopLevel {
    type Output = Ty<modes::Mut>;

    fn infer(self, ctxt: &mut InferCtxt) -> Self::Output {
        match self {
            TopLevel::Empty => todo!(),
            TopLevel::Error(_) => todo!(),
            TopLevel::Using(_) => todo!(),
            TopLevel::Command(_) => todo!(),
            TopLevel::BindingGroup(_) => todo!(),
            TopLevel::ClassDecl(_) => todo!(),
            TopLevel::TraitDecl(_) => todo!(),
            TopLevel::DataDecl(_) => todo!(),
            TopLevel::TypeDecl(_) => todo!(),
        };

        ctxt.void()
    }
}

impl Check for Expr {
    type Output = Ty<modes::Mut>;

    fn check(self, ty: Tau, ctxt: &mut InferCtxt) -> Self::Output {
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

impl Infer for Stmt {
    type Output = Ty<modes::Mut>;

    fn infer(self, ctxt: &mut InferCtxt) -> Self::Output {
        match self {
            Stmt::Empty => {}
            Stmt::Error(_) => {}
            Stmt::Ask(_) => {}
            Stmt::Let(_) => {}
            Stmt::Downgrade(expr) => return expr.infer(ctxt),
        };

        ctxt.void()
    }
}

impl Check for Block {
    type Output = Ty<modes::Mut>;

    fn check(self, ty: Tau, ctxt: &mut InferCtxt) -> Self::Output {
        todo!()
    }
}

impl Check for TopLevel {
    type Output = Ty<modes::Mut>;

    fn check(self, ty: Tau, ctxt: &mut InferCtxt) -> Self::Output {
        todo!()
    }
}

struct MetaTv {}

struct InferCtxt<'tctx> {
    pub db: &'tctx dyn crate::TyperDb,
    pub ty_ctxt: TyCtxt,
}

impl Ty<modes::Mut> {
    fn unify(&self, another: Tau) {
        todo!()
    }
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

    fn eval(&self, type_rep: TypeRep) -> Tau {
        match type_rep {
            TypeRep::Unit => todo!(),
            TypeRep::Empty => todo!(),
            TypeRep::This => todo!(),
            TypeRep::Tt => todo!(),
            TypeRep::Error(_) => todo!(),
            TypeRep::Path(_) => todo!(),
            TypeRep::QPath(_) => todo!(),
            TypeRep::App(_) => todo!(),
            TypeRep::Arrow(_) => todo!(),
            TypeRep::Downgrade(_) => todo!(),
        }
    }

    fn reference(&self, reference: Reference) -> Tau {
        todo!()
    }

    fn void(&mut self) -> Tau {
        Tau::Primary(Primary::Unit)
    }

    fn with_env<F, U>(&mut self, env: TyCtxt, f: F) -> U
    where
        F: for<'tctxn> FnOnce(&mut InferCtxt<'tctxn>) -> U,
    {
        let old_env = std::mem::replace(&mut self.ty_ctxt, env);
        let result = f(self);
        self.ty_ctxt = old_env;
        result
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
