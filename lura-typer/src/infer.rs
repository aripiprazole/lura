#![allow(unused_variables)]
#![allow(dead_code)]

use std::rc::Rc;

use lura_hir::{
    resolve::{Definition, Reference},
    source::{
        expr::{Callee, Expr},
        literal::Literal,
        pattern::{Constructor, Pattern},
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

#[derive(Default, Clone, Hash)]
pub struct TyVariant {
    pub name: Tau,
    pub parameters: im_rc::Vector<Tau>,
}

#[derive(Clone)]
pub struct TyEnv {
    pub level: Level,
    pub variables: im_rc::HashMap<Definition, Sigma>,
    pub constructors: im_rc::HashMap<Definition, Rc<TyVariant>>,
    pub type_references: im_rc::HashMap<Tau, Rc<TyVariant>>,
    pub type_names: im_rc::HashMap<String, TyName>,
}

type Sigma = Ty<modes::Mut>;
type Rho = Ty<modes::Mut>;
type Tau = Ty<modes::Mut>;

trait Infer {
    type Output;

    fn infer(self, ctx: &mut InferCtx) -> Self::Output;
}

trait Check {
    type Output;

    fn check(self, ty: Tau, ctx: &mut InferCtx) -> Self::Output;
}

impl Check for Pattern {
    type Output = Ty<modes::Mut>;

    /// Checks the pattern against the type. This is used to
    /// check the pattern against the type.
    ///
    /// It does generate definition for the bindings in
    /// the context.
    fn check(self, ty: Tau, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel Values
            Pattern::Empty => Tau::Primary(Primary::Error),
            Pattern::Error(_) => Tau::Primary(Primary::Error),

            // SECTION: Patterns
            Pattern::Literal(literal) => {
                // Unifies the actual type with the expected type
                let actual_ty = literal.infer(ctx);
                actual_ty.unify(ty);
                actual_ty
            }
            Pattern::Constructor(constructor) => {
                // Unifies the actual type with the expected type
                let actual_ty = constructor.name(ctx.db).infer(ctx);
                actual_ty.unify(ty);

                // Checks the parameters of the constructor agains't
                // the arguments of the constructor
                let variant = ctx
                    .env
                    .type_references
                    .get(&actual_ty)
                    .cloned()
                    .unwrap_or_default();

                // Gets the parameters of the constructor
                let parameters = variant.parameters.clone();

                for (argument, ty) in constructor.arguments(ctx.db).into_iter().zip(parameters) {
                    // Checks the argument against the type
                    argument.check(ty, ctx);
                }

                actual_ty
            }
            // Returns the default type for the pattern
            //   - Wildcard
            //   - Binding
            Pattern::Binding(binding) => {
                // Adds the binding to the context
                let name = binding.name(ctx.db);
                ctx.env.variables.insert(name, ty.clone());
                ty
            }
            Pattern::Wildcard(_) => ty,
            Pattern::Rest(_) => todo!("rest pattern"),
        }
    }
}

impl Infer for Constructor {
    type Output = Ty<modes::Mut>;

    /// Infers the type of the callee. This is used
    /// to infer the type of the callee.
    fn infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Constructor
            Constructor::Unit => Ty::Primary(Primary::Unit), // Unit = Unit
            // SECTION: Builtin
            Constructor::Array => todo!("array builtin"),
            Constructor::Tuple => todo!("tuple builtin"),
            // SECTION: Reference
            Constructor::Path(path) => ctx.constructor(path),
        }
    }
}

impl Infer for Callee {
    type Output = Ty<modes::Mut>;

    /// Infers the type of the callee. This is used
    /// to infer the type of the callee.
    fn infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Callee
            Callee::Unit => Ty::Primary(Primary::Unit), // Unit = Unit
            // SECTION: Builtin
            Callee::Array => todo!("array builtin"),
            Callee::Tuple => todo!("tuple builtin"),
            Callee::Pure => todo!("pure builtin"),
            Callee::Do => todo!("do builtin"),
            // SECTION: Reference
            Callee::Reference(path) => ctx.reference(path),
            // SECTION: Expressions
            // Handle expressions in callee
            Callee::Expr(expr) => expr.infer(ctx),
        }
    }
}

impl Infer for Expr {
    type Output = Ty<modes::Mut>;

    /// Infers the type of the expression. This is used
    /// to infer the type of the expression.
    fn infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel Values
            Expr::Empty => Tau::Primary(Primary::Error),
            Expr::Error(_) => Tau::Primary(Primary::Error),

            // SECTION: Expressions
            // Handles literals, references and expressions that are not handled by other cases
            Expr::Path(reference) => ctx.reference(reference),
            Expr::Literal(literal) => literal.infer(ctx),
            Expr::Call(call) => {
                // Infers the type of each argument
                let parameters = call
                    .arguments(ctx.db)
                    .into_iter()
                    .map(|argument| argument.infer(ctx))
                    .collect::<Vec<_>>();

                // Creates a new type variable to represent the type of the result
                let hole = ctx.new_meta();
                let pi = Ty::from_pi(parameters.into_iter(), hole.clone());

                // Infers the type of the callee, and then applies the arguments
                // E.G Giving a `f : Int -> Int -> Int`
                //     and `pi : Int -> Int -> ?hole`,
                //     we get `?hole = Int`, in the unification
                //     process, and we get the result of value.
                pi.unify(call.callee(ctx.db).infer(ctx));

                // Returns the type of the result
                hole
            }

            Expr::Ann(ann) => {
                let ty = ctx.eval(ann.type_rep(ctx.db));
                let value = ann.value(ctx.db);

                value.check(ty, ctx)
            }

            Expr::Abs(abs) => {
                let ty_ctxt = abs
                    .parameters(ctx.db)
                    .iter()
                    .fold(ctx.env.clone(), |acc, parameter| acc); // TODO

                // Creates a new environment with the parameters locally, and
                // infers the type of the body
                ctx.with_env(ty_ctxt, |local| abs.value(local.db).infer(local))
            }

            Expr::Match(match_expr) => {
                let scrutinee = match_expr.scrutinee(ctx.db).infer(ctx);

                // Creates a new type variable to represent the type of the value
                let hole = ctx.new_meta();

                // Infers the type of each arm, to find the common type
                // between all of their arms' values
                match_expr
                    .clauses(ctx.db)
                    .into_iter()
                    .fold(hole, |acc, arm| {
                        // Checks the pattern against the scrutinee
                        let pattern = arm.pattern.check(scrutinee.clone(), ctx);

                        // Checks agains't the old type, to check if both are compatible
                        arm.value.check(acc, ctx)
                    })
            }

            // SECTION: Type Representations
            // Evaluates the type of the expression, and then checks if it is
            // compatible with the expected type
            Expr::Upgrade(type_rep) => ctx.eval(*type_rep),
        }
    }
}

impl Infer for Spanned<Literal> {
    type Output = Ty<modes::Mut>;

    /// Infers the type of the literal. This is used
    /// to infer the type of the literal.
    fn infer(self, _: &mut InferCtx) -> Self::Output {
        match self.value {
            // SECTION: Sentinel Values
            Literal::Empty => Tau::Primary(Primary::Error),

            // SECTION: Literals
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

    /// Infers the type of the top level. This is used
    /// to infer the type of the top level.
    ///
    /// It does not return a value, as it is a top level
    /// declaration. It does only add to the context and
    /// return unit type.
    fn infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel Values
            TopLevel::Empty => todo!(),
            TopLevel::Error(_) => todo!(),

            // SECTION: Top Level
            TopLevel::Using(_) => todo!(),
            TopLevel::Command(_) => todo!(),
            TopLevel::BindingGroup(_) => todo!(),
            TopLevel::ClassDecl(_) => todo!(),
            TopLevel::TraitDecl(_) => todo!(),
            TopLevel::DataDecl(_) => todo!(),
            TopLevel::TypeDecl(_) => todo!(),
        };

        ctx.void()
    }
}

impl Check for Expr {
    type Output = Ty<modes::Mut>;

    /// Checks the type of the expression. This is used
    /// to check the type of the expression.
    fn check(self, ty: Tau, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel Values
            Expr::Empty => todo!(),
            Expr::Error(_) => todo!(),

            // SECTION: Expressions
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

    /// Infers the type of the statement. This is used
    /// to infer the type of the statement.
    fn infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel values
            Stmt::Empty => {}
            Stmt::Error(_) => {}

            // SECTION: Statements
            Stmt::Ask(_) => {}
            Stmt::Let(_) => {}

            // SECTION: Expressions
            Stmt::Downgrade(expr) => return expr.infer(ctx),
        };

        ctx.void()
    }
}

impl Check for Block {
    type Output = Ty<modes::Mut>;

    /// Checks the type of the block. This is used
    /// to check the type of the block.
    fn check(self, ty: Tau, ctx: &mut InferCtx) -> Self::Output {
        todo!()
    }
}

impl Check for TopLevel {
    type Output = Ty<modes::Mut>;

    /// Checks the type of the top level. This is used
    /// to check the type of the top level.
    fn check(self, ty: Tau, ctx: &mut InferCtx) -> Self::Output {
        todo!()
    }
}

struct MetaTv {}

struct InferCtx<'tctx> {
    pub db: &'tctx dyn crate::TyperDb,
    pub env: TyEnv,
}

impl Ty<modes::Mut> {
    fn unify(&self, another: Tau) {
        todo!()
    }
}

impl TyEnv {
    fn lookup(name: Definition) -> Sigma {
        todo!()
    }

    fn extend(&mut self, name: Definition, ty: Sigma) {
        todo!()
    }
}

impl<'tctx> InferCtx<'tctx> {
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

    fn constructor(&self, reference: Reference) -> Tau {
        todo!()
    }

    fn void(&mut self) -> Tau {
        Tau::Primary(Primary::Unit)
    }

    fn with_env<F, U>(&mut self, env: TyEnv, f: F) -> U
    where
        F: for<'tctxn> FnOnce(&mut InferCtx<'tctxn>) -> U,
    {
        let old_env = std::mem::replace(&mut self.env, env);
        let result = f(self);
        self.env = old_env;
        result
    }

    fn extend<F, U>(&mut self, name: Definition, ty: Sigma, f: F) -> U
    where
        F: for<'tctxn> FnOnce(&mut InferCtx<'tctxn>) -> U,
    {
        let mut new_env = self.env.clone();
        new_env.extend(name, ty);

        let old_env = std::mem::replace(&mut self.env, new_env);
        let result = f(self);
        self.env = old_env;
        result
    }
}
