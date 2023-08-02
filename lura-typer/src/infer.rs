use std::{fmt::Debug, marker::PhantomData, mem::replace, rc::Rc};

use lura_diagnostic::{code, message, Diagnostics, Report};
use lura_hir::{
    lower::hir_lower,
    package::Package,
    resolve::{Definition, Reference},
    source::{
        declaration::Declaration,
        expr::{Callee, Expr},
        literal::Literal,
        pattern::{Constructor, Pattern},
        stmt::{Block, Stmt},
        top_level::{BindingGroup, TopLevel},
        type_rep::{ArrowKind, TypeRep},
        HirElement, Location, Spanned,
    },
};

use crate::{
    thir::{ThirDiagnostic, ThirLocation, ThirTextRange},
    ty::{
        holes::{Hole, HoleRef},
        *,
    },
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeError {
    CannotUnify(Tau, Tau),
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct TyName {
    pub ty: Tau,
    pub rigidness: Rigidness,
}

#[derive(Default, Clone, Hash)]
pub struct InternalVariant {
    pub name: Tau,
    pub parameters: im_rc::Vector<Tau>,
}

#[derive(Clone)]
pub struct TyEnv {
    pub level: Level,
    pub variables: im_rc::HashMap<Definition, Sigma>,
    pub constructors: im_rc::HashMap<Definition, Rc<InternalVariant>>,
    pub references: im_rc::HashMap<Tau, Rc<InternalVariant>>,
    pub rigid_variables: im_rc::HashMap<Definition, Tau>,
    pub names: im_rc::HashSet<TyName>,
}

pub struct Substitution<'a, 'db> {
    ctx: &'a mut InferCtx<'db>,
    errors: im_rc::Vector<TypeError>,
}

impl Substitution<'_, '_> {
    /// Unify holes with the type. This is used to
    /// unify a hole with a type.
    ///
    /// It does this by checking the hole with the following
    /// checks:
    /// - "occurs  check" : check that the hole doesn't occur in the type
    ///                     or simplier, check that you aren't making recursive types
    /// - "scope check"   : check that you aren't using bound vars outside its scope
    fn hole_unify_prechecking(&mut self, ty: Tau, scope: Level, hole: HoleMut) {
        let _ = scope;
        let _ = hole;

        match ty {
            // SECTION: Types
            Ty::Primary(_) => todo!(),
            Ty::Constructor(_) => todo!(),
            Ty::Forall(_) => todo!(),
            Ty::Pi(_) => todo!(),
            Ty::Hole(_) => todo!(),
            Ty::Bound(_, _) => todo!(),
            Ty::App(_, _) => todo!(),
        }
    }

    /// Unifies a hole with a type. This is used to
    /// unify a hole with a type.
    fn unify_hole(&mut self, ty: Tau, hole: HoleMut) {
        use holes::HoleKind::*;

        match hole.borrow().kind() {
            // SECTION: Sentinel Values
            Error => {} // TODO

            // SECTION: Holes
            Empty { scope } => {
                // Checks that the hole doesn't occur in the type
                if ty == Tau::Hole(hole.clone()) {
                    return;
                }

                // Unify the hole with the type, and then set the kind
                // of the hole to filled
                self.hole_unify_prechecking(ty.clone(), *scope, hole.clone());
                hole.borrow_mut().set_kind(Filled(ty));
            }
            Filled(a) => {
                self.internal_unify(a.clone(), ty);
            }
        }
    }

    /// Unifies two types. This is used to unify two types. It
    /// equates the two types.
    fn internal_unify(&mut self, a: Tau, b: Tau) {
        // Matches the types to equate them.
        match (a, b) {
            // SECTION: Unification
            (Ty::Hole(hole_a), b) => self.unify_hole(b, hole_a),
            (a, Ty::Hole(hole_b)) => self.unify_hole(a, hole_b),
            (_, Ty::Primary(Primary::Error)) => {}
            (Ty::Primary(Primary::Error), _) => {}
            (Ty::Primary(_), Ty::Primary(_)) => todo!("unify primary"),
            (Ty::Constructor(_), Ty::Constructor(_)) => todo!("unify constructors"),
            (Ty::Pi(_), Ty::Pi(_)) => todo!("unify pi"),
            (Ty::Forall(_), Ty::Forall(_)) => todo!("unify forall"),
            (Ty::Bound(_, _), Ty::Bound(_, _)) => todo!("unify bounds"),

            // SECTION: Type Error
            // Report accumulating type check error, when the types
            // cannot be unified.
            (a, b) => {
                self.errors.push_back(TypeError::CannotUnify(a, b));
            }
        };
    }

    fn publish_all_errors(&mut self) {
        for error in self.errors.iter() {
            self.ctx.accumulate::<()>(ThirDiagnostic {
                location: ThirLocation::CallSite,
                message: match error.clone() {
                    TypeError::CannotUnify(a, b) => {
                        message!["cannot unify", code!(a.seal()), "with", code!(b.seal())]
                    }
                },
            });
        }
    }
}

impl Debug for Substitution<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Substituition")
            .field("errors", &self.errors)
            .finish()
    }
}

type Sigma = Ty<modes::Mut>;
type Rho = Ty<modes::Mut>;
type Tau = Ty<modes::Mut>;
type HoleMut = HoleRef<modes::Mut>;

trait Infer {
    type Output;

    #[inline(always)]
    fn store_ty_var(self, target: Self::Output, ctx: &mut InferCtx)
    where
        Self: Sized,
    {
        let _ = target;
        let _ = ctx;
        // Memoizes the type variable
    }

    /// Infers the type of the element.
    #[inline(always)]
    fn infer(self, ctx: &mut InferCtx) -> Self::Output
    where
        Self: Sized + HirElement + Clone,
        Self::Output: Clone,
    {
        // Save the location, and update the location
        let old_location = replace(&mut ctx.location, self.location(ctx.db));
        let ty = self.clone().internal_infer(ctx);

        self.store_ty_var(ty.clone(), ctx);

        // Get the location back
        ctx.location = old_location;
        ty
    }

    /// Infers the type of the element.
    #[inline(always)]
    fn infer_with(self, el: &impl HirElement, ctx: &mut InferCtx) -> Self::Output
    where
        Self: Sized,
    {
        // Save the location, and update the location
        let old_location = replace(&mut ctx.location, el.location(ctx.db));
        let ty = self.internal_infer(ctx);

        // Get the location back
        ctx.location = old_location;
        ty
    }

    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output;
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
                actual_ty.unify(ty, ctx);
                actual_ty
            }
            Pattern::Constructor(constructor) => {
                // TODO: handle other builtins
                //
                // Unifies the actual type with the expected type
                let actual_ty = constructor.name(ctx.db).infer_with(&constructor, ctx);
                actual_ty.unify(ty, ctx);

                // Checks the parameters of the constructor agains't
                // the arguments of the constructor
                let variant = ctx
                    .env
                    .references
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
    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Constructor
            Constructor::Unit => Ty::Primary(Primary::Unit), // Unit = Unit
            // SECTION: Builtin
            Constructor::Array => panic!("array builtin should be handled in a different way"),
            Constructor::Tuple => panic!("array builtin should be handled in a different way"),
            // SECTION: Reference
            Constructor::Path(path) => ctx.constructor(path),
        }
    }
}

impl Infer for Callee {
    type Output = Ty<modes::Mut>;

    /// Infers the type of the callee. This is used
    /// to infer the type of the callee.
    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Callee
            Callee::Unit => Ty::Primary(Primary::Unit), // Unit = Unit
            // SECTION: Builtin
            Callee::Array => panic!("array builtin should be handled in a different way"),
            Callee::Tuple => panic!("array builtin should be handled in a different way"),
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

    // Associate the type with the expression
    // This is used to access the type of the expression in
    // a elaboration step
    fn store_ty_var(self, target: Self::Output, ctx: &mut InferCtx) {
        // Associate the type with the expression
        ctx.expressions.insert(self, target);
    }

    /// Infers the type of the expression. This is used
    /// to infer the type of the expression.
    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel Values
            Expr::Empty => Tau::Primary(Primary::Error),
            Expr::Error(_) => Tau::Primary(Primary::Error),

            // SECTION: Expressions
            // Handles literals, references and expressions that are not handled by other cases
            Expr::Path(reference) => ctx.reference(reference),
            Expr::Literal(literal) => literal.infer(ctx),
            Expr::Call(call) => {
                let callee = call.callee(ctx.db);

                // Infers the type of each argument
                let parameters = call
                    .arguments(ctx.db)
                    .into_iter()
                    .map(|argument| argument.infer(ctx))
                    .collect::<Vec<_>>();

                // Matches the callee, because all primary expressions are
                // abstracted in the `Callee` enum.
                //
                // This is used to infer the type of the callee.
                match callee {
                    Callee::Array => todo!("array builtin, should create array type"),
                    Callee::Tuple => todo!("array builtin, should create tuple type"),
                    _ => {
                        // Creates a new type variable to represent the type of the result
                        let hole = ctx.new_meta();
                        let pi = Ty::from_pi(parameters.into_iter(), hole.clone());

                        // Infers the type of the callee, and then applies the arguments
                        // E.G Giving a `f : Int -> Int -> Int`
                        //     and `pi : Int -> Int -> ?hole`,
                        //     we get `?hole = Int`, in the unification
                        //     process, and we get the result of value.
                        pi.unify(callee.infer_with(&call, ctx), ctx);

                        // Returns the type of the result
                        hole
                    }
                }
            }

            Expr::Ann(ann) => {
                let ty = ctx.eval(ann.type_rep(ctx.db));
                let value = ann.value(ctx.db);

                value.check(ty, ctx)
            }

            Expr::Abs(abs) => {
                let mut env = ctx.env.clone();
                let parameters = abs.parameters(ctx.db);

                // Adds the parameters to the context
                for parameter in parameters.into_iter() {
                    if !parameter.is_implicit(ctx.db) {
                        let type_rep = parameter.parameter_type(ctx.db);
                        let name = TyName {
                            ty: ctx.eval(type_rep),
                            rigidness: Rigidness::Flexible,
                        };

                        // Adds the parameter to the environment
                        env.names.insert(name);
                    }
                }

                // Creates a new environment with the parameters locally, and
                // infers the type of the body
                ctx.with_env(env, |local| abs.value(local.db).infer(local))
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
                        arm.pattern.check(scrutinee.clone(), ctx);

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
    fn internal_infer(self, _: &mut InferCtx) -> Self::Output {
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
    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
        #[inline]
        fn create_declaration_ty(ctx: &mut InferCtx, decl: impl Declaration) -> Tau {
            let name = decl.name(ctx.db);
            let parameters = decl
                .parameters(ctx.db)
                .into_iter()
                .filter(|parameter| !parameter.is_implicit(ctx.db))
                .map(|parameter| ctx.eval(parameter.parameter_type(ctx.db)))
                .collect::<im_rc::Vector<_>>();

            let constructor = match decl.type_rep(ctx.db) {
                Some(TypeRep::Empty | TypeRep::Error(_)) => {
                    Tau::Constructor(TyConstructor { name })
                }
                Some(type_rep) => ctx.eval(type_rep),
                None => Tau::Constructor(TyConstructor { name }),
            };

            // Creates the type of the variant
            let variant_ty = Ty::from_pi(parameters.into_iter(), constructor);

            // Quantifies the type of the variant
            let variant_ty = ctx.quantify(variant_ty);

            ctx.env.extend(name, variant_ty.clone());
            variant_ty
        }

        #[inline]
        fn check_binding_group(ctx: &mut InferCtx, binding_group: BindingGroup) {
            // Creates the type of the binding group using the
            // signature of the binding group
            let return_ty = create_declaration_ty(ctx, binding_group.signature(ctx.db));

            // Gets the parameter types
            let parameters = binding_group
                .parameters(ctx.db)
                .into_iter()
                .filter(|parameter| !parameter.is_implicit(ctx.db))
                .map(|parameter| ctx.eval(parameter.parameter_type(ctx.db)))
                .collect::<im_rc::Vector<_>>();

            // Checks the type of each clause
            for clause in binding_group.clauses(ctx.db) {
                // Checks the pattern against the scrutinee
                let arguments = clause.arguments(ctx.db);
                for (pattern, ty) in arguments.into_iter().zip(parameters.clone()) {
                    pattern.check(ty, ctx);
                }

                // Checks the return type of the clause
                clause.value(ctx.db).check(return_ty.clone(), ctx);
            }
        }

        match self {
            // SECTION: Sentinel Values
            TopLevel::Empty => {}
            TopLevel::Error(_) => {}

            // SECTION: Top Level
            TopLevel::Using(_) => {}
            TopLevel::Command(command) => {
                for argument in command.arguments(ctx.db) {
                    argument.infer(ctx);
                }
            }
            TopLevel::BindingGroup(binding_group) => check_binding_group(ctx, binding_group),
            TopLevel::ClassDecl(_) => todo!(),
            TopLevel::TraitDecl(_) => todo!(),
            TopLevel::DataDecl(data_declaration) => {
                let ty = create_declaration_ty(ctx, data_declaration);
                let self_type = replace(&mut ctx.self_type, ty.clone().into());

                // Creates the type of the variant
                for variant in data_declaration.variants(ctx.db) {
                    let name = variant.name(ctx.db);
                    let parameters = variant
                        .parameters(ctx.db)
                        .into_iter()
                        .filter(|parameter| !parameter.is_implicit(ctx.db))
                        .map(|parameter| ctx.eval(parameter.parameter_type(ctx.db)))
                        .collect::<im_rc::Vector<_>>();

                    let variant_ty = match variant.type_rep(ctx.db) {
                        Some(TypeRep::Empty | TypeRep::Error(_)) => ty.clone(),
                        Some(type_rep) => ctx.eval(type_rep),
                        None => ty.clone(),
                    };

                    // Creates the type of the variant
                    let variant_ty = Ty::from_pi(parameters.clone().into_iter(), variant_ty);

                    // Quantifies the type of the variant
                    let variant_ty = ctx.quantify(variant_ty);

                    // Creates the internal variant
                    let internal = Rc::new(InternalVariant {
                        name: ty.clone(),
                        parameters,
                    });
                    ctx.env.extend(name, variant_ty.clone());
                    ctx.env.references.insert(ty.clone(), internal.clone());
                    ctx.env.constructors.insert(name, internal);
                }

                // Returns the old self type to the original position,
                // as the self type is only valid in the data declaration
                ctx.self_type = self_type;
            }
            TopLevel::TypeDecl(type_declaration) => {
                create_declaration_ty(ctx, type_declaration);
            }
        };

        ctx.void()
    }
}

impl Check for Expr {
    type Output = Ty<modes::Mut>;

    /// Checks the type of the expression. This is used
    /// to check the type of the expression.
    fn check(self, tau: Tau, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel Values
            Expr::Empty => Tau::Primary(Primary::Error),
            Expr::Error(_) => Tau::Primary(Primary::Error),

            // SECTION: Expressions
            _ => {
                let actual_ty = self.infer(ctx);

                // Checks that the actual type is a subtype of the expected type
                actual_ty.unify(tau, ctx);
                actual_ty
            }
        }
    }
}

impl Infer for Stmt {
    type Output = Ty<modes::Mut>;

    /// Infers the type of the statement. This is used
    /// to infer the type of the statement.
    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel values
            Stmt::Empty => {}
            Stmt::Error(_) => {}

            // SECTION: Statements
            Stmt::Ask(ask_stmt) => {
                // Infers the type of the value, and then checks the pattern
                let value_ty = ask_stmt.value(ctx.db).infer(ctx);

                ask_stmt.pattern(ctx.db).check(value_ty, ctx);
            }
            Stmt::Let(let_stmt) => {
                // Infers the type of the value, and then checks the pattern
                let value_ty = let_stmt.value(ctx.db).infer(ctx);

                let_stmt.pattern(ctx.db).check(value_ty, ctx);
            }

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
    fn check(self, tau: Tau, ctx: &mut InferCtx) -> Self::Output {
        for statement in self.statements(ctx.db) {
            statement.infer(ctx);
        }

        // Checks the type of the last statement
        // TODO: check returns
        let actual_ty = match self.statements(ctx.db).last() {
            Some(value) => value.clone().infer(ctx),
            None => Tau::UNIT,
        };

        actual_ty.unify(tau, ctx);
        actual_ty
    }
}

struct InferCtx<'tctx> {
    pub db: &'tctx dyn crate::TyperDb,
    pub pkg: Package,
    pub self_type: Option<Tau>,
    pub location: Location,
    pub expressions: im_rc::HashMap<Expr, Tau>,
    pub env: TyEnv,
}

impl Ty<modes::Mut> {
    /// Unifies the type with another type. This is used
    /// to equate two types.
    fn unify(&self, tau: Tau, ctx: &mut InferCtx) {
        let mut substitution = Substitution {
            ctx,
            errors: im_rc::vector![],
        };
        substitution.internal_unify(self.clone(), tau);
        substitution.publish_all_errors();
    }
}

impl TyEnv {
    // Creates a new definition in the environment
    fn extend(&mut self, name: Definition, ty: Sigma) {
        self.variables.insert(name, ty);
    }
}

impl<'tctx> InferCtx<'tctx> {
    fn instantiate(&self, ty: Sigma) -> Rho {
        ty
    }

    fn quantify(&self, ty: Rho) -> Sigma {
        ty
    }

    /// Creates a new meta type. This is used to
    /// create a new empty hole.
    fn new_meta(&mut self) -> Tau {
        Tau::Hole(HoleRef::new(Hole {
            kind: holes::HoleKind::Empty {
                scope: self.env.level,
            },
        }))
    }

    /// Creates a new semantic type from a syntatic type representation,
    /// it does report errors and means the type is not inferred.
    fn eval(&mut self, type_rep: TypeRep) -> Tau {
        match type_rep {
            // SECTION: Sentinel Values
            TypeRep::Empty => self.new_meta(), // Infer the type
            TypeRep::Error(_) => Tau::ERROR,

            // SECTION: Primary Types
            TypeRep::Unit => Tau::UNIT,
            TypeRep::Type => Tau::TYPE,
            TypeRep::Path(reference) => self
                .env
                .rigid_variables
                .get(&reference.definition(self.db))
                .cloned()
                .unwrap_or_else(|| {
                    // TODO: maybe skip this diagnostic, because it should be handled
                    // by the resolver, maybe ?
                    let name = reference.definition(self.db).to_string(self.db);

                    self.accumulate(ThirDiagnostic {
                        location: self.new_location(type_rep.location(self.db)),
                        message: message!("unresolved type", code!(name)),
                    })
                }),
            TypeRep::App(app) => {
                let callee = self.eval(app.callee(self.db));

                app.arguments(self.db)
                    .into_iter()
                    .fold(callee, |acc, next| {
                        Tau::App(acc.into(), self.eval(next).into())
                    })
            }
            TypeRep::Arrow(pi) if matches!(pi.kind(self.db), ArrowKind::Pi) => {
                // Transforms a type representation of pi arrow into
                // a semantic type arrow with a pi arrow

                let value = self.eval(pi.value(self.db));

                pi.parameters(self.db)
                    .into_iter()
                    .map(|parameter| {
                        // Checks the type of the parameter
                        let ty = self.eval(parameter.parameter_type(self.db));

                        parameter.binding(self.db).check(ty, self)
                    })
                    .fold(value, |acc, next| {
                        Tau::Pi(Arrow {
                            parameters: next.into(),
                            value: acc.into(),
                            _phantom: PhantomData,
                        })
                    })
            }
            TypeRep::Arrow(forall) if matches!(forall.kind(self.db), ArrowKind::Forall) => {
                // Transforms a type representation of forall arrow into
                // a semantic type arrow with a forall quantifier

                let value = self.eval(forall.value(self.db));
                let parameters = forall
                    .parameters(self.db)
                    .into_iter()
                    .map(|parameter| {
                        // Checks the type of the parameter
                        let ty = self.eval(parameter.parameter_type(self.db));

                        parameter.binding(self.db).check(ty, self)
                    })
                    .collect::<Vec<_>>();

                Tau::Forall(Arrow {
                    parameters,
                    value: value.into(),
                    _phantom: PhantomData,
                })
            }
            TypeRep::SelfType => self.self_type.clone().unwrap_or_else(|| {
                self.accumulate(ThirDiagnostic {
                    location: self.new_location(type_rep.location(self.db)),
                    message: message!("self type outside of a self context"),
                })
            }),
            // SECTION: Unsupported
            TypeRep::Arrow(_) => self.accumulate(ThirDiagnostic {
                location: self.new_location(type_rep.location(self.db)),
                message: message!("sigma types is not supported yet"),
            }),
            TypeRep::QPath(_) => self.accumulate(ThirDiagnostic {
                location: self.new_location(type_rep.location(self.db)),
                message: message!("qualified types is not supported yet"),
            }),
            // SECTION: Expressions
            TypeRep::Downgrade(expr) => expr.infer(self),
        }
    }

    /// Accumulates a diagnostic and returns a default value. This is used
    /// to accumulate diagnostics and return a default value.
    fn accumulate<T: Default>(&self, mut diagnostic: ThirDiagnostic) -> T {
        if let ThirLocation::CallSite = diagnostic.location {
            diagnostic.location = self.new_location(self.location.clone());
        }

        // We push the diagnostic to the diagnostics
        Diagnostics::push(self.db, Report::new(diagnostic));

        // We return the default value
        Default::default()
    }

    fn new_location(&self, location: Location) -> ThirLocation {
        // We set the location of the diagnostic, lowering the
        // source of location
        match location {
            Location::TextRange(ref text_range) => {
                // We lower the hir source, if the location is a text range
                let hir_source = hir_lower(self.db, self.pkg, text_range.source);

                ThirLocation::TextRange(ThirTextRange {
                    source: hir_source,
                    start: text_range.start,
                    end: text_range.end,
                    file_name: text_range.file_name.clone(),
                    text: text_range.text.clone(),
                })
            }
            Location::CallSite => ThirLocation::CallSite,
        }
    }

    fn reference(&self, reference: Reference) -> Tau {
        let let_ty = self
            .env
            .variables
            .get(&reference.definition(self.db))
            .cloned()
            .unwrap_or_else(|| Tau::Primary(Primary::Error));

        self.instantiate(let_ty)
    }

    fn constructor(&self, reference: Reference) -> Tau {
        let let_ty = self
            .env
            .constructors
            .get(&reference.definition(self.db))
            .cloned()
            .unwrap_or_else(|| {
                // If the constructor is not found, then we return the error type
                Rc::new(InternalVariant {
                    name: Tau::Primary(Primary::Error),
                    parameters: im_rc::vector![],
                })
            });

        self.instantiate(let_ty.name.clone())
    }

    fn void(&mut self) -> Tau {
        Tau::Primary(Primary::Unit)
    }

    fn with_env<F, U>(&mut self, env: TyEnv, f: F) -> U
    where
        F: for<'tctxn> FnOnce(&mut InferCtx<'tctxn>) -> U,
    {
        let old_env = replace(&mut self.env, env);
        let result = f(self);
        self.env = old_env;
        result
    }
}
