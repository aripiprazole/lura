use std::cell::RefCell;
use std::{fmt::Debug, mem::replace, rc::Rc};

use fxhash::FxBuildHasher;
use if_chain::if_chain;
use lura_diagnostic::{code, message, ErrorId};
use lura_hir::{
    lower::hir_lower,
    package::Package,
    resolve::{unresolved, Definition, HirLevel, Reference},
    source::{
        declaration::{Declaration, Parameter},
        expr::{Callee, Expr},
        literal::Literal,
        pattern::{Constructor, Pattern},
        stmt::{Block, Stmt},
        top_level::{BindingGroup, TopLevel},
        type_rep::{ArrowKind, TypeRep},
        HirElement, HirLocation, Location, Spanned,
    },
};

use crate::type_rep::forall::HoasForall;
use crate::type_rep::pi::HoasPi;
use crate::utils::LocalDashMap;
use crate::whnf::Whnf;
use crate::{
    adhoc::{ClassEnv, Pred, Qual},
    thir::{ThirDiagnostic, ThirLocation, ThirTextRange},
    type_rep::{
        holes::{Hole, HoleRef},
        *,
    },
};

/// Represents the type errors that can occur during type checking,
/// specially on the unification step.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeError {
    CannotUnify(Tau, Tau),
    OccursCheck(Tau),
    EscapingScope(Level),
    IncompatibleTypes {
        expected: Definition,
        actual: Definition,
    },
    IncompatibleValues {
        expected: Primary,
        actual: Primary,
    },
    IncorrectLevel {
        expected: Level,
        actual: Level,
    },
    IncorrectArity {
        expected: usize,
        actual: usize,
    },
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

#[derive(Default, Clone)]
pub struct TyEnv {
    pub level: Level,
    pub constructors: im_rc::HashMap<Definition, Rc<InternalVariant>, FxBuildHasher>,
    pub references: im_rc::HashMap<Tau, Rc<InternalVariant>, FxBuildHasher>,
    pub variables: im_rc::HashMap<Definition, Tau, FxBuildHasher>,
    pub names: im_rc::Vector<String>,
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
    fn hole_unify_prechecks(&mut self, ty: Tau, scope: Level, hole: HoleMut) {
        let _ = scope;
        let _ = hole;

        match ty {
            // SECTION: Types
            Type::Type => {}
            Type::Primary(_) => {}
            Type::Constructor(_) => {}
            Type::Bound(Bound::Flexible(_)) => {}
            Type::Bound(Bound::Hole) => {}
            Type::Forall(forall) => {
                // Named holes, because this is a dependent
                // function type, we need to create a new hole
                // for each parameter
                let domain = forall
                    .domain
                    .clone()
                    .into_iter()
                    .map(|(name, _)| Tau::Bound(Bound::Flexible(name)))
                    .collect::<Vec<_>>();

                // Exeuctes the HOAS function to get the codomain
                // and then evaluates it to WHNF
                let codomain = forall.instantiate(domain);

                // TODO: check dependent kinds
                // self.hole_unify_prechecks(domain, scope, hole.clone());
                self.hole_unify_prechecks(codomain, scope, hole)
            }
            Type::Pi(pi) => {
                let ctx = self.ctx.snapshot();
                let domain = pi.domain.eval(&ctx, self.ctx.eval_env.clone());

                // Unnamed holes, because this isn't a dependent
                // function type, we can just use the same hole
                let parameter = Tau::Bound(match pi.name {
                    Some(ref name) => Bound::Flexible(name.clone()),
                    None => Bound::Hole,
                });

                // Exeuctes the HOAS function to get the codomain
                // and then evaluates it to WHNF
                let codomain = pi.codomain(parameter);

                self.hole_unify_prechecks(domain, scope, hole.clone());
                self.hole_unify_prechecks(codomain, scope, hole)
            }
            Type::Bound(Bound::Index(_, level)) => {
                if level > scope {
                    self.errors.push_back(TypeError::EscapingScope(level));
                }
            }
            Type::App(a, b) => {
                self.hole_unify_prechecks(*a, scope, hole.clone());
                self.hole_unify_prechecks(*b, scope, hole)
            }
            // SECTION: Hole
            Type::Hole(h) => {
                use holes::HoleKind::*;
                if h == hole {
                    self.errors.push_back(TypeError::OccursCheck(Type::Hole(h)));
                    return;
                }

                let mut hole_ref = h.borrow_mut();

                match hole_ref.kind() {
                    Empty { scope: l } if *l > scope => hole_ref.set_kind(Empty { scope }),
                    Filled(ty) => self.hole_unify_prechecks(ty.clone(), scope, hole),
                    _ => {}
                }
            }
        }
    }

    /// Unifies a hole with a type. This is used to
    /// unify a hole with a type.
    fn unify_hole(&mut self, ty: Tau, hole: HoleMut) {
        use holes::HoleKind::*;

        // I don't know but this is needed to avoid reborrow
        match hole.kind() {
            // SECTION: Sentinel Values
            Error => {}

            // SECTION: Holes
            Empty { scope } => {
                // Checks that the hole doesn't occur in the type
                if ty == Tau::Hole(hole.clone()) {
                    return;
                }

                // Unify the hole with the type, and then set the kind
                // of the hole to filled
                self.hole_unify_prechecks(ty.clone(), scope, hole.clone());
                hole.borrow_mut().set_kind(Filled(ty));
            }
            Filled(a) => {
                self.internal_unify(a, ty);
            }
        }
    }

    /// Unifies two types. This is used to unify two types. It
    /// equates the two types.
    fn internal_unify(&mut self, a: Tau, b: Tau) {
        // Matches the types to equate them.
        match (a, b) {
            // SECTION: Unification
            (Type::Hole(hole_a), b) => self.unify_hole(b, hole_a),
            (a, Type::Hole(hole_b)) => self.unify_hole(a, hole_b),
            // SECTION: Sentinel Values
            (_, Type::Primary(Primary::Error)) => {}
            (Type::Primary(Primary::Error), _) => {}
            // SECTION: Types
            (Type::Primary(primary_a), Type::Primary(primary_b)) => {
                if primary_a != primary_b {
                    self.errors.push_back(TypeError::IncompatibleValues {
                        expected: primary_a,
                        actual: primary_b,
                    });
                }
            }
            (Type::Constructor(constructor_a), Type::Constructor(constructor_b)) => {
                // unify the names
                if constructor_a.definition != constructor_b.definition {
                    self.errors.push_back(TypeError::IncompatibleTypes {
                        expected: constructor_a.definition,
                        actual: constructor_b.definition,
                    });
                }
            }
            (Type::Pi(pi_a), Type::Pi(pi_b)) => {
                let ctx = self.ctx.snapshot();
                let domain_a = pi_a.domain.eval(&ctx, self.ctx.eval_env.clone());
                let domain_b = pi_b.domain.eval(&ctx, self.ctx.eval_env.clone());

                let codomain_a = pi_a.codomain(Tau::Bound(Bound::Hole));
                let codomain_b = pi_b.codomain(Tau::Bound(Bound::Hole));

                self.internal_unify(domain_a, domain_b);
                self.internal_unify(codomain_a, codomain_b);
            }
            (Type::App(callee_a, value_a), Type::App(callee_b, value_b)) => {
                self.internal_unify(*callee_a, *callee_b);
                self.internal_unify(*value_a, *value_b);
            }
            (Type::Forall(forall_a), Type::Forall(forall_b)) => {
                // "alpha equivalence": forall a. a -> a = forall b. b -> b
                if forall_a.domain.len() != forall_b.domain.len() {
                    self.errors.push_back(TypeError::IncorrectArity {
                        expected: forall_a.domain.len(),
                        actual: forall_b.domain.len(),
                    });

                    return;
                }

                // let mut acc_a = *forall_a.data.value.clone();
                // let mut acc_b = *forall_b.data.value.clone();
                // for (param_a, param_b) in forall_a.domain.iter().zip(forall_b.domain.iter()) {
                //     let level = self.ctx.add_to_env(param_a.name);
                //     let name = self.ctx.create_new_name(param_a.name);
                //     let debruijin = Tau::Bound(Bound::Index(name, level));
                //     acc_a = acc_a.replace(param_a.name, debruijin.clone());
                //     acc_b = acc_b.replace(param_b.name, debruijin);
                // }

                // As the code doesn't implement subjumption, we need to
                // throw an error whenever this unification is called.
                //
                // TODO: Implement subjumption
                //
                // NOTE: When higher-rank poly doesn't have polymorphic
                // subtyping, it's unsound. So, we need to implement
                // polymorphic subtyping.
                self.ctx.accumulate::<()>(ThirDiagnostic {
                    id: ErrorId("polymorphic-subtyping"),
                    location: ThirLocation::CallSite,
                    message: message!["Polymorphic subtyping is not implemented yet"],
                });

                // Unify the results to compare the results
                // self.internal_unify(acc_a, acc_b);
            }
            (Type::Bound(Bound::Index(_, level_a)), Type::Bound(Bound::Index(_, level_b))) => {
                if level_a != level_b {
                    self.errors.push_back(TypeError::IncorrectLevel {
                        expected: level_a,
                        actual: level_b,
                    });
                }
            }

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
                id: ErrorId(match error {
                    TypeError::CannotUnify(_, _) => "unify",
                    TypeError::OccursCheck(_) => "occurs-check",
                    TypeError::EscapingScope(_) => "escaping-scope",
                    TypeError::IncompatibleTypes { .. } => "incompatible-types",
                    TypeError::IncompatibleValues { .. } => "incompatible-values",
                    TypeError::IncorrectLevel { .. } => "incompatible-level",
                    TypeError::IncorrectArity { .. } => "incompatible-arity",
                }),
                message: match error.clone() {
                    TypeError::CannotUnify(a, b) => {
                        message!["cannot unify", code!(a.seal()), "with", code!(b.seal())]
                    }
                    TypeError::IncorrectLevel { expected, actual } => {
                        message![
                            "incorrect level, expected",
                            code!(expected),
                            "but found",
                            code!(actual),
                        ]
                    }
                    TypeError::IncorrectArity { expected, actual } => {
                        message![
                            "incorrect arity, expected",
                            code!(expected),
                            "but found",
                            code!(actual),
                        ]
                    }
                    TypeError::IncompatibleTypes { expected, actual } => {
                        message![
                            "incompatible types, expected",
                            code!(expected.to_string(self.ctx.db)),
                            "but found",
                            code!(actual.to_string(self.ctx.db)),
                        ]
                    }
                    TypeError::IncompatibleValues { expected, actual } => {
                        message![
                            "incompatible type values, expected",
                            code!(expected),
                            "but found",
                            code!(actual),
                        ]
                    }
                    TypeError::OccursCheck(infinite_type) => {
                        message!["infinite type,", code!(infinite_type.seal())]
                    }
                    TypeError::EscapingScope(level) => {
                        message!["escaping scope,", code!(level)]
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

type Sigma = Type<state::Hoas>;
type Rho = Type<state::Hoas>;
type Tau = Type<state::Hoas>;
type HoleMut = HoleRef<state::Hoas>;

pub(crate) trait Infer {
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
    type Output = Type<state::Hoas>;

    /// Checks the pattern against the type. This is used to
    /// check the pattern against the type.
    ///
    /// It does generate definition for the bindings in
    /// the context.
    fn check(self, ty: Tau, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Sentinel Values
            Pattern::Hole => ty, // It's a hole pattern
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
    type Output = Type<state::Hoas>;

    /// Infers the type of the callee. This is used
    /// to infer the type of the callee.
    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Constructor
            Constructor::Unit => Type::Primary(Primary::Unit), // Unit = Unit
            // SECTION: Builtin
            Constructor::Array => panic!("array builtin should be handled in a different way"),
            Constructor::Tuple => panic!("array builtin should be handled in a different way"),
            // SECTION: Reference
            Constructor::Path(path) => ctx.constructor(path),
        }
    }
}

impl Infer for Callee {
    type Output = Type<state::Hoas>;

    /// Infers the type of the callee. This is used
    /// to infer the type of the callee.
    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
        match self {
            // SECTION: Callee
            Callee::Unit => Type::Primary(Primary::Unit), // Unit = Unit
            // SECTION: Builtin
            Callee::Array => panic!("array builtin should be handled in a different way"),
            Callee::Tuple => panic!("tuple builtin should be handled in a different way"),
            Callee::Pure => panic!("pure builtin should be handled in a different way"),
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
    type Output = Type<state::Hoas>;

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
                    Callee::Tuple => {
                        let arguments = call.arguments(ctx.db);

                        match arguments.len() {
                            // Unit type
                            0 => Type::Primary(Primary::Unit),
                            // Group type
                            1 => arguments[0].clone().infer(ctx),
                            // Tuple type
                            _ => todo!("tuple type"),
                        }
                    }
                    Callee::Do => match call.do_notation(ctx.db) {
                        Some(do_notation) => {
                            // TODO: return monad
                            let ty = ctx.new_meta();
                            do_notation.check(ty.clone(), ctx);
                            ty
                        }
                        None => ctx.accumulate(ThirDiagnostic {
                            location: ThirLocation::CallSite,
                            id: ErrorId("missing-do-notation"),
                            message: message!["do notation but without do notation parameter"],
                        }),
                    },
                    Callee::Pure => {
                        let arguments = call.arguments(ctx.db);
                        let Some(return_type) = ctx.return_type.clone() else {
                            ctx.accumulate::<()>(ThirDiagnostic {
                                location: ThirLocation::CallSite,
                                id: ErrorId("missing-return-type"),
                                message: message!["pure but without return type parameter"],
                            });

                            return ctx.new_meta();
                        };

                        // TODO: return monad
                        match arguments.len() {
                            // Unit type
                            0 => {
                                return_type.unify(Type::Primary(Primary::Unit), ctx);
                                return_type
                            }
                            // Group type
                            1 => {
                                return_type.unify(arguments[0].clone().infer(ctx), ctx);
                                return_type
                            }
                            // Tuple type
                            _ => ctx.accumulate(ThirDiagnostic {
                                location: ThirLocation::CallSite,
                                id: ErrorId("multiple-return-parameters"),
                                message: message!["multiple parameters for pure"],
                            }),
                        }
                    }
                    _ => {
                        // Creates a new type variable to represent the type of the result
                        let hole = ctx.new_meta();
                        let pi = Type::from_pi(parameters.into_iter(), hole.clone());

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
                let env = ctx.env.clone();
                let parameters = abs.parameters(ctx.db);

                // Creates a new environment with the parameters locally, and
                // infers the type of the body
                ctx.with_env(env, |local| {
                    // Adds the parameters to the context
                    for parameter in parameters.into_iter() {
                        if !parameter.is_implicit(local.db) {
                            let type_rep = local.eval(parameter.parameter_type(local.db));

                            let ty = parameter.binding(local.db).check(type_rep, local);

                            // Stores the parameter in the environment
                            // as debug information for the type checker
                            // build a table.
                            local.parameters.insert(parameter, ty.clone());
                        }
                    }

                    abs.value(local.db).infer(local)
                })
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
    type Output = Type<state::Hoas>;

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
    type Output = Type<state::Hoas>;

    /// Infers the type of the top level. This is used
    /// to infer the type of the top level.
    ///
    /// It does not return a value, as it is a top level
    /// declaration. It does only add to the context and
    /// return unit type.
    fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
        /// Creates a new type variable to represent the type of the declaration
        ///
        /// The ty is optional because it does fallback to the type of the
        /// declaration, if it is not provided.
        #[inline]
        fn create_declaration_ty(
            ctx: &mut InferCtx,
            // Used to define if the type is a type Type, or
            // if it should use the return type.
            // has_type_type: bool,
            use_return_type: bool,
            decl: impl Declaration,
        ) -> Tau {
            let name = decl.name(ctx.db);
            let parameters = decl
                .parameters(ctx.db)
                .into_iter()
                .map(|parameter| {
                    let ty = ctx.eval(parameter.parameter_type(ctx.db));

                    // Stores the parameter in the environment
                    // as debug information for the type checker
                    // build a table.
                    ctx.parameters.insert(parameter, ty.clone());

                    ty
                })
                .collect::<im_rc::Vector<_>>();

            let name = Name {
                definition: name,
                name: name.to_string(ctx.db),
            };

            let constructor = match decl.type_rep(ctx.db) {
                Some(TypeRep::Error(_)) => Tau::Constructor(name.clone()),
                Some(TypeRep::Hole) if !use_return_type => {
                    let goal = Tau::Constructor(name.clone());

                    // Generate the type-level function
                    // that represents the declaration
                    parameters.iter().fold(goal, |acc, parameter| {
                        // Creates a new type variable to represent
                        // the type of the declaration
                        Tau::Pi(HoasPi {
                            name: None,
                            domain: parameter.clone().into(),
                            codomain: Rc::new(move |_| acc.clone()),
                        })
                    })
                }
                Some(TypeRep::Arrow(arrow)) if arrow.kind(ctx.db) == ArrowKind::Forall => {
                    // # Safety
                    // We already checked if the type is a forall, so we can
                    // assume it's safe to unwrap, as it's needs to be a [`Some`]
                    // to be a forall.
                    let type_rep = unsafe { decl.type_rep(ctx.db).unwrap_unchecked() };

                    // Checks if arrow is already generalised. If the `forall` is first level,
                    // then it's already generalised, so we don't need to quantify it!
                    //
                    // Creates the type of the variant
                    let variant_ty = Type::from_pi(parameters.into_iter(), ctx.eval(type_rep));

                    // Early return if the type is already generalised
                    ctx.extend(name.definition, variant_ty.clone());
                    return variant_ty;
                }
                Some(type_rep) => ctx.eval(type_rep),
                None => Tau::Constructor(name.clone()),
            };

            // Creates the type of the variant
            let type_rep = Type::from_pi(parameters.into_iter(), constructor);

            // Quantifies the type of the variant
            let type_rep = ctx.quantify(type_rep);

            ctx.extend(name.definition, type_rep.clone());
            type_rep
        }

        #[inline]
        fn check_binding_group(ctx: &mut InferCtx, binding_group: BindingGroup) {
            // Creates the type of the binding group using the
            // signature of the binding group
            let function_ty = create_declaration_ty(ctx, true, binding_group.signature(ctx.db));

            // Adds the value to the environment
            let name = binding_group.name(ctx.db);
            ctx.env.variables.insert(name, function_ty.clone());

            // Creates the type of the binding group using the
            // signature of the binding group
            let return_ty = ctx.new_meta();

            // Gets the parameter types
            let parameters = binding_group
                .parameters(ctx.db)
                .into_iter()
                .map(|parameter| {
                    let type_rep = ctx.eval(parameter.parameter_type(ctx.db));

                    if_chain! {
                        if parameter.is_implicit(ctx.db);
                        if let HirLevel::Type = parameter.level(ctx.db);
                        // TODO: handle error, type parameters can't be patterns, they
                        // should be simple bindings.
                        if let Pattern::Binding(binding) = parameter.binding(ctx.db);
                        then {
                            // Meta information
                            let name = ctx.create_new_name(binding.name(ctx.db));
                            let def = name.definition;

                            // Debruijin index
                            let level = ctx.add_to_env(def);
                            let bound = Tau::Bound(Bound::Index(name, level));
                            ctx.env.variables.insert(def, bound.clone());
                            bound
                        } else {
                            type_rep
                        }
                    }
                })
                .collect::<im_rc::Vector<_>>();

            let pi = Tau::from_pi(parameters.clone().into_iter(), return_ty.clone());

            // Gets the return type
            pi.unify(function_ty, ctx);

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
                let ty = create_declaration_ty(ctx, false, data_declaration);
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
                        Some(TypeRep::Hole | TypeRep::Error(_)) => ty.clone(),
                        Some(type_rep) => ctx.eval(type_rep),
                        None => ty.clone(),
                    };

                    // Creates the type of the variant
                    let variant_ty = Type::from_pi(parameters.clone().into_iter(), variant_ty);

                    // Quantifies the type of the variant
                    let variant_ty = ctx.quantify(variant_ty);

                    // Creates the internal variant
                    let internal = Rc::new(InternalVariant {
                        name: ty.clone(),
                        parameters,
                    });
                    ctx.extend(name, variant_ty.clone());
                    ctx.env.references.insert(ty.clone(), internal.clone());
                    ctx.env.constructors.insert(name, internal);
                }

                // Returns the old self type to the original position,
                // as the self type is only valid in the data declaration
                ctx.self_type = self_type;
            }
            TopLevel::TypeDecl(type_declaration) => {
                create_declaration_ty(ctx, false, type_declaration);
            }
        };

        ctx.void()
    }
}

impl Check for Expr {
    type Output = Type<state::Hoas>;

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
    type Output = Type<state::Hoas>;

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
    type Output = Type<state::Hoas>;

    /// Checks the type of the block. This is used
    /// to check the type of the block.
    fn check(self, tau: Tau, ctx: &mut InferCtx) -> Self::Output {
        let return_type = replace(&mut ctx.return_type, Some(tau.clone()));

        // Checks the type of each statement
        for statement in self.statements(ctx.db) {
            statement.infer(ctx);
        }

        // Checks the type of the last statement
        let actual_ty = match self.statements(ctx.db).last() {
            Some(value) => value.clone().infer(ctx),
            None => Tau::UNIT,
        };

        actual_ty.unify(tau, ctx);
        ctx.return_type = return_type;
        actual_ty
    }
}

#[derive(Default, Clone)]
pub(crate) struct EvalEnv {
    /// The environment that is used to evaluate the expression.
    ///
    /// This is used to evaluate the expression.
    pub env: im_rc::HashMap<Definition, Tau, FxBuildHasher>,
}

/// This is a copy of the type environment that is used to
/// evaluate the expression.
///
/// Have the same fields, but doesn't require lifetimes.
///
/// NODE: #[allow(dead_code)] is used to suppress the warning
/// that the field is not used, because we want to keep the
/// same fields as the type environment.
#[derive(Clone)]
#[allow(dead_code)]
pub(crate) struct Snapshot {
    pub pkg: Package,

    // SECTION: Diagnostics
    /// The diagnostics that are emitted by the type checker.
    pub diagnostics: Rc<RefCell<Vec<ThirDiagnostic>>>,
    pub location: ThirLocation,
    // END SECTION: Diagnostics

    // SECTION: Type environment
    pub env: TyEnv,
    pub adhoc_env: ClassEnv,
    pub eval_env: EvalEnv,
    pub type_env: LocalDashMap<Definition, Tau>,
    // END SECTION: Type environment

    // SECTION: Contextual information
    pub self_type: Option<Tau>,
    pub return_type: Option<Tau>,
    // END SECTION: Contextual information
    /// Statically typed expressions that are used in the program.
    ///
    /// This is used to check that the type of the expression is
    /// correct.
    pub expressions: im_rc::HashMap<Expr, Tau, FxBuildHasher>,

    /// Statically typed statements that are used in the program.
    pub parameters: im_rc::HashMap<Parameter, Tau, FxBuildHasher>,

    /// Statically typed statements that are used in the program.
    pub declarations: im_rc::HashMap<TopLevel, Tau, FxBuildHasher>,

    /// Debruijin index bindings for type names.
    pub debruijin_index: im_rc::HashMap<usize, String, FxBuildHasher>,
}

/// Local context for type inference. This is used
/// to infer the type of an expression, and everything
/// that is needed to infer the type of an expression.
pub(crate) struct InferCtx<'tctx> {
    pub db: &'tctx dyn crate::TyperDb,
    pub pkg: Package,

    // SECTION: Diagnostics
    /// The diagnostics that are emitted by the type checker.
    pub diagnostics: Rc<RefCell<Vec<ThirDiagnostic>>>,
    pub location: Location,
    // END SECTION: Diagnostics

    // SECTION: Type environment
    pub env: TyEnv,
    pub adhoc_env: ClassEnv,
    pub type_env: LocalDashMap<Definition, Tau, FxBuildHasher>,
    pub eval_env: EvalEnv,
    // END SECTION: Type environment

    // SECTION: Contextual information
    pub self_type: Option<Tau>,
    pub return_type: Option<Tau>,
    // END SECTION: Contextual information
    /// Statically typed expressions that are used in the program.
    ///
    /// This is used to check that the type of the expression is
    /// correct.
    pub expressions: im_rc::HashMap<Expr, Tau, FxBuildHasher>,

    /// Statically typed statements that are used in the program.
    pub parameters: im_rc::HashMap<Parameter, Tau, FxBuildHasher>,

    /// Statically typed statements that are used in the program.
    pub declarations: im_rc::HashMap<TopLevel, Tau, FxBuildHasher>,

    /// Debruijin index bindings for type names.
    pub debruijin_index: im_rc::HashMap<usize, String, FxBuildHasher>,
}

impl Pred<state::Hoas> {
    fn replace(self, name: Definition, replacement: Tau) -> Pred<state::Hoas> {
        match self {
            Pred::IsIn(ty, class, str) => Pred::IsIn(ty.replace(name, replacement), class, str),
        }
    }
}

impl Qual<state::Hoas, HoasForall> {
    // Replaces a type variable with a type.
    fn replace(self, name: Definition, replacement: Tau) -> Qual<state::Hoas, HoasForall> {
        Self {
            predicates: self
                .predicates
                .into_iter()
                .map(|pred| pred.replace(name, replacement.clone()))
                .collect(),
            data: HoasForall {
                domain: self.data.domain.clone(),
                codomain: Rc::new(move |parameters| {
                    self.data
                        .instantiate(parameters)
                        .replace(name, replacement.clone())
                }),
            },
        }
    }
}

impl Type<state::Hoas> {
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

    fn replace(self, name: Definition, replacement: Tau) -> Tau {
        use holes::HoleKind::*;
        match self {
            Type::Forall(forall) => Type::Forall(forall.replace(name, replacement)),
            Type::App(a, b) => Type::App(
                a.replace(name, replacement.clone()).into(),
                b.replace(name, replacement).into(),
            ),
            Type::Pi(fun) => Type::Pi(HoasPi {
                name: fun.name.clone(),
                domain: fun.domain.clone().replace(name, replacement.clone()).into(),
                codomain: Rc::new(move |domain| {
                    fun.codomain(domain).replace(name, replacement.clone())
                }),
            }),
            Type::Hole(hole) => match hole.data.borrow_mut().kind() {
                Error => Type::Hole(HoleRef::new(Hole { kind: Error })),
                Empty { scope } => Type::Hole(HoleRef::new(Hole {
                    kind: Empty { scope: *scope },
                })),
                Filled(ty) => ty.clone().replace(name, replacement),
            },
            Type::Bound(Bound::Flexible(flexibile)) if flexibile.definition == name => replacement,
            Type::Bound(bound) => Type::Bound(bound),
            _ => self,
        }
    }
}

impl Snapshot {
    /// Accumulates a diagnostic and returns a default value. This is used
    /// to accumulate diagnostics and return a default value.
    pub fn accumulate<T: Default>(&self, mut diagnostic: ThirDiagnostic) -> T {
        if let ThirLocation::CallSite = diagnostic.location {
            diagnostic.location = self.location.clone();
        }

        self.diagnostics.borrow_mut().push(diagnostic);

        // We return the default value
        Default::default()
    }
}

impl<'tctx> InferCtx<'tctx> {
    fn instantiate(&mut self, sigma: Sigma) -> Rho {
        let Sigma::Forall(forall) = sigma else {
            return sigma;
        };

        let parameters = forall
            .domain
            .iter()
            .map(|_| self.new_meta())
            .collect::<Vec<_>>();

        let mut codomain = forall.data.instantiate(parameters.clone());
        for ((name, _), hole) in forall.domain.iter().zip(parameters) {
            codomain = codomain.replace(name.definition, hole);
        }

        codomain
    }

    fn quantify(&self, ty: Rho) -> Sigma {
        ty
    }

    /// Creates a new meta type. This is used to
    /// create a new empty hole.
    fn new_meta(&mut self) -> Tau {
        self.env.level += 1;

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
            TypeRep::Hole => self.new_meta(), // Infer the type
            TypeRep::Error(_) => Tau::ERROR,

            // SECTION: Primary Types
            TypeRep::Unit => Tau::UNIT,
            TypeRep::Type => Tau::TYPE,
            // We should not resolve the type here, but rather
            // create a new type variable, and as it is resolved, we
            // can replace it with the actual type.
            TypeRep::Path(reference) => self
                .type_env
                .get(&reference.definition(self.db))
                // Tries to get from the environment, if it fails, it means
                // that, the type variable is really a type variable, and
                // not a constructor.
                //
                // A type variable is not rigid!
                .unwrap_or_else(|| {
                    let name = self.create_new_name(reference.definition(self.db));

                    Tau::Bound(Bound::Flexible(name))
                }),
            TypeRep::App(app) => {
                let callee = self.eval(app.callee(self.db));

                app.arguments(self.db)
                    .into_iter()
                    .fold(callee, |acc, next| {
                        Tau::App(acc.into(), self.eval(next).into())
                    })
            }
            TypeRep::Arrow(fun) if matches!(fun.kind(self.db), ArrowKind::Fun) => {
                // Transforms a type representation of pi arrow into
                // a semantic type arrow with a pi arrow
                let value = self.eval(fun.value(self.db));

                fun.parameters(self.db)
                    .into_iter()
                    .map(|parameter| {
                        // Checks the type of the parameter
                        let ty = self.eval(parameter.parameter_type(self.db));

                        parameter.binding(self.db).check(ty, self)
                    })
                    .fold(value, |acc, next| {
                        Tau::Pi(HoasPi {
                            name: None,
                            domain: next.into(),
                            codomain: Rc::new(move |_| acc.clone().into()),
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
                    .filter_map(|parameter| {
                        // Checks the type of the parameter
                        let ty = self.eval(parameter.parameter_type(self.db));
                        let binding = parameter.binding(self.db);

                        let location = binding.location(self.db);

                        // Checks the binding
                        let ty = binding.clone().check(ty, self);

                        // Stores the parameter in the environment
                        // as debug information for the type checker
                        // build a table.
                        self.parameters.insert(parameter, ty);

                        // Gets the forall's binding name.
                        let name = match binding {
                            Pattern::Hole | Pattern::Wildcard(_) | Pattern::Error(_) => {
                                let location = HirLocation::new(self.db, location);

                                Name {
                                    definition: unresolved(self.db, location),
                                    name: "_".into(),
                                }
                            }
                            Pattern::Binding(binding) => Name {
                                definition: binding.name(self.db),
                                name: binding.name(self.db).to_string(self.db),
                            },
                            _ => {
                                return self.accumulate(ThirDiagnostic {
                                    id: ErrorId("unsupported-pattern"),
                                    location: self.new_location(location),
                                    message: message!("unsupported pattern in forall type"),
                                })
                            }
                        };

                        // Falls back to a star if the type is not
                        // specified.
                        //
                        // NOTE: currently the type can't be specified
                        // so, whatever.
                        Some((name, Type::TYPE))
                    })
                    .collect::<Vec<_>>();

                Tau::Forall(Qual::new(HoasForall {
                    domain: parameters,
                    codomain: Rc::new(move |_domain| value.clone()),
                }))
            }
            TypeRep::SelfType => self.self_type.clone().unwrap_or_else(|| {
                self.accumulate(ThirDiagnostic {
                    id: ErrorId("unbounded-self-type"),
                    location: self.new_location(type_rep.location(self.db)),
                    message: message!("self type outside of a self context"),
                })
            }),
            // SECTION: Unsupported
            TypeRep::Arrow(_) => self.accumulate(ThirDiagnostic {
                id: ErrorId("unsupported-arrow-kind"),
                location: self.new_location(type_rep.location(self.db)),
                message: message!("sigma types is not supported yet"),
            }),
            TypeRep::QPath(_) => self.accumulate(ThirDiagnostic {
                id: ErrorId("unsupported-qpath-kind"),
                location: self.new_location(type_rep.location(self.db)),
                message: message!("qualified types is not supported yet"),
            }),
            // SECTION: Expressions
            TypeRep::Downgrade(expr) => expr.infer(self),
        }
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

    fn reference(&mut self, reference: Reference) -> Tau {
        let let_ty = self
            .env
            .variables
            .get(&reference.definition(self.db))
            .cloned()
            .unwrap_or_else(|| {
                let name = reference.definition(self.db).to_string(self.db);

                panic!("variable not found {}", name);
            });

        self.instantiate(let_ty)
    }

    fn constructor(&mut self, reference: Reference) -> Tau {
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

    fn create_new_name(&self, definition: Definition) -> Name {
        let name = definition.to_string(self.db);

        Name {
            definition,
            name: name.clone(),
        }
    }

    fn add_to_env(&mut self, parameter: Definition) -> Level {
        /// Generates a fresh name for a parameter
        fn fresh(ctx: &InferCtx, mut name: String) -> String {
            while ctx.env.names.contains(&name) {
                name.push('\'');
            }

            name
        }

        let refresh = fresh(self, parameter.to_string(self.db));
        let level = self.env.level + 1;

        self.debruijin_index.insert(level, refresh.clone());
        self.env.level += 1;
        self.env.names.push_back(refresh);

        level
    }

    // Creates a new definition in the environment
    fn extend(&mut self, name: Definition, tau: Tau) {
        self.type_env.insert(name, tau);
    }

    /// Creates a new snapshot, just like a photograph, of
    /// the current context.
    pub fn snapshot(&self) -> Snapshot {
        // Computes the current location
        let location = self.new_location(self.location.clone());

        Snapshot {
            location,
            pkg: self.pkg,
            diagnostics: self.diagnostics.clone(),
            env: self.env.clone(),
            adhoc_env: self.adhoc_env.clone(),
            type_env: self.type_env.clone(),
            eval_env: self.eval_env.clone(),
            self_type: self.self_type.clone(),
            return_type: self.return_type.clone(),
            expressions: self.expressions.clone(),
            parameters: self.parameters.clone(),
            declarations: self.declarations.clone(),
            debruijin_index: self.debruijin_index.clone(),
        }
    }

    /// Accumulates a diagnostic and returns a default value. This is used
    /// to accumulate diagnostics and return a default value.
    pub fn accumulate<T: Default>(&self, mut diagnostic: ThirDiagnostic) -> T {
        if let ThirLocation::CallSite = diagnostic.location {
            diagnostic.location = self.new_location(self.location.clone());
        }

        self.diagnostics.borrow_mut().push(diagnostic);

        // We return the default value
        Default::default()
    }
}
