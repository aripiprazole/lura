use fxhash::FxBuildHasher;
use lura_diagnostic::{code, message, ErrorId};
use std::{fmt::Display, ops::Deref};

use crate::ftv::{Ftv, Fv};
use crate::infer::InferCtx;
use crate::thir::{ThirDiagnostic, ThirLocation};
use lura_hir::source::expr::Expr;
use lura_hir::source::pattern::{Constructor, Pattern};
use lura_hir::source::HirElement;
use type_rep::state;

use crate::type_rep::state::Hoas;
use crate::type_rep::{self, Bound, Name, Quote, Type};

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassEnv {
    pub type_predicates: im_rc::HashMap<Type<Hoas>, Vec<Predicate<Hoas>>>,
    pub expressions: im_rc::HashMap<Expr, Vec<Predicate<Hoas>>>,
}

/// Represents a class, i.e. a collection of types that share a common
/// interface.
pub struct Trait<S: state::TypeState> {
    pub superclasses: Vec<String>,
    pub instances: Vec<Instance<S>>,
}

pub type Instance<S> = Qual<S, Predicate<S>>;

/// Represents a qualified type, i.e. a type with predicates.
///
/// For example, `Eq a => a` is a qualified type, where `Eq a` is the predicate
/// and `a` is the type value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qual<S: state::TypeState, T> {
    pub predicates: Vec<Predicate<S>>,
    pub data: T,
}

impl<S: state::TypeState, T> Qual<S, T> {
    /// Creates a new qualified type.
    pub fn new(value: T) -> Self {
        Self {
            predicates: vec![],
            data: value,
        }
    }
}

impl<T: Quote> Quote for Qual<state::Hoas, T> {
    type Sealed = Qual<state::Quoted, T::Sealed>;

    fn seal(self) -> Self::Sealed {
        Qual {
            predicates: self.predicates.into_iter().map(|p| p.seal()).collect(),
            data: self.data.seal(),
        }
    }
}

impl<S: state::TypeState, T> Deref for Qual<S, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<M: state::TypeState, T: Display> Display for Qual<M, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.predicates.is_empty() {
            return self.data.fmt(f);
        }

        for pred in &self.predicates {
            write!(f, "{pred}")?;
        }
        write!(f, " => ")?;
        write!(f, "{}", self.data)
    }
}

/// No predicates are found in the type.
pub fn no_preds() -> Preds {
    Preds::default()
}

/// Represents a list of predicates. A list of constraints
/// that should constrain a type.
pub type Preds = Vec<Predicate<Hoas>>;

/// Represents a predicate, i.e. a constraint on a type.
///
/// For example, `Eq a` is a predicate, where `Eq` is the class and `a` is the
/// type value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Predicate<S: state::TypeState> {
    None,
    IsIn(Name, Vec<Type<S>>),
}

/// NOTE: This is made from scratch because `S` doesn't have to be [`Default`].
#[allow(clippy::derivable_impls)]
impl<S: state::TypeState> Default for Predicate<S> {
    // Creates a new predicate, based on a type value.
    fn default() -> Self {
        Predicate::None
    }
}

impl Predicate<state::Hoas> {
    /// Creates a new predicate, based on a type value.
    ///
    /// Destructs the pattern and creates a predicate based on the type value.
    pub(crate) fn new(
        ctx: &mut InferCtx,
        pattern: Pattern,
    ) -> Option<(Self, im_rc::HashSet<Fv, FxBuildHasher>)> {
        /// Defines a normalised predicate.
        #[allow(dead_code)]
        enum Normalised {
            Binding(Option<Name>),
            Constructor(Name, Vec<Option<Name>>),
        }

        // Normalises the pattern.
        impl Default for Normalised {
            fn default() -> Self {
                Normalised::Binding(Option::None)
            }
        }

        /// Defines a nesting, level, if it is like (Eq (Eq a)), the second
        /// `Eq` is nested.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[allow(dead_code)]
        enum Level {
            Nested,
            Valid,
        }

        /// Normalised the pattern based on level and a database.
        ///
        /// The database is used here to make sure that that we
        /// report the errors.
        fn normalise(ctx: &mut InferCtx, level: Level, pattern: Pattern) -> Normalised {
            match pattern {
                // Unnamed
                Pattern::Hole | Pattern::Wildcard(_) | Pattern::Rest(_) | Pattern::Error(_) => {
                    Normalised::Binding(Option::None)
                }

                // Constructor pattern
                Pattern::Constructor(constructor) if level == Level::Valid => {
                    let definition = match constructor.name {
                        Constructor::Path(path) => path.definition(ctx.db),
                        _ => {
                            return ctx.accumulate(ThirDiagnostic {
                                location: ctx.new_location(constructor.location.clone()),
                                message: message!["constructor is not a valid predicate"],
                                id: ErrorId("invalid-pred"),
                            });
                        }
                    };

                    // Create a type system internal name.
                    let name = Name {
                        definition,
                        name: definition.to_string(ctx.db),
                    };

                    // Normalise the arguments.
                    let arguments = constructor
                        .arguments
                        .into_iter()
                        // Lowers the level of nesting, to make sure that we
                        // don't have a predicate like (Eq (Eq a)).
                        .map(|p| match normalise(ctx, Level::Nested, p) {
                            Normalised::Binding(name) => name,
                            Normalised::Constructor(_, _) => panic!("invalid predicate"),
                        })
                        .collect();

                    Normalised::Constructor(name, arguments)
                }
                Pattern::Binding(pattern) => Normalised::Binding(Some(Name {
                    definition: pattern.name,
                    name: pattern.name.to_string(ctx.db),
                })),

                // SECTION: Errors
                Pattern::Literal(literal) => ctx.accumulate(ThirDiagnostic {
                    location: ctx.new_location(literal.location.clone().unwrap_or_default()),
                    message: message!["literal is not a valid predicate"],
                    id: ErrorId("invalid-pred"),
                }),
                Pattern::Constructor(constructor) => ctx.accumulate(ThirDiagnostic {
                    location: ctx.new_location(constructor.location.clone()),
                    message: message!["nested predicate is not a valid predicate"],
                    id: ErrorId("invalid-pred"),
                }),
            }
        }

        use Level::*;

        // Normalise the pattern as predicate
        let Normalised::Constructor(name, values) = normalise(ctx, Valid, pattern.clone()) else {
            return ctx.accumulate(ThirDiagnostic {
                location: ctx.new_location(pattern.location(ctx.db).clone()),
                message: message!["invalid predicate, expected a constructor pattern"],
                id: ErrorId("invalid-pred"),
            });
        };

        // Map the arguments to types
        let arguments = values
            .into_iter()
            .map(|name| match name {
                Some(name) => Type::Bound(Bound::Flexible(name)),
                None => Type::Bound(Bound::Hole),
            })
            .collect::<Vec<_>>();

        // Collects all free variables to be constructed a full featured
        // predicate, and forall quantifier.
        let free_variables = arguments.iter().flat_map(|tau| tau.ftv()).collect();

        // Create a predicate
        Some((Predicate::IsIn(name, arguments), free_variables))
    }

    pub(crate) fn instantiate(self, ctx: &mut InferCtx) -> Self {
        let Predicate::IsIn(ref name, ref types) = self.clone().force() else {
            return self;
        };

        // Collects all free variables to be constructed a full featured
        // predicate, and forall quantifier.
        let free_variables = types.iter().flat_map(|tau| tau.ftv()).collect::<Vec<_>>();

        // Instantiate the types
        let mut new_types = vec![];
        for mut tau in types.iter().cloned() {
            for variable in free_variables.clone() {
                if let Bound::Flexible(name) = variable {
                    tau = tau.replace(name.definition, ctx.new_meta());
                }
            }
            new_types.push(tau);
        }

        // Create a predicate
        Predicate::IsIn(name.clone(), new_types)
    }

    /// Returns `true` if the predicate is in head normal form.
    pub fn is_hnf(&self) -> bool {
        fn type_is_hnf(tau: Type<state::Hoas>) -> bool {
            match tau.force() {
                Type::App(callee, argument) => type_is_hnf(*callee) && type_is_hnf(*argument),
                Type::Forall(_) => false,
                Type::Pi(_) => false,
                Type::Hole(_) => false,
                Type::Bound(_) => false,
                _ => true,
            }
        }

        match self {
            Predicate::IsIn(_, types) => types.iter().cloned().all(type_is_hnf),
            Predicate::None => false,
        }
    }

    /// Erase the hole definitions from the predicate.
    ///
    /// This is used to make sure that we don't have any holes in the
    /// predicate.
    pub(crate) fn force(self) -> Self {
        match self {
            Predicate::IsIn(name, types) => {
                Predicate::IsIn(name, types.into_iter().map(Type::force).collect())
            }
            Predicate::None => Predicate::None,
        }
    }

    pub(crate) fn unify(self, pred: Self, ctx: &mut InferCtx) -> bool {
        match (self.force(), pred.force()) {
            // SECTION: Unify predicates
            (Predicate::IsIn(name_a, types_a), Predicate::IsIn(name_b, types_b))
                if name_a == name_b =>
            {
                for (tau_a, tau_b) in types_a.into_iter().zip(types_b.into_iter()) {
                    if !tau_a.unify(tau_b, ctx) {
                        return false;
                    }
                }
            }

            (_, _) => {
                ctx.accumulate::<()>(ThirDiagnostic {
                    location: ThirLocation::CallSite,
                    message: message!["cannot unify the predicates"],
                    id: ErrorId("cannot-unify-predicates"),
                });

                return false;
            }
        };

        true
    }

    /// Tries to evaluate the predicate, and if it is possible, it will
    /// return the predicate.
    ///
    /// It does evaluates and tries to find if there is a predicate that
    /// matches the given predicate.
    pub(crate) fn entail(&self, ctx: &mut InferCtx) -> Option<()> {
        let pred = self.clone().force();
        let Predicate::IsIn(ref name, _) = pred else {
            return None;
        };

        let preds = ctx.env.predicates.get(name)?.clone();
        for constraint in preds {
            // If the predicate is in head normal form, then we can
            // compare the predicates.
            if constraint.is_hnf() {
                if pred == constraint {
                    return Some(());
                }
            } else {
                // If the predicate is not in head normal form, then we
                // need to normalise the predicate.
                let new_constraint = constraint.clone().instantiate(ctx).force();

                if new_constraint.unify(pred.clone(), ctx) {
                    return Some(());
                }
            }
        }

        ctx.accumulate::<()>(ThirDiagnostic {
            location: ThirLocation::CallSite,
            message: message!["predicate is not defined", code!(self.quote())],
            id: ErrorId("undefined-pred"),
        });

        Some(())
    }
}

impl Quote for Predicate<state::Hoas> {
    type Sealed = Predicate<state::Quoted>;

    fn seal(self) -> Self::Sealed {
        match self {
            Predicate::IsIn(name, tau) => {
                Predicate::IsIn(name, tau.into_iter().map(Quote::seal).collect())
            }
            Predicate::None => Predicate::None,
        }
    }
}

impl<S: state::TypeState> Display for Predicate<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Predicate::None => write!(f, ""),
            Predicate::IsIn(name, types) => {
                write!(f, "({name}")?;
                for value in types {
                    write!(f, " {}", value)?;
                }
                write!(f, ")")
            }
        }
    }
}
