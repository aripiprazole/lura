use std::{cell::RefCell, fmt::Debug, mem::replace, rc::Rc};

use fxhash::FxBuildHasher;
use if_chain::if_chain;
use lura_diagnostic::{code, message, ErrorId};
use lura_hir::{
  lower::hir_lower,
  package::Package,
  solver::{unresolved, Definition, HirLevel, Reference},
  source::{
    declaration::{Declaration, Parameter},
    expr::{Callee, Expr},
    literal::Literal,
    pattern::{Constructor, Pattern},
    stmt::{Block, Stmt},
    top_level::{BindingGroup, InstanceDecl, TopLevel},
    type_rep::{AppTypeRep, ArrowKind, ArrowTypeRep, TypeReference, TypeRep},
    HirElement, HirLocation, Location, Spanned,
  },
};

use crate::{
  adhoc::{no_preds, ClassEnv, Predicate, Preds, Qual},
  options::TyperFeatures,
  thir::{ThirDiagnostic, ThirLocation, ThirTextRange},
  type_rep::{
    holes::{HoleKind, HoleRef},
    pi::Pi,
    *,
  },
  utils::LocalDashMap,
};

/// Represents the type errors that can occur during type checking,
/// specially on the unification step.
///
/// This reports many errors, that can be renamed to be more descriptive.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeError {
  CannotUnify(Type, Type),
  OccursCheck(Type),
  EscapingScope(Level),
  IncompatibleTypes { expected: Definition, actual: Definition },
  IncompatibleValues { expected: Primary, actual: Primary },
  IncorrectLevel { expected: Level, actual: Level },
  IncorrectArity { expected: usize, actual: usize },
}

/// Internal variants to the type checker.
#[derive(Default, Clone, Hash)]
pub struct InternalVariant {
  pub name: Type,
  pub parameters: im_rc::Vector<Type>,
}

/// Represents the type environment used to type check the program.
///
/// The type environment is used to store the types of the variables
/// and the constructors, and also the level of the variables.
#[derive(Default, Clone)]
pub struct TypeEnv {
  pub level: Level,
  pub predicates: im_rc::HashMap<Name, im_rc::HashSet<Predicate>, FxBuildHasher>,
  pub constructors: im_rc::HashMap<Definition, Rc<InternalVariant>, FxBuildHasher>,
  pub references: im_rc::HashMap<Type, Rc<InternalVariant>, FxBuildHasher>,
  pub variables: im_rc::HashMap<Definition, Type, FxBuildHasher>,
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
  fn hole_unify_prechecks(&mut self, tau: Type, scope: Level, hole: HoleRef) {
    let _ = scope;
    let _ = hole;

    match tau {
      // SECTION: Types
      Type::Forall(_) => {
        // Named holes, because this is a dependent
        // function type, we need to create a new hole
        // for each parameter
        // let domain = forall
        //     .domain
        //     .clone()
        //     .into_iter()
        //     .map(|(name, _)| Tau::Bound(Bound::Flexible(name)))
        //     .collect::<Vec<_>>();

        // Exeuctes the HOAS function to get the codomain
        // and then evaluates it to WHNF
        // let _codomain = forall.instantiate(domain);

        // TODO: check dependent kinds
        // self.hole_unify_prechecks(domain, scope, hole.clone());
        // self.hole_unify_prechecks(codomain, scope, hole)
      }
      Type::Pi(pi) => {
        // TODO
        // let ctx = self.ctx.snapshot();
        // let domain = pi.domain.eval(&ctx, self.ctx.eval_env.clone());
        //
        // // Unnamed holes, because this isn't a dependent
        // // function type, we can just use the same hole
        // let parameter = Tau::Bound(match pi.name {
        //   Some(ref name) => Bound::Flexible(name.clone()),
        //   None => Bound::Hole,
        // });
        // self.hole_unify_prechecks(domain, scope, hole.clone());
        // self.hole_unify_prechecks(codomain, scope, hole)
      }
      // SECTION: Hole
      Type::Rigid(lvl, spine) => {
        for type_rep in spine.into_iter() {
          self.hole_unify_prechecks(type_rep, scope, hole.clone());
        }
      }
      flexible @ Type::Flexible(mut h, ref spine) => {
        use holes::HoleKind::*;
        if h == hole {
          self.errors.push_back(TypeError::OccursCheck(flexible));
          return;
        }
        for type_rep in spine {
          self.hole_unify_prechecks(type_rep.clone(), scope, hole.clone());
        }
        match h.kind() {
          Empty { scope: l } if l > scope => h.set_kind(Empty { scope }),
          Filled(type_rep) => self.hole_unify_prechecks(type_rep.clone(), scope, hole),
          _ => {}
        }
      }
      _ => {}
    }
  }

  /// Unifies a hole with a type. This is used to
  /// unify a hole with a type.
  fn unify_hole(&mut self, type_repr: Type, hole: HoleRef) -> Option<()> {
    use holes::HoleKind::*;

    // I don't know but this is needed to avoid reborrow
    match hole.kind() {
      // SECTION: Sentinel Values
      Error => None,

      // SECTION: Holes
      Empty { scope } => {
        // Checks that the hole doesn't occur in the type
        // if type_rep == Tau::Hole(hole.clone()) {
        //   return None;
        // }
        if let Type::Flexible(h, spine) = type_repr.clone() {
          if h == hole {
            self.errors.push_back(TypeError::OccursCheck(Type::Flexible(h, spine)));
            return None;
          }
        }

        // Unify the hole with the type, and then set the kind
        // of the hole to filled
        self.hole_unify_prechecks(type_repr.clone(), scope, hole.clone());
        hole.set_kind(Filled(type_repr));
        Some(())
      }
      Filled(a) => self.internal_unify(a, type_repr),
    }
  }

  /// Unifies two spines. This is used to unify two list of applications
  /// of types. It equates the two spines.
  fn unify_sp(&mut self, spine_a: &[Type], spine_b: &[Type]) {
    if spine_a.len() != spine_b.len() {
      // TODO: error
      return;
    }

    for (item_a, item_b) in spine_a.iter().zip(spine_b) {
      self.internal_unify(item_a.clone(), item_b.clone())?;
    }
  }

  /// Unifies two types. This is used to unify two types. It
  /// equates the two types.
  #[rustfmt::skip]
  fn internal_unify(&mut self, a: Type, b: Type) -> Option<()> {
    // Matches the types to equate them.
    match (a, b) {
      // SECTION: Unification
      (Type::Flexible(hole_a, spine_a), Type::Flexible(hole_b, spine_b)) if hole_a == hole_b => self.unify_sp(&spine_a, &spine_b),
      (Type::Rigid(hole_a, spine_a)   ,    Type::Rigid(hole_b, spine_b)) if hole_a == hole_b => self.unify_sp(&spine_a, &spine_b),
      (Type::Flexible(hole_a, spine_a),                        type_rep) => self.unify_hole(type_rep, hole_a)?,
      (type_rep                       , Type::Flexible(hole_b, spine_b)) => self.unify_hole(type_rep, hole_b)?,

      // SECTION: Concrete values
      (Type::Primary(value_a)         ,        Type::Primary(value_b)) if value_a == value_b => {}
      (Type::Primary(value_a)         ,        Type::Primary(value_b)) if value_a != value_b => {
        self.errors.push_back(TypeError::IncompatibleValues {
          expected: value_a,
          actual: value_b,
        });

        return None;
      }
      (Type::Universe                 ,                Type::Universe) => {}
      (_                              , Type::Primary(Primary::Error)) => {}
      (Type::Primary(Primary::Error)  ,                             _) => {}

      // SECTION: Type level functions
      (Type::Pi(pi_a)                 , Type::Pi(pi_b)) => {
        let Pi {name: _, domain: box domain_a, codomain: codomain_a} = pi_a;
        let Pi {name: _, domain: box domain_b, codomain: codomain_b} = pi_b;

        self.internal_unify(domain_a, domain_b)?;
      }
      (Type::Forall(forall_a), Type::Forall(forall_b)) => {
        // SECTION: OLD HINDLEY MILNER CODE
        //
        // ```rs
        // let mut acc_a = *forall_a.data.value.clone();
        // let mut acc_b = *forall_b.data.value.clone();
        // for (param_a, param_b) in forall_a.domain.iter().zip(forall_b.domain.iter()) {
        //     let level = self.ctx.add_to_env(param_a.name);
        //     let name = self.ctx.create_new_name(param_a.name);
        //     let debruijin = Tau::Bound(Bound::Index(name, level));
        //     acc_a = acc_a.replace(param_a.name, debruijin.clone());
        //     acc_b = acc_b.replace(param_b.name, debruijin);
        // }
        //
        // // Unify the results to compare the results
        // self.internal_unify(acc_a, acc_b);
        // ```
        //
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

        return None;
      }

      // SECTION: Type Error
      // Report accumulating type check error, when the types
      // cannot be unified.
      (a, b) => {
        self.errors.push_back(TypeError::CannotUnify(a, b));

        return None;
      }
    };

    Some(())
  }

  /// Accumulate all errors that were reported during the unification
  /// into the context, reporting it as THIR diagnostics.
  ///
  /// This is used to report all errors at once, instead of reporting
  /// them one by one, so we can change the error message if there's
  /// another message for the context!
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
            message!["incorrect level, expected", code!(expected), "but found", code!(actual),]
          }
          TypeError::IncorrectArity { expected, actual } => {
            message!["incorrect arity, expected", code!(expected), "but found", code!(actual),]
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
    f.debug_struct("Substituition").field("errors", &self.errors).finish()
  }
}

/// Defines the inference crate, it does make
/// the work to infer the type of the expression.
///
/// It also stores a key to the type, so we can index
/// it later, and report errors, etc.
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

  fn check(self, type_repr: Type, ctx: &mut InferCtx) -> Self::Output;
}

impl Check for Pattern {
  type Output = Type;

  /// Checks the pattern against the type. This is used to
  /// check the pattern against the type.
  ///
  /// It does generate definition for the bindings in
  /// the context.
  fn check(self, repr: Type, ctx: &mut InferCtx) -> Self::Output {
    match self {
      // SECTION: Sentinel Values
      Pattern::Hole => repr, // It's a hole pattern
      Pattern::Error(_) => Type::Primary(Primary::Error),

      // SECTION: Patterns
      Pattern::Literal(literal) => {
        // Unifies the actual type with the expected type
        let tau = literal.infer(ctx);
        tau.unify(repr, ctx);
        tau
      }
      Pattern::Constructor(constructor) => {
        // TODO: handle other builtins
        //
        // Unifies the actual type with the expected type
        let tau = constructor.name(ctx.db).infer_with(&constructor, ctx);
        tau.unify(repr, ctx);

        // Checks the parameters of the constructor agains't
        // the arguments of the constructor
        let variant = ctx.env.references.get(&tau).cloned().unwrap_or_default();

        // Gets the parameters of the constructor
        let parameters = variant.parameters.clone();

        for (argument, repr) in constructor.arguments(ctx.db).into_iter().zip(parameters) {
          // Checks the argument against the type
          argument.check(repr, ctx);
        }

        tau
      }
      // Returns the default type for the pattern
      //   - Wildcard
      //   - Binding
      Pattern::Binding(binding) => {
        // Adds the binding to the context
        let name = binding.name(ctx.db);
        ctx.env.variables.insert(name, repr.clone());
        repr
      }
      Pattern::Wildcard(_) => repr,
      Pattern::Rest(_) => todo!("rest pattern"),
    }
  }
}

impl Infer for Constructor {
  type Output = Type;

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
  type Output = (Preds, Type);

  /// Infers the type of the callee. This is used
  /// to infer the type of the callee.
  fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
    match self {
      // SECTION: Callee
      Callee::Unit => (no_preds(), Type::Primary(Primary::Unit)), // Unit = Unit
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
  type Output = (Preds, Type);

  // Associate the type with the expression
  // This is used to access the type of the expression in
  // a elaboration step
  fn store_ty_var(self, target: Self::Output, ctx: &mut InferCtx) {
    // Associate the type with the expression
    ctx.expressions.insert(self, target.1);
  }

  /// Infers the type of the expression. This is used
  /// to infer the type of the expression.
  fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
    match self {
      // SECTION: Sentinel Values
      Expr::Empty => (no_preds(), Type::Primary(Primary::Error)),
      Expr::Error(_) => (no_preds(), Type::Primary(Primary::Error)),

      // SECTION: Expressions
      // Handles literals, references and expressions that are not handled by other cases
      Expr::Path(reference) => ctx.reference(reference),
      Expr::Literal(literal) => (no_preds(), literal.infer(ctx)),
      Expr::Call(call) => {
        let mut preds = vec![];
        let callee = call.callee(ctx.db);

        // Infers the type of each argument
        let parameters = call
          .arguments(ctx.db)
          .into_iter()
          .map(|argument| {
            let (local_preds, tau) = argument.infer(ctx);

            // Adds the local predicates to the global predicates
            preds.extend(local_preds);

            tau
          })
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
              0 => (no_preds(), Type::Primary(Primary::Unit)),
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
              (no_preds(), ty)
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

              return (no_preds(), ctx.new_meta());
            };

            // TODO: return monad
            match arguments.len() {
              // Unit type
              0 => {
                return_type.unify(Type::Primary(Primary::Unit), ctx);
                (no_preds(), return_type)
              }
              // Group type
              1 => {
                // Creates a new type variable to represent the type of the result
                //
                // And returns predicates and return type.
                let (preds, tau) = arguments[0].clone().infer(ctx);
                return_type.unify(tau, ctx);
                (preds, return_type)
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
            let (new_preds, callee) = callee.infer_with(&call, ctx);

            // Unify the callee with the pi type
            pi.unify(callee, ctx);

            // // Creates a snapshot of the context, to make sure that
            // // the predicates are present in the context.
            // let snapshot = ctx.snapshot();

            // Normalises the predicates to make sure that
            // the predicates are in the right form.
            //
            // And to check if they are present in the context!
            for predicate in new_preds.clone() {
              let entailment = predicate.entail(ctx);

              match (entailment, &ctx.type_rep) {
                (Ok(local), _) => {
                  // Adds the local predicates to the global predicates
                  let constraints = local.values().flat_map(|p| p.clone());

                  preds.extend(constraints);
                }
                (Err(_), Some(location)) => {
                  ctx.accumulate::<()>(ThirDiagnostic {
                    location: ctx.new_location(location.clone()),
                    id: ErrorId("missing-predicates in the context"),
                    message: message!["missing predicates in the context, you should add it here"],
                  });
                }
                _ => {}
              }
            }

            // Adds the new predicates to the global predicates
            preds.extend(new_preds);

            // Returns the type of the result
            (preds, hole)
          }
        }
      }

      Expr::Ann(ann) => {
        let tau = ctx.translate(ann.type_rep(ctx.db));
        let value = ann.value(ctx.db);

        value.check(tau, ctx)
      }

      Expr::Abs(abs) => {
        let env = ctx.env.clone();
        let parameters = abs.parameters(ctx.db);

        // Creates a new environment with the parameters locally, and
        // infers the type of the body
        ctx.with_env(env, |local| {
          // Adds the parameters to the context
          for parameter in parameters.into_iter() {
            let type_rep = local.translate(parameter.parameter_type(local.db));

            let ty = parameter.binding(local.db).check(type_rep, local);

            // Stores the parameter in the environment
            // as debug information for the type checker
            // build a table.
            local.parameters.insert(parameter, ty.clone());
          }

          abs.value(local.db).infer(local)
        })
      }

      Expr::Match(match_expr) => {
        let (preds, scrutinee) = match_expr.scrutinee(ctx.db).infer(ctx);

        // Creates a new type variable to represent the type of the value
        let hole = (preds, ctx.new_meta());

        // Infers the type of each arm, to find the common type
        // between all of their arms' values
        match_expr
          .clauses(ctx.db)
          .into_iter()
          .fold(hole, |(mut preds, acc), arm| {
            // Checks the pattern against the scrutinee
            arm.pattern.check(scrutinee.clone(), ctx);

            // Checks against the old type, to check if both are compatible
            let (new_preds, value) = arm.value.check(acc, ctx);
            preds.extend(new_preds);
            (preds, value)
          })
      }

      // SECTION: Type Representations
      // Evaluates the type of the expression, and then checks if it is
      // compatible with the expected type
      Expr::Upgrade(type_rep) => (no_preds(), ctx.translate(*type_rep)),
    }
  }
}

impl Infer for Spanned<Literal> {
  type Output = Type;

  /// Infers the type of the literal. This is used
  /// to infer the type of the literal.
  fn internal_infer(self, _: &mut InferCtx) -> Self::Output {
    match self.value {
      // SECTION: Sentinel Values
      Literal::Empty => Type::Primary(Primary::Error),

      // SECTION: Literals
      Literal::Int8(_) => Type::Primary(Primary::I8),
      Literal::UInt8(_) => Type::Primary(Primary::U8),
      Literal::Int16(_) => Type::Primary(Primary::I16),
      Literal::UInt16(_) => Type::Primary(Primary::U16),
      Literal::Int32(_) => Type::Primary(Primary::I32),
      Literal::UInt32(_) => Type::Primary(Primary::U32),
      Literal::Int64(_) => Type::Primary(Primary::I64),
      Literal::UInt64(_) => Type::Primary(Primary::U64),
      Literal::String(_) => Type::Primary(Primary::String),
      Literal::Boolean(_) => Type::Primary(Primary::Bool),
      Literal::Char(_) => Type::Primary(Primary::Char),
    }
  }
}

impl Infer for TopLevel {
  type Output = Type;

  /// Infers the type of the top level. This is used
  /// to infer the type of the top level.
  ///
  /// It does not return a value, as it is a top level
  /// declaration. It does only add to the context and
  /// return unit type.
  fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
    /// Creates a new predicate based on instance to add in the context
    /// and returns the predicate.
    #[inline]
    fn create_predicate_instance(ctx: &mut InferCtx, instance: InstanceDecl) {
      let name = instance.name(ctx.db);
      let _parameters = instance
        .parameters(ctx.db)
        .into_iter()
        .map(|parameter| {
          // Creates a new type variable to represent
          // the type of the parameter
          let type_rep = ctx.translate(parameter.parameter_type(ctx.db));

          // // Evaluates the type of the parameter
          // let type_rep = type_rep.eval(&ctx.snapshot(), ctx.eval_env.clone());

          // Stores the parameter in the environment
          // as debug information for the type checker
          // build a table.
          ctx.parameters.insert(parameter, type_rep.clone());

          type_rep
        })
        .collect::<Vec<_>>();

      let types = instance
        .types(ctx.db)
        .into_iter()
        .map(|tau| {
          // Creates a new type variable to represent
          // the type of the implementation
          ctx.translate(tau)
        })
        .collect::<Vec<_>>();

      // Creates the name of the type
      let name = Name {
        definition: name,
        name: name.to_string(ctx.db),
      };

      // TODO: Add trait to the context, if it's polymorphic
      ctx
        .env
        .predicates
        .entry(name.clone())
        .or_default()
        .insert(Predicate::IsIn(name, types));
    }

    /// Creates a new type variable to represent the type of the declaration
    ///
    /// The ty is optional because it does fallback to the type of the
    /// declaration, if it is not provided.
    #[inline]
    fn create_declaration_type(
      ctx: &mut InferCtx,
      // Used to define if the type is a type Type, or
      // if it should use the return type.
      // has_type_type: bool,
      use_return_type: bool,
      decl: impl Declaration,
    ) -> Type {
      let name = decl.name(ctx.db);
      let parameters = decl
        .parameters(ctx.db)
        .into_iter()
        .map(|parameter| {
          // Creates a new type variable to represent
          // the type of the parameter
          let type_rep = ctx.translate(parameter.parameter_type(ctx.db));

          // // Evaluates the type of the parameter
          // let type_rep = type_rep.eval(&ctx.snapshot(), ctx.eval_env.clone());

          // Stores the parameter in the environment
          // as debug information for the type checker
          // build a table.
          ctx.parameters.insert(parameter, type_rep.clone());

          type_rep
        })
        .collect::<Vec<_>>();

      let name = Name {
        definition: name,
        name: name.to_string(ctx.db),
      };

      let constructor = match decl.type_rep(ctx.db) {
        Some(TypeRep::Error(_)) => Type::constructor(name.clone()),
        Some(TypeRep::Hole) if !use_return_type => {
          // type List a b = Cons a b
          // Type -> Type -> Type
          let goal = Type::constructor(name.clone());

          // Generate the type-level function
          // that represents the declaration
          let return_type = parameters.iter().fold(goal, |acc, parameter| {
            // Creates a new type variable to represent
            // the type of the declaration
            Type::Pi(Pi {
              name: None,
              domain: parameter.clone().into(),
              codomain: acc.quote(&ctx),
            })
          });

          ctx.extend(name.definition, return_type.clone());
          return return_type;
        }
        Some(TypeRep::Arrow(arrow)) if arrow.kind(ctx.db) == ArrowKind::Forall => {
          // # Safety
          // We already checked if the type is a forall, so we can
          // assume it's safe to unwrap, as it's needs to be a [`Some`]
          // to be a forall.
          let type_rep = unsafe { decl.type_rep(ctx.db).unwrap_unchecked() };

          // Creates a parameter iterator
          let parameters = parameters.into_iter();

          // Checks if arrow is already generalised. If the `forall` is first level,
          // then it's already generalised, so we don't need to quantify it!
          //
          // Creates the type of the variant
          let variant_type = Type::from_pi(parameters, ctx.translate(type_rep));

          // Early return if the type is already generalised
          ctx.extend(name.definition, variant_type.clone());
          return variant_type;
        }
        Some(type_rep) => ctx.translate(type_rep),
        None => Type::constructor(name.clone()),
      };

      // Creates the type of the variant
      let type_rep = Type::from_pi(parameters.into_iter(), constructor);

      // Quantifies the type of the type representation
      // to get it's type-level function
      let type_rep = ctx.quantify(type_rep);

      ctx.extend(name.definition, type_rep.clone());
      type_rep
    }

    /// Checks the binding group agains't it's return types
    #[inline]
    fn check_binding_group(ctx: &mut InferCtx, binding_group: BindingGroup) {
      // Creates the type of the binding group using the
      // signature of the binding group
      let function_type = create_declaration_type(ctx, true, binding_group.signature(ctx.db));
      let old_location = replace(
        &mut ctx.location,
        // Tries to get the location of the type, but if
        // it's should be inferred, we should get the
        // location of the function directly.
        binding_group
          .type_rep(ctx.db)
          .map(|type_rep| type_rep.location(ctx.db))
          .unwrap_or(binding_group.location(ctx.db)),
      );

      // Adds the value to the environment
      let name = binding_group.name(ctx.db);

      // Creates the type of the binding group using the
      // signature of the binding group
      let return_type = ctx.new_meta();

      // Gets the parameter types
      let parameters = binding_group
        .parameters(ctx.db)
        .into_iter()
        .map(|parameter| {
          // Translates the type of the parameter
          // into semantic information.
          let type_rep = ctx.translate(parameter.parameter_type(ctx.db));

          // Creates the type of the parameter
          if_chain! {
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

              // Creates a new type variable to represent
              // the type of the declaration
              // Tau::Bound(Bound::Index(name, level))
              todo!()
            } else {
              type_rep
            }
          }
        })
        .collect::<im_rc::Vector<_>>();

      let pi = Type::from_pi(parameters.clone().into_iter(), return_type.clone());
      let mut preds = no_preds();

      if let Type::Forall(ref forall) = function_type {
        // Transforms into a normal `Vec`
        let qual = forall.instantiate(parameters.clone().into_iter().collect());
        preds = qual.predicates;
      }

      // Gets the return type
      pi.unify(function_type.clone(), ctx);

      let type_repr = ctx.quantify(function_type.clone());
      ctx.env.variables.insert(name, type_repr.clone());
      log::debug!("{} : {}", name.to_string(ctx.db), type_repr.seal());

      // Creates a new environment for the clauses
      let mut env = ctx.env.clone();

      // Populate the environment with new predicates
      for pred in preds {
        if let Predicate::IsIn(ref name, _) = pred {
          // Inserts the predicate into the environment
          env.predicates.entry(name.clone()).or_default().insert(pred.clone());
        }
      }

      ctx.with_env(env, |local| {
        // Gets the type parameters and return type of the function
        let (type_parameters, return_type) = return_type.clone().spine();

        // Chain all parameters
        let parameters = parameters
          .clone()
          .into_iter()
          .chain(type_parameters.clone())
          .collect::<Vec<_>>();

        // Checks the type of each clause
        for clause in binding_group.clauses(local.db) {
          let env = local.env.clone();

          // NOTE: Borrow checker stuff, so it doesn't move
          let return_type = return_type.clone();
          let parameters = parameters.clone();

          // Creates a local env for the parameters
          local.with_env(env, move |local| {
            // Checks the pattern against the scrutinee
            let arguments = clause.arguments(local.db);

            // Get the total arity of the function
            let arity = arguments.len();

            for (i, pattern) in arguments.into_iter().enumerate() {
              // Gets the type of the parameter
              let Some(type_rep) = parameters.get(i).cloned() else {
                local.accumulate::<()>(ThirDiagnostic {
                  location: local.new_location(pattern.location(local.db)),
                  message: message![
                    "too many arguments in function definition",
                    code!(name.to_string(local.db)),
                    "with",
                    code!(arity),
                    "arguments"
                  ],
                  id: ErrorId("too-many-arguments"),
                });

                break;
              };

              pattern.check(type_rep.clone(), local);
            }

            // Curry the function type to match correctly
            //
            // For example, if the function type is `a -> b -> c -> d`,
            // and a clause is well defined as:
            // ```
            // f x y z = ...
            // ```
            // We need to curry and check the type of the clause as:
            // ```
            // d
            // ```
            // Because it's the final type, but if the clause is:
            // ```
            // f x y = ...
            // ```
            // We need to curry and check the type of the clause as:
            // ```
            // c -> d
            // ```
            // Because it's the curried type of the function.
            let return_type = parameters
              .clone()
              .into_iter()
              .skip(arity)
              .fold(return_type.clone(), |acc, param| Type::from_pi(vec![param].into_iter(), acc));

            // Checks the return type of the clause
            clause.value(local.db).check(return_type.clone(), local);
          })
        }
      });

      // Returns the location
      ctx.type_rep = Some(old_location.clone());

      // Fallback to unit type, if return_type isn't defined
      //
      // The unit type is the default return type of a function
      // if it's not defined.
      if return_type.is_unbound() {
        if let Type::Flexible(ref hole, _) = return_type {
          hole.set_kind(HoleKind::Filled(Type::UNIT))
        }
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
      TopLevel::InstanceDecl(instance_declaration) => {
        // Creates a new predicate instance.
        //
        // NOTE: There's no Self for instances, as they're
        // just a collection of predicates.
        create_predicate_instance(ctx, instance_declaration);
      }
      TopLevel::TraitDecl(trait_declaration) => {
        create_declaration_type(ctx, false, trait_declaration);
      }
      TopLevel::DataDecl(data_declaration) => {
        let tau = create_declaration_type(ctx, false, data_declaration);
        let self_type = replace(&mut ctx.self_type, tau.clone().into());

        // Creates the type of the variant
        for variant in data_declaration.variants(ctx.db) {
          let name = variant.name(ctx.db);
          let parameters = variant
            .parameters(ctx.db)
            .into_iter()
            .map(|parameter| ctx.translate(parameter.parameter_type(ctx.db)))
            .collect::<im_rc::Vector<_>>();

          let variant_type = match variant.type_rep(ctx.db) {
            Some(TypeRep::Hole | TypeRep::Error(_)) => tau.clone(),
            Some(type_rep) => ctx.translate(type_rep),
            None => tau.clone(),
          };

          // Creates the type of the variant
          let variant_type = Type::from_pi(parameters.clone().into_iter(), variant_type);

          // Quantifies the type of the variant
          let variant_type = ctx.quantify(variant_type);

          // Creates the internal variant
          let internal = Rc::new(InternalVariant {
            name: tau.clone(),
            parameters,
          });
          ctx.extend(name, variant_type.clone());
          ctx.env.references.insert(tau.clone(), internal.clone());
          ctx.env.constructors.insert(name, internal);
        }

        // Returns the old self type to the original position,
        // as the self type is only valid in the data declaration
        ctx.self_type = self_type;
      }
      TopLevel::TypeDecl(type_declaration) => {
        create_declaration_type(ctx, false, type_declaration);
      }
    };

    ctx.void()
  }
}

impl Check for Expr {
  type Output = (Preds, Type);

  /// Checks the type of the expression. This is used
  /// to check the type of the expression.
  fn check(self, tau: Type, ctx: &mut InferCtx) -> Self::Output {
    match self {
      // SECTION: Sentinel Values
      Expr::Empty => (no_preds(), Type::ERROR),
      Expr::Error(_) => (no_preds(), Type::ERROR),

      // SECTION: Expressions
      _ => {
        let (preds, new_tau) = self.infer(ctx);

        // Checks that the actual type is a subtype of the expected type
        new_tau.unify(tau, ctx);
        (preds, new_tau)
      }
    }
  }
}

impl Infer for Stmt {
  type Output = (Preds, Type);

  /// Infers the type of the statement. This is used
  /// to infer the type of the statement.
  fn internal_infer(self, ctx: &mut InferCtx) -> Self::Output {
    // Create empty predicates to fulfill
    // when evaluating expressions.
    let mut preds = no_preds();

    match self {
      // SECTION: Sentinel values
      Stmt::Empty => {}
      Stmt::Error(_) => {}

      // SECTION: Statements
      Stmt::Ask(ask_stmt) => {
        // Infers the type of the value, and then checks the pattern
        let (local_preds, value_tau) = ask_stmt.value(ctx.db).infer(ctx);

        // Adds the local predicates to the global predicates
        preds.extend(local_preds);

        ask_stmt.pattern(ctx.db).check(value_tau, ctx);
      }
      Stmt::Let(let_stmt) => {
        // Infers the type of the value, and then checks the pattern
        let (local_preds, value_tau) = let_stmt.value(ctx.db).infer(ctx);

        // Adds the local predicates to the global predicates
        preds.extend(local_preds);

        let_stmt.pattern(ctx.db).check(value_tau, ctx);
      }

      // SECTION: Expressions
      Stmt::Downgrade(expr) => return expr.infer(ctx),
    };

    (preds, ctx.void())
  }
}

impl Check for Block {
  type Output = (Preds, Type);

  /// Checks the type of the block. This is used
  /// to check the type of the block.
  fn check(self, tau: Type, ctx: &mut InferCtx) -> Self::Output {
    let return_type = replace(&mut ctx.return_type, Some(tau.clone()));

    // Checks the type of each statement
    for statement in self.statements(ctx.db) {
      statement.infer(ctx);
    }

    // Checks the type of the last statement
    let (preds, new_tau) = match self.statements(ctx.db).last() {
      Some(value) => value.clone().infer(ctx),
      None => (no_preds(), Type::UNIT),
    };

    new_tau.unify(tau, ctx);
    ctx.return_type = return_type;
    (preds, new_tau)
  }
}

/// Defines an environment that is used in a evaluation context.
///
/// This is used to evaluate a type-level expression.
#[derive(Default, Clone)]
pub(crate) struct EvalEnv {
  /// The environment that is used to evaluate the expression.
  ///
  /// This is used to evaluate the expression.
  pub env: im_rc::HashMap<Definition, Type, FxBuildHasher>,
}

/// This is a copy of the type environment that is used to
/// evaluate the expression.
///
/// Have the same fields, but doesn't require lifetimes.
///
/// NODE: `#[allow(dead_code)]` is used to suppress the warning
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
  pub env: TypeEnv,
  pub adhoc_env: ClassEnv,
  pub eval_env: EvalEnv,
  pub type_env: LocalDashMap<Definition, Type>,
  // END SECTION: Type environment

  // SECTION: Contextual information
  pub self_type: Option<Type>,
  pub return_type: Option<Type>,
  // END SECTION: Contextual information
  /// Statically typed expressions that are used in the program.
  ///
  /// This is used to check that the type of the expression is
  /// correct.
  pub expressions: im_rc::HashMap<Expr, Type, FxBuildHasher>,

  /// Statically typed statements that are used in the program.
  pub parameters: im_rc::HashMap<Parameter, Type, FxBuildHasher>,

  /// Statically typed statements that are used in the program.
  pub declarations: im_rc::HashMap<TopLevel, Type, FxBuildHasher>,

  /// Debruijin index bindings for type names.
  pub debruijin_index: im_rc::HashMap<usize, String, FxBuildHasher>,
}

/// Local context for type inference. This is used
/// to infer the type of an expression, and everything
/// that is needed to infer the type of an expression.
#[allow(dead_code)]
pub(crate) struct InferCtx<'tctx> {
  pub db: &'tctx dyn crate::TyperDb,
  pub pkg: Package,
  pub options: TyperFeatures,

  // SECTION: Diagnostics
  /// The diagnostics that are emitted by the type checker.
  pub diagnostics: Rc<RefCell<Vec<ThirDiagnostic>>>,
  pub location: Location,
  // END SECTION: Diagnostics

  // SECTION: Type environment
  pub env: TypeEnv,
  pub adhoc_env: ClassEnv,
  pub eval_env: EvalEnv,
  pub type_env: LocalDashMap<Definition, Type>,
  // END SECTION: Type environment

  // SECTION: Contextual information
  pub self_type: Option<Type>,
  pub return_type: Option<Type>,
  pub type_rep: Option<Location>,
  // END SECTION: Contextual information
  /// Statically typed expressions that are used in the program.
  ///
  /// This is used to check that the type of the expression is
  /// correct.
  pub expressions: im_rc::HashMap<Expr, Type, FxBuildHasher>,

  /// Statically typed statements that are used in the program.
  pub parameters: im_rc::HashMap<Parameter, Type, FxBuildHasher>,

  /// Statically typed statements that are used in the program.
  pub declarations: im_rc::HashMap<TopLevel, Type, FxBuildHasher>,

  /// Debruijin index bindings for type names.
  pub debruijin_index: im_rc::HashMap<usize, String, FxBuildHasher>,
}

impl Predicate {
  // Replaces a type variable with a type.
  pub fn replace(self, name: Definition, replacement: Type) -> Predicate {
    match self {
      Predicate::None => Predicate::None,
      Predicate::IsIn(class_name, arguments) => Predicate::IsIn(
        /* name      = */ class_name,
        /* arguments = */
        arguments
          .into_iter()
          .map(|value| value.replace(name, replacement.clone()))
          .collect(),
      ),
    }
  }
}

impl Type {
  /// Unifies the type with another type. This is used
  /// to equate two types.
  pub(crate) fn unify(&self, tau: Type, ctx: &mut InferCtx) -> bool {
    let mut substitution = Substitution {
      ctx,
      errors: im_rc::vector![],
    };
    let unify_result = substitution.internal_unify(self.clone(), tau);
    substitution.publish_all_errors();

    // Returns if the unification was successful
    unify_result.is_some()
  }

  /// Substitutes a type variable with a type. This is used
  /// to make substitutions in the type.
  pub fn replace(self, name: Definition, replacement: Type) -> Type {
    use holes::HoleKind::*;
    match self {
      Type::Universe => todo!(),
      Type::Primary(_) => todo!(),
      Type::Ix(_) => todo!(),
      Type::Lam(_) => todo!(),
      Type::Forall(_) => todo!(),
      Type::Pi(_) => todo!(),
      Type::Rigid(_, _) => todo!(),
      Type::Flexible(_, _) => todo!(),
    }
    // match self {
    //   Type::Forall(forall) => Type::Forall(forall.replace(name, replacement)),
    //   Type::App(a, b) => Type::App(a.replace(name, replacement.clone()).into(), b.replace(name, replacement).into()),
    //   Type::Pi(fun) => Type::Pi(Pi {
    //     name: fun.name.clone(),
    //     domain: fun.domain.clone().replace(name, replacement.clone()).into(),
    //     codomain: Rc::new(move |domain| fun.codomain(domain).replace(name, replacement.clone())),
    //   }),
    //   Type::Hole(hole) => match hole.data.borrow_mut().kind() {
    //     Error => Type::Hole(HoleRef::new(Hole { kind: Error })),
    //     Empty { scope } => Type::Hole(HoleRef::new(Hole {
    //       kind: Empty { scope: *scope },
    //     })),
    //     Filled(ty) => ty.clone().replace(name, replacement),
    //   },
    //   Type::Bound(Bound::Flexible(flexible)) if flexible.definition == name => replacement,
    //   Type::Bound(bound) => Type::Bound(bound),
    //   _ => self,
    // }
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
  // Instantiates a type with variables. It does replaces it's
  // variables with unfilled holes.
  //
  // This only works with [`Type::Forall`] types.
  pub(crate) fn instantiate(&mut self, sigma: Type) -> Qual<Type> {
    // Gets a forall type to instantiate, can't instantiate
    // non-forall types.
    match sigma.force() {
      Type::Forall(forall) => {
        // Instantiate the forall type with new meta/hole types.
        forall.instantiate(self.new_meta())
      }
      sigma => Qual::new(sigma),
    }
  }

  /// Create a new pi type. This is used to create a new pi type.
  ///
  /// # Parameters
  ///
  /// - `domain`: The domain of the pi type.
  /// - `value`: The value of the pi type.
  fn from_pi<I>(mut parameters: I, return_type: Self) -> Self
    where
      I: Iterator<Item = Self>,
  {
    let Some(first) = parameters.next() else {
      return return_type;
    };

    todo!()

    // let mut result = Self::Pi(Pi {
    //   name: None,
    //   domain: first.into(),
    //   codomain: return_type,
    // });
    //
    // for domain in parameters {
    //   result = Type::Pi(Pi {
    //     name: None,
    //     domain: domain.into(),
    //     codomain: Rc::new(move |_| result.clone()),
    //   });
    // }
    //
    // result
  }

  /// Quantifies a type with a forall. This is used to
  /// make let-generalisation possible.
  ///
  /// This is only enabled by language feature flag in the
  /// CLI.
  fn quantify(&self, tau: Type) -> Type {
    fn generalize(variables: &mut Vec<(Name, Type)>, tau: Type) {
      todo!()
      // match tau {
      //   Type::App(callee, argument) => {
      //     generalize(variables, *callee);
      //     generalize(variables, *argument);
      //   }
      //   Type::Stuck(stuck) => {
      //     generalize(variables, *stuck.base);
      //     for tau in stuck.spine {
      //       generalize(variables, tau.clone());
      //     }
      //   }
      //   Type::Hole(hole) => match hole.data.borrow_mut().kind() {
      //     HoleKind::Error => {}
      //     HoleKind::Empty { .. } => {}
      //     HoleKind::Filled(value) => {
      //       generalize(variables, value.clone());
      //     }
      //   },
      //   Type::Bound(Bound::Flexible(name)) => {
      //     variables.push((name, Tau::Universe));
      //   }
      //   _ => {}
      // }
    }

    let mut variables = vec![];
    generalize(&mut variables, tau.clone());

    if variables.is_empty() {
      tau
    } else {
      todo!()
      // Tau::Forall(HoasForall {
      //   domain: variables.clone(),
      //   codomain: Rc::new(move |domain| {
      //     let mut tau = tau.clone();
      //     for ((name, _), value) in variables.iter().zip(domain) {
      //       tau = tau.replace(name.definition, value);
      //     }
      //     Qual::new(tau)
      //   }),
      // })
    }
  }

  /// Creates a new meta type. This is used to
  /// create a new empty hole.
  pub(crate) fn new_meta(&mut self) -> Type {
    Type::flexible(holes::HoleKind::Empty { scope: self.env.level })
  }

  /// Creates a new semantic type from a syntactical type representation,
  /// it does report errors and means the type is not inferred.
  ///
  /// This is used to transform a type representation into a semantic type.
  fn translate(&mut self, type_rep: TypeRep) -> Type {
    // Validates a forall binding to check if it is
    // compatible with the a type variable.
    fn forall_binding(ctx: &mut InferCtx, pattern: Pattern, loc: Location) -> Option<Name> {
      match pattern {
        Pattern::Hole | Pattern::Wildcard(_) | Pattern::Error(_) => {
          let location = HirLocation::new(ctx.db, loc);

          // The binding has no name, but it's a valid binding
          // so, we name with `_`, and goes on.
          Some(Name {
            definition: unresolved(ctx.db, location),
            name: "_".into(),
          })
        }
        Pattern::Binding(binding) => Some(Name {
          definition: binding.name(ctx.db),
          name: binding.name(ctx.db).to_string(ctx.db),
        }),
        _ => None,
      }
    }

    // Evaluates a forall resolved type into a semantic type.
    //
    // This is used to transform a type representation into a semantic type.
    fn eval_forall(ctx: &mut InferCtx, forall: ArrowTypeRep) -> Type {
      // Transforms a type representation of forall arrow into
      // a semantic type arrow with a forall quantifier
      let value = ctx.translate(forall.value(ctx.db));

      let parameters = forall
        .parameters(ctx.db)
        .into_iter()
        .filter_map(|parameter| {
          // Checks the type of the parameter
          let ty = ctx.translate(parameter.parameter_type(ctx.db));
          let binding = parameter.binding(ctx.db);

          let location = binding.location(ctx.db);

          // Checks the binding
          let type_rep = binding.clone().check(ty, ctx);

          // Stores the parameter in the environment
          // as debug information for the type checker
          // build a table.
          ctx.parameters.insert(parameter, type_rep);

          // Gets the forall's binding name.
          let Some(name) = forall_binding(ctx, binding, location.clone()) else {
            return ctx.accumulate(ThirDiagnostic {
              id: ErrorId("unsupported-pattern"),
              location: ctx.new_location(location),
              message: message!("unsupported pattern in forall type"),
            });
          };

          // Falls back to a star if the type is not
          // specified.
          //
          // NOTE: currently the type can't be specified
          // so, whatever.
          Some((name, Type::Universe))
        })
        .collect::<Vec<_>>();

      todo!()
      // Tau::Forall(HoasForall {
      //   domain: parameters.clone(),
      //   codomain: Rc::new(move |domain| {
      //     // This is the forall's codomain, it is a function
      //     // that takes a list of types and returns a type.
      //     let mut value = value.clone();
      //
      //     // This substitutes the forall's parameters names with
      //     // its equivalent values.
      //     //
      //     // TODO: add to the context and use the context to
      //     // evaluate the type.
      //     for ((name, _), type_rep) in parameters.clone().into_iter().zip(domain) {
      //       value = value.replace(name.definition, type_rep)
      //     }
      //
      //     Qual::new(value)
      //   }),
      // })
    }

    /// Evaluates a function type into a semantic type.
    ///
    /// It does create a dependent function type from a simple
    /// arrow type in the Lura language.
    fn eval_fun(ctx: &mut InferCtx, fun: ArrowTypeRep) -> Type {
      // Transforms a type representation of pi arrow into
      // a semantic type arrow with a pi arrow
      let value = ctx.translate(fun.value(ctx.db));

      fun
        .parameters(ctx.db)
        .into_iter()
        .map(|parameter| {
          // Checks the type of the parameter
          let type_rep = ctx.translate(parameter.parameter_type(ctx.db));

          parameter.binding(ctx.db).check(type_rep, ctx)
        })
        .fold(value, |acc, next| {
          todo!()
          // Tau::Pi(Pi {
          //   name: None,
          //   domain: next.into(),
          //   codomain: Rc::new(move |_| acc.clone()),
          // })
        })
    }

    /// Evaluates a sigma type into a semantic type.
    ///
    /// Sigma types should be used to represent a product type
    /// in the Lura language.
    ///
    /// But when we can infer the pattern to a constraint, the
    /// sigma, should be a generalisation of the constraint.
    ///
    /// It's called sigma, because the semantics behaviours like
    /// a sigma, but it's not a literal sigma in the language.
    fn eval_sigma(ctx: &mut InferCtx, sigma: ArrowTypeRep) -> Type {
      // The variables that should be added to forall to make the language
      // easier to use
      // let mut ftv: im_rc::HashSet<Fv, FxBuildHasher> = im_rc::HashSet::default();

      let predicates = sigma
        .parameters(ctx.db)
        .into_iter()
        .filter_map(|parameter| {
          // NOTE: The constraint type is the default
          // type for the parameter.
          let constraint = parameter.binding(ctx.db);

          let (pattern, pattern_ftv) = Predicate::new(ctx, constraint)?;

          // Accumulates the free type variables
          // ftv.extend(pattern_ftv);

          Some(pattern)
        })
        .collect::<Vec<_>>();

      // let domain = ftv
      //   .iter()
      //   .filter_map(|fv| {
      //     let name = match fv {
      //       Fv::Flexible(name) | Fv::Index(name, _) => name,
      //       Fv::Hole => {
      //         return ctx.accumulate(ThirDiagnostic {
      //           id: ErrorId("unsupported-hole-implicits"),
      //           location: ctx.new_location(sigma.location(ctx.db)),
      //           message: message!["hole implicits are not supported"],
      //         });
      //       }
      //     };
      //
      //     // NOTE: this is a hack to make the language easier to use
      //     // and to make the type checker easier to implement.
      //     Some((name.clone(), Type::Universe))
      //   })
      //   .collect::<Vec<_>>();

      let value = ctx.translate(sigma.value(ctx.db));

      todo!()
      // Tau::Forall(HoasForall {
      //   domain: domain.clone(),
      //   codomain: Rc::new(move |parameters| {
      //     // This is the forall's codomain, it is a function
      //     // that takes a list of types and returns a type.
      //     let mut value = value.clone();
      //
      //     // This substitutes the forall's parameters names with
      //     // its equivalent values.
      //     let mut new_predicates = vec![];
      //
      //     // Normalises the predicates to make sure that
      //     // the predicates are in the right form.
      //     //
      //     // And to check if they are present in the context!
      //     for mut predicate in predicates.clone() {
      //       let parameters = domain.clone().into_iter().zip(parameters.clone());
      //
      //       // This substitutes the forall's parameters names with
      //       // its equivalent values.
      //       for ((name, _), tau) in parameters {
      //         predicate = predicate.replace(name.definition, tau);
      //       }
      //
      //       new_predicates.push(predicate);
      //     }
      //
      //     // This substitutes the forall's parameters names with
      //     // its equivalent values.
      //     for ((name, _), type_rep) in domain.clone().into_iter().zip(parameters) {
      //       value = value.replace(name.definition, type_rep)
      //     }
      //
      //     Qual {
      //       predicates: new_predicates,
      //       data: value,
      //     }
      //   }),
      // })
    }

    /// Evaluates an application type into a semantic type.
    ///
    /// It does create stuck types from a simple application
    /// type in the Lura language.
    fn eval_app(ctx: &mut InferCtx, app: AppTypeRep) -> Type {
      let mut spine = vec![];
      let callee = ctx.translate(app.callee(ctx.db));

      // Applies the spine of arguments to the callee
      let app = app.arguments(ctx.db).into_iter().fold(callee, |acc, next| {
        // Creates a spine of arguments to build
        // a stuck type.
        let value = ctx.translate(next);
        spine.push(value.clone());

        todo!()
        // Tau::App(acc.into(), value.into())
      });

      // The base type for a stuck
      let base = app.eval(&ctx.snapshot(), ctx.eval_env.clone());

      // If the spine is empty, we just return the base type.
      //
      // This is used to avoid creating stuck types when
      // the application is not a stuck type.
      if spine.is_empty() {
        base
      } else {
        // If the spine is not empty, we create a stuck type
        // with the base type and the spine.
        todo!()
        // Tau::Stuck(Stuck {
        //   base: base.into(),
        //   spine,
        // })
      }
    }

    match type_rep {
      // SECTION: Sentinel Values
      TypeRep::Hole => self.new_meta(), // Infer the type
      TypeRep::Error(_) => Type::ERROR,

      // SECTION: Primary Types
      TypeRep::Unit => Type::UNIT,
      TypeRep::Type => Type::TYPE,

      // SECTION: Function Types
      TypeRep::Arrow(fun) if matches!(fun.kind(self.db), ArrowKind::Fun) => eval_fun(self, fun),
      TypeRep::Arrow(forall) if matches!(forall.kind(self.db), ArrowKind::Forall) => eval_forall(self, forall),
      TypeRep::Arrow(sigma) if matches!(sigma.kind(self.db), ArrowKind::Sigma) => eval_sigma(self, sigma),

      // SECTION: Type application
      TypeRep::App(app) => eval_app(self, app),

      // SECTION: Primitive types
      TypeRep::Path(TypeReference::Bool, _) => Type::BOOL,
      TypeRep::Path(TypeReference::Unit, _) => Type::UNIT,
      TypeRep::Path(TypeReference::Int8, _) => Type::Primary(Primary::I8),
      TypeRep::Path(TypeReference::UInt8, _) => Type::Primary(Primary::U8),
      TypeRep::Path(TypeReference::Int16, _) => Type::Primary(Primary::I16),
      TypeRep::Path(TypeReference::UInt16, _) => Type::Primary(Primary::U16),
      TypeRep::Path(TypeReference::Int32, _) => Type::Primary(Primary::I32),
      TypeRep::Path(TypeReference::UInt32, _) => Type::Primary(Primary::U32),
      TypeRep::Path(TypeReference::Int64, _) => Type::Primary(Primary::I64),
      TypeRep::Path(TypeReference::UInt64, _) => Type::Primary(Primary::U64),
      TypeRep::Path(TypeReference::String, _) => Type::Primary(Primary::String),
      TypeRep::Path(TypeReference::Nat, _) => Type::Primary(Primary::I32),

      // SECTION: Paths
      // We should not resolve the type here, but rather
      // create a new type variable, and as it is resolved, we
      // can replace it with the actual type.
      TypeRep::Path(TypeReference::Reference(reference), _) => self
        .type_env
        .get(&reference.definition(self.db))
        // Tries to get from the environment, if it fails, it means
        // that, the type variable is really a type variable, and
        // not a constructor.
        //
        // A type variable is not rigid!
        .unwrap_or_else(|| {
          let name = self.create_new_name(reference.definition(self.db));

          todo!()
          // Tau::Bound(Bound::Flexible(name))
        }),

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
        message: message!("internal type error, this type should be supported"),
      }),
      TypeRep::QPath(_) => self.accumulate(ThirDiagnostic {
        id: ErrorId("unsupported-qpath-kind"),
        location: self.new_location(type_rep.location(self.db)),
        message: message!("qualified types is not supported yet"),
      }),
      // SECTION: Expressions
      // Gets the type despites the predicates
      TypeRep::Downgrade(expr) => expr.infer(self).1,
    }
  }

  pub(crate) fn new_location(&self, location: Location) -> ThirLocation {
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

  /// Finds a reference to a variable in the environment
  /// and returns its type.
  fn reference(&mut self, reference: Reference) -> (Preds, Type) {
    let def = reference.definition(self.db);

    let generalised = self.env.variables.get(&def).cloned().unwrap_or_else(|| {
      let name = reference.definition(self.db).to_string(self.db);

      self.accumulate(ThirDiagnostic {
        location: self.new_location(reference.location(self.db)),
        id: ErrorId("unbound-variable"),
        message: message!["unbound variable", code!(name)],
      })
    });

    // Gets the predicates from the generalised type
    let qual = self.instantiate(generalised);

    (qual.predicates, qual.data)
  }

  /// Finds a reference to a constructor in the environment
  /// and returns its type.
  fn constructor(&mut self, reference: Reference) -> Type {
    let let_ty = self
      .env
      .constructors
      .get(&reference.definition(self.db))
      .cloned()
      .unwrap_or_else(|| {
        // If the constructor is not found, then we return the error type
        Rc::new(InternalVariant {
          name: Type::Primary(Primary::Error),
          parameters: im_rc::vector![],
        })
      });

    // Constructors should not have predicates
    //
    // TODO: validate this
    self.instantiate(let_ty.name.clone()).data
  }

  /// Gets a default void value for the type
  /// system.
  fn void(&mut self) -> Type {
    Type::Primary(Primary::Unit)
  }

  /// Replaces temporally the environment with a new one.
  ///
  /// This is useful for example when we are inferring the type
  /// of a function, we need to add the parameters to the environment
  /// temporally, and then remove them.
  fn with_env<F, U>(&mut self, env: TypeEnv, f: F) -> U
    where
      F: for<'tctxn> FnOnce(&mut InferCtx<'tctxn>) -> U,
  {
    let old_env = replace(&mut self.env, env);
    let result = f(self);
    self.env = old_env;
    result
  }

  // Creates a new name based on a definition, it does
  // evaluates the definition to a string, and then
  // creates a new name with the definition and the string
  // representation of the definition.
  fn create_new_name(&self, definition: Definition) -> Name {
    let name = definition.to_string(self.db);

    Name {
      definition,
      name: name.clone(),
    }
  }

  /// Adds to the environment a new type variable and returns
  /// it debriujin index.
  ///
  /// It does creates a new unique name for the type variable
  fn add_to_env(&mut self, parameter: Definition) -> Uniq {
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
    self.env.names.push_front(refresh);

    level
  }

  // Creates a new definition in the environment
  fn extend(&mut self, name: Definition, tau: Type) {
    self.type_env.insert(name, tau);
  }

  /// Creates a new snapshot, just like a photograph, of
  /// the current context.
  ///
  /// This saves all the current state of the context, and
  /// returns a snapshot that can be used to restore the
  /// context to the current state.
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
  ///
  /// It has a similar method that is [`Snapshot::accumulate`].
  pub fn accumulate<T: Default>(&self, mut diagnostic: ThirDiagnostic) -> T {
    if let ThirLocation::CallSite = diagnostic.location {
      diagnostic.location = self.new_location(self.location.clone());
    }

    self.diagnostics.borrow_mut().push(diagnostic);

    // We return the default value
    Default::default()
  }
}
