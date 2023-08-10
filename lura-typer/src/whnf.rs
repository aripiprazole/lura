use crate::infer::{EvalEnv, InferCtx, Snapshot};
use crate::thir::{ThirDiagnostic, ThirLocation};
use crate::type_rep::pi::HoasPi;
use crate::type_rep::state;
use crate::type_rep::Quote;
use crate::type_rep::Type;
use lura_diagnostic::{code, message, ErrorId};
use std::rc::Rc;

pub(crate) trait Whnf {
    /// Evaluates type to weak head normal form.
    ///
    /// This is used to normalize types before
    /// comparing them for equality.
    fn eval(&self, ctx: &Snapshot, env: EvalEnv) -> Self;
}

impl Whnf for Type<state::Hoas> {
    fn eval(&self, ctx: &Snapshot, env: EvalEnv) -> Self {
        match self {
            Type::App(callee, value) => {
                let callee = callee.eval(ctx, env);

                match callee {
                    Type::Pi(pi) => pi.codomain(*value.clone()),
                    _ => ctx.accumulate(ThirDiagnostic {
                        // TODO: improve error message
                        id: ErrorId("non-pi-apply"),
                        location: ThirLocation::CallSite,
                        message: message![
                            "cannot apply type to not function type",
                            code!(callee.seal())
                        ],
                    }),
                }
            }

            // Evaluates the pi type to get it's
            // equivalent type, like `Bool` to `Bool`.
            Type::Pi(pi) => {
                let domain = pi.domain.eval(ctx, env.clone());

                let ctx = ctx.clone();
                let name = pi.name.clone();
                let codomain = pi.codomain.clone();

                Type::Pi(HoasPi {
                    name: pi.name.clone(),
                    domain: domain.clone().into(),
                    codomain: Rc::new(move |parameter| {
                        let mut local = env.clone();
                        //
                        // Creates a new environment with the
                        // parameter bound to the name.
                        if let Some(ref name) = name.clone() {
                            local.env.insert(name.definition, parameter.clone());
                        }

                        (codomain.clone())(parameter).eval(&ctx, local)
                    }),
                })
            }

            // Evaluates the constructor to get it's
            // equivalent type, like `Bool` to `Bool`.
            //
            // Or, if it has a kind like: `* -> *`, it should
            // return a pi type. To be executed.
            Type::Constructor(name) => {
                ctx.type_env
                    .get(&name.definition)
                    .cloned()
                    .unwrap_or_else(|| {
                        ctx.accumulate::<()>(ThirDiagnostic {
                            id: ErrorId("unbound-constructor"),
                            location: ThirLocation::CallSite,
                            message: message!["unbound constructor", code!(name.to_string())],
                        });

                        Type::Constructor(name.clone())
                    })
            }
            _ => self.clone(),
        }
    }
}
