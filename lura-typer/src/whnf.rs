use std::rc::Rc;

use lura_diagnostic::{code, message, ErrorId};

use crate::{
  infer::{EvalEnv, Snapshot},
  thir::{ThirDiagnostic, ThirLocation},
  type_rep::{pi::Pi, Quote, Type, Term},
};

impl Term {
  pub(crate) fn eval(&self, environment: Vec<Type>) -> Type {
    todo!();
    // match self {
    //   // Applies the function to the value,
    //   // like `Bool -> Bool` to `Bool`.
    //   //
    //   // TODO: add stuck on application
    //   Type::App(callee, value) => {
    //     let callee = callee.eval(ctx, env);

    //     match callee {
    //       Type::Pi(pi) => pi.codomain(*value.clone()),
    //       _ => ctx.accumulate(ThirDiagnostic {
    //         // TODO: improve error message
    //         id: ErrorId("non-pi-apply"),
    //         location: ThirLocation::CallSite,
    //         message: message!["cannot apply type to not function type", code!(callee.seal())],
    //       }),
    //     }
    //   }

    //   // Evaluates the pi type to get it's
    //   // equivalent type, like `Bool` to `Bool`.
    //   Type::Pi(pi) => {
    //     let domain = pi.domain.eval(ctx, env.clone());

    //     let name = pi.name.clone();
    //     let codomain = pi.codomain.clone();

    //     Type::Pi(Pi {
    //       name: pi.name.clone(),
    //       domain: domain.clone().into(),
    //       codomain: Rc::new(move |parameter| {
    //         let mut local = env.clone();

    //         // Creates a new environment with the
    //         // parameter bound to the name.
    //         if let Some(ref name) = name.clone() {
    //           local.env.insert(name.definition, parameter.clone());
    //         }

    //         (codomain.clone())(parameter)
    //       }),
    //     })
    //   }

    //   // Evaluates the constructor to get it's
    //   // equivalent type, like `Bool` to `Bool`.
    //   //
    //   // Or, if it has a kind like: `* -> *`, it should
    //   // return a pi type. To be executed.
    //   Type::Constructor(name) => ctx.type_env.get(&name.definition).unwrap_or_else(|| {
    //     ctx.accumulate::<()>(ThirDiagnostic {
    //       id: ErrorId("unbound-constructor"),
    //       location: ThirLocation::CallSite,
    //       message: message!["unbound constructor", code!(name.to_string())],
    //     });

    //     Type::Constructor(name.clone())
    //   }),
    //   _ => self.clone(),
    // }
  }
}
