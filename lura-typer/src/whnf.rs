use crate::infer::InferCtx;
use crate::thir::{ThirDiagnostic, ThirLocation};
use crate::type_rep::state;
use crate::type_rep::Type;
use lura_diagnostic::{message, ErrorId};

pub(crate) trait Whnf {
    /// Evaluates type to weak head normal form.
    ///
    /// This is used to normalize types before
    /// comparing them for equality.
    fn eval(&self, ctx: &mut InferCtx) -> Self;
}

impl Whnf for Type<state::Hoas> {
    fn eval(&self, ctx: &mut InferCtx) -> Self {
        match self {
            Type::App(callee, value) => {
                let callee = callee.eval(ctx);

                match callee {
                    // TODO: dependent types application
                    Type::Pi(pi) => pi.codomain(*value.clone()),
                    _ => ctx.accumulate(ThirDiagnostic {
                        // TODO: improve error message
                        id: ErrorId("non-pi-apply"),
                        location: ThirLocation::CallSite,
                        message: message!["cannot apply type to not function type",],
                    }),
                }
            }
            Type::Forall(_) => todo!(),
            Type::Pi(_) => todo!(),
            _ => self.clone(),
        }
    }
}
