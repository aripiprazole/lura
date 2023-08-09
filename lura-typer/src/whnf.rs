use crate::infer::InferCtx;
use crate::kind::Kind;
use crate::thir::{ThirDiagnostic, ThirLocation};
use crate::type_rep::state::Hoas;
use crate::type_rep::Type;
use crate::type_rep::{state, Bound};
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
            Type::Primary(primary) => Type::Primary(*primary),
            Type::Constructor(constructor, kind) => {
                Type::Constructor(constructor.clone(), kind.clone())
            }
            Type::App(callee, value) => {
                let callee = callee.eval(ctx);

                match callee {
                    // List (a : *) : *
                    // (List Int) : *
                    Type::Constructor(constructor, kind) => {
                        // Validate that the kind is a function
                        // and that the argument is a valid kind
                        // match kind {
                        //     Kind::Star => ctx.accumulate(ThirDiagnostic {
                        //         id: ErrorId("non-pi-apply"),
                        //         location: ThirLocation::CallSite,
                        //         message: message!["cannot apply to star kind"],
                        //     }),
                        //     Kind::Type(_) => todo!("dependent kinds"),
                        //     Kind::Fun(_, value) => {
                        //         // Erases the callee value,
                        //         // and returns the value
                        //         (*value).clone()
                        //     }
                        // };

                        return Type::App(
                            Type::Constructor(constructor, kind).into(),
                            value.eval(ctx).into(),
                        );
                    }
                    // TODO: dependent types application
                    Type::Pi(_) => todo!(),
                    _ => ctx.accumulate(ThirDiagnostic {
                        // TODO: improve error message
                        id: ErrorId("non-pi-apply"),
                        location: ThirLocation::CallSite,
                        message: message!["cannot apply type to not function type",],
                    }),
                }

                todo!()
            }
            Type::Forall(_) => todo!(),
            Type::Pi(_) => todo!(),
            Type::Fun(_) => todo!(),
            _ => self.clone(),
        }
    }
}
