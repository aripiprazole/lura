use crate::infer::InferCtx;
use crate::type_rep::Type;
use crate::type_rep::state;

pub(crate) trait Whnf {
    /// Evaluates type to weak head normal form.
    ///
    /// This is used to normalize types before
    /// comparing them for equality.
    fn eval(&self, ctx: &mut InferCtx) -> Self;
}

impl Whnf for Type<state::Hoas> {
    fn eval(&self, ctx: &mut InferCtx) -> Self {
        todo!()
    }
}
