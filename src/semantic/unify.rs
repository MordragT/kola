use crate::syntax::Span;

use super::{error::InferReport, Context};

/// Principal Type: Most general type that can be inferred for a given expression
/// Unify algorithm in J performs mutation, in W it does not.
// /// This uses "in place" mutation similar to J but with the help of substitutions similar to w
/// Builds up constraints (Substitutions) within the context so that lhs and rhs unify.
/// Most general unifier, builds up a substitution S such that S(lhs) is congruent to S(rhs).
pub trait Unify<With> {
    fn unify(&self, with: With, ctx: &mut Context);

    /// Performs unification on the type with another type.
    /// If successful, results in a solution to the unification problem,
    /// in the form of a substitution. If there is no solution to the
    /// unification problem then unification fails and an error is reported.
    fn try_unify(&self, with: With, span: Span, ctx: &mut Context) -> Result<Self, InferReport>
    where
        Self: Sized + Clone,
    {
        self.unify(with, ctx);

        if ctx.has_errors() {
            Err(ctx.report_with(span))
        } else {
            Ok(self.clone())
        }
    }
}

/// Helper function that concatenates two vectors into a single vector while removing duplicates.
pub(crate) fn union<T: PartialEq>(mut vars: Vec<T>, mut with: Vec<T>) -> Vec<T> {
    with.retain(|tv| !vars.contains(tv));
    vars.append(&mut with);
    vars
}
