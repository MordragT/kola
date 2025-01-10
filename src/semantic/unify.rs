use super::Context;

/// Principal Type: Most general type that can be inferred for a given expression
/// Unify algorithm in J performs mutation, in W it does not.
// /// This uses "in place" mutation similar to J but with the help of substitutions similar to w
/// Builds up constraints (Substitutions) within the context so that lhs and rhs unify.
/// Most general unifier, builds up a substitution S such that S(lhs) is congruent to S(rhs).
pub trait Unify<With> {
    fn unify(&self, with: With, ctx: &mut Context);
}

/// Helper function that concatenates two vectors into a single vector while removing duplicates.
pub(crate) fn union<T: PartialEq>(mut vars: Vec<T>, mut with: Vec<T>) -> Vec<T> {
    with.retain(|tv| !vars.contains(tv));
    vars.append(&mut with);
    vars
}
