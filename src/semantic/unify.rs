use std::sync::Arc;

use miette::NamedSource;

use crate::{source::Source, syntax::Span};

use super::{
    error::{InferError, InferReport},
    types::MonoType,
    Substitutable, Substitution,
};

// TODO create trait for Unifier and implement this for Inferer
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unifier {
    pub substitution: Substitution,
    pub errors: Vec<InferError>,
    pub named_source: NamedSource<Arc<str>>,
}

impl Unifier {
    pub fn new(source: &Source) -> Self {
        Self {
            substitution: Substitution::empty(),
            errors: Vec::new(),
            named_source: source.named_source(),
        }
    }

    pub fn clear(&mut self) {
        self.substitution.clear();
        self.errors.clear();
    }

    pub fn named_source(&self) -> NamedSource<Arc<str>> {
        self.named_source.clone()
    }

    pub fn error(&mut self, err: InferError) {
        self.errors.push(err)
    }

    pub fn report_with(&mut self, span: Span) -> InferReport {
        let related = std::mem::take(&mut self.errors);

        InferReport::new(related, span, self.named_source())
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn branch_errors<F, T, E>(&mut self, f: F, context: E) -> T
    where
        F: FnOnce(&mut Self) -> T,
        E: FnOnce(Vec<InferError>) -> InferError,
    {
        let former = std::mem::take(&mut self.errors);
        let result = f(self);
        let errors = std::mem::replace(&mut self.errors, former);
        self.errors.push(context(errors));

        result
    }

    pub fn finish(&mut self, ty: &MonoType, span: Span) -> Result<MonoType, InferReport> {
        if self.has_errors() {
            Err(self.report_with(span))
        } else {
            let ty = ty.apply_cow(&mut self.substitution);

            self.clear();

            Ok(ty.into_owned())
        }
    }
}

/// Principal Type: Most general type that can be inferred for a given expression
/// Unify algorithm in J performs mutation, in W it does not.
// /// This uses "in place" mutation similar to J but with the help of substitutions similar to w
/// Builds up constraints (Substitutions) within the context so that lhs and rhs unify.
/// Most general unifier, builds up a substitution S such that S(lhs) is congruent to S(rhs).
pub trait Unify<With> {
    fn unify(&self, with: With, ctx: &mut Unifier);

    /// Performs unification on the type with another type.
    /// If successful, results in a solution to the unification problem,
    /// in the form of a substitution. If there is no solution to the
    /// unification problem then unification fails and an error is reported.
    fn try_unify(&self, with: With, span: Span, ctx: &mut Unifier) -> Result<Self, InferReport>
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
