use std::marker::PhantomData;

use crate::Id;
use crate::presence::Optional;
use crate::slice::SliceId;

pub mod plan;
pub mod run;
mod terminal;
pub mod update;

pub use plan::*;
pub use run::*;
pub use update::*;

/// A prepared query plan. The optional seed is carried in the type: `()` for
/// seedless plans and `Id<P::Src>` for plans bound to a single seed.
#[derive(Clone, Copy)]
pub struct Query<P, S = ()> {
    pub plan: P,
    seed: S,
}

/// Helper starter function for constructing a seedless relational query plan.
#[inline]
pub fn query<T: 'static>() -> Query<ScanOneQuery<T>, ()> {
    Query {
        plan: ScanOneQuery(PhantomData),
        seed: (),
    }
}

impl<T: 'static> Id<T> {
    /// Start a query plan bound to this seed.
    #[inline]
    pub fn query(self) -> Query<ScanOneQuery<T>, Id<T>> {
        Query {
            plan: ScanOneQuery(PhantomData),
            seed: self,
        }
    }
}

impl<P: Plan, S> Query<P, S> {
    /// Transform the plan while preserving the seed.
    #[inline]
    pub fn map<New>(self, f: impl FnOnce(P) -> New) -> Query<New, S>
    where
        New: Plan<Src = P::Src>,
    {
        Query {
            plan: f(self.plan),
            seed: self.seed,
        }
    }
}

impl<P: Plan, S> Query<P, S> {
    /// Plan-scoped composition: build a batch or check from this query without
    /// executing it. This needs `Copy` because the query handle may be reused
    /// to build several sibling plans.
    #[inline]
    pub fn then<B>(self, f: impl FnOnce(Self) -> B) -> B
    where
        Self: Copy,
    {
        f(self)
    }

    /// Monadic combinator: Filter target values
    #[inline]
    pub fn filter<F>(self, predicate: F) -> Query<FilterQuery<P, F>, S>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>) -> bool,
    {
        self.map(|plan| FilterQuery { plan, predicate })
    }

    /// Cast the current target to a concrete type, asserting that the cast succeeds.
    #[inline]
    pub fn cast<U: 'static>(self) -> Query<ExpectQuery<CastQuery<P, U>>, S> {
        self.try_cast::<U>().expect()
    }

    /// Cast the current target to a concrete type, returning `None` if it does not match.
    #[inline]
    pub fn try_cast<U: 'static>(self) -> Query<CastQuery<P, U>, S> {
        self.map(|plan| CastQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Assert that the plan yields a focus, panicking otherwise
    #[inline]
    pub fn expect(self) -> Query<ExpectQuery<P>, S> {
        self.map(|plan| ExpectQuery { plan })
    }

    /// Monadic combinator: Project a single target ID
    #[inline]
    pub fn project<U: 'static, F>(self, projector: F) -> Query<ProjectQuery<P, U, F>, S>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>) -> Id<U>,
    {
        self.map(|plan| ProjectQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }

    /// Project a single target ID and require it to cast to `Target`.
    #[inline]
    pub fn project_as<Target: 'static, F>(
        self,
        projector: F,
    ) -> Query<ProjectAsQuery<P, Target, F>, S>
    where
        F: Projection<P>,
    {
        self.map(|plan| ProjectAsQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Project a slice of target IDs
    #[inline]
    pub fn project_slice<U: 'static, F>(self, projector: F) -> Query<ProjectSliceQuery<P, U, F>, S>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>) -> SliceId<U>,
    {
        self.map(|plan| ProjectSliceQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Project an optional target ID
    #[inline]
    pub fn project_opt<U: 'static, F>(self, projector: F) -> Query<ProjectOptQuery<P, U, F>, S>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>) -> Option<Id<U>>,
    {
        self.map(|plan| ProjectOptQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }

    /// Project an optional target ID and require it to cast to `Target`.
    #[inline]
    pub fn project_opt_as<Target: 'static, F>(
        self,
        projector: F,
    ) -> Query<ProjectOptAsQuery<P, Target, F>, S>
    where
        F: OptProjection<P>,
    {
        self.map(|plan| ProjectOptAsQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Inner join metadata
    #[inline]
    pub fn with<M: 'static>(self) -> Query<WithQuery<P, M>, S> {
        self.map(|plan| WithQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Required metadata join
    #[inline]
    pub fn with_required<M: 'static>(self) -> Query<WithRequiredQuery<P, M>, S> {
        self.map(|plan| WithRequiredQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Left join metadata
    #[inline]
    pub fn left_join<M: 'static>(self) -> Query<LeftJoinQuery<P, M>, S> {
        self.map(|plan| LeftJoinQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Anti-join metadata
    #[inline]
    pub fn without<M: 'static>(self) -> Query<WithoutQuery<P, M>, S> {
        self.map(|plan| WithoutQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Narrow a slice element by index, asserting that it exists.
    #[inline]
    pub fn nth(self, index: usize) -> Query<ExpectQuery<GetAt<P>>, S> {
        self.nth_opt(index).expect()
    }

    /// Narrow a slice element by index without asserting that it exists.
    #[inline]
    pub fn nth_opt(self, index: usize) -> Query<GetAt<P>, S> {
        self.map(|plan| GetAt { plan, index })
    }

    /// Inspect each value while preserving the row and presence.
    #[inline]
    pub fn inspect<F>(self, inspect: F) -> Query<InspectQuery<P, F>, S>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>),
    {
        self.map(|plan| InspectQuery { plan, inspect })
    }

    /// Lazy check: assert that the plan yields the expected value.
    #[inline]
    pub fn assert_eq<E>(self, expected: E) -> Query<AssertEqQuery<P, E>, S> {
        self.map(|plan| AssertEqQuery { plan, expected })
    }

    /// Lazy check: assert that an optional plan yields a value.
    #[inline]
    pub fn assert_some(self) -> Query<ExpectQuery<P>, S>
    where
        P: Plan<Presence = Optional>,
    {
        self.expect()
    }

    /// Lazy check: assert that an optional plan yields no value.
    #[inline]
    pub fn assert_none(self) -> Query<AssertNoneQuery<P>, S> {
        self.map(|plan| AssertNoneQuery { plan })
    }

    /// Lazy check: assert the length of a slice-valued plan.
    #[inline]
    pub fn assert_len(self, expected: usize) -> Query<AssertLenQuery<P>, S> {
        self.map(|plan| AssertLenQuery { plan, expected })
    }
}
