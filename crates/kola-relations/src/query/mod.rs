use std::marker::PhantomData;

use crate::Id;
use crate::presence::Presence;
use crate::relation::SliceRelation;
use crate::slice::SliceId;

pub mod plan;
pub mod update;

pub use plan::*;
pub use update::*;

/// A prepared query plan. The seed is supplied at execution time.
#[derive(Clone, Copy)]
pub struct Query<P> {
    pub plan: P,
}

/// Helper starter function for constructing a new relational query plan.
#[inline]
pub fn query<T: 'static>() -> Query<ScanOneQuery<T>> {
    Query {
        plan: ScanOneQuery(PhantomData),
    }
}

impl<P> Query<P> {
    #[inline]
    pub fn map<New>(self, f: impl FnOnce(P) -> New) -> Query<New> {
        Query { plan: f(self.plan) }
    }
}

impl<P: Plan> Query<P> {
    /// Monadic combinator: Filter target values
    #[inline]
    pub fn filter<F>(self, predicate: F) -> Query<FilterQuery<P, F>>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>) -> bool,
    {
        self.map(|plan| FilterQuery { plan, predicate })
    }

    /// Monadic combinator: Cast the current target type to a specific concrete variant/type (prism)
    #[inline]
    pub fn cast<U: 'static>(self) -> Query<CastQuery<P, U>> {
        self.map(|plan| CastQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Assert that the plan yields a focus, panicking otherwise
    #[inline]
    pub fn expect(self) -> Query<ExpectQuery<P>> {
        self.map(|plan| ExpectQuery { plan })
    }

    /// Monadic combinator: Project a single target ID
    #[inline]
    pub fn project<U: 'static, F>(self, projector: F) -> Query<ProjectQuery<P, U, F>>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>) -> Id<U>,
    {
        self.map(|plan| ProjectQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Project a slice of target IDs
    #[inline]
    pub fn project_slice<U: 'static, F>(self, projector: F) -> Query<ProjectSliceQuery<P, U, F>>
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
    pub fn project_opt<U: 'static, F>(self, projector: F) -> Query<ProjectOptQuery<P, U, F>>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>) -> Option<Id<U>>,
    {
        self.map(|plan| ProjectOptQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Inner join metadata
    #[inline]
    pub fn with<M: 'static>(self) -> Query<WithQuery<P, M>> {
        self.map(|plan| WithQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Required metadata join
    #[inline]
    pub fn with_required<M: 'static>(self) -> Query<WithRequiredQuery<P, M>> {
        self.map(|plan| WithRequiredQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Left join metadata
    #[inline]
    pub fn left_join<M: 'static>(self) -> Query<LeftJoinQuery<P, M>> {
        self.map(|plan| LeftJoinQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Anti-join metadata
    #[inline]
    pub fn without<M: 'static>(self) -> Query<WithoutQuery<P, M>> {
        self.map(|plan| WithoutQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Narrow slice element by index
    #[inline]
    pub fn nth<T: 'static>(self, index: usize) -> Query<GetAt<P, T>> {
        self.map(|plan| GetAt {
            plan,
            index,
            _marker: PhantomData,
        })
    }

    /// Inspect each value while preserving the row and presence.
    #[inline]
    pub fn inspect<F>(self, inspect: F) -> Query<InspectQuery<P, F>>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>),
    {
        self.map(|plan| InspectQuery { plan, inspect })
    }

    /// Executes the plan from a single seed id.
    #[inline]
    pub fn get<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        id: Id<P::Src>,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<<P as Plan>::Value<'ctx>>
    where
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        <<P as Plan>::Presence as Presence>::map(self.plan.execute(ctx, id), |(_, value)| value)
    }

    /// Executes the plan from a single seed id and returns only the target ID.
    #[inline]
    pub fn get_id<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        id: Id<P::Src>,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<Id<<P as Plan>::Target>>
    where
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        <<P as Plan>::Presence as Presence>::map(self.plan.execute(ctx, id), |(target_id, _)| {
            target_id
        })
    }

    /// Executes the plan from a single seed id and returns the target ID together with its value.
    #[inline]
    pub fn rows<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        id: Id<P::Src>,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<(
        Id<<P as Plan>::Target>,
        <P as Plan>::Value<'ctx>,
    )>
    where
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        self.plan.execute(ctx, id)
    }

    /// Executes a slice-valued plan from a single seed id.
    #[inline]
    pub fn get_slice<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        id: Id<P::Src>,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<SliceId<<P as Plan>::Target>>
    where
        P: SliceTraversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        self.plan.execute_slice(ctx, id)
    }

    /// Executes the plan from many seed ids.
    #[inline]
    pub fn iter<'ctx, Ctx, I>(
        self,
        ctx: &'ctx Ctx,
        ids: I,
    ) -> impl Iterator<Item = <<P as Plan>::Presence as Presence>::Wrap<<P as Plan>::Value<'ctx>>> + 'ctx
    where
        I: IntoIterator<Item = Id<P::Src>>,
        I::IntoIter: 'ctx,
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        let plan = self.plan;
        ids.into_iter().map(move |id| {
            <<P as Plan>::Presence as Presence>::map(plan.execute(ctx, id), |(_, value)| value)
        })
    }

    /// Executes the plan from the seed ids stored in a slice.
    #[inline]
    pub fn iter_slice<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        slice: SliceId<P::Src>,
    ) -> impl Iterator<Item = <<P as Plan>::Presence as Presence>::Wrap<<P as Plan>::Value<'ctx>>> + 'ctx
    where
        Ctx: SliceRelation<P::Src> + 'ctx,
        P: Traversal<'ctx, Ctx> + 'ctx,
    {
        self.iter(ctx, ctx.get_slice(slice).iter().copied())
    }

    /// Executes the plan from many seed ids and yields target IDs with values.
    #[inline]
    pub fn iter_rows<'ctx, Ctx, I>(
        self,
        ctx: &'ctx Ctx,
        ids: I,
    ) -> impl Iterator<
        Item = <<P as Plan>::Presence as Presence>::Wrap<(
            Id<<P as Plan>::Target>,
            <P as Plan>::Value<'ctx>,
        )>,
    > + 'ctx
    where
        I: IntoIterator<Item = Id<P::Src>>,
        I::IntoIter: 'ctx,
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        let plan = self.plan;
        ids.into_iter().map(move |id| plan.execute(ctx, id))
    }

    /// Executes the plan from the seed ids stored in a slice and yields target IDs with values.
    #[inline]
    pub fn iter_slice_rows<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        slice: SliceId<P::Src>,
    ) -> impl Iterator<
        Item = <<P as Plan>::Presence as Presence>::Wrap<(
            Id<<P as Plan>::Target>,
            <P as Plan>::Value<'ctx>,
        )>,
    > + 'ctx
    where
        Ctx: SliceRelation<P::Src> + 'ctx,
        P: Traversal<'ctx, Ctx> + 'ctx,
    {
        self.iter_rows(ctx, ctx.get_slice(slice).iter().copied())
    }

    /// Executes a slice-valued plan from many seed ids.
    #[inline]
    pub fn iter_slices<'ctx, Ctx, I>(
        self,
        ctx: &'ctx Ctx,
        ids: I,
    ) -> impl Iterator<
        Item = <<P as Plan>::Presence as Presence>::Wrap<SliceId<<P as Plan>::Target>>,
    > + 'ctx
    where
        I: IntoIterator<Item = Id<P::Src>>,
        I::IntoIter: 'ctx,
        P: SliceTraversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        let plan = self.plan;
        ids.into_iter().map(move |id| plan.execute_slice(ctx, id))
    }
}
