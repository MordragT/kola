use std::marker::PhantomData;

use crate::Id;
use crate::presence::Presence;
use crate::relation::SliceRelation;
use crate::slice::SliceId;

pub mod plan;
pub mod update;

pub use plan::*;
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
    #[inline]
    pub(crate) fn map<New>(self, f: impl FnOnce(P) -> New) -> Query<New, S>
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
    /// Monadic combinator: Filter target values
    #[inline]
    pub fn filter<F>(self, predicate: F) -> Query<FilterQuery<P, F>, S>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>) -> bool,
    {
        self.map(|plan| FilterQuery { plan, predicate })
    }

    /// Monadic combinator: Cast the current target type to a specific concrete variant/type (prism)
    #[inline]
    pub fn cast<U: 'static>(self) -> Query<CastQuery<P, U>, S> {
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

    /// Cast to a concrete target and assert that the cast succeeds.
    #[inline]
    pub fn cast_expect<U: 'static>(self) -> Query<ExpectQuery<CastQuery<P, U>>, S> {
        self.cast::<U>().expect()
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

    /// Monadic combinator: Narrow slice element by index
    #[inline]
    pub fn nth<T: 'static>(self, index: usize) -> Query<GetAt<P, T>, S> {
        self.map(|plan| GetAt {
            plan,
            index,
            _marker: PhantomData,
        })
    }

    /// Inspect each value while preserving the row and presence.
    #[inline]
    pub fn inspect<F>(self, inspect: F) -> Query<InspectQuery<P, F>, S>
    where
        F: for<'a> Fn(<P as Plan>::Value<'a>),
    {
        self.map(|plan| InspectQuery { plan, inspect })
    }
}

impl<P: Plan> Query<P, ()> {
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

    /// Executes the plan from a single seed id and returns the target ID plus its value.
    #[inline]
    pub fn get_row<'ctx, Ctx>(
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
    pub fn get_slice_id<'ctx, Ctx>(
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

    /// Executes a slice-valued plan and resolves the resulting slice to its IDs.
    #[inline]
    pub fn get_slice_elements<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        id: Id<P::Src>,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<&'ctx [Id<<P as Plan>::Target>]>
    where
        P: SliceTraversal<'ctx, Ctx> + 'ctx,
        Ctx: SliceRelation<<P as Plan>::Target> + 'ctx,
    {
        <<P as Plan>::Presence as Presence>::map(self.plan.execute_slice(ctx, id), |slice_id| {
            ctx.get_slice(slice_id)
        })
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

    /// Executes the plan from many seed ids and yields target IDs.
    #[inline]
    pub fn iter_ids<'ctx, Ctx, I>(
        self,
        ctx: &'ctx Ctx,
        ids: I,
    ) -> impl Iterator<Item = <<P as Plan>::Presence as Presence>::Wrap<Id<<P as Plan>::Target>>> + 'ctx
    where
        I: IntoIterator<Item = Id<P::Src>>,
        I::IntoIter: 'ctx,
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        let plan = self.plan;
        ids.into_iter().map(move |id| {
            <<P as Plan>::Presence as Presence>::map(plan.execute(ctx, id), |(target_id, _)| {
                target_id
            })
        })
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

    /// Executes the plan from the seed ids stored in a slice.
    #[inline]
    pub fn iter_from_slice<'ctx, Ctx>(
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

    /// Executes the plan from the seed ids stored in a slice and yields target IDs.
    #[inline]
    pub fn iter_ids_from_slice<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        slice: SliceId<P::Src>,
    ) -> impl Iterator<Item = <<P as Plan>::Presence as Presence>::Wrap<Id<<P as Plan>::Target>>> + 'ctx
    where
        Ctx: SliceRelation<P::Src> + 'ctx,
        P: Traversal<'ctx, Ctx> + 'ctx,
    {
        self.iter_ids(ctx, ctx.get_slice(slice).iter().copied())
    }

    /// Executes the plan from the seed ids stored in a slice and yields target IDs with values.
    #[inline]
    pub fn iter_rows_from_slice<'ctx, Ctx>(
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

    /// Branches each seed in a slice into a batch of seeded queries.
    #[inline]
    pub fn branch_slice<'ctx, Ctx, B, F>(
        self,
        ctx: &'ctx Ctx,
        slice: SliceId<P::Src>,
        mut f: F,
    ) -> Vec<<<P as Plan>::Presence as Presence>::Wrap<B::Output>>
    where
        Ctx: SliceRelation<P::Src> + 'ctx,
        P: Traversal<'ctx, Ctx> + 'ctx,
        F: FnMut(Id<<P as Plan>::Target>, <P as Plan>::Value<'ctx>) -> B,
        B: Batch<'ctx, Ctx>,
    {
        ctx.get_slice(slice)
            .iter()
            .copied()
            .map(|seed| {
                <<P as Plan>::Presence as Presence>::map(
                    self.plan.execute(ctx, seed),
                    |(target_id, value)| f(target_id, value).run(ctx),
                )
            })
            .collect()
    }
}

impl<P: Plan> Query<P, Id<P::Src>> {
    /// Executes the seeded plan.
    #[inline]
    pub fn get<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<<P as Plan>::Value<'ctx>>
    where
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        let Query { plan, seed } = self;
        Query { plan, seed: () }.get(ctx, seed)
    }

    /// Executes the seeded plan and returns only the target ID.
    #[inline]
    pub fn get_id<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<Id<<P as Plan>::Target>>
    where
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        let Query { plan, seed } = self;
        Query { plan, seed: () }.get_id(ctx, seed)
    }

    /// Executes the seeded plan and returns the target ID plus its value.
    #[inline]
    pub fn get_row<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<(
        Id<<P as Plan>::Target>,
        <P as Plan>::Value<'ctx>,
    )>
    where
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        let Query { plan, seed } = self;
        Query { plan, seed: () }.get_row(ctx, seed)
    }

    /// Executes a seeded slice-valued plan.
    #[inline]
    pub fn get_slice_id<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<SliceId<<P as Plan>::Target>>
    where
        P: SliceTraversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        let Query { plan, seed } = self;
        Query { plan, seed: () }.get_slice_id(ctx, seed)
    }

    /// Executes a seeded slice-valued plan and resolves the resulting slice to its IDs.
    #[inline]
    pub fn get_slice_elements<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<&'ctx [Id<<P as Plan>::Target>]>
    where
        P: SliceTraversal<'ctx, Ctx> + 'ctx,
        Ctx: SliceRelation<<P as Plan>::Target> + 'ctx,
    {
        let Query { plan, seed } = self;
        Query { plan, seed: () }.get_slice_elements(ctx, seed)
    }

    /// Branches the seeded value into a batch of seeded queries.
    #[inline]
    pub fn branch<'ctx, Ctx, B, F>(
        self,
        ctx: &'ctx Ctx,
        f: F,
    ) -> <<P as Plan>::Presence as Presence>::Wrap<B::Output>
    where
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
        F: FnOnce(Id<<P as Plan>::Target>, <P as Plan>::Value<'ctx>) -> B,
        B: Batch<'ctx, Ctx>,
    {
        <<P as Plan>::Presence as Presence>::map(self.get_row(ctx), |(target_id, value)| {
            f(target_id, value).run(ctx)
        })
    }
}

/// A batch of seeded queries that can execute against one context.
pub trait Batch<'ctx, Ctx> {
    type Output;

    fn run(self, ctx: &'ctx Ctx) -> Self::Output;
}

impl<'ctx, Ctx, P> Batch<'ctx, Ctx> for Query<P, Id<P::Src>>
where
    P: Traversal<'ctx, Ctx> + 'ctx,
    Ctx: 'ctx,
{
    type Output = <<P as Plan>::Presence as Presence>::Wrap<<P as Plan>::Value<'ctx>>;

    #[inline]
    fn run(self, ctx: &'ctx Ctx) -> Self::Output {
        self.get(ctx)
    }
}

macro_rules! impl_batch_tuple {
    ($($T:ident:$idx:tt),+) => {
        impl<'ctx, Ctx, $($T,)+> Batch<'ctx, Ctx> for ($($T,)+)
        where
            $($T: Batch<'ctx, Ctx>,)+
        {
            type Output = ($($T::Output,)+);

            #[inline]
            fn run(self, ctx: &'ctx Ctx) -> Self::Output {
                ($(self.$idx.run(ctx),)+)
            }
        }
    };
}

impl_batch_tuple!(A:0, B:1);
impl_batch_tuple!(A:0, B:1, C:2);
impl_batch_tuple!(A:0, B:1, C:2, D:3);
impl_batch_tuple!(A:0, B:1, C:2, D:3, E:4);
impl_batch_tuple!(A:0, B:1, C:2, D:3, E:4, F:5);
impl_batch_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6);
impl_batch_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7);
