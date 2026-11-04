use super::{Plan, Query, SliceTraversal, Traversal};
use crate::Id;
use crate::presence::Presence;
use crate::relation::SliceRelation;
use crate::slice::SliceId;

impl<P: Plan> Query<P, ()> {
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
}
