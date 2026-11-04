use std::marker::PhantomData;

use crate::Id;
use crate::presence::Presence;
use crate::relation::SliceRelation;
use crate::slice::SliceId;

use super::{Plan, Query, ScanOneQuery, SliceTraversal, Traversal};

/// The single execution boundary for query plans.
pub trait Run<'ctx, Ctx> {
    type Output;

    fn run(self, ctx: &'ctx Ctx) -> Self::Output;
}

impl<'ctx, Ctx, P> Run<'ctx, Ctx> for Query<P, Id<P::Src>>
where
    P: Traversal<'ctx, Ctx> + 'ctx,
    Ctx: 'ctx,
{
    type Output = ();

    #[inline]
    fn run(self, ctx: &'ctx Ctx) {
        let Query { plan, seed } = self;
        let _ = plan.execute(ctx, seed);
    }
}

impl<'ctx, Ctx> Run<'ctx, Ctx> for () {
    type Output = ();

    #[inline]
    fn run(self, _ctx: &'ctx Ctx) {}
}

macro_rules! impl_run_tuple {
    ($($T:ident:$idx:tt),+) => {
        impl<'ctx, Ctx, $($T,)+> Run<'ctx, Ctx> for ($($T,)+)
        where
            $($T: Run<'ctx, Ctx>,)+
        {
            type Output = ($($T::Output,)+);

            #[inline]
            fn run(self, ctx: &'ctx Ctx) -> Self::Output {
                ($(self.$idx.run(ctx),)+)
            }
        }
    };
}

impl_run_tuple!(A:0);
impl_run_tuple!(A:0, B:1);
impl_run_tuple!(A:0, B:1, C:2);
impl_run_tuple!(A:0, B:1, C:2, D:3);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M:12);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M:12, N:13);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M:12, N:13, O:14);
impl_run_tuple!(A:0, B:1, C:2, D:3, E:4, F:5, G:6, H:7, I:8, J:9, K:10, L:11, M:12, N:13, O:14, P:15);

#[derive(Clone, Copy)]
pub struct Get<P: Plan> {
    plan: P,
    seed: Id<P::Src>,
}

#[derive(Clone, Copy)]
pub struct GetId<P: Plan> {
    plan: P,
    seed: Id<P::Src>,
}

#[derive(Clone, Copy)]
pub struct GetRow<P: Plan> {
    plan: P,
    seed: Id<P::Src>,
}

#[derive(Clone, Copy)]
pub struct GetSliceId<P: Plan> {
    plan: P,
    seed: Id<P::Src>,
}

#[derive(Clone, Copy)]
pub struct GetSliceElements<P: Plan> {
    plan: P,
    seed: Id<P::Src>,
}

impl<P: Plan> Query<P, Id<P::Src>> {
    #[inline]
    pub fn get(self) -> Get<P> {
        let Query { plan, seed } = self;
        Get { plan, seed }
    }

    #[inline]
    pub fn get_id(self) -> GetId<P> {
        let Query { plan, seed } = self;
        GetId { plan, seed }
    }

    #[inline]
    pub fn get_row(self) -> GetRow<P> {
        let Query { plan, seed } = self;
        GetRow { plan, seed }
    }

    #[inline]
    pub fn get_slice_id(self) -> GetSliceId<P> {
        let Query { plan, seed } = self;
        GetSliceId { plan, seed }
    }

    #[inline]
    pub fn get_slice_elements(self) -> GetSliceElements<P> {
        let Query { plan, seed } = self;
        GetSliceElements { plan, seed }
    }
}

impl<P: Plan> Query<P, ()> {
    #[inline]
    pub fn get(self, seed: Id<P::Src>) -> Get<P> {
        Get {
            plan: self.plan,
            seed,
        }
    }

    #[inline]
    pub fn get_id(self, seed: Id<P::Src>) -> GetId<P> {
        GetId {
            plan: self.plan,
            seed,
        }
    }

    #[inline]
    pub fn get_row(self, seed: Id<P::Src>) -> GetRow<P> {
        GetRow {
            plan: self.plan,
            seed,
        }
    }

    #[inline]
    pub fn get_slice_id(self, seed: Id<P::Src>) -> GetSliceId<P> {
        GetSliceId {
            plan: self.plan,
            seed,
        }
    }

    #[inline]
    pub fn get_slice_elements(self, seed: Id<P::Src>) -> GetSliceElements<P> {
        GetSliceElements {
            plan: self.plan,
            seed,
        }
    }
}

impl<'ctx, Ctx, P> Run<'ctx, Ctx> for Get<P>
where
    P: Traversal<'ctx, Ctx> + 'ctx,
    Ctx: 'ctx,
{
    type Output = <<P as Plan>::Presence as Presence>::Wrap<<P as Plan>::Value<'ctx>>;

    #[inline]
    fn run(self, ctx: &'ctx Ctx) -> Self::Output {
        <<P as Plan>::Presence as Presence>::map(self.plan.execute(ctx, self.seed), |(_, value)| {
            value
        })
    }
}

impl<'ctx, Ctx, P> Run<'ctx, Ctx> for GetId<P>
where
    P: Traversal<'ctx, Ctx> + 'ctx,
    Ctx: 'ctx,
{
    type Output = <<P as Plan>::Presence as Presence>::Wrap<Id<<P as Plan>::Target>>;

    #[inline]
    fn run(self, ctx: &'ctx Ctx) -> Self::Output {
        <<P as Plan>::Presence as Presence>::map(self.plan.execute(ctx, self.seed), |(id, _)| id)
    }
}

impl<'ctx, Ctx, P> Run<'ctx, Ctx> for GetRow<P>
where
    P: Traversal<'ctx, Ctx> + 'ctx,
    Ctx: 'ctx,
{
    type Output = <<P as Plan>::Presence as Presence>::Wrap<(
        Id<<P as Plan>::Target>,
        <P as Plan>::Value<'ctx>,
    )>;

    #[inline]
    fn run(self, ctx: &'ctx Ctx) -> Self::Output {
        self.plan.execute(ctx, self.seed)
    }
}

impl<'ctx, Ctx, P> Run<'ctx, Ctx> for GetSliceId<P>
where
    P: SliceTraversal<'ctx, Ctx> + 'ctx,
    Ctx: 'ctx,
{
    type Output = <<P as Plan>::Presence as Presence>::Wrap<SliceId<<P as Plan>::Target>>;

    #[inline]
    fn run(self, ctx: &'ctx Ctx) -> Self::Output {
        self.plan.execute_slice(ctx, self.seed)
    }
}

impl<'ctx, Ctx, P> Run<'ctx, Ctx> for GetSliceElements<P>
where
    P: SliceTraversal<'ctx, Ctx> + 'ctx,
    Ctx: SliceRelation<<P as Plan>::Target> + 'ctx,
{
    type Output = <<P as Plan>::Presence as Presence>::Wrap<&'ctx [Id<<P as Plan>::Target>]>;

    #[inline]
    fn run(self, ctx: &'ctx Ctx) -> Self::Output {
        <<P as Plan>::Presence as Presence>::map(
            self.plan.execute_slice(ctx, self.seed),
            |slice_id| ctx.get_slice(slice_id),
        )
    }
}

/// Lazy per-element execution over a slice-valued query.
pub struct Each<P: Plan, F> {
    plan: P,
    seed: Id<P::Src>,
    each: F,
}

/// Lazy per-seed execution over an explicit slice of source ids.
pub struct EachSlice<P: Plan, F> {
    plan: P,
    slice: SliceId<P::Src>,
    each: F,
}

impl<P: Plan> Query<P, Id<P::Src>> {
    #[inline]
    pub fn each<F, B>(self, each: F) -> Each<P, F>
    where
        F: FnMut(Query<ScanOneQuery<P::Target>, Id<P::Target>>) -> B,
    {
        let Query { plan, seed } = self;
        Each { plan, seed, each }
    }
}

impl<P: Plan> Query<P, ()> {
    #[inline]
    pub fn each<F, B>(self, seed: Id<P::Src>, each: F) -> Each<P, F>
    where
        F: FnMut(Query<ScanOneQuery<P::Target>, Id<P::Target>>) -> B,
    {
        Each {
            plan: self.plan,
            seed,
            each,
        }
    }

    #[inline]
    pub fn each_slice<F, B>(self, slice: SliceId<P::Src>, each: F) -> EachSlice<P, F>
    where
        F: FnMut(Query<ScanOneQuery<P::Target>, Id<P::Target>>) -> B,
    {
        EachSlice {
            plan: self.plan,
            slice,
            each,
        }
    }
}

impl<'ctx, Ctx, P, F, B> Run<'ctx, Ctx> for Each<P, F>
where
    P: SliceTraversal<'ctx, Ctx> + 'ctx,
    Ctx: SliceRelation<<P as Plan>::Target> + 'ctx,
    F: FnMut(Query<ScanOneQuery<<P as Plan>::Target>, Id<<P as Plan>::Target>>) -> B,
    B: Run<'ctx, Ctx>,
{
    type Output = ();

    #[inline]
    fn run(mut self, ctx: &'ctx Ctx) {
        let slice = <<P as Plan>::Presence as Presence>::into_option(
            self.plan.execute_slice(ctx, self.seed),
        );

        if let Some(slice) = slice {
            for id in ctx.get_slice(slice).iter().copied() {
                let query = Query {
                    plan: ScanOneQuery(PhantomData),
                    seed: id,
                };
                let _ = (self.each)(query).run(ctx);
            }
        }
    }
}

impl<'ctx, Ctx, P, F, B> Run<'ctx, Ctx> for EachSlice<P, F>
where
    P: Traversal<'ctx, Ctx> + 'ctx,
    Ctx: SliceRelation<<P as Plan>::Src> + 'ctx,
    F: FnMut(Query<ScanOneQuery<<P as Plan>::Target>, Id<<P as Plan>::Target>>) -> B,
    B: Run<'ctx, Ctx>,
{
    type Output = ();

    #[inline]
    fn run(mut self, ctx: &'ctx Ctx) {
        for seed in ctx.get_slice(self.slice).iter().copied() {
            let row =
                <<P as Plan>::Presence as Presence>::into_option(self.plan.execute(ctx, seed));

            if let Some((target_id, _)) = row {
                let query = Query {
                    plan: ScanOneQuery(PhantomData),
                    seed: target_id,
                };
                let _ = (self.each)(query).run(ctx);
            }
        }
    }
}
