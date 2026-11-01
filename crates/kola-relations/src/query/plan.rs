use std::marker::PhantomData;

use crate::Id;
use crate::presence::{Optional, Presence, Required};
use crate::relation::{PartialRelation, SliceRelation, TotalRelation};
use crate::slice::SliceId;

/// Conversion trait for attempting to extract/project a child ID of a specific type.
/// Implemented by the compiler AST crate.
pub trait TryCast<Target: 'static> {
    fn try_cast(&self) -> Option<Id<Target>>;
}

impl<T, Target: 'static> TryCast<Target> for &T
where
    T: TryCast<Target>,
{
    #[inline]
    fn try_cast(&self) -> Option<Id<Target>> {
        (*self).try_cast()
    }
}

/// Describes the physical traversal of a query path `Src` -> `Target` against an active context `'ctx`.
pub trait Traversal<'ctx, Ctx>: Plan {
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<<Self as Plan>::Src>,
    ) -> <<Self as Plan>::Presence as Presence>::Wrap<(
        Id<<Self as Plan>::Target>,
        <Self as Plan>::Value<'ctx>,
    )>;
}

/// Describes the physical traversal of a query path `Src` -> `SliceId<Target>` against an active context `'ctx`.
pub trait SliceTraversal<'ctx, Ctx>: Plan {
    fn execute_slice(
        &self,
        ctx: &'ctx Ctx,
        start: Id<<Self as Plan>::Src>,
    ) -> <<Self as Plan>::Presence as Presence>::Wrap<SliceId<<Self as Plan>::Target>>;
}

pub trait Plan {
    type Src: 'static;
    type Target: 'static;
    type Presence: Presence;
    type Value<'a>: Copy + Clone + 'a;
}

/// Plan step: Initial single ID scan path
#[derive(Clone, Copy)]
pub struct ScanOneQuery<T>(pub PhantomData<T>);

impl<T: 'static> Plan for ScanOneQuery<T> {
    type Src = T;
    type Target = T;
    type Presence = Required;
    type Value<'a> = &'a T;
}

impl<'ctx, Ctx, T: 'static> Traversal<'ctx, Ctx> for ScanOneQuery<T>
where
    Ctx: TotalRelation<T, T> + 'ctx,
{
    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<T>) -> (Id<T>, &'ctx T) {
        (start, ctx.get(start))
    }
}

/// Plan step: Value filtering path
#[derive(Clone, Copy)]
pub struct FilterQuery<P, F> {
    pub(crate) plan: P,
    pub(crate) predicate: F,
}

impl<P: Plan, F> Plan for FilterQuery<P, F> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = Optional;
    type Value<'a> = <P as Plan>::Value<'a>;
}

impl<'ctx, Ctx, P, F> Traversal<'ctx, Ctx> for FilterQuery<P, F>
where
    P: Traversal<'ctx, Ctx>,
    F: for<'a> Fn(<P as Plan>::Value<'a>) -> bool,
{
    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<Self::Target>, <Self as Plan>::Value<'ctx>)> {
        let (target_id, value) =
            <P::Presence as Presence>::into_option(self.plan.execute(ctx, start))?;

        if (self.predicate)(value) {
            Some((target_id, value))
        } else {
            None
        }
    }
}

/// Plan step: Inspect a value while preserving the row and presence.
#[derive(Clone, Copy)]
pub struct InspectQuery<P, F> {
    pub(crate) plan: P,
    pub(crate) inspect: F,
}

impl<P: Plan, F> Plan for InspectQuery<P, F> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = P::Presence;
    type Value<'a> = <P as Plan>::Value<'a>;
}

impl<'ctx, Ctx, P, F> Traversal<'ctx, Ctx> for InspectQuery<P, F>
where
    P: Traversal<'ctx, Ctx>,
    F: for<'a> Fn(<P as Plan>::Value<'a>),
{
    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<(Id<P::Target>, <P as Plan>::Value<'ctx>)> {
        <P::Presence as Presence>::map(self.plan.execute(ctx, start), |(target_id, value)| {
            (self.inspect)(value);
            (target_id, value)
        })
    }
}

impl<'ctx, Ctx, P, F> SliceTraversal<'ctx, Ctx> for InspectQuery<P, F>
where
    P: SliceTraversal<'ctx, Ctx>,
    F: Fn(SliceId<P::Target>),
{
    #[inline]
    fn execute_slice(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<SliceId<P::Target>> {
        <P::Presence as Presence>::map(self.plan.execute_slice(ctx, start), |slice_id| {
            (self.inspect)(slice_id);
            slice_id
        })
    }
}

/// Plan step: Type casting path (prism)
#[derive(Clone, Copy)]
pub struct CastQuery<P, Target> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Plan, Target: 'static> Plan for CastQuery<P, Target> {
    type Src = P::Src;
    type Target = Target;
    type Presence = Optional;
    type Value<'a> = &'a Target;
}

impl<'ctx, Ctx, P, Target> Traversal<'ctx, Ctx> for CastQuery<P, Target>
where
    P: Traversal<'ctx, Ctx>,
    <P as Plan>::Value<'ctx>: TryCast<Target>,
    Ctx: TotalRelation<Target, Target> + 'ctx,
    Target: 'static,
{
    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<(Id<Target>, &'ctx Target)> {
        let (_, mid) = <P::Presence as Presence>::into_option(self.plan.execute(ctx, start))?;
        let target_id = mid.try_cast()?;
        Some((target_id, ctx.get(target_id)))
    }
}

/// Plan step: Assert that the inner plan yields a focus
#[derive(Clone, Copy)]
pub struct ExpectQuery<P> {
    pub(crate) plan: P,
}

impl<P: Plan> Plan for ExpectQuery<P> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = Required;
    type Value<'a> = <P as Plan>::Value<'a>;
}

impl<'ctx, Ctx, P> Traversal<'ctx, Ctx> for ExpectQuery<P>
where
    P: Traversal<'ctx, Ctx>,
{
    #[inline]
    #[track_caller]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> (Id<P::Target>, <P as Plan>::Value<'ctx>) {
        <P::Presence as Presence>::into_option(self.plan.execute(ctx, start)).unwrap_or_else(|| {
            panic!(
                "expected a `{}` at {}",
                std::any::type_name::<P::Target>(),
                std::panic::Location::caller(),
            )
        })
    }
}

impl<'ctx, Ctx, P> SliceTraversal<'ctx, Ctx> for ExpectQuery<P>
where
    P: SliceTraversal<'ctx, Ctx>,
{
    #[inline]
    #[track_caller]
    fn execute_slice(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> SliceId<P::Target> {
        <P::Presence as Presence>::into_option(self.plan.execute_slice(ctx, start)).unwrap_or_else(
            || {
                panic!(
                    "expected a slice of `{}` at {}",
                    std::any::type_name::<P::Target>(),
                    std::panic::Location::caller(),
                )
            },
        )
    }
}

/// Plan step: Field projection path
#[derive(Clone, Copy)]
pub struct ProjectQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Plan, Target: 'static, F> Plan for ProjectQuery<P, Target, F> {
    type Src = P::Src;
    type Target = Target;
    type Presence = <P as Plan>::Presence;
    type Value<'a> = &'a Target;
}

impl<'ctx, Ctx, P, Target, F> Traversal<'ctx, Ctx> for ProjectQuery<P, Target, F>
where
    P: Traversal<'ctx, Ctx>,
    F: for<'a> Fn(<P as Plan>::Value<'a>) -> Id<Target>,
    Ctx: TotalRelation<Target, Target> + 'ctx,
    Target: 'static,
{
    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<(Id<Target>, &'ctx Target)> {
        <P::Presence as Presence>::map(self.plan.execute(ctx, start), |(_, mid)| {
            let target_id = (self.projector)(mid);
            (target_id, ctx.get(target_id))
        })
    }
}

/// Plan step: Slice projection path
#[derive(Clone, Copy)]
pub struct ProjectSliceQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Plan, Target: 'static, F> Plan for ProjectSliceQuery<P, Target, F> {
    type Src = P::Src;
    type Target = Target;
    type Presence = <P as Plan>::Presence;
    type Value<'a> = SliceId<Target>;
}

impl<'ctx, Ctx, P, Target, F> SliceTraversal<'ctx, Ctx> for ProjectSliceQuery<P, Target, F>
where
    P: Traversal<'ctx, Ctx>,
    F: for<'a> Fn(<P as Plan>::Value<'a>) -> SliceId<Target>,
    Target: 'static,
{
    #[inline]
    fn execute_slice(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<SliceId<Target>> {
        <P::Presence as Presence>::map(self.plan.execute(ctx, start), |(_, mid)| {
            (self.projector)(mid)
        })
    }
}

/// Plan step: Optional projection path
#[derive(Clone, Copy)]
pub struct ProjectOptQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Plan, Target: 'static, F> Plan for ProjectOptQuery<P, Target, F> {
    type Src = P::Src;
    type Target = Target;
    type Presence = Optional;
    type Value<'a> = &'a Target;
}

impl<'ctx, Ctx, P, Target, F> Traversal<'ctx, Ctx> for ProjectOptQuery<P, Target, F>
where
    P: Traversal<'ctx, Ctx>,
    F: for<'a> Fn(<P as Plan>::Value<'a>) -> Option<Id<Target>>,
    Ctx: TotalRelation<Target, Target> + 'ctx,
    Target: 'static,
{
    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<(Id<Target>, &'ctx Target)> {
        let (_, mid) = <P::Presence as Presence>::into_option(self.plan.execute(ctx, start))?;
        let target_id = (self.projector)(mid)?;
        Some((target_id, ctx.get(target_id)))
    }
}

/// Plan step: Inner join metadata
#[derive(Clone, Copy)]
pub struct WithQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<P: Plan, M: 'static> Plan for WithQuery<P, M> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = Optional;
    type Value<'a> = (<P as Plan>::Value<'a>, &'a M);
}

impl<'ctx, Ctx, P, M: 'static> Traversal<'ctx, Ctx> for WithQuery<P, M>
where
    P: Traversal<'ctx, Ctx>,
    Ctx: PartialRelation<P::Target, M> + TotalRelation<P::Target, P::Target> + 'ctx,
{
    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<Self::Target>, Self::Value<'ctx>)> {
        let (target_id, value) =
            <P::Presence as Presence>::into_option(self.plan.execute(ctx, start))?;
        let meta = ctx.get_opt(target_id)?;
        Some((target_id, (value, meta)))
    }
}

/// Plan step: Required metadata join (cannot drop a row)
#[derive(Clone, Copy)]
pub struct WithRequiredQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<P: Plan, M: 'static> Plan for WithRequiredQuery<P, M> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = <P as Plan>::Presence;
    type Value<'a> = (<P as Plan>::Value<'a>, &'a M);
}

impl<'ctx, Ctx, P, M: 'static> Traversal<'ctx, Ctx> for WithRequiredQuery<P, M>
where
    P: Traversal<'ctx, Ctx>,
    Ctx: TotalRelation<P::Target, M> + TotalRelation<P::Target, P::Target> + 'ctx,
{
    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<(Id<Self::Target>, Self::Value<'ctx>)> {
        <P::Presence as Presence>::map(self.plan.execute(ctx, start), |(target_id, value)| {
            let meta = <Ctx as TotalRelation<P::Target, M>>::get(ctx, target_id);
            (target_id, (value, meta))
        })
    }
}

/// Plan step: Left join metadata
#[derive(Clone, Copy)]
pub struct LeftJoinQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<P: Plan, M: 'static> Plan for LeftJoinQuery<P, M> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = <P as Plan>::Presence;
    type Value<'a> = (<P as Plan>::Value<'a>, Option<&'a M>);
}

impl<'ctx, Ctx, P, M: 'static> Traversal<'ctx, Ctx> for LeftJoinQuery<P, M>
where
    P: Traversal<'ctx, Ctx>,
    Ctx: PartialRelation<P::Target, M> + TotalRelation<P::Target, P::Target> + 'ctx,
{
    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<(Id<Self::Target>, Self::Value<'ctx>)> {
        <P::Presence as Presence>::map(self.plan.execute(ctx, start), |(target_id, value)| {
            let meta = ctx.get_opt(target_id);
            (target_id, (value, meta))
        })
    }
}

/// Plan step: Anti-join metadata
#[derive(Clone, Copy)]
pub struct WithoutQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<P: Plan, M: 'static> Plan for WithoutQuery<P, M> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = Optional;
    type Value<'a> = <P as Plan>::Value<'a>;
}

impl<'ctx, Ctx, P, M: 'static> Traversal<'ctx, Ctx> for WithoutQuery<P, M>
where
    P: Traversal<'ctx, Ctx>,
    Ctx: PartialRelation<P::Target, M> + TotalRelation<P::Target, P::Target> + 'ctx,
{
    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<Self::Target>, Self::Value<'ctx>)> {
        let (target_id, value) =
            <P::Presence as Presence>::into_option(self.plan.execute(ctx, start))?;

        if ctx.get_opt(target_id).is_none() {
            Some((target_id, value))
        } else {
            None
        }
    }
}

/// Plan step: Narrow slice element by index
#[derive(Clone, Copy)]
pub struct GetAt<P, T> {
    pub(crate) plan: P,
    pub(crate) index: usize,
    pub(crate) _marker: PhantomData<T>,
}

impl<P: Plan, T: 'static> Plan for GetAt<P, T> {
    type Src = P::Src;
    type Target = T;
    type Presence = Optional;
    type Value<'a> = &'a T;
}

impl<'ctx, Ctx, P, T: 'static> Traversal<'ctx, Ctx> for GetAt<P, T>
where
    P: SliceTraversal<'ctx, Ctx, Target = T>,
    Ctx: SliceRelation<T> + TotalRelation<T, T> + 'ctx,
{
    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<(Id<T>, &'ctx T)> {
        let slice_id = <P::Presence as Presence>::into_option(self.plan.execute_slice(ctx, start))?;
        let ids = ctx.get_slice(slice_id);
        let id = *ids.get(self.index)?;
        Some((id, ctx.get(id)))
    }
}
