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
pub struct ScanOneQuery<T>(pub PhantomData<T>);

impl<T> Copy for ScanOneQuery<T> {}

impl<T> Clone for ScanOneQuery<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

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

/// Projection helper that hides the intermediate child type of a projector
/// closure for `project_as`.
pub trait Projection<P: Plan>: for<'a> Fn(<P as Plan>::Value<'a>) -> Id<Self::Child> {
    type Child: 'static;
}

impl<P, U, F> Projection<P> for F
where
    P: Plan,
    U: 'static,
    F: for<'a> Fn(<P as Plan>::Value<'a>) -> Id<U>,
{
    type Child = U;
}

/// Projection helper that hides the intermediate child type of an optional
/// projector closure for `project_opt_as`.
pub trait OptProjection<P: Plan>:
    for<'a> Fn(<P as Plan>::Value<'a>) -> Option<Id<Self::Child>>
{
    type Child: 'static;
}

impl<P, U, F> OptProjection<P> for F
where
    P: Plan,
    U: 'static,
    F: for<'a> Fn(<P as Plan>::Value<'a>) -> Option<Id<U>>,
{
    type Child = U;
}

/// Plan step: Project a child and require it to cast to `Target`.
pub struct ProjectAsQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Copy, Target, F: Copy> Copy for ProjectAsQuery<P, Target, F> {}

impl<P: Clone, Target, F: Clone> Clone for ProjectAsQuery<P, Target, F> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            projector: self.projector.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Plan, Target: 'static, F> Plan for ProjectAsQuery<P, Target, F> {
    type Src = P::Src;
    type Target = Target;
    type Presence = P::Presence;
    type Value<'a> = &'a Target;
}

impl<'ctx, Ctx, P, Target, F> Traversal<'ctx, Ctx> for ProjectAsQuery<P, Target, F>
where
    P: Traversal<'ctx, Ctx>,
    Target: 'static,
    F: Projection<P>,
    <F as Projection<P>>::Child: TryCast<Target> + 'static,
    Ctx: TotalRelation<Target, Target>
        + TotalRelation<<F as Projection<P>>::Child, <F as Projection<P>>::Child>
        + 'ctx,
{
    #[inline]
    #[track_caller]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<(Id<Target>, &'ctx Target)> {
        <P::Presence as Presence>::map(self.plan.execute(ctx, start), |(_, mid)| {
            let child = (self.projector)(mid);
            let target = ctx
                .get(child)
                .try_cast()
                .unwrap_or_else(|| panic!("expected a `{}`", std::any::type_name::<Target>()));
            (target, ctx.get(target))
        })
    }
}

/// Plan step: Project an optional child and require it to cast to `Target`.
pub struct ProjectOptAsQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Copy, Target, F: Copy> Copy for ProjectOptAsQuery<P, Target, F> {}

impl<P: Clone, Target, F: Clone> Clone for ProjectOptAsQuery<P, Target, F> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            projector: self.projector.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Plan, Target: 'static, F> Plan for ProjectOptAsQuery<P, Target, F> {
    type Src = P::Src;
    type Target = Target;
    type Presence = Optional;
    type Value<'a> = &'a Target;
}

impl<'ctx, Ctx, P, Target, F> Traversal<'ctx, Ctx> for ProjectOptAsQuery<P, Target, F>
where
    P: Traversal<'ctx, Ctx>,
    Target: 'static,
    F: OptProjection<P>,
    <F as OptProjection<P>>::Child: TryCast<Target> + 'static,
    Ctx: TotalRelation<Target, Target>
        + TotalRelation<<F as OptProjection<P>>::Child, <F as OptProjection<P>>::Child>
        + 'ctx,
{
    #[inline]
    #[track_caller]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<(Id<Target>, &'ctx Target)> {
        let (_, mid) = <P::Presence as Presence>::into_option(self.plan.execute(ctx, start))?;
        let child = (self.projector)(mid)?;
        let target = ctx
            .get(child)
            .try_cast()
            .unwrap_or_else(|| panic!("expected a `{}`", std::any::type_name::<Target>()));
        Some((target, ctx.get(target)))
    }
}

/// Plan step: Type casting path (prism)
pub struct CastQuery<P, Target> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Copy, Target> Copy for CastQuery<P, Target> {}

impl<P: Clone, Target> Clone for CastQuery<P, Target> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
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
pub struct ProjectQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Copy, Target, F: Copy> Copy for ProjectQuery<P, Target, F> {}

impl<P: Clone, Target, F: Clone> Clone for ProjectQuery<P, Target, F> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            projector: self.projector.clone(),
            _marker: PhantomData,
        }
    }
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
pub struct ProjectSliceQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Copy, Target, F: Copy> Copy for ProjectSliceQuery<P, Target, F> {}

impl<P: Clone, Target, F: Clone> Clone for ProjectSliceQuery<P, Target, F> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            projector: self.projector.clone(),
            _marker: PhantomData,
        }
    }
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
pub struct ProjectOptQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: Copy, Target, F: Copy> Copy for ProjectOptQuery<P, Target, F> {}

impl<P: Clone, Target, F: Clone> Clone for ProjectOptQuery<P, Target, F> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            projector: self.projector.clone(),
            _marker: PhantomData,
        }
    }
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
pub struct WithQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<P: Copy, M> Copy for WithQuery<P, M> {}

impl<P: Clone, M> Clone for WithQuery<P, M> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
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
pub struct WithRequiredQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<P: Copy, M> Copy for WithRequiredQuery<P, M> {}

impl<P: Clone, M> Clone for WithRequiredQuery<P, M> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
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
pub struct LeftJoinQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<P: Copy, M> Copy for LeftJoinQuery<P, M> {}

impl<P: Clone, M> Clone for LeftJoinQuery<P, M> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
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
pub struct WithoutQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<P: Copy, M> Copy for WithoutQuery<P, M> {}

impl<P: Clone, M> Clone for WithoutQuery<P, M> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
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

/// Plan step: Assert an expected value while preserving the row and presence.
#[derive(Clone, Copy)]
pub struct AssertEqQuery<P, E> {
    pub(crate) plan: P,
    pub(crate) expected: E,
}

impl<P: Plan, E> Plan for AssertEqQuery<P, E> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = P::Presence;
    type Value<'a> = <P as Plan>::Value<'a>;
}

/// Comparison helper for assertion plans, allowing a projected value (`&T`)
/// to be compared with an owned expected value (`T`).
pub trait AssertValue<E> {
    fn assert_value(&self, expected: &E);
}

impl<T> AssertValue<T> for &T
where
    T: PartialEq + std::fmt::Debug,
{
    #[inline]
    #[track_caller]
    fn assert_value(&self, expected: &T) {
        assert_eq!(*self, expected);
    }
}

impl<T> AssertValue<T> for T
where
    T: PartialEq + std::fmt::Debug,
{
    #[inline]
    #[track_caller]
    fn assert_value(&self, expected: &T) {
        assert_eq!(self, expected);
    }
}

impl<'ctx, Ctx, P, E> Traversal<'ctx, Ctx> for AssertEqQuery<P, E>
where
    P: Traversal<'ctx, Ctx>,
    E: std::fmt::Debug,
    for<'a> <P as Plan>::Value<'a>: AssertValue<E> + std::fmt::Debug,
{
    #[inline]
    #[track_caller]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<(Id<P::Target>, <P as Plan>::Value<'ctx>)> {
        <P::Presence as Presence>::map(self.plan.execute(ctx, start), |(target_id, value)| {
            value.assert_value(&self.expected);
            (target_id, value)
        })
    }
}

/// Plan step: Assert that an optional plan yields no value.
#[derive(Clone, Copy)]
pub struct AssertNoneQuery<P> {
    pub(crate) plan: P,
}

impl<P: Plan> Plan for AssertNoneQuery<P> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = Optional;
    type Value<'a> = <P as Plan>::Value<'a>;
}

impl<'ctx, Ctx, P> Traversal<'ctx, Ctx> for AssertNoneQuery<P>
where
    P: Traversal<'ctx, Ctx> + Plan<Presence = Optional>,
{
    #[inline]
    #[track_caller]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<P::Target>, <P as Plan>::Value<'ctx>)> {
        let row = self.plan.execute(ctx, start);
        assert!(row.is_none(), "expected no value");
        None
    }
}

/// Plan step: Assert the length of a slice-valued plan.
#[derive(Clone, Copy)]
pub struct AssertLenQuery<P> {
    pub(crate) plan: P,
    pub(crate) expected: usize,
}

impl<P: Plan> Plan for AssertLenQuery<P> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = P::Presence;
    type Value<'a> = SliceId<P::Target>;
}

impl<'ctx, Ctx, P> SliceTraversal<'ctx, Ctx> for AssertLenQuery<P>
where
    P: SliceTraversal<'ctx, Ctx>,
{
    #[inline]
    #[track_caller]
    fn execute_slice(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<SliceId<P::Target>> {
        <P::Presence as Presence>::map(self.plan.execute_slice(ctx, start), |slice_id| {
            assert_eq!(slice_id.len(), self.expected);
            slice_id
        })
    }
}

/// Plan step: Narrow slice element by index
pub struct GetAt<P> {
    pub(crate) plan: P,
    pub(crate) index: usize,
}

impl<P: Copy> Copy for GetAt<P> {}

impl<P: Clone> Clone for GetAt<P> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            index: self.index,
        }
    }
}

impl<P: Plan> Plan for GetAt<P> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = Optional;
    type Value<'a> = &'a <P as Plan>::Target;
}

impl<'ctx, Ctx, P> Traversal<'ctx, Ctx> for GetAt<P>
where
    P: SliceTraversal<'ctx, Ctx>,
    Ctx: SliceRelation<P::Target> + TotalRelation<P::Target, P::Target> + 'ctx,
{
    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<P::Target>, &'ctx P::Target)> {
        let slice_id = <P::Presence as Presence>::into_option(self.plan.execute_slice(ctx, start))?;
        let ids = ctx.get_slice(slice_id);
        let id = *ids.get(self.index)?;
        Some((id, ctx.get(id)))
    }
}
