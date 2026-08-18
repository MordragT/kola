use std::marker::PhantomData;

use crate::Id;
use crate::query::{Query, SliceTraversal, Traversal, TryProject};
use crate::relation::{PartialRelation, SliceRelation, TotalRelation, TotalRelationMut};
use crate::slice::SliceId;

/// Plan step: Initial single ID scan path
#[derive(Clone, Copy)]
pub struct ScanOneQuery<T>(pub PhantomData<T>);

impl<'ctx, Ctx, T: 'static> Traversal<'ctx, Ctx> for ScanOneQuery<T>
where
    Ctx: TotalRelation<T, T> + 'ctx,
{
    type Src = T;
    type Target = T;
    type Output = &'ctx T;

    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<T>) -> Option<(Id<T>, &'ctx T)> {
        Some((start, ctx.get(start)))
    }
}

impl<T: 'static> Query<T, T, ScanOneQuery<T>> {
    /// Mutates the entire column in a single-pass contiguous loop.
    #[inline]
    pub fn update_direct<'ctx, Ctx, F>(self, ctx: &'ctx mut Ctx, mut f: F)
    where
        Ctx: TotalRelationMut<T, T> + 'ctx,
        F: FnMut(&mut T),
    {
        for (_id, val) in ctx.rows_mut() {
            f(val);
        }
    }
}

/// Plan step: Value filtering path
#[derive(Clone, Copy)]
pub struct FilterQuery<P, F> {
    pub(crate) plan: P,
    pub(crate) predicate: F,
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Filter target values
    #[inline]
    pub fn filter<F>(self, predicate: F) -> Query<Src, Target, FilterQuery<Plan, F>, State>
    where
        F: Fn(&Target) -> bool,
    {
        self.map(|plan| FilterQuery { plan, predicate })
    }
}

impl<'ctx, Ctx, P, F> Traversal<'ctx, Ctx> for FilterQuery<P, F>
where
    P: Traversal<'ctx, Ctx>,
    F: Fn(P::Output) -> bool,
{
    type Src = P::Src;
    type Target = P::Target;
    type Output = P::Output;

    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<Self::Target>, Self::Output)> {
        let (target_id, val) = self.plan.execute(ctx, start)?;
        if (self.predicate)(val) {
            Some((target_id, val))
        } else {
            None
        }
    }
}

impl<T: 'static, F> Query<T, T, FilterQuery<ScanOneQuery<T>, F>> {
    /// Mutates filtered values in a single-pass contiguous loop without allocating or repeated lookups.
    #[inline]
    pub fn update_direct<'ctx, Ctx, U>(self, ctx: &'ctx mut Ctx, mut f: U)
    where
        Ctx: TotalRelationMut<T, T> + 'ctx,
        F: Fn(&T) -> bool,
        U: FnMut(&mut T),
    {
        for (_id, val) in ctx.rows_mut() {
            let val_ref: &T = val;
            if (self.plan.predicate)(val_ref) {
                f(val);
            }
        }
    }
}

/// Plan step: Type casting path
#[derive(Clone, Copy)]
pub struct CastQuery<P, Target> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<Target>,
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Cast the current target type to a specific concrete variant/type
    #[inline]
    pub fn cast<U: 'static>(self) -> Query<Src, U, CastQuery<Plan, U>, State> {
        self.map(|plan| CastQuery {
            plan,
            _marker: PhantomData,
        })
    }
}

impl<'ctx, Ctx, P, Target> Traversal<'ctx, Ctx> for CastQuery<P, Target>
where
    P: Traversal<'ctx, Ctx>,
    P::Output: TryProject<Target>,
    Ctx: TotalRelation<Target, Target> + 'ctx,
    Target: 'static,
{
    type Src = P::Src;
    type Target = Target;
    type Output = &'ctx Target;

    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<(Id<Target>, &'ctx Target)> {
        let (_, mid) = self.plan.execute(ctx, start)?;
        let target_id = mid.try_project()?;
        Some((target_id, ctx.get(target_id)))
    }
}

/// Plan step: Field projection path
#[derive(Clone, Copy)]
pub struct ProjectQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Project a single target ID
    #[inline]
    pub fn project<U: 'static, F>(
        self,
        projector: F,
    ) -> Query<Src, U, ProjectQuery<Plan, U, F>, State>
    where
        F: Fn(&Target) -> Id<U>,
    {
        self.map(|plan| ProjectQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }
}

impl<'ctx, Ctx, P, Target, F> Traversal<'ctx, Ctx> for ProjectQuery<P, Target, F>
where
    P: Traversal<'ctx, Ctx>,
    F: Fn(P::Output) -> Id<Target>,
    Ctx: TotalRelation<Target, Target> + 'ctx,
    Target: 'static,
{
    type Src = P::Src;
    type Target = Target;
    type Output = &'ctx Target;

    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<(Id<Target>, &'ctx Target)> {
        let (_, mid) = self.plan.execute(ctx, start)?;
        let target_id = (self.projector)(mid);
        Some((target_id, ctx.get(target_id)))
    }
}

/// Plan step: Slice projection path
#[derive(Clone, Copy)]
pub struct ProjectSliceQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Project a slice of target IDs
    #[inline]
    pub fn project_slice<U: 'static, F>(
        self,
        projector: F,
    ) -> Query<Src, U, ProjectSliceQuery<Plan, U, F>, State>
    where
        F: Fn(&Target) -> SliceId<U>,
    {
        self.map(|plan| ProjectSliceQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }
}

impl<'ctx, Ctx, P, Target, F> SliceTraversal<'ctx, Ctx> for ProjectSliceQuery<P, Target, F>
where
    P: Traversal<'ctx, Ctx>,
    F: Fn(P::Output) -> SliceId<Target>,
    Target: 'static,
{
    type Src = P::Src;
    type Target = Target;

    #[inline]
    fn execute_slice(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<SliceId<Target>> {
        let (_, mid) = self.plan.execute(ctx, start)?;
        Some((self.projector)(mid))
    }
}

/// Plan step: Optional projection path
#[derive(Clone, Copy)]
pub struct ProjectOptQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Project an optional target ID
    #[inline]
    pub fn project_opt<U: 'static, F>(
        self,
        projector: F,
    ) -> Query<Src, U, ProjectOptQuery<Plan, U, F>, State>
    where
        F: Fn(&Target) -> Option<Id<U>>,
    {
        self.map(|plan| ProjectOptQuery {
            plan,
            projector,
            _marker: PhantomData,
        })
    }
}

impl<'ctx, Ctx, P, Target, F> Traversal<'ctx, Ctx> for ProjectOptQuery<P, Target, F>
where
    P: Traversal<'ctx, Ctx>,
    F: Fn(P::Output) -> Option<Id<Target>>,
    Ctx: TotalRelation<Target, Target> + 'ctx,
    Target: 'static,
{
    type Src = P::Src;
    type Target = Target;
    type Output = &'ctx Target;

    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<(Id<Target>, &'ctx Target)> {
        let (_, mid) = self.plan.execute(ctx, start)?;
        let target_id = (self.projector)(mid)?;
        Some((target_id, ctx.get(target_id)))
    }
}

/// Plan step: Join metadata
#[derive(Clone, Copy)]
pub struct WithQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Join metadata
    #[inline]
    pub fn with<M: 'static>(self) -> Query<Src, Target, WithQuery<Plan, M>, State> {
        self.map(|plan| WithQuery {
            plan,
            _marker: PhantomData,
        })
    }
}

impl<'ctx, Ctx, P, M: 'static> Traversal<'ctx, Ctx> for WithQuery<P, M>
where
    P: Traversal<'ctx, Ctx>,
    Ctx: PartialRelation<P::Target, M> + TotalRelation<P::Target, P::Target> + 'ctx,
{
    type Src = P::Src;
    type Target = P::Target;
    type Output = (P::Output, &'ctx M);

    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<Self::Target>, Self::Output)> {
        let (target_id, val) = self.plan.execute(ctx, start)?;
        let meta = ctx.get_opt(target_id)?;
        Some((target_id, (val, meta)))
    }
}

/// Plan step: Left join metadata
#[derive(Clone, Copy)]
pub struct LeftJoinQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Left join metadata
    #[inline]
    pub fn left_join<M: 'static>(self) -> Query<Src, Target, LeftJoinQuery<Plan, M>, State> {
        self.map(|plan| LeftJoinQuery {
            plan,
            _marker: PhantomData,
        })
    }
}

impl<'ctx, Ctx, P, M: 'static> Traversal<'ctx, Ctx> for LeftJoinQuery<P, M>
where
    P: Traversal<'ctx, Ctx>,
    Ctx: PartialRelation<P::Target, M> + TotalRelation<P::Target, P::Target> + 'ctx,
{
    type Src = P::Src;
    type Target = P::Target;
    type Output = (P::Output, Option<&'ctx M>);

    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<Self::Target>, Self::Output)> {
        let (target_id, val) = self.plan.execute(ctx, start)?;
        let meta = ctx.get_opt(target_id);
        Some((target_id, (val, meta)))
    }
}

/// Plan step: Anti-join metadata
#[derive(Clone, Copy)]
pub struct WithoutQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Anti-join metadata
    #[inline]
    pub fn without<M: 'static>(self) -> Query<Src, Target, WithoutQuery<Plan, M>, State> {
        self.map(|plan| WithoutQuery {
            plan,
            _marker: PhantomData,
        })
    }
}

impl<'ctx, Ctx, P, M: 'static> Traversal<'ctx, Ctx> for WithoutQuery<P, M>
where
    P: Traversal<'ctx, Ctx>,
    Ctx: PartialRelation<P::Target, M> + TotalRelation<P::Target, P::Target> + 'ctx,
{
    type Src = P::Src;
    type Target = P::Target;
    type Output = P::Output;

    #[inline]
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<Self::Target>, Self::Output)> {
        let (target_id, val) = self.plan.execute(ctx, start)?;
        if ctx.get_opt(target_id).is_none() {
            Some((target_id, val))
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

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    /// Monadic combinator: Narrow slice element by index
    #[inline]
    pub fn get_at<T: 'static>(self, index: usize) -> Query<Src, T, GetAt<Plan, T>, State> {
        self.map(|plan| GetAt {
            plan,
            index,
            _marker: PhantomData,
        })
    }
}

impl<'ctx, Ctx, P, T: 'static> Traversal<'ctx, Ctx> for GetAt<P, T>
where
    P: SliceTraversal<'ctx, Ctx, Target = T>,
    Ctx: SliceRelation<T> + TotalRelation<T, T> + 'ctx,
{
    type Src = P::Src;
    type Target = T;
    type Output = &'ctx T;

    #[inline]
    fn execute(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<(Id<T>, &'ctx T)> {
        let slice_id = self.plan.execute_slice(ctx, start)?;
        let ids = ctx.get_slice(slice_id);
        let id = ids[self.index];
        Some((id, ctx.get(id)))
    }
}
