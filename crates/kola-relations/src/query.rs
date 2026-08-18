use crate::TotalRelationMut;
use crate::id::Id;
use crate::relation::{PartialRelation, RelationIds, SliceRelation, TotalRelation};
use crate::slice::SliceId;
use std::marker::PhantomData;

/// Conversion trait for attempting to extract/project a child ID of a specific type.
/// Implemented by the compiler AST crate.
pub trait TryProject<Target: 'static> {
    fn try_project(&self) -> Option<Id<Target>>;
}

impl<T, Target: 'static> TryProject<Target> for &T
where
    T: TryProject<Target>,
{
    #[inline]
    fn try_project(&self) -> Option<Id<Target>> {
        (*self).try_project()
    }
}

/// Describes the physical traversal of a query path `Src` -> `Target` against an active context `'ctx`.
/// Leverages associated types to uniquely constrain source, target, and output types.
pub trait Traversal<'ctx, Ctx> {
    type Src: 'static;
    type Target: 'static;
    type Output: Copy + Clone + 'ctx;

    /// Eagerly executes the traversal starting at a specific ID by reference,
    /// yielding both the target ID and the output value.
    fn execute(
        &self,
        ctx: &'ctx Ctx,
        start: Id<Self::Src>,
    ) -> Option<(Id<Self::Target>, Self::Output)>;
}

/// Describes the physical traversal of a query path `Src` -> `SliceId<Target>` against an active context `'ctx`.
pub trait SliceTraversal<'ctx, Ctx> {
    type Src: 'static;
    type Target: 'static;

    /// Eagerly executes the traversal and returns the SliceId key directly.
    fn execute_slice(&self, ctx: &'ctx Ctx, start: Id<Self::Src>) -> Option<SliceId<Self::Target>>;
}

/// A stateless, context-free, 'static representation of a relational query.
/// It contains absolutely no data references, allowing it to compile flawlessly.
#[derive(Clone, Copy)]
pub struct Query<Src, Target, Plan, State = ()> {
    pub plan: Plan,
    pub state: State,
    pub(crate) _marker: PhantomData<(Src, Target)>,
}

/// Helper starter function for constructing a new relational query path
#[inline]
pub fn query<T: 'static>() -> Query<T, T, ScanOneQuery<T>> {
    Query {
        plan: ScanOneQuery(PhantomData),
        state: (),
        _marker: PhantomData,
    }
}

/// Helper starter function for constructing a new relational query path
#[inline]
pub fn query_id<T: 'static>(id: Id<T>) -> Query<T, T, ScanOneQuery<T>, Id<T>> {
    Query {
        plan: ScanOneQuery(PhantomData),
        state: id,
        _marker: PhantomData,
    }
}

// pub fn query_slice<T: 'static>(id: SliceId<T>) -> Query<T, T, ScanSliceQuery<T>, SliceId<T>> {
//     Query {
//         plan: ScanSliceQuery(PhantomData),
//         state: id,
//         _marker: PhantomData,
//     }
// }

/// Plan step: Initial single ID scan path
#[derive(Clone, Copy)]
pub struct ScanOneQuery<T>(pub PhantomData<T>);

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

/// Plan step: Field projection path
#[derive(Clone, Copy)]
pub struct ProjectQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

/// Plan step: Slice projection path
#[derive(Clone, Copy)]
pub struct ProjectSliceQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

/// Plan step: Optional projection path
#[derive(Clone, Copy)]
pub struct ProjectOptQuery<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

/// Plan step: Value filtering path
#[derive(Clone, Copy)]
pub struct FilterQuery<P, F> {
    pub(crate) plan: P,
    pub(crate) predicate: F,
}

/// Plan step: Join metadata
#[derive(Clone, Copy)]
pub struct WithQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

/// Plan step: Left join metadata
#[derive(Clone, Copy)]
pub struct LeftJoinQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

/// Plan step: Anti-join metadata
#[derive(Clone, Copy)]
pub struct WithoutQuery<P, M> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<M>,
}

/// Plan step: Narrow slice element by index
#[derive(Clone, Copy)]
pub struct GetAt<P, T> {
    pub(crate) plan: P,
    pub(crate) index: usize,
    pub(crate) _marker: PhantomData<T>,
}

impl<Src, Target, Plan> Query<Src, Target, Plan> {
    pub fn new(plan: Plan) -> Self {
        Self {
            plan,
            state: (),
            _marker: PhantomData,
        }
    }
}

impl<Src, Target, Plan, State> Query<Src, Target, Plan, State> {
    pub fn with_state(plan: Plan, state: State) -> Self {
        Self {
            plan,
            state,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn map<U, New>(self, f: impl FnOnce(Plan) -> New) -> Query<Src, U, New, State> {
        let Self {
            plan,
            state,
            _marker,
        } = self;

        let plan = f(plan);

        Query {
            plan,
            state,
            _marker: PhantomData,
        }
    }

    /// Monadic combinator: Cast the current target type to a specific concrete variant/type
    #[inline]
    pub fn cast<U: 'static>(self) -> Query<Src, U, CastQuery<Plan, U>, State> {
        self.map(|plan| CastQuery {
            plan,
            _marker: PhantomData,
        })
    }

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

    /// Monadic combinator: Filter target values
    #[inline]
    pub fn filter<F>(self, predicate: F) -> Query<Src, Target, FilterQuery<Plan, F>, State>
    where
        F: Fn(&Target) -> bool,
    {
        self.map(|plan| FilterQuery { plan, predicate })
    }

    /// Monadic combinator: Join metadata
    #[inline]
    pub fn with<M: 'static>(self) -> Query<Src, Target, WithQuery<Plan, M>, State> {
        self.map(|plan| WithQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Left join metadata
    #[inline]
    pub fn left_join<M: 'static>(self) -> Query<Src, Target, LeftJoinQuery<Plan, M>, State> {
        self.map(|plan| LeftJoinQuery {
            plan,
            _marker: PhantomData,
        })
    }

    /// Monadic combinator: Anti-join metadata
    #[inline]
    pub fn without<M: 'static>(self) -> Query<Src, Target, WithoutQuery<Plan, M>, State> {
        self.map(|plan| WithoutQuery {
            plan,
            _marker: PhantomData,
        })
    }

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

// Implementation for ScanOneQuery (Start at a single ID)
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

// Implementation for CastQuery
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

// Implementation for ProjectQuery
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

// Implementation for ProjectSliceQuery (Implements SliceTraversal)
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

// Implementation of GetAt (Consumes SliceTraversal, narrows back to Traversal!)
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

// Implementation for ProjectOptQuery
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

// Implementation for FilterQuery
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

// Implementation for WithQuery
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

// Implementation for LeftJoinQuery
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

// Implementation for WithoutQuery
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

impl<Src: 'static, Target, Plan> Query<Src, Target, Plan, Id<Src>> {
    /// Eager loader: Executes the traversal starting from a single ID and returns the single target.
    #[inline]
    pub fn get_one<'ctx, Ctx>(self, ctx: &'ctx Ctx) -> Plan::Output
    where
        Plan: Traversal<'ctx, Ctx, Src = Src, Target = Target>,
    {
        self.plan
            .execute(ctx, self.state)
            .map(|(_, val)| val)
            .expect("get_one failed")
    }

    /// Eager loader: Executes the traversal starting from a single ID, returning an Option.
    #[inline]
    pub fn get_opt<'ctx, Ctx>(self, ctx: &'ctx Ctx) -> Option<Plan::Output>
    where
        Plan: Traversal<'ctx, Ctx, Src = Src, Target = Target>,
    {
        self.plan.execute(ctx, self.state).map(|(_, val)| val)
    }

    /// Eager loader: Executes the traversal starting from a single ID, returning the SliceId.
    #[inline]
    pub fn get_slice<'ctx, Ctx>(self, ctx: &'ctx Ctx) -> SliceId<Target>
    where
        Plan: SliceTraversal<'ctx, Ctx, Src = Src, Target = Target>,
    {
        self.plan
            .execute_slice(ctx, self.state)
            .expect("get_slice failed")
    }

    /// Eager loader: Executes the traversal starting from a single ID, returning Option<SliceId>.
    #[inline]
    pub fn get_slice_opt<'ctx, Ctx>(self, ctx: &'ctx Ctx) -> Option<SliceId<Target>>
    where
        Plan: SliceTraversal<'ctx, Ctx, Src = Src, Target = Target>,
    {
        self.plan.execute_slice(ctx, self.state)
    }
}

impl<Src: 'static, Target, Plan> Query<Src, Target, Plan> {
    /// Eager loader: Executes the traversal starting from a single ID and returns the single target.
    #[inline]
    pub fn get_one<'ctx, Ctx>(self, ctx: &'ctx Ctx, start: Id<Src>) -> Plan::Output
    where
        Plan: Traversal<'ctx, Ctx, Src = Src, Target = Target>,
    {
        self.plan
            .execute(ctx, start)
            .map(|(_, val)| val)
            .expect("get_one failed")
    }

    /// Eager loader: Executes the traversal starting from a single ID, returning an Option.
    #[inline]
    pub fn get_opt<'ctx, Ctx>(self, ctx: &'ctx Ctx, start: Id<Src>) -> Option<Plan::Output>
    where
        Plan: Traversal<'ctx, Ctx, Src = Src, Target = Target>,
    {
        self.plan.execute(ctx, start).map(|(_, val)| val)
    }

    /// Eager loader: Executes the traversal starting from a slice of IDs and yields a lazy iterator of targets.
    /// This is 100% zero-allocation and extremely high-performance!
    #[inline]
    pub fn get_slice_items<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
        start_slice: SliceId<Src>,
    ) -> impl Iterator<Item = Plan::Output> + 'ctx
    where
        Plan: Traversal<'ctx, Ctx, Src = Src, Target = Target> + 'ctx,
        Ctx: SliceRelation<Src> + 'ctx,
    {
        ctx.get_slice(start_slice)
            .iter()
            .filter_map(move |&id| self.plan.execute(ctx, id).map(|(_, val)| val))
    }

    /// Eager loader: Executes the traversal starting from all IDs inside the context's relation.
    /// This is used to query/scan the entire relation/table dynamically!
    #[inline]
    pub fn get_row<'ctx, Ctx>(
        self,
        ctx: &'ctx Ctx,
    ) -> impl Iterator<Item = (Id<Target>, Plan::Output)> + 'ctx
    where
        Plan: Traversal<'ctx, Ctx, Src = Src, Target = Target> + 'ctx,
        Ctx: RelationIds<Src> + 'ctx,
    {
        Ctx::ids(ctx).filter_map(move |id| {
            let (target_id, output) = self.plan.execute(ctx, id)?;
            Some((target_id, output))
        })
    }
}

// Plural Loaders (when Plan implements SliceTraversal)
impl<Src: 'static, Target: 'static, Plan> Query<Src, Target, Plan> {
    /// Eager loader: Executes the traversal starting from a single ID, returning the SliceId.
    #[inline]
    pub fn get_slice<'ctx, Ctx>(self, ctx: &'ctx Ctx, start: Id<Src>) -> SliceId<Target>
    where
        Plan: SliceTraversal<'ctx, Ctx, Src = Src, Target = Target>,
    {
        self.plan
            .execute_slice(ctx, start)
            .expect("get_slice failed")
    }

    /// Eager loader: Executes the traversal starting from a single ID, returning Option<SliceId>.
    #[inline]
    pub fn get_slice_opt<'ctx, Ctx>(self, ctx: &'ctx Ctx, start: Id<Src>) -> Option<SliceId<Target>>
    where
        Plan: SliceTraversal<'ctx, Ctx, Src = Src, Target = Target>,
    {
        self.plan.execute_slice(ctx, start)
    }
}

impl<Src: 'static, Target: 'static, Plan> Query<Src, Target, Plan> {
    /// Eager mutator: Scans the relation, runs the path to find matching target elements,
    /// and mutates them in-place using a mutable closure.
    pub fn update<'ctx, Ctx, F>(self, ctx: &'ctx mut Ctx, mut f: F)
    where
        for<'a> Plan: Traversal<'a, Ctx, Src = Src, Target = Target>,
        Ctx: RelationIds<Src> + TotalRelationMut<Target, Target> + 'ctx,
        F: FnMut(&mut Target),
    {
        // Phase 1: Retrieve matching target IDs using a temporary immutable borrow of ctx
        let ids: Vec<Id<Target>> = {
            let ctx_ref: &Ctx = ctx;
            <Ctx as RelationIds<Src>>::ids(ctx_ref)
                .filter_map(|id| {
                    self.plan
                        .execute(ctx_ref, id)
                        .map(|(target_id, _)| target_id)
                })
                .collect()
        };

        // Phase 2: Perform the mutations mutably on each target ID
        for id in ids {
            let val = ctx.get_mut(id);
            f(val);
        }
    }
}
