use crate::{
    id::Id,
    relation::{PartialRelation, SliceRelation, TotalRelation, TotalRelationMut},
    slice::SliceId,
};
use std::borrow::Borrow;
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

/// Helper trait to safely convert GAT value types to their static counterpart when they are `'static`.
pub trait IsStaticValue {
    type StaticVal: 'static;
    fn into_static(self) -> Self::StaticVal;
}

impl<T: 'static> IsStaticValue for T {
    type StaticVal = T;

    #[inline]
    fn into_static(self) -> T {
        self
    }
}

/// A context-free representation of a relational query plan.
pub trait QueryPlan: Sized {
    type Key: 'static;
    type Value<'a>: Copy + Clone
    where
        Self: 'a;
}

/// The core Relation Query trait that represents a symbolic query step with GAT.
pub trait RelationQuery<Ctx>: QueryPlan {
    fn execute<'a>(
        self,
        ctx: &'a Ctx,
    ) -> impl Iterator<Item = (Id<Self::Key>, Self::Value<'a>)> + 'a
    where
        Ctx: 'a,
        Self: 'a;
}

/// The main lifetime-free, zero-cost query builder.
#[derive(Clone, Copy)]
pub struct KeyedQuery<P> {
    pub(crate) plan: P,
}

impl<P: QueryPlan> KeyedQuery<P> {
    #[inline]
    pub fn filter<F>(self, predicate: F) -> KeyedQuery<Filter<P, F>>
    where
        F: for<'b> Fn(P::Value<'b>) -> bool,
    {
        KeyedQuery {
            plan: Filter {
                plan: self.plan,
                predicate,
            },
        }
    }

    #[inline]
    pub fn filter_keyed<F>(self, predicate: F) -> KeyedQuery<FilterKeyed<P, F>>
    where
        F: for<'b> Fn(Id<P::Key>, P::Value<'b>) -> bool,
    {
        KeyedQuery {
            plan: FilterKeyed {
                plan: self.plan,
                predicate,
            },
        }
    }

    #[inline]
    pub fn with<M: 'static>(self) -> KeyedQuery<With<P, M>> {
        KeyedQuery {
            plan: With {
                plan: self.plan,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn left_join<M: 'static>(self) -> KeyedQuery<LeftJoin<P, M>> {
        KeyedQuery {
            plan: LeftJoin {
                plan: self.plan,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn without<M: 'static>(self) -> KeyedQuery<Without<P, M>> {
        KeyedQuery {
            plan: Without {
                plan: self.plan,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn cast<Target: 'static>(self) -> KeyedQuery<Cast<P, Target>> {
        KeyedQuery {
            plan: Cast {
                plan: self.plan,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn project<Target: 'static, F>(self, projector: F) -> KeyedQuery<Project<P, Target, F>>
    where
        F: Fn(P::Value<'_>) -> Id<Target> + Clone + Copy,
    {
        KeyedQuery {
            plan: Project {
                plan: self.plan,
                projector,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn project_slice<Target: 'static, F>(
        self,
        projector: F,
    ) -> KeyedQuery<ProjectSlice<P, Target, F>>
    where
        F: Fn(P::Value<'_>) -> SliceId<Target> + Clone + Copy,
    {
        KeyedQuery {
            plan: ProjectSlice {
                plan: self.plan,
                projector,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn project_opt<Target: 'static, F>(
        self,
        projector: F,
    ) -> KeyedQuery<ProjectOpt<P, Target, F>>
    where
        F: Fn(P::Value<'_>) -> Option<Id<Target>> + Clone + Copy,
    {
        KeyedQuery {
            plan: ProjectOpt {
                plan: self.plan,
                projector,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn inspect<I>(self, inspector: I) -> KeyedQuery<Inspect<P, I>>
    where
        I: for<'b> Inspector<P::Key, P::Value<'b>>,
    {
        KeyedQuery {
            plan: Inspect {
                plan: self.plan,
                inspector,
            },
        }
    }

    #[inline]
    pub fn inspect_key<F>(self, inspector: F) -> KeyedQuery<InspectKey<P, F>>
    where
        F: FnMut(Id<P::Key>),
    {
        KeyedQuery {
            plan: InspectKey {
                plan: self.plan,
                inspector,
            },
        }
    }

    #[inline]
    pub fn inspect_value<F>(self, inspector: F) -> KeyedQuery<InspectValue<P, F>>
    where
        F: FnMut(P::Value<'_>),
    {
        KeyedQuery {
            plan: InspectValue {
                plan: self.plan,
                inspector,
            },
        }
    }

    #[inline]
    pub fn get_at<T: 'static>(self, index: usize) -> KeyedQuery<GetAt<P, T>> {
        KeyedQuery {
            plan: GetAt {
                plan: self.plan,
                index,
                _marker: PhantomData,
            },
        }
    }

    /// Executes the query plan exactly once against the database context, materializes
    /// its results, and passes a high-performance Scan query over those cached/materialized
    /// results to a branching closure. This completely prevents redundant parent traversals.
    #[inline]
    pub fn branch<'a, Ctx, F>(self, ctx: &'a Ctx, mut f: F)
    where
        P: RelationQuery<Ctx> + 'a,
        Ctx: 'a,
        F: for<'b> FnMut(KeyedQuery<ScanItems<'b, P::Key, P::Value<'a>>>),
    {
        let results: Vec<(Id<P::Key>, P::Value<'a>)> = self.execute(ctx).collect();
        let scan = query_items(&results);
        f(scan);
    }

    /// Execute the query plan against the database context, yielding an iterator of results.
    #[inline]
    pub fn execute<'a, Ctx>(
        self,
        ctx: &'a Ctx,
    ) -> impl Iterator<Item = (Id<P::Key>, P::Value<'a>)> + 'a
    where
        P: RelationQuery<Ctx> + 'a,
        Ctx: 'a,
    {
        self.plan.execute(ctx)
    }

    /// Gather matching IDs first, then run a mutable closure on each of them.
    pub fn update<Ctx, V: 'static, F>(self, ctx: &mut Ctx, mut f: F)
    where
        P: RelationQuery<Ctx>,
        Ctx: TotalRelationMut<P::Key, V>,
        F: FnMut(&mut V),
    {
        let ids: Vec<Id<P::Key>> = self.plan.execute(ctx).map(|(id, _)| id).collect();

        for id in ids {
            let val = ctx.get_mut(id);
            f(val);
        }
    }
}

// ==========================================
// Fast-path contiguous non-allocating updates
// ==========================================

impl<K: 'static, V: 'static> KeyedQuery<Scan<K, V>> {
    /// Mutates the entire column in a single-pass contiguous loop.
    #[inline]
    pub fn update_direct<Ctx, F>(self, ctx: &mut Ctx, mut f: F)
    where
        Ctx: TotalRelationMut<K, V>,
        F: FnMut(&mut V),
    {
        for (_id, val) in ctx.rows_mut() {
            f(val);
        }
    }
}

impl<K: 'static, V: 'static, F> KeyedQuery<Filter<Scan<K, V>, F>> {
    /// Mutates filtered values in a single-pass contiguous loop without allocating or repeated lookups.
    #[inline]
    pub fn update_direct<Ctx, U>(self, ctx: &mut Ctx, mut f: U)
    where
        Ctx: TotalRelationMut<K, V>,
        F: Fn(&V) -> bool,
        U: FnMut(&mut V),
    {
        for (_id, val) in ctx.rows_mut() {
            let val_ref: &V = val;
            if (self.plan.predicate)(val_ref) {
                f(val);
            }
        }
    }
}

// ==========================================
// Starter functions
// ==========================================

// Starter function for scanning a dense AST relation (T -> T)
#[inline]
pub fn query<T: 'static>() -> KeyedQuery<Scan<T, T>> {
    KeyedQuery {
        plan: Scan(PhantomData),
    }
}

// Starter function for scanning a general relation (K -> V)
#[inline]
pub fn query_relation<K: 'static, V: 'static>() -> KeyedQuery<Scan<K, V>> {
    KeyedQuery {
        plan: Scan(PhantomData),
    }
}

// Starter function for querying a single ID in a dense AST relation
#[inline]
pub fn query_one<T: 'static>(id: Id<T>) -> KeyedQuery<ScanOne<T, T>> {
    KeyedQuery {
        plan: ScanOne(id, PhantomData),
    }
}

// Starter function for querying a single ID (for assertions/testing)
#[inline]
pub fn query_one_relation<K: 'static, V: 'static>(id: Id<K>) -> KeyedQuery<ScanOne<K, V>> {
    KeyedQuery {
        plan: ScanOne(id, PhantomData),
    }
}

// Starter function for scanning a slice of IDs
#[inline]
pub fn query_slice<T: 'static>(slice_id: SliceId<T>) -> KeyedQuery<ScanSlice<T>> {
    KeyedQuery {
        plan: ScanSlice { slice: slice_id },
    }
}

// Starter function for scanning an in-memory materialized slice of results
#[inline]
pub fn query_items<'a, K, V>(items: &'a [(Id<K>, V)]) -> KeyedQuery<ScanItems<'a, K, V>> {
    KeyedQuery {
        plan: ScanItems { items },
    }
}

// ==========================================
// Symbolic Query Plan Steps
// ==========================================

// Implementation of Scan
pub struct Scan<K, V>(pub PhantomData<(K, V)>);

impl<K: 'static, V: 'static> QueryPlan for Scan<K, V> {
    type Key = K;
    type Value<'a>
        = &'a V
    where
        Self: 'a;
}

impl<Ctx, K: 'static, V: 'static> RelationQuery<Ctx> for Scan<K, V>
where
    Ctx: TotalRelation<K, V>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<K>, &'a V)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        ctx.rows()
    }
}

impl<K, V> Clone for Scan<K, V> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<K, V> Copy for Scan<K, V> {}

// Implementation of ScanOne
pub struct ScanOne<K, V>(pub Id<K>, pub PhantomData<V>);

impl<K: 'static, V: 'static> QueryPlan for ScanOne<K, V> {
    type Key = K;
    type Value<'a>
        = &'a V
    where
        Self: 'a;
}

impl<Ctx, K: 'static, V: 'static> RelationQuery<Ctx> for ScanOne<K, V>
where
    Ctx: TotalRelation<K, V>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<K>, &'a V)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        std::iter::once((self.0, ctx.get(self.0)))
    }
}

impl<K, V> Clone for ScanOne<K, V> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<K, V> Copy for ScanOne<K, V> {}

// Implementation of ScanSlice
#[derive(Clone, Copy)]
pub struct ScanSlice<T> {
    slice: SliceId<T>,
}

impl<T: 'static> QueryPlan for ScanSlice<T> {
    type Key = T;
    type Value<'a>
        = &'a T
    where
        Self: 'a;
}

impl<Ctx, T: 'static> RelationQuery<Ctx> for ScanSlice<T>
where
    Ctx: SliceRelation<T> + TotalRelation<T, T>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<T>, &'a T)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        ctx.get_slice(self.slice)
            .iter()
            .map(move |&id| (id, ctx.get(id)))
    }
}

// Implementation of ScanItems
pub struct ScanItems<'a, K, V> {
    pub(crate) items: &'a [(Id<K>, V)],
}

impl<'a, K, V> Clone for ScanItems<'a, K, V> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, K, V> Copy for ScanItems<'a, K, V> {}

impl<'a, K: 'static, V: Clone + Copy + 'static> QueryPlan for ScanItems<'a, K, V> {
    type Key = K;
    type Value<'b>
        = V
    where
        Self: 'b;
}

impl<'a, Ctx, K: 'static, V: Clone + Copy + 'static> RelationQuery<Ctx> for ScanItems<'a, K, V> {
    #[inline]
    fn execute<'b>(self, _ctx: &'b Ctx) -> impl Iterator<Item = (Id<K>, Self::Value<'b>)> + 'b
    where
        Ctx: 'b,
        Self: 'b,
    {
        self.items.iter().cloned()
    }
}

// Implementation of Filter (Values only)
#[derive(Clone, Copy)]
pub struct Filter<P, F> {
    pub(crate) plan: P,
    pub(crate) predicate: F,
}

impl<P: QueryPlan, F> QueryPlan for Filter<P, F> {
    type Key = P::Key;
    type Value<'a>
        = P::Value<'a>
    where
        Self: 'a;
}

impl<Ctx, P, F> RelationQuery<Ctx> for Filter<P, F>
where
    P: RelationQuery<Ctx>,
    F: Fn(P::Value<'_>) -> bool,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value<'a>)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan
            .execute(ctx)
            .filter(move |(_, val)| (self.predicate)(*val))
    }
}

// Implementation of FilterKeyed (IDs and Values)
#[derive(Clone, Copy)]
pub struct FilterKeyed<P, F> {
    pub(crate) plan: P,
    pub(crate) predicate: F,
}

impl<P: QueryPlan, F> QueryPlan for FilterKeyed<P, F> {
    type Key = P::Key;
    type Value<'a>
        = P::Value<'a>
    where
        Self: 'a;
}

impl<Ctx, P, F> RelationQuery<Ctx> for FilterKeyed<P, F>
where
    P: RelationQuery<Ctx>,
    F: Fn(Id<P::Key>, P::Value<'_>) -> bool,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value<'a>)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan
            .execute(ctx)
            .filter(move |(id, val)| (self.predicate)(*id, *val))
    }
}

// Implementation of With
pub struct With<P, M> {
    plan: P,
    _marker: PhantomData<M>,
}

impl<P: QueryPlan, M: 'static> QueryPlan for With<P, M> {
    type Key = P::Key;
    type Value<'a>
        = (P::Value<'a>, &'a M)
    where
        Self: 'a;
}

impl<Ctx, P, M: 'static> RelationQuery<Ctx> for With<P, M>
where
    P: RelationQuery<Ctx>,
    Ctx: PartialRelation<P::Key, M>,
{
    #[inline]
    fn execute<'a>(
        self,
        ctx: &'a Ctx,
    ) -> impl Iterator<Item = (Id<P::Key>, (P::Value<'a>, &'a M))> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan
            .execute(ctx)
            .filter_map(move |(id, val)| ctx.get_opt(id).map(|meta| (id, (val, meta))))
    }
}

impl<P: Clone, M> Clone for With<P, M> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Copy, M> Copy for With<P, M> {}

// Implementation of LeftJoin
pub struct LeftJoin<P, M> {
    plan: P,
    _marker: PhantomData<M>,
}

impl<P: QueryPlan, M: 'static> QueryPlan for LeftJoin<P, M> {
    type Key = P::Key;
    type Value<'a>
        = (P::Value<'a>, Option<&'a M>)
    where
        Self: 'a;
}

impl<Ctx, P, M: 'static> RelationQuery<Ctx> for LeftJoin<P, M>
where
    P: RelationQuery<Ctx>,
    Ctx: PartialRelation<P::Key, M>,
{
    #[inline]
    fn execute<'a>(
        self,
        ctx: &'a Ctx,
    ) -> impl Iterator<Item = (Id<P::Key>, (P::Value<'a>, Option<&'a M>))> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan.execute(ctx).map(move |(id, val)| {
            let meta = ctx.get_opt(id);
            (id, (val, meta))
        })
    }
}

impl<P: Clone, M> Clone for LeftJoin<P, M> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Copy, M> Copy for LeftJoin<P, M> {}

// Implementation of Without
pub struct Without<P, M> {
    plan: P,
    _marker: PhantomData<M>,
}

impl<P: QueryPlan, M: 'static> QueryPlan for Without<P, M> {
    type Key = P::Key;
    type Value<'a>
        = P::Value<'a>
    where
        Self: 'a;
}

impl<Ctx, P, M: 'static> RelationQuery<Ctx> for Without<P, M>
where
    P: RelationQuery<Ctx>,
    Ctx: PartialRelation<P::Key, M>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value<'a>)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan
            .execute(ctx)
            .filter(move |(id, _)| ctx.get_opt(*id).is_none())
    }
}

impl<P: Clone, M> Clone for Without<P, M> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Copy, M> Copy for Without<P, M> {}

// Implementation of Cast
pub struct Cast<P, Target> {
    plan: P,
    _marker: PhantomData<Target>,
}

impl<P: QueryPlan, Target: 'static> QueryPlan for Cast<P, Target> {
    type Key = Target;
    type Value<'a>
        = &'a Target
    where
        Self: 'a;
}

impl<Ctx, P, Target: 'static> RelationQuery<Ctx> for Cast<P, Target>
where
    P: RelationQuery<Ctx>,
    for<'b> P::Value<'b>: TryProject<Target>,
    Ctx: TotalRelation<Target, Target>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<Target>, &'a Target)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan.execute(ctx).filter_map(move |(_, val)| {
            val.try_project()
                .map(|target_id| (target_id, ctx.get(target_id)))
        })
    }
}

impl<P: Clone, Target> Clone for Cast<P, Target> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Copy, Target> Copy for Cast<P, Target> {}

// Implementation of Project
pub struct Project<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: QueryPlan, Target: 'static, F> QueryPlan for Project<P, Target, F> {
    type Key = Target;
    type Value<'a>
        = &'a Target
    where
        Self: 'a;
}

impl<Ctx, P, Target: 'static, F> RelationQuery<Ctx> for Project<P, Target, F>
where
    P: RelationQuery<Ctx>,
    F: Fn(P::Value<'_>) -> Id<Target> + Clone + Copy,
    Ctx: TotalRelation<Target, Target>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<Target>, &'a Target)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan.execute(ctx).map(move |(_, val)| {
            let target_id = (self.projector)(val);
            (target_id, ctx.get(target_id))
        })
    }
}

impl<P: Clone, Target, F: Clone> Clone for Project<P, Target, F> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            projector: self.projector.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Copy, Target, F: Copy> Copy for Project<P, Target, F> {}

// Implementation of ProjectSlice
pub struct ProjectSlice<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: QueryPlan, Target: 'static, F> QueryPlan for ProjectSlice<P, Target, F> {
    type Key = P::Key;
    type Value<'a>
        = SliceId<Target>
    where
        Self: 'a;
}

impl<Ctx, P, Target: 'static, F> RelationQuery<Ctx> for ProjectSlice<P, Target, F>
where
    P: RelationQuery<Ctx>,
    F: Fn(P::Value<'_>) -> SliceId<Target> + Clone + Copy,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<P::Key>, SliceId<Target>)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan.execute(ctx).map(move |(id, val)| {
            let slice_id = (self.projector)(val);
            (id, slice_id)
        })
    }
}

impl<P: Clone, Target, F: Clone> Clone for ProjectSlice<P, Target, F> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            projector: self.projector.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Copy, Target, F: Copy> Copy for ProjectSlice<P, Target, F> {}

// Implementation of ProjectOpt
pub struct ProjectOpt<P, Target, F> {
    pub(crate) plan: P,
    pub(crate) projector: F,
    pub(crate) _marker: PhantomData<Target>,
}

impl<P: QueryPlan, Target: 'static, F> QueryPlan for ProjectOpt<P, Target, F> {
    type Key = Target;
    type Value<'a>
        = &'a Target
    where
        Self: 'a;
}

impl<Ctx, P, Target: 'static, F> RelationQuery<Ctx> for ProjectOpt<P, Target, F>
where
    P: RelationQuery<Ctx>,
    F: Fn(P::Value<'_>) -> Option<Id<Target>> + Clone + Copy,
    Ctx: TotalRelation<Target, Target>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<Target>, &'a Target)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan.execute(ctx).filter_map(move |(_, val)| {
            let target_id = (self.projector)(val)?;
            Some((target_id, ctx.get(target_id)))
        })
    }
}

impl<P: Clone, Target, F: Clone> Clone for ProjectOpt<P, Target, F> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            projector: self.projector.clone(),
            _marker: PhantomData,
        }
    }
}

impl<P: Copy, Target, F: Copy> Copy for ProjectOpt<P, Target, F> {}

// Implementation of Inspect
#[derive(Clone, Copy)]
pub struct Inspect<P, I> {
    pub(crate) plan: P,
    pub(crate) inspector: I,
}

impl<P: QueryPlan, I> QueryPlan for Inspect<P, I> {
    type Key = P::Key;
    type Value<'a>
        = P::Value<'a>
    where
        Self: 'a;
}

impl<Ctx, P, I> RelationQuery<Ctx> for Inspect<P, I>
where
    P: RelationQuery<Ctx>,
    I: for<'b> Inspector<P::Key, P::Value<'b>>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value<'a>)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        let mut inspector = self.inspector;
        self.plan.execute(ctx).map(move |(id, val)| {
            inspector.inspect(id, val);
            (id, val)
        })
    }
}

// Implementation of InspectKey
#[derive(Clone, Copy)]
pub struct InspectKey<P, F> {
    pub(crate) plan: P,
    pub(crate) inspector: F,
}

impl<P: QueryPlan, F> QueryPlan for InspectKey<P, F> {
    type Key = P::Key;
    type Value<'a>
        = P::Value<'a>
    where
        Self: 'a;
}

impl<Ctx, P, F> RelationQuery<Ctx> for InspectKey<P, F>
where
    P: RelationQuery<Ctx>,
    F: FnMut(Id<P::Key>),
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value<'a>)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        let mut inspector = self.inspector;
        self.plan.execute(ctx).map(move |(id, val)| {
            inspector(id);
            (id, val)
        })
    }
}

// Implementation of InspectValue
#[derive(Clone, Copy)]
pub struct InspectValue<P, F> {
    pub(crate) plan: P,
    pub(crate) inspector: F,
}

impl<P: QueryPlan, F> QueryPlan for InspectValue<P, F> {
    type Key = P::Key;
    type Value<'a>
        = P::Value<'a>
    where
        Self: 'a;
}

impl<Ctx, P, F> RelationQuery<Ctx> for InspectValue<P, F>
where
    P: RelationQuery<Ctx>,
    F: for<'b> FnMut(P::Value<'b>),
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value<'a>)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        let mut inspector = self.inspector;
        self.plan.execute(ctx).map(move |(id, val)| {
            inspector(val);
            (id, val)
        })
    }
}

// Implementation of GetAt
pub struct GetAt<P, T> {
    pub(crate) plan: P,
    pub(crate) index: usize,
    pub(crate) _marker: PhantomData<T>,
}

impl<P: QueryPlan, T: 'static> QueryPlan for GetAt<P, T> {
    type Key = T;
    type Value<'a>
        = &'a T
    where
        Self: 'a,
        T: 'a;
}

impl<Ctx, P, T: 'static> RelationQuery<Ctx> for GetAt<P, T>
where
    P: RelationQuery<Ctx>,
    for<'b> P::Value<'b>: std::borrow::Borrow<SliceId<T>>,
    Ctx: SliceRelation<T> + TotalRelation<T, T>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<T>, &'a T)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan.execute(ctx).map(move |(_, val)| {
            let slice_id = *val.borrow();
            let ids = ctx.get_slice(slice_id);
            let id = ids[self.index];
            let item = ctx.get(id);
            (id, item)
        })
    }
}

impl<P: Clone, T> Clone for GetAt<P, T> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            index: self.index,
            _marker: PhantomData,
        }
    }
}

impl<P: Copy, T> Copy for GetAt<P, T> {}

// Generic Inspector trait for assertions
pub trait Inspector<K, V> {
    fn inspect(&mut self, id: Id<K>, value: V);
}

impl<K, V, F> Inspector<K, V> for F
where
    F: FnMut(Id<K>, V),
{
    #[inline]
    fn inspect(&mut self, id: Id<K>, value: V) {
        self(id, value);
    }
}
