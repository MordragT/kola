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

/// A context-free representation of a relational query plan.
pub trait QueryPlan<'ctx>: Sized {
    type Key: 'static;
    type Value: Copy + Clone + 'ctx;
}

/// The core Relation Query trait that represents a symbolic query step.
pub trait RelationQuery<'ctx, Ctx>: QueryPlan<'ctx> {
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<Self::Key>, Self::Value)> + 'ctx;
}

/// The main lifetime-free, zero-cost query builder.
pub struct KeyedQuery<'ctx, P> {
    pub(crate) plan: P,
    pub(crate) _marker: PhantomData<&'ctx ()>,
}

impl<'ctx, P> Clone for KeyedQuery<'ctx, P>
where
    P: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        Self {
            plan: self.plan.clone(),
            _marker: PhantomData,
        }
    }
}

impl<'ctx, P> Copy for KeyedQuery<'ctx, P> where P: Copy {}

impl<'ctx, P> KeyedQuery<'ctx, P> {
    #[inline]
    pub fn filter<F>(self, predicate: F) -> KeyedQuery<'ctx, Filter<P, F>>
    where
        P: QueryPlan<'ctx>,
        F: Fn(P::Value) -> bool,
    {
        KeyedQuery {
            plan: Filter {
                plan: self.plan,
                predicate,
            },
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn filter_keyed<F>(self, predicate: F) -> KeyedQuery<'ctx, FilterKeyed<P, F>>
    where
        P: QueryPlan<'ctx>,
        F: Fn(Id<P::Key>, P::Value) -> bool,
    {
        KeyedQuery {
            plan: FilterKeyed {
                plan: self.plan,
                predicate,
            },
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn with<M: 'static>(self) -> KeyedQuery<'ctx, With<P, M>> {
        KeyedQuery {
            plan: With {
                plan: self.plan,
                _marker: PhantomData,
            },
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn left_join<M: 'static>(self) -> KeyedQuery<'ctx, LeftJoin<P, M>> {
        KeyedQuery {
            plan: LeftJoin {
                plan: self.plan,
                _marker: PhantomData,
            },
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn without<M: 'static>(self) -> KeyedQuery<'ctx, Without<P, M>> {
        KeyedQuery {
            plan: Without {
                plan: self.plan,
                _marker: PhantomData,
            },
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn cast<Target: 'static>(self) -> KeyedQuery<'ctx, Cast<P, Target>> {
        KeyedQuery {
            plan: Cast {
                plan: self.plan,
                _marker: PhantomData,
            },
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn project<Target: 'static, F>(
        self,
        projector: F,
    ) -> KeyedQuery<'ctx, Project<P, Target, F>>
    where
        P: QueryPlan<'ctx>,
        F: Fn(P::Value) -> Id<Target> + Clone + Copy,
    {
        KeyedQuery {
            plan: Project {
                plan: self.plan,
                projector,
                _marker: PhantomData,
            },
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn project_slice<Target: 'static, F>(
        self,
        projector: F,
    ) -> KeyedQuery<'ctx, ProjectSlice<P, Target, F>>
    where
        P: QueryPlan<'ctx>,
        F: Fn(P::Value) -> SliceId<Target> + Clone + Copy,
    {
        KeyedQuery {
            plan: ProjectSlice {
                plan: self.plan,
                projector,
                _marker: PhantomData,
            },
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn project_opt<Target: 'static, F>(
        self,
        projector: F,
    ) -> KeyedQuery<'ctx, ProjectOpt<P, Target, F>>
    where
        P: QueryPlan<'ctx>,
        F: Fn(P::Value) -> Option<Id<Target>> + Clone + Copy,
    {
        KeyedQuery {
            plan: ProjectOpt {
                plan: self.plan,
                projector,
                _marker: PhantomData,
            },
            _marker: PhantomData,
        }
    }

    /// Eagerly executes the query and runs the closure on every yielded ID and value.
    /// This is the preferred way to run assertions, as it allows the closure to
    /// cleanly capture local stack variables without running into higher-ranked lifetime errors.
    #[inline]
    pub fn each<Ctx, F>(self, ctx: &'ctx Ctx, mut f: F)
    where
        P: RelationQuery<'ctx, Ctx>,
        Ctx: 'ctx,
        F: FnMut(Id<P::Key>, P::Value),
    {
        for (id, val) in self.execute(ctx) {
            f(id, val);
        }
    }

    /// Eagerly executes the query and runs the closure on every yielded value.
    #[inline]
    pub fn each_value<Ctx, F>(self, ctx: &'ctx Ctx, mut f: F)
    where
        P: RelationQuery<'ctx, Ctx>,
        Ctx: 'ctx,
        F: FnMut(P::Value),
    {
        for (_, val) in self.execute(ctx) {
            f(val);
        }
    }

    #[inline]
    pub fn get_at<T: 'static>(self, index: usize) -> KeyedQuery<'ctx, GetAt<P, T>>
    where
        P: QueryPlan<'ctx>,
        P::Value: std::borrow::Borrow<SliceId<T>>,
    {
        KeyedQuery {
            plan: GetAt {
                plan: self.plan,
                index,
                _marker: PhantomData,
            },
            _marker: PhantomData,
        }
    }

    /// Executes the query plan exactly once against the database context, materializes
    /// its results, and passes a high-performance Scan query over those cached/materialized
    /// results to a branching closure. This completely prevents redundant parent traversals.
    #[inline]
    pub fn branch<Ctx, F>(self, ctx: &'ctx Ctx, mut f: F)
    where
        P: RelationQuery<'ctx, Ctx>,
        Ctx: 'ctx,
        F: FnMut(KeyedQuery<'ctx, ScanItems<'_, P::Key, P::Value>>),
    {
        let results: Vec<(Id<P::Key>, P::Value)> = self.execute(ctx).collect();
        let scan = KeyedQuery {
            plan: ScanItems { items: &results },
            _marker: PhantomData,
        };
        f(scan);
    }

    /// Execute the query plan against the database context, yielding an iterator of results.
    #[inline]
    pub fn execute<Ctx>(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value)> + 'ctx
    where
        P: RelationQuery<'ctx, Ctx>,
        Ctx: 'ctx,
    {
        self.plan.execute(ctx)
    }

    /// Gather matching IDs first, then run a mutable closure on each of them.
    pub fn update<Ctx, V: 'static, F>(self, ctx: &'ctx mut Ctx, mut f: F)
    where
        P: RelationQuery<'ctx, Ctx>,
        Ctx: TotalRelationMut<P::Key, V> + 'ctx,
        F: FnMut(&mut V),
    {
        let ctx_ref: &Ctx = ctx;
        let ids: Vec<Id<P::Key>> = self.plan.execute(ctx_ref).map(|(id, _)| id).collect();
        drop(ctx_ref);

        for id in ids {
            let val = ctx.get_mut(id);
            f(val);
        }
    }
}

// ==========================================
// Fast-path contiguous non-allocating updates
// ==========================================

impl<'ctx, K: 'static, V: 'static> KeyedQuery<'ctx, Scan<K, V>> {
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

impl<'ctx, K: 'static, V: 'static, F> KeyedQuery<'ctx, Filter<Scan<K, V>, F>> {
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
pub fn query<'ctx, T: 'static>() -> KeyedQuery<'ctx, Scan<T, T>> {
    KeyedQuery {
        plan: Scan(PhantomData),
        _marker: PhantomData,
    }
}

// Starter function for scanning a general relation (K -> V)
#[inline]
pub fn query_relation<'ctx, K: 'static, V: 'static>() -> KeyedQuery<'ctx, Scan<K, V>> {
    KeyedQuery {
        plan: Scan(PhantomData),
        _marker: PhantomData,
    }
}

// Starter function for querying a single ID in a dense AST relation
#[inline]
pub fn query_one<'ctx, T: 'static>(id: Id<T>) -> KeyedQuery<'ctx, ScanOne<T, T>> {
    KeyedQuery {
        plan: ScanOne(id, PhantomData),
        _marker: PhantomData,
    }
}

// Starter function for querying a single ID (for assertions/testing)
#[inline]
pub fn query_one_relation<'ctx, K: 'static, V: 'static>(
    id: Id<K>,
) -> KeyedQuery<'ctx, ScanOneOpt<K, V>> {
    KeyedQuery {
        plan: ScanOneOpt(id, PhantomData),
        _marker: PhantomData,
    }
}

// Starter function for scanning a slice of IDs
#[inline]
pub fn query_slice<'ctx, T: 'static>(slice_id: SliceId<T>) -> KeyedQuery<'ctx, ScanSlice<T>> {
    KeyedQuery {
        plan: ScanSlice { slice: slice_id },
        _marker: PhantomData,
    }
}

// Starter function for scanning an in-memory materialized slice of results
#[inline]
pub fn query_items<'ctx, 'a, K, V>(
    items: &'a [(Id<K>, V)],
) -> KeyedQuery<'ctx, ScanItems<'a, K, V>> {
    KeyedQuery {
        plan: ScanItems { items },
        _marker: PhantomData,
    }
}

// ==========================================
// Symbolic Query Plan Steps
// ==========================================

// Implementation of Scan
pub struct Scan<K, V>(pub PhantomData<(K, V)>);

impl<'ctx, K: 'static, V: 'static> QueryPlan<'ctx> for Scan<K, V> {
    type Key = K;
    type Value = &'ctx V;
}

impl<'ctx, Ctx, K: 'static, V: 'static> RelationQuery<'ctx, Ctx> for Scan<K, V>
where
    Ctx: TotalRelation<K, V> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<K>, &'ctx V)> + 'ctx {
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

impl<'ctx, K: 'static, V: 'static> QueryPlan<'ctx> for ScanOne<K, V> {
    type Key = K;
    type Value = &'ctx V;
}

impl<'ctx, Ctx, K: 'static, V: 'static> RelationQuery<'ctx, Ctx> for ScanOne<K, V>
where
    Ctx: TotalRelation<K, V> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<K>, &'ctx V)> + 'ctx {
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

// Implementation of ScanOneOpt
pub struct ScanOneOpt<K, V>(pub Id<K>, pub PhantomData<V>);

impl<'ctx, K: 'static, V: 'static> QueryPlan<'ctx> for ScanOneOpt<K, V> {
    type Key = K;
    type Value = &'ctx V;
}

impl<'ctx, Ctx, K: 'static, V: 'static> RelationQuery<'ctx, Ctx> for ScanOneOpt<K, V>
where
    Ctx: PartialRelation<K, V> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<K>, &'ctx V)> + 'ctx {
        ctx.get_opt(self.0).map(|val| (self.0, val)).into_iter()
    }
}

impl<K, V> Clone for ScanOneOpt<K, V> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<K, V> Copy for ScanOneOpt<K, V> {}

// Implementation of ScanSlice
#[derive(Clone, Copy)]
pub struct ScanSlice<T> {
    slice: SliceId<T>,
}

impl<'ctx, T: 'static> QueryPlan<'ctx> for ScanSlice<T> {
    type Key = T;
    type Value = &'ctx T;
}

impl<'ctx, Ctx, T: 'static> RelationQuery<'ctx, Ctx> for ScanSlice<T>
where
    Ctx: SliceRelation<T> + TotalRelation<T, T> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<T>, &'ctx T)> + 'ctx {
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

impl<'ctx, 'a, K: 'static, V: Clone + Copy + 'ctx> QueryPlan<'ctx> for ScanItems<'a, K, V> {
    type Key = K;
    type Value = V;
}

impl<'ctx, 'a: 'ctx, Ctx, K: 'static, V: Clone + Copy + 'ctx> RelationQuery<'ctx, Ctx>
    for ScanItems<'a, K, V>
{
    #[inline]
    fn execute(self, _ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<K>, Self::Value)> + 'ctx {
        self.items.iter().cloned()
    }
}

// Implementation of Filter (Values only)
#[derive(Clone, Copy)]
pub struct Filter<P, F> {
    pub(crate) plan: P,
    pub(crate) predicate: F,
}

impl<'ctx, P: QueryPlan<'ctx>, F> QueryPlan<'ctx> for Filter<P, F> {
    type Key = P::Key;
    type Value = P::Value;
}

impl<'ctx, Ctx, P, F> RelationQuery<'ctx, Ctx> for Filter<P, F>
where
    P: RelationQuery<'ctx, Ctx>,
    F: Fn(P::Value) -> bool + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value)> + 'ctx {
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

impl<'ctx, P: QueryPlan<'ctx>, F> QueryPlan<'ctx> for FilterKeyed<P, F> {
    type Key = P::Key;
    type Value = P::Value;
}

impl<'ctx, Ctx, P, F> RelationQuery<'ctx, Ctx> for FilterKeyed<P, F>
where
    P: RelationQuery<'ctx, Ctx>,
    F: Fn(Id<P::Key>, P::Value) -> bool + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value)> + 'ctx {
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

impl<'ctx, P: QueryPlan<'ctx>, M: 'static> QueryPlan<'ctx> for With<P, M> {
    type Key = P::Key;
    type Value = (P::Value, &'ctx M);
}

impl<'ctx, Ctx, P, M: 'static> RelationQuery<'ctx, Ctx> for With<P, M>
where
    P: RelationQuery<'ctx, Ctx>,
    Ctx: PartialRelation<P::Key, M> + 'ctx,
{
    #[inline]
    fn execute(
        self,
        ctx: &'ctx Ctx,
    ) -> impl Iterator<Item = (Id<P::Key>, (P::Value, &'ctx M))> + 'ctx {
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

impl<'ctx, P: QueryPlan<'ctx>, M: 'static> QueryPlan<'ctx> for LeftJoin<P, M> {
    type Key = P::Key;
    type Value = (P::Value, Option<&'ctx M>);
}

impl<'ctx, Ctx, P, M: 'static> RelationQuery<'ctx, Ctx> for LeftJoin<P, M>
where
    P: RelationQuery<'ctx, Ctx>,
    Ctx: PartialRelation<P::Key, M> + 'ctx,
{
    #[inline]
    fn execute(
        self,
        ctx: &'ctx Ctx,
    ) -> impl Iterator<Item = (Id<P::Key>, (P::Value, Option<&'ctx M>))> + 'ctx {
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

impl<'ctx, P: QueryPlan<'ctx>, M: 'static> QueryPlan<'ctx> for Without<P, M> {
    type Key = P::Key;
    type Value = P::Value;
}

impl<'ctx, Ctx, P, M: 'static> RelationQuery<'ctx, Ctx> for Without<P, M>
where
    P: RelationQuery<'ctx, Ctx>,
    Ctx: PartialRelation<P::Key, M> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<P::Key>, P::Value)> + 'ctx {
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

impl<'ctx, P: QueryPlan<'ctx>, Target: 'static> QueryPlan<'ctx> for Cast<P, Target> {
    type Key = Target;
    type Value = &'ctx Target;
}

impl<'ctx, Ctx, P, Target: 'static> RelationQuery<'ctx, Ctx> for Cast<P, Target>
where
    P: RelationQuery<'ctx, Ctx>,
    P::Value: TryProject<Target>,
    Ctx: TotalRelation<Target, Target> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<Target>, &'ctx Target)> + 'ctx {
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

impl<'ctx, P: QueryPlan<'ctx>, Target: 'static, F> QueryPlan<'ctx> for Project<P, Target, F> {
    type Key = Target;
    type Value = &'ctx Target;
}

impl<'ctx, Ctx, P, Target: 'static, F> RelationQuery<'ctx, Ctx> for Project<P, Target, F>
where
    P: RelationQuery<'ctx, Ctx>,
    F: Fn(P::Value) -> Id<Target> + Clone + Copy + 'ctx,
    Ctx: TotalRelation<Target, Target> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<Target>, &'ctx Target)> + 'ctx {
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

impl<'ctx, P: QueryPlan<'ctx>, Target: 'static, F> QueryPlan<'ctx> for ProjectSlice<P, Target, F> {
    type Key = P::Key;
    type Value = SliceId<Target>;
}

impl<'ctx, Ctx, P, Target: 'static, F> RelationQuery<'ctx, Ctx> for ProjectSlice<P, Target, F>
where
    P: RelationQuery<'ctx, Ctx>,
    F: Fn(P::Value) -> SliceId<Target> + Clone + Copy + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<P::Key>, SliceId<Target>)> + 'ctx {
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

impl<'ctx, P: QueryPlan<'ctx>, Target: 'static, F> QueryPlan<'ctx> for ProjectOpt<P, Target, F> {
    type Key = Target;
    type Value = &'ctx Target;
}

impl<'ctx, Ctx, P, Target: 'static, F> RelationQuery<'ctx, Ctx> for ProjectOpt<P, Target, F>
where
    P: RelationQuery<'ctx, Ctx>,
    F: Fn(P::Value) -> Option<Id<Target>> + Clone + Copy + 'ctx,
    Ctx: TotalRelation<Target, Target> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<Target>, &'ctx Target)> + 'ctx {
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

// Implementation of GetAt
pub struct GetAt<P, T> {
    pub(crate) plan: P,
    pub(crate) index: usize,
    pub(crate) _marker: PhantomData<T>,
}

impl<'ctx, P: QueryPlan<'ctx>, T: 'static> QueryPlan<'ctx> for GetAt<P, T> {
    type Key = T;
    type Value = &'ctx T;
}

impl<'ctx, Ctx, P, T: 'static> RelationQuery<'ctx, Ctx> for GetAt<P, T>
where
    P: RelationQuery<'ctx, Ctx>,
    P::Value: std::borrow::Borrow<SliceId<T>>,
    Ctx: SliceRelation<T> + TotalRelation<T, T> + 'ctx,
{
    #[inline]
    fn execute(self, ctx: &'ctx Ctx) -> impl Iterator<Item = (Id<T>, &'ctx T)> + 'ctx {
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
