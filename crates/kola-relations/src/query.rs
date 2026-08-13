use crate::{
    id::Id,
    relation::{PartialRelation, SliceRelation, TotalRelation, TotalRelationMut},
    slice::SliceId,
};
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
/// This allows the compiler to perform perfect type-inference for closure
/// parameters without needing the database context type Ctx.
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
    pub fn project<Target: 'static, F>(self, projector: F) -> KeyedQuery<Project<P, Target, F>> {
        KeyedQuery {
            plan: Project {
                plan: self.plan,
                projector,
                _marker: PhantomData,
            },
        }
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
    /// This pattern avoids borrow checker issues by decoupling selection from mutation.
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
    /// Triggers Arc::make_mut exactly once, and avoids repeated index bounds checks.
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

// Starter function for querying a single ID (for assertions/testing)
#[inline]
pub fn query_one<K: 'static, V: 'static>(id: Id<K>) -> KeyedQuery<ScanOne<K, V>> {
    KeyedQuery {
        plan: ScanOne(id, PhantomData),
    }
}

// Starter function for scanning a slice of IDs
#[inline]
pub fn query_slice<T: 'static>(slice_id: SliceId<T>) -> KeyedQuery<ScanSlice<T>> {
    KeyedQuery {
        plan: ScanSlice {
            slice: slice_id,
            _marker: PhantomData,
        },
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

// Implementation of ScanSlice
pub struct ScanSlice<T> {
    slice: SliceId<T>,
    _marker: PhantomData<T>,
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

// Implementation of Filter (Values only)
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
    F: Fn(&P::Value<'_>) -> Id<Target>,
    Ctx: TotalRelation<Target, Target>,
{
    #[inline]
    fn execute<'a>(self, ctx: &'a Ctx) -> impl Iterator<Item = (Id<Target>, &'a Target)> + 'a
    where
        Ctx: 'a,
        Self: 'a,
    {
        self.plan.execute(ctx).map(move |(_, val)| {
            let target_id = (self.projector)(&val);
            (target_id, ctx.get(target_id))
        })
    }
}
