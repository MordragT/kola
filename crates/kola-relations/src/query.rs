use crate::{
    id::Id,
    relation::{PartialRelation, SliceRelation, TotalRelation, TotalRelationMut},
    slice::SliceId,
};
use std::marker::PhantomData;

/// Conversion trait for attempting to extract/project a child ID of a specific type.
/// Implemented by the compiler AST crate.
pub trait TryProject<Target> {
    fn try_project(&self) -> Option<Id<Target>>;
}

/// The core Relation Query trait that represents a symbolic query step with GAT.
pub trait RelationQuery<Ctx> {
    type Key;
    type Value<'a>
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>: Iterator<Item = (Id<Self::Key>, Self::Value<'a>)> + 'a
    where
        Ctx: 'a,
        Self: 'a;

    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a>;
}

/// The main lifetime-free, zero-cost query builder.
pub struct KeyedQuery<P> {
    pub(crate) plan: P,
}

impl<P> KeyedQuery<P> {
    #[inline]
    pub fn filter<F>(self, predicate: F) -> KeyedQuery<Filter<P, F>> {
        KeyedQuery {
            plan: Filter {
                plan: self.plan,
                predicate,
            },
        }
    }

    #[inline]
    pub fn with<M>(self) -> KeyedQuery<With<P, M>> {
        KeyedQuery {
            plan: With {
                plan: self.plan,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn left_join<M>(self) -> KeyedQuery<LeftJoin<P, M>> {
        KeyedQuery {
            plan: LeftJoin {
                plan: self.plan,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn without<M>(self) -> KeyedQuery<Without<P, M>> {
        KeyedQuery {
            plan: Without {
                plan: self.plan,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn cast<Target>(self) -> KeyedQuery<Cast<P, Target>> {
        KeyedQuery {
            plan: Cast {
                plan: self.plan,
                _marker: PhantomData,
            },
        }
    }

    #[inline]
    pub fn project<Target, F>(self, projector: F) -> KeyedQuery<Project<P, Target, F>> {
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
    pub fn execute<'a, Ctx>(&'a self, ctx: &'a Ctx) -> P::Iter<'a>
    where
        P: RelationQuery<Ctx>,
    {
        self.plan.execute(ctx)
    }

    /// Gather matching IDs first, then run a mutable closure on each of them.
    /// This pattern avoids borrow checker issues by decoupling selection from mutation.
    pub fn update<Ctx, V, F>(self, ctx: &mut Ctx, mut f: F)
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

// Starter function for scanning a dense relation
#[inline]
pub fn query<K, V>() -> KeyedQuery<Scan<K, V>> {
    KeyedQuery {
        plan: Scan(PhantomData),
    }
}

// Starter function for querying a single ID (for assertions/testing)
#[inline]
pub fn query_one<K, V>(id: Id<K>) -> KeyedQuery<ScanOne<K, V>> {
    KeyedQuery {
        plan: ScanOne(id, PhantomData),
    }
}

// Starter function for scanning a slice of IDs
#[inline]
pub fn query_slice<T>(slice_id: SliceId<T>) -> KeyedQuery<ScanSlice<T>> {
    KeyedQuery {
        plan: ScanSlice {
            slice: slice_id,
            _marker: PhantomData,
        },
    }
}

// Implementation of Scan
pub struct Scan<K, V>(PhantomData<(K, V)>);

impl<Ctx, K, V> RelationQuery<Ctx> for Scan<K, V>
where
    Ctx: TotalRelation<K, V>,
{
    type Key = K;
    type Value<'a>
        = &'a V
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = Ctx::Rows<'a>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        ctx.rows()
    }
}

// Implementation of ScanOne
pub struct ScanOne<K, V>(pub Id<K>, pub PhantomData<V>);

impl<Ctx, K, V> RelationQuery<Ctx> for ScanOne<K, V>
where
    Ctx: TotalRelation<K, V>,
{
    type Key = K;
    type Value<'a>
        = &'a V
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = std::iter::Once<(Id<K>, &'a V)>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        std::iter::once((self.0, ctx.get(self.0)))
    }
}

// Implementation of ScanSlice
pub struct ScanSlice<T> {
    slice: SliceId<T>,
    _marker: PhantomData<T>,
}

impl<Ctx, T> RelationQuery<Ctx> for ScanSlice<T>
where
    Ctx: SliceRelation<T> + TotalRelation<T, T>,
{
    type Key = T;
    type Value<'a>
        = &'a T
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = ScanSliceIter<'a, Ctx, T>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        ScanSliceIter {
            slice_ids: ctx.get_slice(self.slice),
            index: 0,
            ctx,
            _marker: PhantomData,
        }
    }
}

pub struct ScanSliceIter<'a, Ctx, T> {
    slice_ids: &'a [Id<T>],
    index: usize,
    ctx: &'a Ctx,
    _marker: PhantomData<T>,
}

impl<'a, Ctx, T> Iterator for ScanSliceIter<'a, Ctx, T>
where
    Ctx: TotalRelation<T, T>,
{
    type Item = (Id<T>, &'a T);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.slice_ids.len() {
            let id = self.slice_ids[self.index];
            self.index += 1;
            Some((id, self.ctx.get(id)))
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.slice_ids.len() - self.index;
        (len, Some(len))
    }
}

impl<'a, Ctx, T> ExactSizeIterator for ScanSliceIter<'a, Ctx, T> where Ctx: TotalRelation<T, T> {}

// Implementation of Filter
pub struct Filter<P, F> {
    plan: P,
    predicate: F,
}

impl<Ctx, P, F> RelationQuery<Ctx> for Filter<P, F>
where
    P: RelationQuery<Ctx>,
    F: for<'b> Fn(&(Id<P::Key>, P::Value<'b>)) -> bool,
{
    type Key = P::Key;
    type Value<'a>
        = P::Value<'a>
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = FilterIter<'a, P::Iter<'a>, F>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        FilterIter {
            iter: self.plan.execute(ctx),
            predicate: &self.predicate,
        }
    }
}

pub struct FilterIter<'a, I, F> {
    iter: I,
    predicate: &'a F,
}

impl<'a, I, F, K, V> Iterator for FilterIter<'a, I, F>
where
    I: Iterator<Item = (Id<K>, V)>,
    F: Fn(&(Id<K>, V)) -> bool,
{
    type Item = (Id<K>, V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.find(|item| (self.predicate)(item))
    }
}

// Implementation of With
pub struct With<P, M> {
    plan: P,
    _marker: PhantomData<M>,
}

impl<Ctx, P, M> RelationQuery<Ctx> for With<P, M>
where
    P: RelationQuery<Ctx>,
    Ctx: PartialRelation<P::Key, M>,
{
    type Key = P::Key;
    type Value<'a>
        = (P::Value<'a>, &'a M)
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = WithIter<'a, P::Iter<'a>, Ctx, M>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        WithIter {
            iter: self.plan.execute(ctx),
            ctx,
            _marker: PhantomData,
        }
    }
}

pub struct WithIter<'a, I, Ctx, M> {
    iter: I,
    ctx: &'a Ctx,
    _marker: PhantomData<M>,
}

impl<'a, I, Ctx, M, K, V> Iterator for WithIter<'a, I, Ctx, M>
where
    I: Iterator<Item = (Id<K>, V)>,
    Ctx: PartialRelation<K, M>,
    M: 'a,
{
    type Item = (Id<K>, (V, &'a M));

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        for (id, val) in &mut self.iter {
            if let Some(meta) = self.ctx.get_opt(id) {
                return Some((id, (val, meta)));
            }
        }
        None
    }
}

// Implementation of LeftJoin
pub struct LeftJoin<P, M> {
    plan: P,
    _marker: PhantomData<M>,
}

impl<Ctx, P, M> RelationQuery<Ctx> for LeftJoin<P, M>
where
    P: RelationQuery<Ctx>,
    Ctx: PartialRelation<P::Key, M>,
{
    type Key = P::Key;
    type Value<'a>
        = (P::Value<'a>, Option<&'a M>)
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = LeftJoinIter<'a, P::Iter<'a>, Ctx, M>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        LeftJoinIter {
            iter: self.plan.execute(ctx),
            ctx,
            _marker: PhantomData,
        }
    }
}

pub struct LeftJoinIter<'a, I, Ctx, M> {
    iter: I,
    ctx: &'a Ctx,
    _marker: PhantomData<M>,
}

impl<'a, I, Ctx, M, K, V> Iterator for LeftJoinIter<'a, I, Ctx, M>
where
    I: Iterator<Item = (Id<K>, V)>,
    Ctx: PartialRelation<K, M>,
    M: 'a,
{
    type Item = (Id<K>, (V, Option<&'a M>));

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(id, val)| {
            let meta = self.ctx.get_opt(id);
            (id, (val, meta))
        })
    }
}

// Implementation of Without
pub struct Without<P, M> {
    plan: P,
    _marker: PhantomData<M>,
}

impl<Ctx, P, M> RelationQuery<Ctx> for Without<P, M>
where
    P: RelationQuery<Ctx>,
    Ctx: PartialRelation<P::Key, M>,
{
    type Key = P::Key;
    type Value<'a>
        = P::Value<'a>
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = WithoutIter<'a, P::Iter<'a>, Ctx, M>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        WithoutIter {
            iter: self.plan.execute(ctx),
            ctx,
            _marker: PhantomData,
        }
    }
}

pub struct WithoutIter<'a, I, Ctx, M> {
    iter: I,
    ctx: &'a Ctx,
    _marker: PhantomData<M>,
}

impl<'a, I, Ctx, M, K, V> Iterator for WithoutIter<'a, I, Ctx, M>
where
    I: Iterator<Item = (Id<K>, V)>,
    Ctx: PartialRelation<K, M>,
{
    type Item = (Id<K>, V);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        for (id, val) in &mut self.iter {
            if self.ctx.get_opt(id).is_none() {
                return Some((id, val));
            }
        }
        None
    }
}

// Implementation of Cast
pub struct Cast<P, Target> {
    plan: P,
    _marker: PhantomData<Target>,
}

impl<Ctx, P, Target> RelationQuery<Ctx> for Cast<P, Target>
where
    P: RelationQuery<Ctx>,
    for<'b> P::Value<'b>: TryProject<Target>,
    Ctx: TotalRelation<Target, Target>,
{
    type Key = Target;
    type Value<'a>
        = &'a Target
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = CastIter<'a, P::Iter<'a>, Ctx, Target>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        CastIter {
            iter: self.plan.execute(ctx),
            ctx,
            _marker: PhantomData,
        }
    }
}

pub struct CastIter<'a, I, Ctx, Target> {
    iter: I,
    ctx: &'a Ctx,
    _marker: PhantomData<Target>,
}

impl<'a, I, Ctx, Target, K, V> Iterator for CastIter<'a, I, Ctx, Target>
where
    I: Iterator<Item = (Id<K>, V)>,
    V: TryProject<Target>,
    Ctx: TotalRelation<Target, Target>,
    Target: 'a,
{
    type Item = (Id<Target>, &'a Target);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        for (_id, val) in &mut self.iter {
            if let Some(target_id) = val.try_project() {
                return Some((target_id, self.ctx.get(target_id)));
            }
        }
        None
    }
}

// Implementation of Project
pub struct Project<P, Target, F> {
    plan: P,
    projector: F,
    _marker: PhantomData<Target>,
}

impl<Ctx, P, Target, F> RelationQuery<Ctx> for Project<P, Target, F>
where
    P: RelationQuery<Ctx>,
    F: for<'b> Fn(&P::Value<'b>) -> Id<Target>,
    Ctx: TotalRelation<Target, Target>,
{
    type Key = Target;
    type Value<'a>
        = &'a Target
    where
        Ctx: 'a,
        Self: 'a;

    type Iter<'a>
        = ProjectIter<'a, P::Iter<'a>, Ctx, Target, F>
    where
        Ctx: 'a,
        Self: 'a;

    #[inline]
    fn execute<'a>(&'a self, ctx: &'a Ctx) -> Self::Iter<'a> {
        ProjectIter {
            iter: self.plan.execute(ctx),
            ctx,
            projector: &self.projector,
            _marker: PhantomData,
        }
    }
}

pub struct ProjectIter<'a, I, Ctx, Target, F> {
    iter: I,
    ctx: &'a Ctx,
    projector: &'a F,
    _marker: PhantomData<Target>,
}

impl<'a, I, Ctx, Target, F, K, V> Iterator for ProjectIter<'a, I, Ctx, Target, F>
where
    I: Iterator<Item = (Id<K>, V)>,
    F: Fn(&V) -> Id<Target>,
    Ctx: TotalRelation<Target, Target>,
    Target: 'a,
{
    type Item = (Id<Target>, &'a Target);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(_id, val)| {
            let target_id = (self.projector)(&val);
            (target_id, self.ctx.get(target_id))
        })
    }
}

// Special fast-path update direct method for un-joined single-column scans
impl<K, V> KeyedQuery<Scan<K, V>> {
    /// Mutates the entire column in a single-pass contiguous loop.
    /// Triggers Arc::make_mut exactly once, and avoids repeated index bounds checks.
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
