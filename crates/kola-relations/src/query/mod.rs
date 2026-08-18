use std::marker::PhantomData;

use crate::Id;
use crate::relation::{RelationIds, SliceRelation, TotalRelationMut};
use crate::slice::SliceId;

pub mod combinators;
pub use combinators::*;

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

/// Helper starter function for constructing a new relational query path starting from a SliceId
#[inline]
pub fn query_slice<T: 'static>(id: SliceId<T>) -> Query<T, T, ScanOneQuery<T>, SliceId<T>> {
    Query {
        plan: ScanOneQuery(PhantomData),
        state: id,
        _marker: PhantomData,
    }
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

impl<Src: 'static, Target, Plan> Query<Src, Target, Plan, SliceId<Src>> {
    /// Eager loader: Executes the traversal starting from a stored slice of IDs and yields a lazy iterator of targets.
    #[inline]
    pub fn iter<'ctx, Ctx>(self, ctx: &'ctx Ctx) -> impl Iterator<Item = Plan::Output> + 'ctx
    where
        Plan: Traversal<'ctx, Ctx, Src = Src, Target = Target> + 'ctx,
        Ctx: SliceRelation<Src> + 'ctx,
    {
        ctx.get_slice(self.state)
            .iter()
            .filter_map(move |&id| self.plan.execute(ctx, id).map(|(_, val)| val))
    }
}
