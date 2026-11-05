use std::marker::PhantomData;

use crate::node::Name;
use crate::tree::{TotalSpan, Tree};
pub use kola_relations::id::{Id, IdIter};
pub use kola_relations::slice::SliceId;
use kola_relations::{Plan, Presence, Query, SliceRelation, TotalRelation, Traversal, TryCast};

pub trait IdExt<T: 'static>: Sized {
    fn get<'a, M>(self, tree: &'a Tree<M>) -> &'a T
    where
        Tree<M>: TotalRelation<T, T>,
        M: 'static;

    fn span<'a, M>(self, tree: &'a Tree<M>) -> &'a M
    where
        Tree<M>: TotalSpan<T, M>,
        M: 'static;

    /// Casts an AST enum ID directly to its concrete reference &U!
    fn cast<'a, U: 'static, M>(self, tree: &'a Tree<M>) -> &'a U
    where
        Tree<M>: TotalRelation<T, T> + TotalRelation<U, U>,
        T: TryCast<U>,
        M: 'static,
    {
        let id_u = self.get(tree).try_cast().unwrap_or_else(|| {
            panic!(
                "Failed to cast AST ID: expected concrete type {}, found enum variant",
                std::any::type_name::<U>()
            )
        });
        id_u.get(tree)
    }
}

impl<T: 'static> IdExt<T> for Id<T> {
    #[inline]
    fn get<'a, M>(self, tree: &'a Tree<M>) -> &'a T
    where
        Tree<M>: TotalRelation<T, T>,
        M: 'static,
    {
        TotalRelation::<T, T>::get(tree, self)
    }

    #[inline]
    fn span<'a, M>(self, tree: &'a Tree<M>) -> &'a M
    where
        Tree<M>: TotalSpan<T, M>,
        M: 'static,
    {
        TotalSpan::<T, M>::get_span(tree, self)
    }

    #[inline]
    fn cast<'a, U: 'static, M>(self, tree: &'a Tree<M>) -> &'a U
    where
        Tree<M>: TotalRelation<T, T> + TotalRelation<U, U>,
        T: TryCast<U>,
        M: 'static,
    {
        let id_u = self.get(tree).try_cast().unwrap_or_else(|| {
            panic!(
                "Failed to cast AST ID: expected concrete type {}, found enum variant",
                std::any::type_name::<U>()
            )
        });
        id_u.get(tree)
    }
}

pub trait SliceIdExt<T: 'static> {
    fn get<'a, M>(self, tree: &'a Tree<M>) -> &'a [Id<T>]
    where
        Tree<M>: SliceRelation<T>,
        M: 'static;

    /// Get a concrete node reference directly at a slice index with bounds-checking!
    fn get_at<'a, M>(self, index: usize, tree: &'a Tree<M>) -> &'a T
    where
        Tree<M>: SliceRelation<T> + TotalRelation<T, T>,
        M: 'static;

    /// Get an iterator of all concrete node references in the slice.
    fn get_elements<'a, M>(self, tree: &'a Tree<M>) -> impl Iterator<Item = &'a T>
    where
        Tree<M>: SliceRelation<T> + TotalRelation<T, T>,
        M: 'static;
}

impl<T: 'static> SliceIdExt<T> for SliceId<T> {
    #[inline]
    fn get<'a, M>(self, tree: &'a Tree<M>) -> &'a [Id<T>]
    where
        Tree<M>: SliceRelation<T>,
        M: 'static,
    {
        tree.get_slice(self)
    }

    #[inline]
    fn get_at<'a, M>(self, index: usize, tree: &'a Tree<M>) -> &'a T
    where
        Tree<M>: SliceRelation<T> + TotalRelation<T, T>,
        M: 'static,
    {
        tree.get_slice(self)[index].get(tree)
    }

    #[inline]
    fn get_elements<'a, M>(self, tree: &'a Tree<M>) -> impl Iterator<Item = &'a T>
    where
        Tree<M>: SliceRelation<T> + TotalRelation<T, T>,
        M: 'static,
    {
        tree.get_slice(self)
            .iter()
            .copied()
            .map(move |id| id.get(tree))
    }
}

/// Lazy plan step that asserts the name of the focused node.
#[derive(Clone)]
pub struct AssertNameQuery<'i, P, NS> {
    plan: P,
    expected: String,
    interner: &'i kola_interner::StrInterner,
    _marker: PhantomData<NS>,
}

impl<'i, P: Plan, NS: 'static> Plan for AssertNameQuery<'i, P, NS> {
    type Src = P::Src;
    type Target = P::Target;
    type Presence = P::Presence;
    type Value<'a> = <P as Plan>::Value<'a>;
}

impl<'ctx, 'i, M, P, NS> Traversal<'ctx, Tree<M>> for AssertNameQuery<'i, P, NS>
where
    M: 'static,
    NS: 'static,
    P: Traversal<'ctx, Tree<M>> + Plan<Target = Name<NS>>,
    for<'a> P: Plan<Value<'a> = &'a Name<NS>>,
    Tree<M>: TotalRelation<Name<NS>, Name<NS>>,
{
    #[inline]
    #[track_caller]
    fn execute(
        &self,
        ctx: &'ctx Tree<M>,
        start: Id<Self::Src>,
    ) -> <P::Presence as Presence>::Wrap<(Id<P::Target>, <P as Plan>::Value<'ctx>)> {
        <P::Presence as Presence>::map(self.plan.execute(ctx, start), |(target_id, value)| {
            value.assert_name(&self.expected, self.interner);
            (target_id, value)
        })
    }
}

/// Lazy name assertion for queries.
pub trait AssertQueryNameExt<'i> {
    type Name: 'static;
    type Output;

    fn assert_name(self, expected: &str, interner: &'i kola_interner::StrInterner) -> Self::Output;
}

impl<'i, P, S, NS> AssertQueryNameExt<'i> for Query<P, S>
where
    NS: 'static,
    P: Plan<Target = Name<NS>> + 'static,
{
    type Name = Name<NS>;
    type Output = Query<AssertNameQuery<'i, P, NS>, S>;

    #[inline]
    fn assert_name(self, expected: &str, interner: &'i kola_interner::StrInterner) -> Self::Output {
        self.map(|plan| AssertNameQuery {
            plan,
            expected: expected.to_owned(),
            interner,
            _marker: PhantomData,
        })
    }
}
