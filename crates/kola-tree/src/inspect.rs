use crate::node::{
    FunctorNamespace, KindNamespace, ModuleNamespace, ModuleTypeNamespace, Name, TypeNamespace,
    ValueNamespace,
};
use crate::tree::{TotalSpan, Tree};
use kola_interner::StrInterner;
pub use kola_relations::id::{Id, IdIter};
pub use kola_relations::slice::SliceId;
use kola_relations::{SliceRelation, TotalRelation, TryCast};

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

// Flat Name Assertion helper trait
pub trait AssertNameExt<M: 'static> {
    fn assert_name(&self, expected: &str, tree: &Tree<M>, interner: &StrInterner);
}

macro_rules! impl_assert_name {
    ($($Namespace:ty),* $(,)?) => {
        $(
            // Implemented on the ID reference (e.g. Id<Name<Namespace>>)
            impl<M: 'static> AssertNameExt<M> for Id<Name<$Namespace>>
            where
                Tree<M>: TotalRelation<Name<$Namespace>, Name<$Namespace>>,
            {
                #[inline]
                fn assert_name(&self, expected: &str, tree: &Tree<M>, interner: &StrInterner) {
                    self.get(tree).assert_name(expected, interner);
                }
            }
        )*
    };
}

impl_assert_name!(
    FunctorNamespace,
    ModuleTypeNamespace,
    ModuleNamespace,
    KindNamespace,
    TypeNamespace,
    ValueNamespace,
);

// Generic fluent PartialEq assertion helper
pub trait AssertEqExt {
    fn assert_eq(&self, expected: &Self)
    where
        Self: PartialEq + std::fmt::Debug;
}

impl<T: PartialEq + std::fmt::Debug> AssertEqExt for T {
    #[inline]
    fn assert_eq(&self, expected: &Self) {
        assert_eq!(self, expected);
    }
}
