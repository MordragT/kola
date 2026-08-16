use crate::tree::{TotalSpan, Tree};
pub use kola_relations::id::{Id, IdIter};

pub trait IdExt<T: 'static> {
    fn get<'a, M>(self, tree: &'a Tree<M>) -> &'a T
    where
        Tree<M>: kola_relations::TotalRelation<T, T>,
        M: 'static;

    fn span<'a, M>(self, tree: &'a Tree<M>) -> &'a M
    where
        Tree<M>: TotalSpan<T, M>,
        M: 'static;
}

impl<T: 'static> IdExt<T> for Id<T> {
    #[inline]
    fn get<'a, M>(self, tree: &'a Tree<M>) -> &'a T
    where
        Tree<M>: kola_relations::TotalRelation<T, T>,
        M: 'static,
    {
        kola_relations::TotalRelation::<T, T>::get(tree, self)
    }

    #[inline]
    fn span<'a, M>(self, tree: &'a Tree<M>) -> &'a M
    where
        Tree<M>: TotalSpan<T, M>,
        M: 'static,
    {
        TotalSpan::<T, M>::get_span(tree, self)
    }
}
