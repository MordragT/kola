use crate::{
    id::Id,
    relation::{PartialRelation, RelationIds, TotalRelation, TotalRelationMut},
};

/// A semantic overlay representing a shadow database context.
/// Lookups are first attempted on `Local` (shadow map); if not found,
/// they fall back to `Base` (underlying DB).
pub struct Overlay<Base, Local> {
    pub base: Base,
    pub local: Local,
}

impl<Base, Local> Overlay<Base, Local> {
    #[inline]
    pub fn new(base: Base, local: Local) -> Self {
        Self { base, local }
    }
}

impl<Base, Local, K> RelationIds<K> for Overlay<Base, Local>
where
    Base: RelationIds<K>,
{
    type Ids<'a>
        = Base::Ids<'a>
    where
        Self: 'a;

    #[inline]
    fn ids(&self) -> Self::Ids<'_> {
        self.base.ids()
    }
}

impl<Base, Local, K, V> PartialRelation<K, V> for Overlay<Base, Local>
where
    Base: PartialRelation<K, V>,
    Local: PartialRelation<K, V>,
{
    #[inline]
    fn get_opt(&self, id: Id<K>) -> Option<&V> {
        self.local.get_opt(id).or_else(|| self.base.get_opt(id))
    }
}

impl<Base, Local, K, V> TotalRelation<K, V> for Overlay<Base, Local>
where
    Base: TotalRelation<K, V>,
    Local: PartialRelation<K, V>,
{
    #[inline]
    fn get(&self, id: Id<K>) -> &V {
        self.local.get_opt(id).unwrap_or_else(|| self.base.get(id))
    }

    type Rows<'a>
        = Base::Rows<'a>
    where
        Self: 'a,
        V: 'a;

    #[inline]
    fn rows(&self) -> Self::Rows<'_> {
        self.base.rows()
    }
}

impl<Base, Local, K, V> TotalRelationMut<K, V> for Overlay<Base, Local>
where
    Base: TotalRelationMut<K, V>,
    Local: PartialRelation<K, V>,
{
    #[inline]
    fn get_mut(&mut self, id: Id<K>) -> &mut V {
        // Since Local is typically read-only / temporary, mutation delegates to Base.
        self.base.get_mut(id)
    }

    type RowsMut<'a>
        = Base::RowsMut<'a>
    where
        Self: 'a,
        V: 'a;

    #[inline]
    fn rows_mut(&mut self) -> Self::RowsMut<'_> {
        self.base.rows_mut()
    }
}
