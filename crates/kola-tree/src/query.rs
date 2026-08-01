use crate::id::Id;

/// Access to a `Item` inside a storage.
pub trait Get<T> {
    type Item;

    fn get(&self, id: Id<T>) -> &Self::Item;
    fn get_mut(&mut self, id: Id<T>) -> &mut Self::Item;

    fn set(&mut self, id: Id<T>, value: Self::Item) -> Self::Item {
        std::mem::replace(self.get_mut(id), value)
    }
}

/// Access to an optional `Item` inside a storage.
pub trait GetOpt<T> {
    type Item;

    fn get_opt(&self, id: Id<T>) -> Option<&Self::Item>;
    fn get_opt_mut(&mut self, id: Id<T>) -> Option<&mut Self::Item>;
    fn set(&mut self, id: Id<T>, value: Self::Item) -> Option<Self::Item>;

    fn get_unchecked(&self, id: Id<T>) -> &Self::Item {
        self.get_opt(id).expect("id not found")
    }

    fn get_unchecked_mut(&mut self, id: Id<T>) -> &mut Self::Item {
        self.get_opt_mut(id).expect("id not found")
    }
}

/// Access to a column of items inside a storage.
pub trait Col<T> {
    type Column;
    type Ids<'a>: Iterator<Item = Id<T>> + 'a
    where
        Self: 'a;

    fn col(&self) -> &Self::Column;
    fn col_mut(&mut self) -> &mut Self::Column;

    fn ids<'a>(&'a self) -> Self::Ids<'a>;
}
