use super::{Split, ToRef};
use crate::index::ManyIndex;

pub trait Select<T, I>
where
    T: ToRef,
    I: ManyIndex,
{
    fn select(&self) -> T::Ref<'_>;
    fn select_mut(&mut self) -> T::Mut<'_>;
}

impl<L, T, I> Select<T, I> for L
where
    L: ToRef,
    T: ToRef,
    I: ManyIndex,
    for<'a> L::Ref<'a>: Split<T::Ref<'a>, I>,
    for<'a> L::Mut<'a>: Split<T::Mut<'a>, I>,
{
    fn select(&self) -> T::Ref<'_> {
        let refs = self.to_ref();
        let (many, _remainder) = refs.split();
        many
    }

    fn select_mut(&mut self) -> T::Mut<'_> {
        let muts = self.to_mut();
        let (many, _remainder) = muts.split();
        many
    }
}
