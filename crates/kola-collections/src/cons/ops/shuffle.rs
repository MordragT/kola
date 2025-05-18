use super::Split;
use crate::cons::{Nil, index::ManyIndex};

pub trait Shuffle<T, I>: Split<T, I, Remainder = Nil>
where
    I: ManyIndex,
{
    fn shuffle(self) -> T;
}

impl<T, L, I> Shuffle<L, I> for T
where
    T: Split<L, I, Remainder = Nil>,
    I: ManyIndex,
{
    fn shuffle(self) -> L {
        let (many, _) = self.split();
        many
    }
}
