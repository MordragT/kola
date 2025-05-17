use super::Split;
use crate::{Empty, index::ManyIndex};

pub trait Shuffle<T, I>: Split<T, I, Remainder = Empty>
where
    I: ManyIndex,
{
    fn shuffle(self) -> T;
}

impl<T, L, I> Shuffle<L, I> for T
where
    T: Split<L, I, Remainder = Empty>,
    I: ManyIndex,
{
    fn shuffle(self) -> L {
        let (many, _) = self.split();
        many
    }
}
