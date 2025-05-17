use super::Remove;
use crate::{
    Ctx, Empty,
    index::{Index, ManyIndex},
};

pub trait Split<T, I>
where
    I: ManyIndex,
{
    type Remainder;

    fn split(self) -> (T, Self::Remainder);
}

impl<T> Split<Empty, Empty> for T {
    type Remainder = Self;

    fn split(self) -> (Empty, Self::Remainder) {
        (Empty, self)
    }
}

impl<Head, Tail, OtherHead, OtherTail, IndexHead, IndexTail>
    Split<Ctx<OtherHead, OtherTail>, Ctx<IndexHead, IndexTail>> for Ctx<Head, Tail>
where
    IndexHead: Index,
    IndexTail: ManyIndex,
    Self: Remove<OtherHead, IndexHead>,
    <Self as Remove<OtherHead, IndexHead>>::Remainder: Split<OtherTail, IndexTail>,
{
    type Remainder = <<Self as Remove<OtherHead, IndexHead>>::Remainder as Split<
        OtherTail,
        IndexTail,
    >>::Remainder;

    fn split(self) -> (Ctx<OtherHead, OtherTail>, Self::Remainder) {
        let (head, remainder) = self.remove();
        let (tail, final_remainder) = remainder.split();
        (Ctx(head, tail), final_remainder)
    }
}
