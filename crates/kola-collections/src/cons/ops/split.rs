use super::Remove;
use crate::cons::{
    Cons, Nil,
    index::{Index, ManyIndex},
};

pub trait Split<T, I>
where
    I: ManyIndex,
{
    type Remainder;

    fn split(self) -> (T, Self::Remainder);
}

impl<T> Split<Nil, Nil> for T {
    type Remainder = Self;

    fn split(self) -> (Nil, Self::Remainder) {
        (Nil, self)
    }
}

impl<Head, Tail, OtherHead, OtherTail, IndexHead, IndexTail>
    Split<Cons<OtherHead, OtherTail>, Cons<IndexHead, IndexTail>> for Cons<Head, Tail>
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

    fn split(self) -> (Cons<OtherHead, OtherTail>, Self::Remainder) {
        let (head, remainder) = self.remove();
        let (tail, final_remainder) = remainder.split();
        (Cons(head, tail), final_remainder)
    }
}
