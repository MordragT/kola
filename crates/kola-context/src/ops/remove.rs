use crate::{
    Ctx,
    index::{Here, Index, There},
};

pub trait Remove<T, I>
where
    I: Index,
{
    type Remainder;

    fn remove(self) -> (T, Self::Remainder);
}

impl<Head, Tail> Remove<Head, Here> for Ctx<Head, Tail> {
    type Remainder = Tail;

    fn remove(self) -> (Head, Self::Remainder) {
        let Ctx(head, tail) = self;
        (head, tail)
    }
}

impl<Head, Tail, FromTail, TailIndex> Remove<FromTail, There<TailIndex>> for Ctx<Head, Tail>
where
    Tail: Remove<FromTail, TailIndex>,
    TailIndex: Index,
{
    type Remainder = Ctx<Head, Tail::Remainder>;

    fn remove(self) -> (FromTail, Self::Remainder) {
        let Ctx(head, tail) = self;
        let (from_tail, remainder) = tail.remove();
        (from_tail, Ctx(head, remainder))
    }
}
