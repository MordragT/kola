use crate::cons::{
    Cons,
    index::{Here, Index, There},
};

pub trait Remove<T, I>
where
    I: Index,
{
    type Remainder;

    fn remove(self) -> (T, Self::Remainder);
}

impl<Head, Tail> Remove<Head, Here> for Cons<Head, Tail> {
    type Remainder = Tail;

    fn remove(self) -> (Head, Self::Remainder) {
        let Cons(head, tail) = self;
        (head, tail)
    }
}

impl<Head, Tail, FromTail, TailIndex> Remove<FromTail, There<TailIndex>> for Cons<Head, Tail>
where
    Tail: Remove<FromTail, TailIndex>,
    TailIndex: Index,
{
    type Remainder = Cons<Head, Tail::Remainder>;

    fn remove(self) -> (FromTail, Self::Remainder) {
        let Cons(head, tail) = self;
        let (from_tail, remainder) = tail.remove();
        (from_tail, Cons(head, remainder))
    }
}
