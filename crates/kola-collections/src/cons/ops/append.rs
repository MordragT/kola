use crate::cons::{Cons, Nil};

pub trait Append {
    type Output<T>;

    fn append<T>(self, item: T) -> Self::Output<T>;
}

impl Append for Nil {
    type Output<T> = Cons<T, Nil>;

    fn append<T>(self, item: T) -> Self::Output<T> {
        Cons(item, Nil)
    }
}

impl<Head, Tail> Append for Cons<Head, Tail>
where
    Tail: Append,
{
    type Output<T> = Cons<Head, Tail::Output<T>>;

    fn append<T>(self, item: T) -> Self::Output<T> {
        let Cons(head, tail) = self;
        let tail = tail.append(item);
        Cons(head, tail)
    }
}
