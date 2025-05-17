use crate::{Ctx, Empty};

pub trait Append {
    type Output<T>;

    fn append<T>(self, item: T) -> Self::Output<T>;
}

impl Append for Empty {
    type Output<T> = Ctx<T, Empty>;

    fn append<T>(self, item: T) -> Self::Output<T> {
        Ctx(item, Empty)
    }
}

impl<Head, Tail> Append for Ctx<Head, Tail>
where
    Tail: Append,
{
    type Output<T> = Ctx<Head, Tail::Output<T>>;

    fn append<T>(self, item: T) -> Self::Output<T> {
        let Ctx(head, tail) = self;
        let tail = tail.append(item);
        Ctx(head, tail)
    }
}
