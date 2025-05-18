use crate::cons::Cons;

pub trait Prepend {
    type Output<T>;

    fn prepend<T>(self, item: T) -> Self::Output<T>;
}

impl<L> Prepend for L {
    type Output<T> = Cons<T, L>;

    fn prepend<T>(self, item: T) -> Self::Output<T> {
        Cons(item, self)
    }
}
