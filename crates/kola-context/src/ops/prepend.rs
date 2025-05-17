use crate::Ctx;

pub trait Prepend {
    type Output<T>;

    fn prepend<T>(self, item: T) -> Self::Output<T>;
}

impl<L> Prepend for L {
    type Output<T> = Ctx<T, L>;

    fn prepend<T>(self, item: T) -> Self::Output<T> {
        Ctx(item, self)
    }
}
