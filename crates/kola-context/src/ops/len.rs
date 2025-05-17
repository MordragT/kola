use crate::{Ctx, Empty};

pub trait Len {
    fn len(&self) -> usize;

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Len for Empty {
    fn len(&self) -> usize {
        0
    }
}

impl<Head, Tail> Len for Ctx<Head, Tail>
where
    Tail: Len,
{
    fn len(&self) -> usize {
        1 + self.1.len()
    }
}
