use crate::cons::{Cons, Nil};

pub trait Len {
    fn len(&self) -> usize;

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Len for Nil {
    fn len(&self) -> usize {
        0
    }
}

impl<Head, Tail> Len for Cons<Head, Tail>
where
    Tail: Len,
{
    fn len(&self) -> usize {
        1 + self.1.len()
    }
}
