pub mod convert;
pub mod index;
pub mod ops;

#[macro_export]
macro_rules! Cons {
    () => {
        $crate::cons::Nil
    };

    ($first:ty $(, $rest:ty)* $(,)?) => {
        $crate::cons::Cons<$first, $crate::Cons!($($rest),*)>
    };

}

#[macro_export]
macro_rules! cons {
    () => {
        $crate::cons::Nil
    };

    ($first:ident $(, $rest:ident)* $(,)?) => {
        $crate::cons::Cons($first, $crate::cons!($($rest),*))
    };

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Nil;

impl Nil {
    pub const fn new() -> Self {
        Nil
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Cons<Head, Tail>(pub Head, pub Tail)
where
    Tail: ?Sized;

impl<Head> Cons<Head, Nil> {
    pub const fn new(head: Head) -> Self {
        Cons(head, Nil)
    }
}

impl<Head, Tail> Cons<Head, Tail> {
    pub const fn head(&self) -> &Head {
        &self.0
    }

    pub const fn head_mut(&mut self) -> &mut Head {
        &mut self.0
    }

    pub const fn tail(&self) -> &Tail {
        &self.1
    }

    pub const fn tail_mut(&mut self) -> &mut Tail {
        &mut self.1
    }
}
