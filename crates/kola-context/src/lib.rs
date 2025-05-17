pub mod convert;
pub mod index;
pub mod ops;

pub mod prelude {
    pub use crate::index::*;
    pub use crate::ops::*;
    pub use crate::{Ctx, Empty};
}

#[macro_export]
macro_rules! Ctx {
    () => {
        $crate::Empty
    };

    ($first:ty $(, $rest:ty)* $(,)?) => {
        $crate::Ctx<$first, $crate::Ctx!($($rest),*)>
    };

}

#[macro_export]
macro_rules! ctx {
    () => {
        $crate::Empty
    };

    ($first:ident $(, $rest:ident)* $(,)?) => {
        $crate::Ctx($first, $crate::ctx!($($rest),*))
    };

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Empty;

impl Empty {
    pub const fn new() -> Self {
        Empty
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Ctx<Head, Tail>(pub Head, pub Tail)
where
    Tail: ?Sized;

impl<Head> Ctx<Head, Empty> {
    pub const fn new(head: Head) -> Self {
        Ctx(head, Empty)
    }
}

impl<Head, Tail> Ctx<Head, Tail> {
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
