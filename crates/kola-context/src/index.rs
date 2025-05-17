use std::marker::PhantomData;

pub trait Index {}

/// Index pointing to he head of the cons list.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Here;

impl Index for Here {}

/// Index pointing to the tail of the cons list.
pub struct There<T: Index>(PhantomData<T>);

impl<T: Index> There<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T> Index for There<T> where T: Index {}

/// Essentially another cons list but composed of `Index`es.
pub trait ManyIndex {}

impl ManyIndex for super::Empty {}

impl<Head, Tail> ManyIndex for super::Ctx<Head, Tail>
where
    Head: Index,
    Tail: ManyIndex,
{
}
