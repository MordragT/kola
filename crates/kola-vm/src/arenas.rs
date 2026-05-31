use std::{marker::PhantomData, num::NonZeroU32, range::Range};

#[derive(Debug)]
pub struct Idx<T: ?Sized>(NonZeroU32, PhantomData<T>);

impl<T: ?Sized> Copy for Idx<T> {}

impl<T: ?Sized> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized> Eq for Idx<T> {}

impl<T: ?Sized> PartialOrd for Idx<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T: ?Sized> Ord for Idx<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

#[derive(Debug)]
pub struct Arena<T: Clone>(pub Vec<T>);

impl<T: Clone> Arena<T> {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    #[inline]
    pub fn alloc(&mut self, value: T) -> Idx<T> {
        let idx = (self.0.len() + 1) as u32;
        self.0.push(value);
        Idx(NonZeroU32::new(idx).expect("arena overflow"), PhantomData)
    }

    #[inline]
    pub fn get(&self, idx: Idx<T>) -> &T {
        &self.0[idx.0.get() as usize - 1]
    }
}

#[derive(Debug)]
pub struct RangeIdx<T: ?Sized>(Range<NonZeroU32>, PhantomData<T>);

impl<T> RangeIdx<T> {
    pub fn is_empty(&self) -> bool {
        self.0.start == self.0.end
    }

    pub fn len(&self) -> usize {
        (self.0.end.get() - self.0.start.get()) as usize
    }
}

impl<T: ?Sized> Copy for RangeIdx<T> {}

impl<T: ?Sized> Clone for RangeIdx<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> PartialEq for RangeIdx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: ?Sized> Eq for RangeIdx<T> {}

#[derive(Debug)]
pub struct RangeArena<T: Clone>(pub Vec<T>);

impl<T: Clone> RangeArena<T> {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    #[inline]
    pub fn alloc(&mut self, values: &[T]) -> RangeIdx<T>
    where
        T: Clone,
    {
        let start = NonZeroU32::new((self.0.len() + 1) as u32).expect("arena overflow");
        self.0.extend_from_slice(values);
        let end = NonZeroU32::new((self.0.len() + 1) as u32).expect("arena overflow");
        RangeIdx(Range { start, end }, PhantomData)
    }

    #[inline]
    pub fn get(&self, idx: RangeIdx<T>) -> &[T] {
        &self.0[idx.0.start.get() as usize - 1..idx.0.end.get() as usize - 1]
    }
}
