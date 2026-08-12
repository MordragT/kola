use std::marker::PhantomData;

/// A typed range representing a contiguous range of `Id<T>` values.
/// This replaces `Vec<Id<T>>` in node structs.
#[derive(serde::Serialize, serde::Deserialize)]
pub struct SliceId<T: ?Sized> {
    start: u32,
    length: u32,
    t: PhantomData<T>,
}

impl<T: ?Sized> Clone for SliceId<T> {
    fn clone(&self) -> Self {
        Self {
            start: self.start,
            length: self.length,
            t: std::marker::PhantomData,
        }
    }
}

impl<T: ?Sized> Copy for SliceId<T> {}

impl<T: ?Sized> PartialEq for SliceId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.length == other.length
    }
}

impl<T: ?Sized> Eq for SliceId<T> {}

impl<T: ?Sized> PartialOrd for SliceId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for SliceId<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start
            .cmp(&other.start)
            .then(self.length.cmp(&other.length))
    }
}

impl<T: ?Sized> std::hash::Hash for SliceId<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.length.hash(state);
    }
}

impl<T: ?Sized> std::fmt::Debug for SliceId<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}[{}..{}]",
            std::any::type_name::<T>(),
            self.start,
            self.start + self.length
        )
    }
}

impl<T> SliceId<T> {
    #[inline]
    pub fn new(start: u32, length: u32) -> Self {
        Self {
            start,
            length,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn empty() -> Self {
        Self {
            start: 0,
            length: 0,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn start(&self) -> usize {
        self.start as usize
    }

    #[inline]
    pub fn end(&self) -> usize {
        (self.start + self.length) as usize
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.length as usize
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
}
