use crate::Id;
use crate::relation::SliceRelation;
use std::marker::PhantomData;

/// A typed range representing a contiguous range of `Id<T>` values.
/// This replaces `Vec<Id<T>>` in node structs.
#[derive(serde::Serialize, serde::Deserialize)]
pub struct SliceId<T: ?Sized> {
    pub(crate) start: u32,
    pub(crate) length: u32,
    pub(crate) t: PhantomData<T>,
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

/// A container managing contiguous slices of `Id<T>` inside a single allocation.
#[derive(Debug, Clone, Default)]
pub struct SliceStorage(Vec<u32>);

impl SliceStorage {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn checkpoint(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn restore(&mut self, cp: usize) {
        self.0.truncate(cp);
    }

    #[inline]
    pub fn alloc<T>(&mut self, iter: impl IntoIterator<Item = Id<T>>) -> SliceId<T> {
        let start = self.0.len();
        self.0.extend(iter.into_iter().map(|id| id.id()));
        let len = self.0.len() - start;
        SliceId::new(start as u32, len as u32)
    }

    #[inline]
    pub fn get<T>(&self, slice_id: SliceId<T>) -> &[Id<T>] {
        let start = slice_id.start() as usize;
        let end = start + slice_id.len() as usize;
        let ids = &self.0[start..end];
        unsafe { std::slice::from_raw_parts(ids.as_ptr() as *const Id<T>, ids.len()) }
    }

    #[inline]
    pub fn get_mut<T>(&mut self, slice_id: SliceId<T>) -> &mut [Id<T>] {
        let start = slice_id.start() as usize;
        let end = start + slice_id.len() as usize;
        let ids = &mut self.0[start..end];
        unsafe { std::slice::from_raw_parts_mut(ids.as_mut_ptr() as *mut Id<T>, ids.len()) }
    }

    #[inline]
    pub fn builder<T>() -> SliceBuilder<T> {
        SliceBuilder {
            buffer: Vec::new(),
            t: PhantomData,
        }
    }
}

/// Helper builder for incrementally constructing a `SliceId<T>`.
pub struct SliceBuilder<T> {
    buffer: Vec<u32>,
    t: PhantomData<T>,
}

impl<T> SliceBuilder<T> {
    #[inline]
    pub fn push(&mut self, id: Id<T>) {
        self.buffer.push(id.id());
    }

    #[inline]
    #[must_use]
    pub fn finish(mut self, storage: &mut SliceStorage) -> SliceId<T> {
        let start = storage.0.len() as u32;
        let len = self.buffer.len() as u32;
        storage.0.append(&mut self.buffer);
        SliceId::new(start, len)
    }
}

impl<T> Clone for SliceBuilder<T> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            buffer: self.buffer.clone(),
            t: PhantomData,
        }
    }
}

impl<T> std::fmt::Debug for SliceBuilder<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "SliceBuilder<{}>({})",
            std::any::type_name::<T>(),
            self.buffer.len()
        )
    }
}

impl<T: 'static> SliceRelation<T> for SliceStorage {
    #[inline]
    fn get_slice(&self, id: SliceId<T>) -> &[Id<T>] {
        self.get(id)
    }
}
