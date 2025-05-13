use std::borrow::Cow;
use std::fmt;
use std::hash::{BuildHasher, RandomState};
use std::ops::Index;

use camino::Utf8Path;
use serde::{Deserialize, Serialize};

use super::Interner;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PathKey(usize);

impl fmt::Display for PathKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'p{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathInterner<S: BuildHasher = RandomState>(Interner<Utf8Path, S>);

impl Default for PathInterner {
    fn default() -> Self {
        Self(Interner::default())
    }
}

impl PathInterner {
    /// Creates a new `PathInterner` with the default hasher.
    pub fn new() -> Self {
        Self(Interner::new())
    }

    /// Creates a new `PathInterner` with the given capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Interner::with_capacity(capacity))
    }
}

impl<S: BuildHasher> PathInterner<S> {
    /// Creates a new `PathInterner` with the given hasher.
    pub const fn with_hasher(hasher: S) -> Self {
        Self(Interner::with_hasher(hasher))
    }

    /// Creates a new `PathInterner` with the given capacity and hasher.
    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self(Interner::with_capacity_and_hasher(capacity, hasher))
    }

    /// Interns a path, returning its key.
    ///
    /// If the path is already interned, returns its existing key.
    /// Otherwise, adds the path to the interner and returns the new key.
    pub fn intern<'a>(&mut self, value: impl Into<Cow<'a, Utf8Path>>) -> PathKey {
        PathKey(self.0.intern(value))
    }

    /// Gets a reference to a path by its key.
    ///
    /// Returns `None` if the key is invalid.
    pub fn get(&self, key: PathKey) -> Option<&Utf8Path> {
        self.0.get(key.0).map(|p| p.as_path())
    }

    /// Checks if the interner contains a path.
    pub fn contains(&self, value: &Utf8Path) -> bool {
        self.0.contains(value)
    }

    /// Looks up a path in the interner.
    ///
    /// If the path is interned, returns its key.
    /// Otherwise, returns None.
    pub fn lookup(&self, value: &Utf8Path) -> Option<PathKey> {
        self.0.lookup(value).map(PathKey)
    }

    /// Returns the number of unique paths in the interner.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns the total capacity of the interner.
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Returns `true` if the interner contains no paths.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Reserves capacity for at least `additional` more paths.
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    /// Returns an iterator over all paths in the interner.
    pub fn iter(&self) -> impl Iterator<Item = &Utf8Path> {
        self.0.iter().map(|p| p.as_path())
    }

    /// Shrinks the capacity of the interner to fit its current length.
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }
}

// Implement Index trait for convenient access by PathKey
impl<S: BuildHasher> Index<PathKey> for PathInterner<S> {
    type Output = Utf8Path;

    fn index(&self, key: PathKey) -> &Self::Output {
        &self.0[key.0]
    }
}
