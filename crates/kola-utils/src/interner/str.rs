use std::borrow::Cow;
use std::fmt;
use std::hash::{BuildHasher, RandomState};
use std::ops::Index;

use serde::{Deserialize, Serialize};

// pub static STR_INTERNER: RwLock<StrInterner<BuildHasherDefault<DefaultHasher>>> =
//     RwLock::new(StrInterner::with_hasher(BuildHasherDefault::new()));

use super::Interner;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct StrKey(usize);

impl fmt::Display for StrKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'s{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StrInterner<S: BuildHasher = RandomState>(Interner<str, S>);

impl Default for StrInterner {
    fn default() -> Self {
        Self(Interner::default())
    }
}

impl StrInterner {
    /// Creates a new `StrInterner` with the default hasher.
    pub fn new() -> Self {
        Self(Interner::new())
    }

    /// Creates a new `StrInterner` with the given capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Interner::with_capacity(capacity))
    }
}

impl<S: BuildHasher> StrInterner<S> {
    /// Creates a new `StrInterner` with the given hasher.
    pub const fn with_hasher(hasher: S) -> Self {
        Self(Interner::with_hasher(hasher))
    }

    /// Creates a new `StrInterner` with the given capacity and hasher.
    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self(Interner::with_capacity_and_hasher(capacity, hasher))
    }

    /// Interns a string, returning its key.
    ///
    /// If the string is already interned, returns its existing key.
    /// Otherwise, adds the string to the interner and returns the new key.
    pub fn intern<'a>(&mut self, value: impl Into<Cow<'a, str>>) -> StrKey {
        StrKey(self.0.intern(value))
    }

    /// Gets a reference to a string by its key.
    ///
    /// Returns `None` if the key is invalid.
    pub fn get(&self, key: StrKey) -> Option<&str> {
        self.0.get(key.0).map(|s| s.as_str())
    }

    /// Checks if the interner contains a string with the given key.
    pub fn contains(&self, value: &str) -> bool {
        self.0.contains(value)
    }

    /// Looks up a string in the interner.
    ///
    /// If the string is interned, returns its key.
    /// Otherwise, returns None.
    pub fn lookup(&self, value: &str) -> Option<StrKey> {
        self.0.lookup(value).map(StrKey)
    }

    /// Returns the number of unique strings in the interner.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns the total capacity of the interner.
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Returns `true` if the interner contains no strings.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Reserves capacity for at least `additional` more strings.
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    /// Returns an iterator over all strings in the interner.
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.0.iter().map(|s| s.as_str())
    }

    /// Shrinks the capacity of the interner to fit its current length.
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }
}

// Implement Index trait for convenient access by StrKey
impl<S: BuildHasher> Index<StrKey> for StrInterner<S> {
    type Output = str;

    fn index(&self, key: StrKey) -> &Self::Output {
        &self.0[key.0].as_str()
    }
}
