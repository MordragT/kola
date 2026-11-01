#![feature(const_cmp)]
#![feature(const_trait_impl)]

use std::borrow::{Borrow, Cow, ToOwned};
use std::fmt;
use std::hash::{BuildHasher, Hash, RandomState};
use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::ops::Index;

use camino::Utf8Path;
use hashbrown::HashTable;
use kola_utils::DisplayWith;
use rustc_hash::FxBuildHasher;

pub mod ext;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Key<T: ?Sized> {
    id: NonZeroU32,
    t: std::marker::PhantomData<T>,
}

const impl<T: ?Sized> PartialEq for Key<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: ?Sized> Clone for Key<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            t: std::marker::PhantomData,
        }
    }
}

impl<T: ?Sized> Copy for Key<T> {}

// impl<T: ?Sized> PartialEq for Key<T> {
//     fn eq(&self, other: &Self) -> bool {
//         self.id == other.id
//     }
// }

impl<T: ?Sized> Eq for Key<T> {}

impl<T: ?Sized> PartialOrd for Key<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for Key<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T: ?Sized> std::hash::Hash for Key<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T: ?Sized> std::fmt::Debug for Key<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", std::any::type_name::<T>(), self.id)
    }
}

impl<T: ?Sized> Key<T> {
    pub fn as_usize(&self) -> usize {
        self.id.get() as usize - 1
    }

    pub fn id(&self) -> u32 {
        self.id.get() - 1
    }

    const fn from_usize(key: usize) -> Self {
        Self {
            id: NonZeroU32::new((key as u32) + 1).expect("key overflow"),
            t: PhantomData,
        }
    }
}

impl<T: ?Sized> fmt::Display for Key<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

pub type PathKey = Key<Utf8Path>;
pub type PathInterner = Interner<Utf8Path, FxBuildHasher>;

pub type StrKey = Key<str>;
pub type StrInterner = Interner<str, FxBuildHasher>;

impl DisplayWith<StrInterner> for StrKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &StrInterner) -> fmt::Result {
        write!(f, "{}", interner[*self])
    }
}

impl DisplayWith<PathInterner> for PathKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, interner: &PathInterner) -> fmt::Result {
        write!(f, "{}", interner[*self])
    }
}

/// A flexible interner that efficiently stores unique values.
///
/// This implementation:
/// - Stores each unique value exactly once
/// - Supports efficient lookups with borrowed forms
/// - Accepts both borrowed and owned values via `Cow`
/// - Minimizes cloning by only creating owned values when necessary
///
/// # Type Parameters
///
/// - `B`: The borrowed type (e.g., `str`)
/// - `S`: The hasher type (default is `RandomState`)
///
/// The `B` type must be:
/// - ToOwned (can create an owned type from B)
/// - Eq + Hash (required for HashMap lookups)
pub struct Interner<B, S = RandomState>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    /// Storage for all unique values (owned form)
    values: Vec<<B as ToOwned>::Owned>,

    /// Hash index from value hashes to interned keys.
    index: HashTable<Key<B>>,

    /// Hasher used for value hashes.
    hasher: S,
}

impl<B, S> fmt::Debug for Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + fmt::Debug + 'static,
    <B as ToOwned>::Owned: Borrow<B> + fmt::Debug + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Interner")
            .field("values", &self.values)
            .field("index", &self.index)
            .finish()
    }
}

impl<B, S> Default for Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
    S: Default,
{
    fn default() -> Self {
        Self {
            values: Vec::new(),
            index: HashTable::new(),
            hasher: S::default(),
        }
    }
}

impl<B, S> Clone for Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + Clone + 'static,
    S: Clone,
{
    fn clone(&self) -> Self {
        Self {
            values: self.values.clone(),
            index: self.index.clone(),
            hasher: self.hasher.clone(),
        }
    }
}

impl<B, S> PartialEq for Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + PartialEq + 'static,
    S: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.values == other.values
    }
}

impl<B, S> Eq for Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + Eq + 'static,
    S: BuildHasher,
{
}

impl<B> Interner<B>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            index: HashTable::new(),
            hasher: RandomState::default(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
            index: HashTable::with_capacity(capacity),
            hasher: RandomState::default(),
        }
    }
}

impl<B, S> Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
    S: BuildHasher,
{
    /// Creates a new empty interner with the specified hasher.
    pub const fn with_hasher(hasher: S) -> Self {
        Self {
            values: Vec::new(),
            index: HashTable::new(),
            hasher,
        }
    }

    /// Creates a new empty interner with the specified capacity and hasher.l
    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
            index: HashTable::with_capacity(capacity),
            hasher,
        }
    }

    /// Interns a value, returning its index.
    ///
    /// Accepts either borrowed or owned values via `Cow`.
    /// If the value is already interned, returns its existing index.
    /// Otherwise, adds the owned value to the interner and returns the new index.
    pub fn intern<'a>(&mut self, value: impl Into<Cow<'a, B>>) -> Key<B> {
        let value = value.into();
        let Self {
            values,
            index,
            hasher,
        } = self;

        let borrowed: &B = value.borrow();
        let hash = hasher.hash_one(borrowed);

        if let Some(&key) = index.find(hash, |key| values[key.as_usize()].borrow() == borrowed) {
            return key;
        }

        let key = Key::from_usize(values.len());
        values.push(value.into_owned());
        index.insert_unique(hash, key, |key| {
            hasher.hash_one(values[key.as_usize()].borrow())
        });

        key
    }

    /// Gets a reference to a value by its index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn get(&self, key: Key<B>) -> Option<&B> {
        self.values.get(key.as_usize()).map(Borrow::borrow)
    }

    /// Checks if the interner contains a value.
    pub fn contains(&self, value: &B) -> bool {
        self.lookup(value).is_some()
    }

    /// Looks up a value in the interner.
    ///
    /// If the value is interned, returns its index.
    /// Otherwise, returns None.
    pub fn lookup(&self, value: &B) -> Option<Key<B>> {
        let hash = self.hasher.hash_one(value);
        self.index
            .find(hash, |key| self.values[key.as_usize()].borrow() == value)
            .copied()
    }

    /// Returns the number of unique values in the interner.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns the total capacity of the interner.
    pub fn capacity(&self) -> usize {
        self.values.capacity().min(self.index.capacity())
    }

    /// Returns `true` if the interner contains no values.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Reserves capacity for at least `additional` more items.
    pub fn reserve(&mut self, additional: usize) {
        let Self {
            values,
            index,
            hasher,
        } = self;
        values.reserve(additional);
        index.reserve(additional, |key| {
            hasher.hash_one(values[key.as_usize()].borrow())
        });
    }

    /// Returns an iterator over all values in the interner.
    pub fn iter(&self) -> impl Iterator<Item = &B> {
        self.values.iter().map(Borrow::borrow)
    }

    /// Shrinks the capacity of the interner to fit its current length.
    ///
    /// Note that this never removes any values, it only reclaims unused capacity.
    pub fn shrink_to_fit(&mut self) {
        let Self {
            values,
            index,
            hasher,
        } = self;
        values.shrink_to_fit();
        index.shrink_to_fit(|key| hasher.hash_one(values[key.as_usize()].borrow()));
    }
}

// Implement Index trait for convenient access by index
impl<B, S> Index<Key<B>> for Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    type Output = <B as ToOwned>::Owned;

    fn index(&self, key: Key<B>) -> &Self::Output {
        &self.values[key.as_usize()]
    }
}

impl<B, S> Index<&B> for Interner<B, S>
where
    S: BuildHasher,
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    type Output = Key<B>;

    fn index(&self, value: &B) -> &Self::Output {
        let hash = self.hasher.hash_one(value);
        self.index
            .find(hash, |key| self.values[key.as_usize()].borrow() == value)
            .unwrap_or_else(|| panic!("value is not interned"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_interning() {
        // Create string interner (B = str, <B as ToOwned>::Owned = String)
        let mut interner = Interner::new();

        // Intern using owned values
        let idx1 = interner.intern(String::from("hello"));
        let idx2 = interner.intern(String::from("world"));

        // Intern using borrowed values
        let idx3 = interner.intern("hello");

        // Check that equal values get the same index
        assert_eq!(idx1, idx3);
        assert_ne!(idx1, idx2);

        // Check that we can retrieve values
        assert_eq!(&interner[idx1], "hello");
        assert_eq!(&interner[idx2], "world");

        // Verify lookup works
        assert_eq!(interner.lookup("hello"), Some(idx1));
        assert_eq!(interner.lookup("missing"), None);
    }

    #[test]
    fn cow_flexibility() {
        let mut interner = Interner::new();

        // Different ways to intern the same string
        let s = "hello";

        // 1. As a borrowed Cow
        let idx1 = interner.intern(Cow::Borrowed(s));

        // 2. As an owned Cow
        let idx2 = interner.intern(Cow::Owned(String::from(s)));

        // 3. From a String reference
        let owned = String::from(s);
        let idx3 = interner.intern(Cow::Borrowed(owned.as_str()));

        // All should be the same index
        assert_eq!(idx1, idx2);
        assert_eq!(idx1, idx3);

        // The value should be stored correctly
        assert_eq!(&interner[idx1], "hello");
    }

    #[test]
    fn clone_owns_its_map_keys() {
        let mut interner = Interner::new();
        let hello = interner.intern("hello");

        let mut clone = interner.clone();
        drop(interner);

        assert_eq!(&clone[hello], "hello");
        assert_eq!(clone.lookup("hello"), Some(hello));

        let world = clone.intern("world");
        assert_eq!(&clone[world], "world");
    }

    #[test]
    fn test_many_values() {
        let mut interner = Interner::new();

        // Add many values
        let mut indices = Vec::new();
        for i in 0..50 {
            let s = i.to_string();

            // Alternate between borrowed and owned
            if i % 2 == 0 {
                indices.push(interner.intern(s.as_str()));
            } else {
                indices.push(interner.intern(s));
            }
        }

        assert_eq!(interner.len(), 50);

        // Check all values
        for (i, &idx) in indices.iter().enumerate() {
            assert_eq!(interner[idx], i.to_string());
        }
    }
}
