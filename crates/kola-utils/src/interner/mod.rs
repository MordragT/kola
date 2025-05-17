use std::borrow::{Borrow, Cow, ToOwned};
use std::collections::HashMap;
use std::fmt;
use std::hash::{BuildHasher, Hash, RandomState};
use std::ops::Index;

mod path;
mod str;

pub use path::{PathInterner, PathKey};
pub use str::{StrInterner, StrKey};

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

    /// Maps from static references to indices
    /// Uses the borrowed form for lookups
    map: HashMap<&'static B, usize, S>,
}

impl<B, S> fmt::Debug for Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + fmt::Debug + 'static,
    <B as ToOwned>::Owned: Borrow<B> + fmt::Debug + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Interner")
            .field("values", &self.values)
            .field("map", &self.map)
            .finish()
    }
}

impl<B> Default for Interner<B>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    fn default() -> Self {
        Self {
            values: Vec::new(),
            map: HashMap::new(),
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
            map: self.map.clone(),
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
        self.values == other.values && self.map == other.map
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
    /// Creates a new empty interner.
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            map: HashMap::new(),
        }
    }

    /// Creates a new empty interner with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
            map: HashMap::with_capacity(capacity),
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
            map: HashMap::with_hasher(hasher),
        }
    }

    /// Creates a new empty interner with the specified capacity and hasher.l
    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self {
            values: Vec::with_capacity(capacity),
            map: HashMap::with_capacity_and_hasher(capacity, hasher),
        }
    }

    /// Interns a value, returning its index.
    ///
    /// Accepts either borrowed or owned values via `Cow`.
    /// If the value is already interned, returns its existing index.
    /// Otherwise, adds the owned value to the interner and returns the new index.
    pub fn intern<'a>(&mut self, value: impl Into<Cow<'a, B>>) -> usize {
        let value = value.into();

        // First check if an equivalent value already exists
        // This works with the borrowed form without any conversion
        let borrowed: &B = value.borrow();

        if let Some(&idx) = self.map.get(borrowed) {
            return idx;
        }

        // Convert to owned if it's not already
        let owned = value.into_owned();

        // Value not found, add it to our values
        let idx = self.values.len();
        self.values.push(owned);

        // Get a reference to the value we just added and convert it to a static reference
        // SAFETY: This is safe because:
        // 1. The value exists in our values vector
        // 2. The values vector is never shrunk or emptied during the interner's lifetime
        // 3. The references are only used within the lifetime of the interner itself
        let borrowed_ref = self.values.last().unwrap().borrow();
        let static_ref: &'static B = unsafe { std::mem::transmute::<&B, &'static B>(borrowed_ref) };

        // Store the reference -> index mapping
        self.map.insert(static_ref, idx);

        idx
    }

    /// Gets a reference to a value by its index.
    ///
    /// Returns `None` if the index is out of bounds.
    pub fn get(&self, idx: usize) -> Option<&<B as ToOwned>::Owned> {
        self.values.get(idx)
    }

    /// Checks if the interner contains a value.
    pub fn contains(&self, value: &B) -> bool {
        self.map.contains_key(value)
    }

    /// Looks up a value in the interner.
    ///
    /// If the value is interned, returns its index.
    /// Otherwise, returns None.
    pub fn lookup(&self, value: &B) -> Option<usize> {
        self.map.get(value).copied()
    }

    /// Returns the number of unique values in the interner.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns the total capacity of the interner.
    pub fn capacity(&self) -> usize {
        self.values.capacity().min(self.map.capacity())
    }

    /// Returns `true` if the interner contains no values.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Reserves capacity for at least `additional` more items.
    pub fn reserve(&mut self, additional: usize) {
        self.values.reserve(additional);
        self.map.reserve(additional);
    }

    /// Returns an iterator over all values in the interner.
    pub fn iter(&self) -> impl Iterator<Item = &<B as ToOwned>::Owned> {
        self.values.iter()
    }

    /// Shrinks the capacity of the interner to fit its current length.
    ///
    /// Note that this never removes any values, it only reclaims unused capacity.
    pub fn shrink_to_fit(&mut self) {
        self.values.shrink_to_fit();
        self.map.shrink_to_fit();
    }
}

// Implement Index trait for convenient access by index
impl<B, S> Index<usize> for Interner<B, S>
where
    B: ?Sized + ToOwned + Eq + Hash + 'static,
    <B as ToOwned>::Owned: Borrow<B> + 'static,
{
    type Output = <B as ToOwned>::Owned;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.values[idx]
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
