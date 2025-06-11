/// A simple ordered map implementation using a vector.
/// Duplicate keys are explicitly allowed.
///
/// Keys are maintained in sorted order with binary search.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ShadowMap<K, V> {
    entries: Vec<(K, V)>,
}

// Basic methods that don't require Ord
impl<K, V> ShadowMap<K, V> {
    /// Creates an empty `ShadowMap`.
    pub fn new() -> Self {
        ShadowMap {
            entries: Vec::new(),
        }
    }

    /// Creates an empty `ShadowMap` with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        ShadowMap {
            entries: Vec::with_capacity(capacity),
        }
    }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns `true` if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Removes all key-value pairs from the map.
    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Returns an iterator over the keys of the map.
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.entries.iter().map(|(k, _)| k)
    }

    /// Returns an iterator over the values of the map.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.entries.iter().map(|(_, v)| v)
    }

    /// Returns a mutable iterator over the values of the map.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.entries.iter_mut().map(|(_, v)| v)
    }

    /// Returns an iterator over the key-value pairs of the map.
    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.entries.iter()
    }

    /// Returns the capacity of the map.
    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    /// Reserves capacity for at least `additional` more elements.
    pub fn reserve(&mut self, additional: usize) {
        self.entries.reserve(additional);
    }
}

impl<K, V> ShadowMap<K, V>
where
    K: Ord,
{
    /// Performs a binary search for the specified key.
    ///
    /// If the key is found, returns the index of the first occurrence.
    /// If the key is not found, returns the index where it can be inserted.
    pub fn binary_search(&self, key: &K) -> Result<usize, usize> {
        let mut idx = self.entries.binary_search_by(|(k, _)| k.cmp(key))?;
        while idx > 0 && self.entries[idx - 1].0 == *key {
            idx -= 1; // Move to the first occurrence
        }
        Ok(idx)
    }

    /// Inserts a key-value pair into the map.
    ///
    /// The key is inserted at its correct position to maintain ordering.
    pub fn insert(&mut self, key: K, value: V) {
        let idx = match self.binary_search(&key) {
            Ok(idx) => idx,  // Key exists, insert after the last occurrence
            Err(idx) => idx, // Key doesn't exist, insert at computed position
        };
        self.entries.insert(idx, (key, value));
    }

    /// Gets a reference to the value corresponding to the key.
    ///
    /// If there are multiple values with the same key, returns the first one found.
    pub fn get(&self, key: &K) -> Option<&V> {
        match self.binary_search(key) {
            Ok(idx) => Some(&self.entries[idx].1),
            Err(_) => None,
        }
    }

    /// Gets a mutable reference to the value corresponding to the key.
    ///
    /// If there are multiple values with the same key, returns the first one found.
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        match self.binary_search(key) {
            Ok(idx) => Some(&mut self.entries[idx].1),
            Err(_) => None,
        }
    }

    /// Returns `true` if the map contains the specified key.
    pub fn contains_key(&self, key: &K) -> bool {
        self.binary_search(key).is_ok()
    }

    /// Removes a key-value pair from the map.
    ///
    /// If the map contains multiple pairs with the same key, removes the first one.
    /// Returns the value associated with the key if it was present in the map.
    pub fn remove(&mut self, key: &K) -> Option<V> {
        match self.binary_search(key) {
            Ok(idx) => Some(self.entries.remove(idx).1),
            Err(_) => None,
        }
    }
}

impl<K, V> Default for ShadowMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> FromIterator<(K, V)> for ShadowMap<K, V>
where
    K: Ord,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut map = ShadowMap::new();
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

impl<K, V> Extend<(K, V)> for ShadowMap<K, V>
where
    K: Ord,
{
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl<K, V> IntoIterator for ShadowMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a ShadowMap<K, V> {
    type Item = &'a (K, V);
    type IntoIter = std::slice::Iter<'a, (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.iter()
    }
}
