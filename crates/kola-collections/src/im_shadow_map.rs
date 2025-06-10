use crate::im_vec::ImVec;

pub use crate::im_vec::{IntoIter, Iter};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImShadowMap<K, V> {
    entries: ImVec<(K, V)>,
}

impl<K, V> ImShadowMap<K, V> {
    /// Creates an empty `OrdMap`.
    pub fn new() -> Self {
        ImShadowMap {
            entries: ImVec::new(),
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

    /// Returns the first key-value pair in the map, if any.
    pub fn first(&self) -> Option<&(K, V)> {
        self.entries.front()
    }

    /// Returns the last key-value pair in the map, if any.
    pub fn last(&self) -> Option<&(K, V)> {
        self.entries.back()
    }

    /// Returns an iterator over the keys of the map.
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.entries.iter().map(|(k, _)| k)
    }

    /// Returns an iterator over the values of the map.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.entries.iter().map(|(_, v)| v)
    }

    /// Returns an iterator over the key-value pairs of the map.
    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.entries.iter()
    }

    /// Removes all key-value pairs from the map.
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

impl<K, V> ImShadowMap<K, V>
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

    /// Gets a reference to the value corresponding to the key.
    ///
    /// If there are multiple values with the same key, returns the first one found.
    pub fn get(&self, key: &K) -> Option<&V> {
        match self.binary_search(key) {
            Ok(idx) => Some(&self.entries[idx].1),
            Err(_) => None,
        }
    }

    /// Returns `true` if the map contains the specified key.
    pub fn contains_key(&self, key: &K) -> bool {
        self.binary_search(key).is_ok()
    }
}

impl<K, V> ImShadowMap<K, V>
where
    K: Clone + Ord,
    V: Clone,
{
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

    /// Creates a new `ImShadowMap` with the specified key-value pair.
    pub fn update(&self, key: K, value: V) -> Self {
        let mut new_map = self.clone();
        new_map.insert(key, value);
        new_map
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

    /// Returns a mutable iterator over the values of the map.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.entries.iter_mut().map(|(_, v)| v)
    }

    // /// Returns a mutable iterator over the key-value pairs of the map.
    // pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut (K, V)> {
    //     self.entries.iter_mut()
    // }
}

impl<K, V> Default for ImShadowMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> FromIterator<(K, V)> for ImShadowMap<K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut map = ImShadowMap::new();
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

impl<K, V> Extend<(K, V)> for ImShadowMap<K, V>
where
    K: Ord + Clone,
    V: Clone,
{
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl<K, V> IntoIterator for ImShadowMap<K, V>
where
    K: Clone,
    V: Clone,
{
    type Item = (K, V);
    type IntoIter = IntoIter<(K, V), crate::Ptr>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a ImShadowMap<K, V> {
    type Item = &'a (K, V);
    type IntoIter = Iter<'a, (K, V), crate::Ptr>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.iter()
    }
}

// impl<'a, K, V> IntoIterator for &'a mut ImShadowMap<K, V>
// where
//     K: Clone,
//     V: Clone,
// {
//     type Item = &'a mut (K, V);
//     type IntoIter = imbl::vector::IterMut<'a, (K, V), crate::Ptr>;

//     fn into_iter(self) -> Self::IntoIter {
//         self.entries.iter_mut()
//     }
// }
