use std::collections::HashSet;

/// Iterator that yields only visible (non-shadowed) entries from a ShadowMap
pub struct VisibleIter<'a, K, V> {
    inner: std::slice::Iter<'a, (K, V)>,
    seen_keys: HashSet<&'a K>,
}

impl<'a, K, V> VisibleIter<'a, K, V> {
    fn new(entries: &'a [(K, V)]) -> Self {
        Self {
            inner: entries.iter(),
            seen_keys: HashSet::new(),
        }
    }
}

impl<'a, K, V> Iterator for VisibleIter<'a, K, V>
where
    K: Eq + std::hash::Hash,
{
    type Item = &'a (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(entry) = self.inner.next() {
            if self.seen_keys.insert(&entry.0) {
                return Some(entry);
            }
        }
        None
    }
}

/// Iterator that yields only visible (non-shadowed) entries from a ShadowMap by value
pub struct VisibleIntoIter<K, V> {
    inner: std::vec::IntoIter<(K, V)>,
    seen_keys: HashSet<K>,
}

impl<K, V> VisibleIntoIter<K, V> {
    fn new(entries: Vec<(K, V)>) -> Self {
        Self {
            inner: entries.into_iter(),
            seen_keys: HashSet::new(),
        }
    }
}

impl<K, V> Iterator for VisibleIntoIter<K, V>
where
    K: Eq + std::hash::Hash + Clone,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(entry) = self.inner.next() {
            if self.seen_keys.insert(entry.0.clone()) {
                return Some(entry);
            }
        }
        None
    }
}

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

    /// Returns an iterator over the visible keys of the map (excluding shadowed duplicates).
    pub fn keys(&self) -> impl Iterator<Item = &K>
    where
        K: Eq + std::hash::Hash,
    {
        self.iter().map(|(k, _)| k)
    }

    /// Returns an iterator over all keys of the map (including shadowed duplicates).
    pub fn keys_all(&self) -> impl Iterator<Item = &K> {
        self.entries.iter().map(|(k, _)| k)
    }

    /// Returns an iterator over the visible values of the map (excluding shadowed duplicates).
    pub fn values(&self) -> impl Iterator<Item = &V>
    where
        K: Eq + std::hash::Hash,
    {
        self.iter().map(|(_, v)| v)
    }

    /// Returns an iterator over all values of the map (including shadowed duplicates).
    pub fn values_all(&self) -> impl Iterator<Item = &V> {
        self.entries.iter().map(|(_, v)| v)
    }

    /// Returns a mutable iterator over the visible values of the map (excluding shadowed duplicates).
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V>
    where
        K: Eq + std::hash::Hash + Clone,
        V: Clone,
    {
        // Collect visible indices first
        let mut seen_keys = HashSet::new();
        let visible_indices: Vec<usize> = self.entries
            .iter()
            .enumerate()
            .filter_map(|(i, (k, _))| {
                if seen_keys.insert(k) {
                    Some(i)
                } else {
                    None
                }
            })
            .collect();

        // Clear and rebuild with only visible entries
        let mut new_entries = Vec::new();
        for &idx in &visible_indices {
            new_entries.push(self.entries[idx].clone());
        }
        self.entries = new_entries;

        // Return mutable iterator over all entries (now only visible ones)
        self.entries.iter_mut().map(|(_, v)| v)
    }

    /// Returns a mutable iterator over all values of the map (including shadowed duplicates).
    pub fn values_mut_all(&mut self) -> impl Iterator<Item = &mut V> {
        self.entries.iter_mut().map(|(_, v)| v)
    }

    /// Returns an iterator over the visible (non-shadowed) key-value pairs of the map.
    /// For keys that appear multiple times, only the first occurrence (most recent) is included.
    pub fn iter(&self) -> VisibleIter<K, V>
    where
        K: Eq + std::hash::Hash,
    {
        VisibleIter::new(&self.entries)
    }

    /// Returns an iterator over all key-value pairs of the map, including shadowed values.
    /// This returns all entries in the map in sorted order by key.
    pub fn iter_all(&self) -> impl Iterator<Item = &(K, V)> {
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
    K: Ord + Clone,
    V: Clone,
{
    /// Returns an iterator that consumes the map and yields only visible (non-shadowed) entries.
    pub fn into_iter_all(self) -> std::vec::IntoIter<(K, V)> {
        self.entries.into_iter()
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

impl<K, V> IntoIterator for ShadowMap<K, V>
where
    K: Eq + std::hash::Hash + Clone,
{
    type Item = (K, V);
    type IntoIter = VisibleIntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        VisibleIntoIter::new(self.entries)
    }
}

impl<'a, K, V> IntoIterator for &'a ShadowMap<K, V>
where
    K: Eq + std::hash::Hash,
{
    type Item = &'a (K, V);
    type IntoIter = VisibleIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shadowing_iterators() {
        let mut map = ShadowMap::new();
        
        // Insert some values with duplicate keys
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("a", 10); // This should shadow the first "a"
        map.insert("c", 3);
        map.insert("b", 20); // This should shadow the first "b"

        // Test iter() - should only return visible (non-shadowed) values
        let visible: Vec<_> = map.iter().collect();
        assert_eq!(visible.len(), 3);
        let mut found_a = false;
        let mut found_b = false;
        let mut found_c = false;
        for (k, v) in visible {
            match *k {
                "a" => { assert_eq!(*v, 10); found_a = true; },
                "b" => { assert_eq!(*v, 20); found_b = true; },
                "c" => { assert_eq!(*v, 3); found_c = true; },
                _ => panic!("Unexpected key"),
            }
        }
        assert!(found_a && found_b && found_c);

        // Test iter_all() - should return all values including shadowed
        let all: Vec<_> = map.iter_all().collect();
        assert_eq!(all.len(), 5);

        // Test keys() - should only return visible keys
        let visible_keys: Vec<_> = map.keys().collect();
        assert_eq!(visible_keys.len(), 3);
        let key_set: std::collections::HashSet<_> = visible_keys.into_iter().collect();
        assert!(key_set.contains(&"a"));
        assert!(key_set.contains(&"b"));
        assert!(key_set.contains(&"c"));

        // Test keys_all() - should return all keys including duplicates
        let all_keys: Vec<_> = map.keys_all().collect();
        assert_eq!(all_keys.len(), 5);

        // Test values() - should only return visible values
        let visible_values: Vec<_> = map.values().collect();
        assert_eq!(visible_values.len(), 3);
        let value_set: std::collections::HashSet<_> = visible_values.into_iter().collect();
        assert!(value_set.contains(&10));
        assert!(value_set.contains(&20));
        assert!(value_set.contains(&3));

        // Test values_all() - should return all values including shadowed
        let all_values: Vec<_> = map.values_all().collect();
        assert_eq!(all_values.len(), 5);
        let all_value_set: std::collections::HashSet<_> = all_values.into_iter().collect();
        assert!(all_value_set.contains(&1)); // shadowed value
        assert!(all_value_set.contains(&2)); // shadowed value
    }

    #[test]
    fn test_into_iter_shadowing() {
        let mut map = ShadowMap::new();
        map.insert("x", 100);
        map.insert("y", 200);
        map.insert("x", 300); // shadows first x

        // Test into_iter() - should only return visible entries
        let visible: Vec<_> = map.clone().into_iter().collect();
        assert_eq!(visible.len(), 2);
        let visible_set: std::collections::HashSet<_> = visible.into_iter().collect();
        assert!(visible_set.contains(&("x", 300)));
        assert!(visible_set.contains(&("y", 200)));

        // Test into_iter_all() - should return all entries
        let all: Vec<_> = map.into_iter_all().collect();
        assert_eq!(all.len(), 3);
    }

    #[test]
    fn test_values_mut_shadowing() {
        let mut map = ShadowMap::new();
        map.insert("a", 1);
        map.insert("b", 2);
        map.insert("a", 10); // shadows first a

        // Test values_mut() - should only return mutable references to visible values
        // Note: values_mut() removes shadowed entries as a side effect
        let mut visible_count = 0;
        for value in map.values_mut() {
            *value += 1000;
            visible_count += 1;
        }
        assert_eq!(visible_count, 2); // Only visible values

        // Verify the changes were applied to visible values
        assert_eq!(map.get(&"a"), Some(&1010));
        assert_eq!(map.get(&"b"), Some(&1002));

        // After values_mut(), the map should only contain visible entries
        assert_eq!(map.len(), 2); // Shadowed entries removed
    }
}
