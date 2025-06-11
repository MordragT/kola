/// A stack-based map that maintains insertion order with LIFO semantics and allows duplicate keys.
///
/// `StackMap<K, V>` is essentially a `Vec<(K, V)>` with map-like operations, optimized for
/// scenarios where push/pop operations are frequent and lookups are moderately common.
/// Most recent bindings shadow older ones, making it ideal for lexical scoping and
/// variable binding contexts.
///
/// ## Key Properties
///
/// - **LIFO semantics**: Push/pop operations work on the most recent end
/// - **Duplicate keys**: Multiple values can be associated with the same key
/// - **Insertion order**: Maintains the order in which items were added
/// - **Shadowing**: Lookups return the most recently inserted value for a key
/// - **Memory efficient**: Single contiguous allocation with excellent cache locality
///
/// ## Time Complexity
///
/// | Operation           | Complexity     | Notes                                       |
/// |---------------------|----------------|---------------------------------------------|
/// | `push(k, v)`        | O(1) amortized | Append to end of vector                     |
/// | `pop()`             | O(1)           | Remove from end of vector                   |
/// | `get_first(k)`      | O(n)           | Linear search from start, early termination |
/// | `get_last(k)`       | O(n)           | Linear search from end, early termination   |
/// | `contains_key(k)`   | O(n)           | Linear search, early termination            |
/// | `remove_first(k)`   | O(n)           | Linear search + array shifting              |
/// | `remove_last(k)`    | O(n)           | Linear search + array shifting              |
/// | `remove_all(k)`     | O(n)           | Single pass with drain, preserves order     |
/// | `len()`, `clear()`  | O(1)           | Direct vector operations                    |
///
/// **Performance Notes**:
/// - Lookups are O(n) but benefit from cache locality and early termination
/// - Recent bindings are found quickly due to reverse search order
/// - Push/pop operations are highly optimized for LIFO workloads
///
/// ## Comparison with Alternatives
///
/// | Data Structure        | Push     | Pop      | Lookup   | Memory    | Best For                              |
/// |-----------------------|----------|----------|----------|-----------|---------------------------------------|
/// | `StackMap`            | O(1)     | O(1)     | O(n)     | Excellent | Push/pop heavy, moderate lookups      |
/// | `HashMap<K, Vec<V>>`  | O(1)     | O(1)     | O(1)     | Good      | Lookup heavy workloads                |
/// | `BTreeMap<K, Vec<V>>` | O(log n) | O(log n) | O(log n) | Good      | Ordered iteration + fast lookups      |
/// | `Vec<(K, V)>`         | O(1)     | O(1)     | O(n)     | Excellent | Same as StackMap but no map API       |
/// | `ShadowStack`         | O(n)     | O(n)     | O(log n) | Good      | Mixed ops, needs ordering             |
///
/// # Examples
///
/// ```rust
/// use kola_collections::StackMap;
///
/// let mut scope = StackMap::new();
///
/// // Bind variables with shadowing support
/// scope.push("x", 1);
/// scope.push("y", 2);
/// scope.push("x", 3);  // Shadows previous "x"
///
/// assert_eq!(scope.get(&"x"), Some(&3)); // Most recent binding
/// assert_eq!(scope.get(&"y"), Some(&2));
/// assert_eq!(scope.len(), 3);
///
/// // LIFO removal for scope exit
/// assert_eq!(scope.pop(), Some(("x", 3)));
/// assert_eq!(scope.get(&"x"), Some(&1)); // Previous binding restored
///
/// // Efficient bulk operations
/// scope.extend([("a", 10), ("b", 20)]);
/// assert_eq!(scope.len(), 4);
/// ```
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackMap<K, V>(Vec<(K, V)>);

impl<K, V> StackMap<K, V> {
    /// Creates a new empty `StackMap`.
    #[inline]
    pub fn new() -> Self {
        StackMap(Vec::new())
    }

    /// Creates a new `StackMap` with the specified capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        StackMap(Vec::with_capacity(capacity))
    }

    /// Pushes a key-value pair onto the stack.
    #[inline]
    pub fn push(&mut self, key: K, value: V) {
        self.0.push((key, value));
    }

    /// Removes and returns the most recently pushed key-value pair.
    #[inline]
    pub fn pop(&mut self) -> Option<(K, V)> {
        self.0.pop()
    }

    /// Returns the number of key-value pairs in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the map contains no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns the capacity of the underlying vector.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Clears the map, removing all key-value pairs.
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Reserves capacity for at least `additional` more elements.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional);
    }

    /// Shrinks the capacity as much as possible.
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit();
    }

    /// Returns an iterator over the key-value pairs in insertion order.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.0.iter().map(|(k, v)| (k, v))
    }

    /// Returns a mutable iterator over the key-value pairs in insertion order.
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.0.iter_mut().map(|(k, v)| (&*k, v))
    }

    /// Returns an iterator over the keys in insertion order.
    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.0.iter().map(|(k, _)| k)
    }

    /// Returns an iterator over the values in insertion order.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.0.iter().map(|(_, v)| v)
    }

    /// Returns a mutable iterator over the values in insertion order.
    #[inline]
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.0.iter_mut().map(|(_, v)| v)
    }

    /// Returns a reference to the underlying vector.
    #[inline]
    pub fn as_slice(&self) -> &[(K, V)] {
        &self.0
    }

    /// Converts the `StackMap` into its underlying vector.
    #[inline]
    pub fn into_vec(self) -> Vec<(K, V)> {
        self.0
    }
}

impl<K, V> StackMap<K, V>
where
    K: PartialEq,
{
    #[inline]
    fn search_first(&self, key: &K) -> Option<usize> {
        self.0.iter().position(|(k, _)| k == key)
    }

    /// Searches for a key from the end (most recent) to the beginning.
    #[inline]
    fn search_last(&self, key: &K) -> Option<usize> {
        self.0.iter().rposition(|(k, _)| k == key)
    }

    /// Removes the first occurrence of a key and returns its value.
    /// This removes the earliest inserted value for the key.
    #[inline]
    pub fn remove_first(&mut self, key: &K) -> Option<V> {
        if let Some(pos) = self.search_first(key) {
            Some(self.0.remove(pos).1)
        } else {
            None
        }
    }

    /// Removes the last occurrence of a key and returns its value.
    /// This removes the most recently inserted value for the key.
    #[inline]
    pub fn remove_last(&mut self, key: &K) -> Option<V> {
        if let Some(pos) = self.search_last(key) {
            Some(self.0.remove(pos).1)
        } else {
            None
        }
    }

    /// Gets a reference to the first occurrence of the key.
    /// Returns the earliest inserted value if there are duplicates.
    #[inline]
    pub fn get_first(&self, key: &K) -> Option<&V> {
        self.0
            .iter()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    /// Gets a mutable reference to the first occurrence of the key.
    /// Returns the earliest inserted value if there are duplicates.
    #[inline]
    pub fn get_first_mut(&mut self, key: &K) -> Option<&mut V> {
        self.0
            .iter_mut()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    /// Gets a reference to the last occurrence of the key.
    /// Returns the most recently inserted value if there are duplicates.
    #[inline]
    pub fn get_last(&self, key: &K) -> Option<&V> {
        self.0
            .iter()
            .rev()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    /// Gets a mutable reference to the last occurrence of the key.
    /// Returns the most recently inserted value if there are duplicates.
    #[inline]
    pub fn get_last_mut(&mut self, key: &K) -> Option<&mut V> {
        self.0
            .iter_mut()
            .rev()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    /// Returns a reference to the first key-value pair.
    pub fn first(&self) -> Option<(&K, &V)> {
        self.0.first().map(|(k, v)| (k, v))
    }

    /// Returns a mutable reference to the first key-value pair.
    pub fn first_mut(&mut self) -> Option<(&K, &mut V)> {
        self.0.first_mut().map(|(k, v)| (&*k, v))
    }

    /// Returns a reference to the last key-value pair.
    pub fn last(&self) -> Option<(&K, &V)> {
        self.0.last().map(|(k, v)| (k, v))
    }

    /// Returns a mutable reference to the last key-value pair.
    pub fn last_mut(&mut self) -> Option<(&K, &mut V)> {
        self.0.last_mut().map(|(k, v)| (&*k, v))
    }

    /// Returns `true` if the map contains a value for the specified key.
    #[inline]
    pub fn contains_key(&self, key: &K) -> bool {
        self.0.iter().any(|(k, _)| k == key)
    }

    /// Retains only the elements specified by the predicate.
    #[inline]
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&K, &mut V) -> bool,
    {
        self.0.retain_mut(|(k, v)| f(k, v));
    }

    /// Returns all values associated with the given key.
    #[inline]
    pub fn get_all(&self, key: &K) -> impl Iterator<Item = &V> {
        self.0
            .iter()
            .filter_map(move |(k, v)| if k == key { Some(v) } else { None })
    }

    /// Removes all entries with the given key and returns their values in insertion order.
    pub fn remove_all(&mut self, key: &K) -> Vec<V> {
        let mut removed = Vec::new();
        let mut kept = Vec::with_capacity(self.0.len());

        // Single pass: separate matching and non-matching items
        for (k, v) in self.0.drain(..) {
            if &k == key {
                removed.push(v);
            } else {
                kept.push((k, v));
            }
        }

        // Replace the vector with kept items
        self.0 = kept;
        removed
    }
}

impl<K, V> Default for StackMap<K, V> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> std::fmt::Debug for StackMap<K, V>
where
    K: std::fmt::Debug,
    V: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<K, V> From<Vec<(K, V)>> for StackMap<K, V> {
    #[inline]
    fn from(vec: Vec<(K, V)>) -> Self {
        StackMap(vec)
    }
}

impl<K, V> FromIterator<(K, V)> for StackMap<K, V> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        StackMap(iter.into_iter().collect())
    }
}

impl<K, V> IntoIterator for StackMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a StackMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = std::iter::Map<std::slice::Iter<'a, (K, V)>, fn(&'a (K, V)) -> (&'a K, &'a V)>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().map(|(k, v)| (k, v))
    }
}

impl<'a, K, V> IntoIterator for &'a mut StackMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter =
        std::iter::Map<std::slice::IterMut<'a, (K, V)>, fn(&'a mut (K, V)) -> (&'a K, &'a mut V)>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut().map(|(k, v)| (&*k, v))
    }
}

impl<K, V> Extend<(K, V)> for StackMap<K, V> {
    #[inline]
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}
