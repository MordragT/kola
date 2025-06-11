/// A stack-based map with variable shadowing and efficient LIFO removal operations.
///
/// `ShadowStack<K, V>` maintains a sorted array for fast lookups combined with
/// a stack for LIFO removal semantics.
///
/// ## Key Properties
///
/// - **Sorted storage**: Enables binary search lookups
/// - **LIFO semantics**: Most recent bindings can be efficiently removed
/// - **Shadowing support**: Duplicate keys allowed, lookups return newest binding
/// - **Ordered iteration**: Keys maintain sorted order
///
/// ## Time Complexity
///
/// | Operation          | Average Case | Worst Case | Notes                           |
/// |--------------------|--------------|------------|---------------------------------|
/// | `push(k, v)`       | O(n)         | O(n)       | Binary search + array insertion |
/// | `pop()`            | O(n)         | O(n)       | Array removal with shifting     |
/// | `get(k)`           | O(log n)     | O(n)       | Binary search + duplicate scan  |
/// | `contains_key(k)`  | O(log n)     | O(n)       | Same as `get(k)`                |
/// | `len()`, `clear()` | O(1)         | O(1)       | Vector operations               |
///
/// **Note**: `get()` worst case occurs when all entries share the same key.
/// For typical lexical scoping (few duplicates per key), performance is O(log n).
///
/// ## Comparison with Alternatives
///
/// | Data Structure     | Push     | Pop      | Lookup    | Use Case                         |
/// |--------------------|----------|----------|-----------|----------------------------------|
/// | `ShadowStack`      | O(n)     | O(n)     | O(log n)* | Mixed operations with shadowing  |
/// | `Vec<(K,V)>`       | O(1)     | O(1)     | O(n)      | Push/pop-heavy, rare lookups     |
/// | `BTreeMap` + stack | O(log n) | O(log n) | O(log n)  | No shadowing support             |
/// | `HashMap` + stack  | O(1)     | O(1)     | O(1)      | No ordering, no shadowing        |
///
/// *Average case; O(n) worst case with many duplicate keys.
///
/// # Examples
///
/// ```rust
/// use kola_collections::ShadowStack;
///
/// let mut stack = ShadowStack::new();
///
/// // Push bindings - newer bindings shadow older ones
/// stack.push("x", 1);
/// stack.push("x", 2);  // Shadows previous "x"
/// stack.push("y", 3);
///
/// assert_eq!(stack.get(&"x"), Some(&2)); // Returns newest binding
/// assert_eq!(stack.get(&"y"), Some(&3));
///
/// // LIFO removal restores previous bindings
/// assert_eq!(stack.pop(), Some(("y", 3)));
/// assert_eq!(stack.pop(), Some(("x", 2)));
/// assert_eq!(stack.get(&"x"), Some(&1)); // Original "x" restored
/// ```
pub struct ShadowStack<K, V> {
    pub entries: Vec<(K, V)>,
    pub indices: Vec<usize>,
}

impl<K, V> ShadowStack<K, V> {
    /// Creates an empty `ShadowStack`.
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            indices: Vec::new(),
        }
    }

    /// Creates an empty `ShadowStack` with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            entries: Vec::with_capacity(capacity),
            indices: Vec::with_capacity(capacity),
        }
    }

    /// Returns the number of elements in the stack.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns `true` if the stack contains no elements.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Removes all key-value pairs from the stack.
    pub fn clear(&mut self) {
        self.entries.clear();
        self.indices.clear();
    }

    /// Returns the first key-value pair in the stack, if any.
    pub fn first(&self) -> Option<&(K, V)> {
        self.indices
            .first()
            .and_then(|&index| self.entries.get(index))
    }

    /// Returns the last key-value pair in the stack, if any.
    pub fn last(&self) -> Option<&(K, V)> {
        self.indices
            .last()
            .and_then(|&index| self.entries.get(index))
    }

    /// Returns an iterator over the keys of the stack.
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.entries.iter().map(|(k, _)| k)
    }

    /// Returns an iterator over the values of the stack.
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.entries.iter().map(|(_, v)| v)
    }

    /// Returns a mutable iterator over the values of the stack.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.entries.iter_mut().map(|(_, v)| v)
    }

    /// Returns an iterator over the key-value pairs of the stack.
    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.entries.iter()
    }

    /// Returns the capacity of the stack.
    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    /// Reserves capacity for at least `additional` more elements.
    pub fn reserve(&mut self, additional: usize) {
        self.entries.reserve(additional);
        self.indices.reserve(additional);
    }
}

impl<K, V> ShadowStack<K, V>
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

    /// Inserts a key-value pair into the stack.
    ///
    /// The key is inserted at its correct position to maintain ordering.
    ///
    /// ## Index Invalidation Safety
    ///
    /// When inserting into the middle of `entries`, elements shift right and invalidate
    /// some indices in the `indices` stack. However, this is safe because:
    ///
    /// 1. **LIFO constraint**: Only the topmost (most recent) element can be popped
    /// 2. **Topmost index is always valid**: The last pushed index always points to
    ///    the correct location of the most recently inserted element
    /// 3. **Self-correcting on pop**: When the topmost element is removed, the
    ///    remaining indices automatically become valid again due to the left-shift
    ///
    /// Example:
    /// ```text
    /// Initial:    entries=[("a",1), ("c",3)]  indices=[0, 1]
    /// Push("b"):  entries=[("a",1), ("b",2), ("c",3)]  indices=[0, 1, 1]
    ///             // Note: indices[1] now points to ("b",2) instead of ("c",3)
    /// Pop():      entries=[("a",1), ("c",3)]  indices=[0, 1]
    ///             // After removal, indices[1] correctly points to ("c",3) again
    /// ```
    pub fn push(&mut self, key: K, value: V) {
        let idx = match self.binary_search(&key) {
            Ok(idx) => idx,  // Key exists, insert after the last occurrence
            Err(idx) => idx, // Key doesn't exist, insert at computed position
        };
        self.entries.insert(idx, (key, value));
        self.indices.push(idx);
    }

    /// Removes and returns the most recently pushed key-value pair, if any.
    pub fn pop(&mut self) -> Option<(K, V)> {
        self.indices.pop().map(|idx| self.entries.remove(idx))
    }

    /// Gets a reference to the value corresponding to the key.
    ///
    /// If there are multiple values with the same key, returns the newest one found.
    pub fn get(&self, key: &K) -> Option<&V> {
        match self.binary_search(key) {
            Ok(idx) => Some(&self.entries[idx].1),
            Err(_) => None,
        }
    }

    /// Gets a mutable reference to the value corresponding to the key.
    ///
    /// If there are multiple values with the same key, returns the newest one found.
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        match self.binary_search(key) {
            Ok(idx) => Some(&mut self.entries[idx].1),
            Err(_) => None,
        }
    }

    /// Returns `true` if the stack contains the specified key.
    pub fn contains_key(&self, key: &K) -> bool {
        self.binary_search(key).is_ok()
    }
}

impl<K, V> Default for ShadowStack<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> FromIterator<(K, V)> for ShadowStack<K, V>
where
    K: Ord,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut stack = Self::new();
        for (k, v) in iter {
            stack.push(k, v);
        }
        stack
    }
}

impl<K, V> Extend<(K, V)> for ShadowStack<K, V>
where
    K: Ord,
{
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (k, v) in iter {
            self.push(k, v);
        }
    }
}

impl<K, V> IntoIterator for ShadowStack<K, V> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a ShadowStack<K, V> {
    type Item = &'a (K, V);
    type IntoIter = std::slice::Iter<'a, (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.iter()
    }
}
