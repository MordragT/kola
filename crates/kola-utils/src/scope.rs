use std::fmt::Debug;

use kola_collections::StackMap;

/// A linear scope stack implementing single-binding lexical environments.
///
/// Implements the scope model where each variable introduction creates a new
/// scope frame containing exactly one binding. Used in languages where
/// variables are introduced individually rather than in declaration blocks.
///
/// ## Implementation
/// Built on `StackMap` which maintains variable bindings in a vector with LIFO semantics,
/// allowing duplicate keys where later bindings shadow earlier ones. Provides
/// O(n) lookup and O(1) scope entry/exit operations.
///
/// ## Scope Model
/// ```text
/// scope_stack: [("x", 1), ("y", 2), ("x", 3)]
///                                      ↑
///                                   current binding for "x"
/// ```
///
/// ## Use Cases
/// - Let-expressions: `let x = 1 in let x = 2 in x`
/// - Lambda parameters: `λx. λy. λx. body`
///
/// # Example
/// ```rust
/// let mut scope = LinearScope::new();
/// scope.enter("x", 42);     // Push binding
/// scope.enter("x", 100);    // Shadow previous
/// assert_eq!(scope.get(&"x"), Some(&100));
/// scope.exit(&"x");         // Restore previous
/// assert_eq!(scope.get(&"x"), Some(&42));
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LinearScope<K, V>(StackMap<K, V>);

impl<K, V> Default for LinearScope<K, V> {
    #[inline]
    fn default() -> Self {
        Self(StackMap::new())
    }
}

impl<K, V> LinearScope<K, V> {
    /// Creates a new empty scope stack
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Clears all scope levels from the stack
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Returns the number of scope levels in the stack
    #[inline]
    pub fn depth(&self) -> usize {
        self.0.len()
    }

    pub fn restore_depth(&mut self, depth: usize) {
        self.0.truncate(depth)
    }

    /// Returns true if the scope stack contains no bindings
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns an iterator over all key-value pairs in insertion order
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.0.iter()
    }

    /// Returns an iterator over all keys in insertion order
    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.0.keys()
    }

    /// Returns an iterator over all values in insertion order
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.0.values()
    }
}

impl<K, V> LinearScope<K, V>
where
    K: PartialEq,
{
    /// Returns the topmost (most recent) binding in the stack
    #[inline]
    pub fn current(&self) -> Option<(&K, &V)> {
        self.0.last()
    }

    /// Returns a mutable reference to the topmost binding in the stack
    #[inline]
    pub fn current_mut(&mut self) -> Option<(&K, &mut V)> {
        self.0.last_mut()
    }

    /// Gets the topmost (most recent) binding for the key
    #[inline]
    pub fn get(&self, key: &K) -> Option<&V> {
        self.0.get_last(key)
    }

    /// Gets a mutable reference to the topmost binding for the key
    #[inline]
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.0.get_last_mut(key)
    }

    /// Returns true if any scope level contains the given key
    #[inline]
    pub fn contains(&self, key: &K) -> bool {
        self.0.contains_key(key)
    }

    /// Enters a new scope by pushing a binding onto the stack
    #[inline]
    pub fn enter(&mut self, key: K, value: V) {
        self.0.push(key, value);
    }

    /// Exits the current scope by popping the most recent binding
    #[inline]
    pub fn exit(&mut self, key: &K) -> V
    where
        K: Debug,
    {
        let (k, v) = self.0.pop().expect("no binding to exit");
        assert_eq!(k, *key, "exiting scope for different key");
        v
    }

    // pub fn try_exit(&mut self, key: &K) -> Result<V, TODO> {
    //     todo!()
    // }
}

impl<K, V> FromIterator<(K, V)> for LinearScope<K, V> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        Self(StackMap::from_iter(iter))
    }
}
