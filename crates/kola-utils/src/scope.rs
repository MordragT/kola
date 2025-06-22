use std::fmt::Debug;

use kola_collections::{HashMap, StackMap};

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

/// A nested scope environment implementing block-structured lexical scoping.
///
/// Implements the classical scope model found in block-structured languages
/// where each scope frame can contain multiple variable declarations. Each
/// scope level maintains its own symbol table while referencing parent scopes
/// through explicit links, forming a tree-like environment structure.
///
/// ## Implementation
/// Each scope frame contains a `HashMap` for local bindings and an optional
/// parent reference. Variable lookup traverses the parent chain until a
/// binding is found or the root is reached. Provides O(1) local operations
/// and O(d) lookup where d is the scope nesting depth.
///
/// ## Scope Model
/// ```text
/// ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
/// │ global      │◄───│ function    │◄───│ block       │
/// │ x: int      │    │ y: string   │    │ x: bool     │ ← current
/// │ f: func     │    │ z: array    │    │ temp: int   │
/// └─────────────┘    └─────────────┘    └─────────────┘
/// ```
///
/// ## Use Cases
/// - Function scope: parameter lists and local variables
/// - Block scope: compound statements with multiple declarations
/// - Module scope: top-level bindings and imports
/// - Class scope: member variables and methods
///
/// # Example
/// ```rust
/// let mut scope = NestedScope::new();
/// scope.insert("x", 42);
/// scope.insert("y", 24);
///
/// scope.enter();              // New block scope
/// scope.insert("x", 100);     // Shadow outer x
/// assert_eq!(scope.get(&"x"), Some(&100));
/// assert_eq!(scope.get(&"y"), Some(&24));  // Inherited from parent
///
/// scope.exit();               // Return to parent
/// assert_eq!(scope.get(&"x"), Some(&42));  // Original x restored
/// ```
#[derive(Debug, Clone)]
pub struct NestedScope<K, V> {
    parent: Option<Box<Self>>,
    scope: HashMap<K, V>,
}

impl<K, V> Default for NestedScope<K, V> {
    fn default() -> Self {
        Self {
            parent: None,
            scope: HashMap::new(),
        }
    }
}

impl<K, V> NestedScope<K, V> {
    /// Creates a new empty scope
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new scope with the given parent
    pub fn with_parent(parent: Self) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            scope: HashMap::new(),
        }
    }

    /// Enters a new scope level, making the current scope the parent
    pub fn enter(&mut self) {
        let parent = std::mem::replace(self, Self::new());
        self.parent = Some(Box::new(parent));
    }

    /// Enters a new scope and immediately inserts a binding
    pub fn enter_with(&mut self, key: K, value: V)
    where
        K: Eq + std::hash::Hash,
    {
        self.enter();
        self.insert(key, value);
    }

    /// Exits the current scope level, returning the exited scope
    pub fn exit(&mut self) -> Self {
        let mut parent = self.parent.take().expect("no active scope");
        std::mem::swap(self, &mut parent);
        *parent
    }

    /// Returns the depth of the scope hierarchy
    pub fn depth(&self) -> usize {
        match &self.parent {
            None => 0,
            Some(parent) => 1 + parent.depth(),
        }
    }

    /// Returns a reference to the parent scope, if any
    pub fn parent(&self) -> Option<&Self> {
        self.parent.as_deref()
    }

    /// Returns a reference to the current scope
    pub fn current(&self) -> &HashMap<K, V> {
        &self.scope
    }

    /// Consumes this scope and returns the current scope level
    pub fn into_current(self) -> HashMap<K, V> {
        self.scope
    }

    /// Flattens the scope hierarchy into a single scope
    pub fn flatten(&self) -> Self
    where
        K: Clone + Eq + std::hash::Hash,
        V: Clone,
    {
        let mut result = Self::new();
        self.collect_all_bindings(&mut result);
        result
    }

    fn collect_all_bindings(&self, target: &mut Self)
    where
        K: Clone + Eq + std::hash::Hash,
        V: Clone,
    {
        if let Some(parent) = &self.parent {
            parent.collect_all_bindings(target);
        }
        for (k, v) in self.scope.iter() {
            target.insert(k.clone(), v.clone());
        }
    }
}

impl<K, V> NestedScope<K, V>
where
    K: Eq + std::hash::Hash,
{
    /// Gets a value by key, searching through parent scopes if necessary
    pub fn get(&self, key: &K) -> Option<&V> {
        if let Some(t) = self.scope.get(key) {
            Some(t)
        } else if let Some(t) = self.parent.as_ref().and_then(|env| env.get(key)) {
            Some(t)
        } else {
            None
        }
    }

    /// Gets a value by key from the current scope only (not parent scopes)
    pub fn get_local(&self, key: &K) -> Option<&V> {
        self.scope.get(key)
    }

    /// Gets a mutable reference to a value in the current scope only
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.scope.get_mut(key)
    }

    /// Returns true if the key exists in this scope or any parent scope
    pub fn contains_key(&self, key: &K) -> bool {
        self.scope.contains_key(key)
            || self
                .parent
                .as_ref()
                .map_or(false, |parent| parent.contains_key(key))
    }

    /// Returns true if the key exists in the current scope only
    pub fn contains_key_local(&self, key: &K) -> bool {
        self.scope.contains_key(key)
    }

    /// Returns the total number of bindings across all scope levels
    pub fn total_len(&self) -> usize {
        let parent_len = self.parent.as_ref().map_or(0, |p| p.total_len());
        self.scope.len() + parent_len
    }

    /// Inserts a key-value pair into the current scope
    pub fn insert(&mut self, key: K, value: V) {
        self.scope.insert(key, value);
    }

    /// Removes a key-value pair from the current scope only
    pub fn remove(&mut self, key: &K) -> Option<V> {
        self.scope.remove(key)
    }

    /// Clears all bindings in the current scope (preserves parent)
    pub fn clear(&mut self) {
        self.scope.clear();
    }
}

impl<K, V> FromIterator<(K, V)> for NestedScope<K, V>
where
    K: Eq + std::hash::Hash,
{
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut scope = Self::new();
        scope.extend(iter);
        scope
    }
}

impl<K, V> Extend<(K, V)> for NestedScope<K, V>
where
    K: Eq + std::hash::Hash,
{
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (key, value) in iter {
            self.insert(key, value);
        }
    }
}
