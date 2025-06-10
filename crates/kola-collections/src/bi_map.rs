use std::{
    collections::hash_map,
    hash::{BuildHasher, RandomState},
    ops::Index,
};

use crate::hash_map::HashMap;

#[derive(Debug, Clone)]
pub struct BiMap<K, V, S: BuildHasher = RandomState> {
    forward: HashMap<K, V, S>,
    backward: HashMap<V, K, S>,
}

impl<K, V> Default for BiMap<K, V> {
    fn default() -> Self {
        Self {
            forward: HashMap::new(),
            backward: HashMap::new(),
        }
    }
}

impl<K, V> BiMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            forward: HashMap::with_capacity(capacity),
            backward: HashMap::with_capacity(capacity),
        }
    }
}

impl<K, V, S> BiMap<K, V, S>
where
    K: Eq + std::hash::Hash + Clone,
    V: Eq + std::hash::Hash + Clone,
    S: std::hash::BuildHasher + Clone,
{
    pub fn with_hasher(hasher: S) -> Self {
        Self {
            forward: HashMap::with_hasher(hasher.clone()),
            backward: HashMap::with_hasher(hasher),
        }
    }

    pub fn with_capacity_and_hasher(capacity: usize, hasher: S) -> Self {
        Self {
            forward: HashMap::with_capacity_and_hasher(capacity, hasher.clone()),
            backward: HashMap::with_capacity_and_hasher(capacity, hasher),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.forward.insert(key.clone(), value.clone());
        self.backward.insert(value, key);
    }

    pub fn get_by_key(&self, key: &K) -> Option<&V> {
        self.forward.get(key)
    }

    pub fn get_by_value(&self, value: &V) -> Option<&K> {
        self.backward.get(value)
    }

    pub fn remove_by_key(&mut self, key: &K) -> Option<V> {
        if let Some(value) = self.forward.remove(key) {
            self.backward.remove(&value);
            Some(value)
        } else {
            None
        }
    }

    pub fn remove_by_value(&mut self, value: &V) -> Option<K> {
        if let Some(key) = self.backward.remove(value) {
            self.forward.remove(&key);
            Some(key)
        } else {
            None
        }
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.forward.contains_key(key)
    }

    pub fn contains_value(&self, value: &V) -> bool {
        self.backward.contains_key(value)
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.forward.keys()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.forward.values()
    }
}

impl<K, V, S> Index<K> for BiMap<K, V, S>
where
    K: Eq + std::hash::Hash + Clone,
    V: Eq + std::hash::Hash + Clone,
    S: std::hash::BuildHasher + Clone,
{
    type Output = V;

    fn index(&self, key: K) -> &Self::Output {
        self.get_by_key(&key).unwrap()
    }
}

impl<K, V, S> IntoIterator for BiMap<K, V, S>
where
    K: Eq + std::hash::Hash + Clone,
    V: Eq + std::hash::Hash + Clone,
    S: std::hash::BuildHasher + Clone,
{
    type Item = (K, V);
    type IntoIter = hash_map::IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.forward.into_iter()
    }
}

impl<'a, K, V, S> IntoIterator for &'a BiMap<K, V, S>
where
    K: Eq + std::hash::Hash + Clone,
    V: Eq + std::hash::Hash + Clone,
    S: std::hash::BuildHasher + Clone,
{
    type Item = (&'a K, &'a V);
    type IntoIter = hash_map::Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.forward.iter()
    }
}
