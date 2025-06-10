use std::{
    hash::{BuildHasher, Hash, RandomState},
    ops::Index,
};

use crate::im_hash_map::ImHashMap;

pub use crate::im_hash_map::{IntoIter, Iter};

#[derive(Debug, Clone)]
pub struct ImBiMap<K, V, S: BuildHasher = RandomState> {
    forward: ImHashMap<K, V, S>,
    backward: ImHashMap<V, K, S>,
}

impl<K, V> Default for ImBiMap<K, V> {
    fn default() -> Self {
        Self {
            forward: ImHashMap::new(),
            backward: ImHashMap::new(),
        }
    }
}

impl<K, V> ImBiMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<K, V, S> ImBiMap<K, V, S>
where
    K: Eq + Hash + Clone,
    V: Eq + Hash + Clone,
    S: BuildHasher + Clone,
{
    pub fn with_hasher(hasher: S) -> Self {
        Self {
            forward: ImHashMap::with_hasher(hasher.clone()),
            backward: ImHashMap::with_hasher(hasher),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.forward.insert(key.clone(), value.clone());
        self.backward.insert(value, key);
    }

    pub fn update(&self, key: K, value: V) -> Self {
        let mut new_bimap = self.clone();
        new_bimap.insert(key, value);
        new_bimap
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

impl<K, V, S> Index<K> for ImBiMap<K, V, S>
where
    K: Eq + Hash + Clone,
    V: Eq + Hash + Clone,
    S: BuildHasher + Clone,
{
    type Output = V;

    fn index(&self, key: K) -> &Self::Output {
        self.get_by_key(&key).unwrap()
    }
}

impl<K, V, S> IntoIterator for ImBiMap<K, V, S>
where
    K: Eq + Hash + Clone,
    V: Eq + Hash + Clone,
    S: BuildHasher + Clone,
{
    type Item = (K, V);
    type IntoIter = IntoIter<(K, V), crate::Ptr>;

    fn into_iter(self) -> Self::IntoIter {
        self.forward.into_iter()
    }
}

impl<'a, K, V, S> IntoIterator for &'a ImBiMap<K, V, S>
where
    K: Eq + Hash + Clone,
    V: Eq + Hash + Clone,
    S: BuildHasher + Clone,
{
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V, crate::Ptr>;

    fn into_iter(self) -> Self::IntoIter {
        self.forward.iter()
    }
}
