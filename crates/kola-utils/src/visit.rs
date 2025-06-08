use std::{
    collections::HashMap,
    ops::{Index, IndexMut},
};

use derive_more::Display;

// TODO maybe error state ??

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VisitState {
    Unvisited,
    Visiting,
    Visited,
}

#[derive(Debug, Clone)]
pub struct VisitMap<T>(HashMap<T, VisitState>);

impl<T> VisitMap<T>
where
    T: Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&self, key: &T) -> VisitState {
        self.0.get(key).copied().unwrap_or(VisitState::Unvisited)
    }

    pub fn set(&mut self, key: T, state: VisitState) {
        self.0.insert(key, state);
    }

    pub fn insert(&mut self, key: T, state: VisitState) -> Option<VisitState> {
        self.0.insert(key, state)
    }

    pub fn contains_key(&self, key: &T) -> bool {
        self.0.contains_key(key)
    }

    pub fn remove(&mut self, key: &T) -> Option<VisitState> {
        self.0.remove(key)
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T> Default for VisitMap<T>
where
    T: Eq + std::hash::Hash,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Index<T> for VisitMap<T>
where
    T: Eq + std::hash::Hash + Copy,
{
    type Output = VisitState;

    fn index(&self, key: T) -> &Self::Output {
        self.0.get(&key).unwrap_or(&VisitState::Unvisited)
    }
}

impl<T> IndexMut<T> for VisitMap<T>
where
    T: Eq + std::hash::Hash + Copy,
{
    fn index_mut(&mut self, key: T) -> &mut Self::Output {
        self.0.entry(key).or_insert(VisitState::Unvisited)
    }
}
