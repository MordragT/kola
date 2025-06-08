//! Module for tracking dependencies between modules.

use std::{collections::HashMap, hash::Hash};
use thiserror::Error;

/// Error representing a dependency cycle
#[derive(Debug, Error)]
#[error("Dependency cycle detected")]
pub struct CycleError<T>(Vec<T>);

impl<T> CycleError<T> {
    /// Create a new cycle error
    pub fn new(path: Vec<T>) -> Self {
        Self(path)
    }

    /// Get the cycle path
    pub fn path(&self) -> &[T] {
        &self.0
    }
}

// /// Resolver for tracking and resolving dependencies between items.
// ///
// /// This type manages the resolution state of items, tracking which items are
// /// pending resolution, currently being processed (to detect cycles), and maintaining
// /// the dependency graph between items.
// #[derive(Debug, Clone)]
// pub struct DependencyResolver<T> {
//     /// Pending items to be resolved
//     pending: HashSet<T>,
//     /// Active items being processed (used for cycle detection)
//     active: HashSet<T>,
//     /// Dependency graph tracking relationships between items
//     graph: DependencyGraph<T>,
// }

// impl<T> Default for DependencyResolver<T> {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl<T> DependencyResolver<T> {
//     /// Create a new empty dependency resolver
//     pub fn new() -> Self {
//         Self {
//             pending: HashSet::new(),
//             active: HashSet::new(),
//             graph: DependencyGraph::new(),
//         }
//     }

//     /// Get all pending items
//     pub fn pending_items(&self) -> impl Iterator<Item = &T> {
//         self.pending.iter()
//     }

//     /// Get all active items
//     pub fn active_items(&self) -> impl Iterator<Item = &T> {
//         self.active.iter()
//     }

//     /// Get the dependency graph
//     pub fn graph(&self) -> &DependencyGraph<T> {
//         &self.graph
//     }

//     /// Get a mutable reference to the dependency graph
//     pub fn graph_mut(&mut self) -> &mut DependencyGraph<T> {
//         &mut self.graph
//     }

//     /// Clear all pending and active items (but preserve the dependency graph)
//     pub fn clear_state(&mut self) {
//         self.pending.clear();
//         self.active.clear();
//     }

//     /// Check if there are any pending items
//     pub fn has_pending(&self) -> bool {
//         !self.pending.is_empty()
//     }

//     /// Check if there are any active items
//     pub fn has_active(&self) -> bool {
//         !self.active.is_empty()
//     }

//     /// Get the number of pending items
//     pub fn pending_count(&self) -> usize {
//         self.pending.len()
//     }

//     /// Get the number of active items
//     pub fn active_count(&self) -> usize {
//         self.active.len()
//     }
// }

// impl<T: Eq + Hash> DependencyResolver<T> {
//     /// Add an item to the pending set for resolution
//     pub fn add_pending(&mut self, item: T) {
//         self.pending.insert(item);
//     }

//     /// Check if an item is pending resolution
//     pub fn is_pending(&self, item: &T) -> bool {
//         self.pending.contains(item)
//     }

//     /// Remove an item from the pending set
//     pub fn remove_pending(&mut self, item: &T) -> bool {
//         self.pending.remove(item)
//     }

//     /// Mark an item as actively being processed
//     ///
//     /// Returns `true` if the item was not already active, `false` if it was
//     /// already active (indicating a potential cycle).
//     pub fn mark_active(&mut self, item: T) -> bool {
//         self.active.insert(item)
//     }

//     /// Check if an item is currently being processed
//     pub fn is_active(&self, item: &T) -> bool {
//         self.active.contains(item)
//     }

//     /// Mark an item as no longer being processed
//     pub fn mark_inactive(&mut self, item: &T) -> bool {
//         self.active.remove(item)
//     }

//     /// Get the dependencies of an item
//     pub fn dependencies_of(&self, item: T) -> &[T] {
//         self.graph.dependencies_of(item)
//     }

//     /// Get the dependents of an item
//     pub fn dependents_of(&self, item: T) -> &[T] {
//         self.graph.dependents_of(item)
//     }
// }

// impl<T: Eq + Hash + Copy> DependencyResolver<T> {
//     /// Add a dependency relationship between two items
//     pub fn add_dependency(&mut self, from: T, to: T) {
//         self.graph.add_dependency(from, to);
//     }

//     /// Perform topological sort of all items in the dependency graph
//     pub fn topological_sort(&self) -> Result<Vec<T>, CycleError<T>> {
//         self.graph.topological_sort()
//     }
// }

/// Graph of dependencies between modules
#[derive(Debug, Clone)]
pub struct DependencyGraph<T> {
    /// Forward dependencies (module -> dependencies)
    forward: HashMap<T, Vec<T>>,

    /// Reverse dependencies (module -> dependents)
    reverse: HashMap<T, Vec<T>>,
}

impl<T> Default for DependencyGraph<T> {
    fn default() -> Self {
        Self {
            forward: HashMap::default(),
            reverse: HashMap::default(),
        }
    }
}

impl<T> DependencyGraph<T> {
    /// Create a new empty dependency graph
    pub fn new() -> Self {
        Self {
            forward: HashMap::new(),
            reverse: HashMap::new(),
        }
    }
}

impl<T: Eq + Hash> DependencyGraph<T> {
    /// Get dependencies of a module
    pub fn dependencies_of(&self, module: T) -> &[T] {
        self.forward
            .get(&module)
            .map(|deps| deps.as_slice())
            .unwrap_or(&[])
    }

    /// Get dependents of a module
    pub fn dependents_of(&self, module: T) -> &[T] {
        self.reverse
            .get(&module)
            .map(|deps| deps.as_slice())
            .unwrap_or(&[])
    }
}

impl<T: Eq + Hash + Copy> DependencyGraph<T> {
    /// Add a dependency edge
    pub fn add_dependency(&mut self, from: T, to: T) {
        // Add forward dependency
        self.forward.entry(from).or_default().push(to);

        // Add reverse dependency
        self.reverse.entry(to).or_default().push(from);
    }

    /// Perform a topological sort of the modules
    pub fn topological_sort(&self) -> Result<Vec<T>, CycleError<T>> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        enum VisitState {
            Unvisited,
            Visiting,
            Visited,
        }

        // Track visited and in-progress modules
        let mut visited = HashMap::new();
        let mut order = Vec::new();
        let mut stack = self.forward.keys().copied().collect::<Vec<_>>();

        while let Some(module) = stack.pop() {
            match visited
                .get(&module)
                .copied()
                .unwrap_or(VisitState::Unvisited)
            {
                VisitState::Unvisited => {
                    // Mark as visiting
                    visited.insert(module, VisitState::Visiting);
                    stack.push(module);

                    // Visit dependencies
                    for &dep in self.dependencies_of(module) {
                        match visited.get(&dep).copied().unwrap_or(VisitState::Unvisited) {
                            VisitState::Unvisited => {
                                stack.push(dep);
                            }
                            VisitState::Visiting => {
                                return Err(CycleError::new(order));
                            }
                            VisitState::Visited => {}
                        }
                    }
                }
                VisitState::Visiting => {
                    visited.insert(module, VisitState::Visited);
                    order.push(module);
                }
                VisitState::Visited => {}
            }
        }
        Ok(order)
    }
}
