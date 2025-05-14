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

/// Graph of dependencies between modules
#[derive(Debug, Clone)]
pub struct DependencyGraph<T> {
    /// Forward dependencies (module -> dependencies)
    forward: HashMap<T, Vec<T>>,

    /// Reverse dependencies (module -> dependents)
    reverse: HashMap<T, Vec<T>>,
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
