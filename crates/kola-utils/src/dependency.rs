//! Module for tracking dependencies between modules.

use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Display},
    hash::Hash,
};

use graphwiz::{Builder, Graph, Kind, attributes as attrs};

use crate::visit::{VisitMap, VisitState};

/// Error representing a dependency cycle
#[derive(Debug)]
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

impl<T: fmt::Display> fmt::Display for CycleError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Dependency cycle detected: ")?;

        let mut iter = self.0.iter();

        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
        }

        for el in iter {
            write!(f, " -> {}", el)?;
        }

        Ok(())
    }
}

/// Graph of dependencies between modules
#[derive(Debug, Clone)]
pub struct DependencyGraph<T> {
    /// Forward dependencies (module -> dependencies)
    forward: HashMap<T, HashSet<T>>,

    /// Reverse dependencies (module -> dependents)
    reverse: HashMap<T, HashSet<T>>,
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
    /// Get dependencies of an item
    pub fn dependencies_of(&self, item: T) -> impl Iterator<Item = &T> {
        self.forward
            .get(&item)
            .map(|deps| deps.iter())
            .into_iter()
            .flatten()
    }

    /// Get dependents of an item
    pub fn dependents_of(&self, item: T) -> impl Iterator<Item = &T> {
        self.reverse
            .get(&item)
            .map(|deps| deps.iter())
            .into_iter()
            .flatten()
    }
}

impl<T: Eq + Hash + Copy> DependencyGraph<T> {
    pub fn add_node(&mut self, item: T) {
        // Ensure the item exists in both forward and reverse maps
        self.forward.entry(item).or_default();
        self.reverse.entry(item).or_default();
    }

    /// Add a dependency edge
    pub fn add_dependency(&mut self, from: T, to: T) {
        // Add forward dependency
        self.forward.entry(from).or_default().insert(to);

        // Add reverse dependency
        self.reverse.entry(to).or_default().insert(from);
    }

    /// Perform a topological sort of the modules
    pub fn topological_sort(&self) -> Result<Vec<T>, CycleError<T>>
    where
        T: Debug,
    {
        let mut visited = VisitMap::new();
        let mut order = Vec::new();
        let mut stack = self.forward.keys().copied().collect::<Vec<_>>();
        let mut path = Vec::new();

        while let Some(module) = stack.pop() {
            match visited.get(&module) {
                VisitState::Unvisited => {
                    // Mark as visiting
                    visited.insert(module, VisitState::Visiting);
                    stack.push(module);
                    path.push(module);

                    // Visit dependencies
                    for &dep in self.dependencies_of(module) {
                        match visited.get(&dep) {
                            VisitState::Unvisited => {
                                stack.push(dep);
                            }
                            VisitState::Visiting => {
                                let start = path.iter().position(|&m| m == dep).unwrap(); // Safety: If dep is Visiting, it must also be in path
                                let mut cycle = path[start..].to_vec();
                                cycle.push(dep);
                                return Err(CycleError::new(cycle));
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

    pub fn to_dot(&self) -> String
    where
        T: Display,
    {
        let mut root = Graph::new_builder();

        root.defaults_mut(Kind::Node).extend([
            (attrs::FILLCOLOR, "lavender".to_owned()),
            (attrs::STYLE, "filled".to_owned()),
        ]);

        let mut nodes = HashMap::new();

        for (from, neigh) in &self.forward {
            let from_str = from.to_string();
            let from = nodes
                .entry(from_str.clone())
                .or_insert_with(|| root.new_node(from_str))
                .clone();
            for to in neigh {
                let to_str = to.to_string();
                let to = nodes
                    .entry(to_str.clone())
                    .or_insert_with(|| root.new_node(to_str))
                    .clone();
                root.new_edge(from, to);
            }
        }

        let graph = root.build();
        graphwiz::render_digraph(&graph)
    }
}
