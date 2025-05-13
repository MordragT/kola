use std::hash::BuildHasher;

use kola_utils::interner::StrInterner;

use crate::tree::TreeBuilder;

/// A fluent API for inspecting and making assertions about nodes in a syntax tree.
/// Acts as a state machine that tracks the current node being inspected.
#[derive(Debug)]
pub struct NodeInspector<'t, T, S: BuildHasher> {
    pub node: T,
    pub tree: &'t TreeBuilder,
    pub interner: &'t StrInterner<S>,
}

impl<'t, T, S> Clone for NodeInspector<'t, T, S>
where
    T: Clone,
    S: BuildHasher,
{
    fn clone(&self) -> Self {
        Self {
            node: self.node.clone(),
            tree: self.tree,
            interner: self.interner,
        }
    }
}

impl<'t, T, S> Copy for NodeInspector<'t, T, S>
where
    T: Copy,
    S: BuildHasher,
{
}

impl<'t, T, S: BuildHasher> NodeInspector<'t, T, S> {
    /// Create a new inspector from a node and tree
    pub fn new(node: T, tree: &'t TreeBuilder, interner: &'t StrInterner<S>) -> Self {
        Self {
            node,
            tree,
            interner,
        }
    }

    /// Map the current node to another node type
    pub fn map<U, F>(self, f: F) -> NodeInspector<'t, U, S>
    where
        F: FnOnce(T, &'t TreeBuilder) -> U,
    {
        NodeInspector {
            node: f(self.node, self.tree),
            tree: self.tree,
            interner: self.interner,
        }
    }

    /// Get the underlying node
    pub fn into_node(self) -> T {
        self.node
    }

    /// Execute a function with the current node and tree, then return self
    pub fn inspect<F>(self, f: F) -> Self
    where
        F: FnOnce(&T, &'t TreeBuilder),
    {
        f(&self.node, self.tree);
        self
    }

    /// Assert a condition on the current node, with a custom message
    pub fn assert<F>(self, f: F, message: &str) -> Self
    where
        F: FnOnce(&T, &'t TreeBuilder) -> bool,
    {
        assert!(f(&self.node, self.tree), "{}", message);
        self
    }
}

// Common traits for inspecting nodes with similar properties
pub trait NamedNode {
    fn assert_name(self, expected: &str, node_type: &str) -> Self;
}
