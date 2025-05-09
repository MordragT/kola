use kola_utils::StrInterner;

use crate::tree::TreeBuilder;

/// A fluent API for inspecting and making assertions about nodes in a syntax tree.
/// Acts as a state machine that tracks the current node being inspected.
#[derive(Debug, Clone, Copy)]
pub struct NodeInspector<'t, T> {
    pub node: T,
    pub tree: &'t TreeBuilder,
    pub interner: &'t StrInterner,
}

impl<'t, T> NodeInspector<'t, T> {
    /// Create a new inspector from a node and tree
    pub fn new(node: T, tree: &'t TreeBuilder, interner: &'t StrInterner) -> Self {
        Self {
            node,
            tree,
            interner,
        }
    }

    /// Map the current node to another node type
    pub fn map<U, F>(self, f: F) -> NodeInspector<'t, U>
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
