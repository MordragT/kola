use crate::tree::Tree;

/// A fluent API for inspecting and making assertions about nodes in a syntax tree.
/// Acts as a state machine that tracks the current node being inspected.
#[derive(Debug, Clone, Copy)]
pub struct NodeInspector<'t, T> {
    pub node: T,
    pub tree: &'t Tree,
}

impl<'t, T> NodeInspector<'t, T> {
    /// Create a new inspector from a node and tree
    pub fn new(node: T, tree: &'t Tree) -> Self {
        Self { node, tree }
    }

    /// Map the current node to another node type
    pub fn map<U, F>(self, f: F) -> NodeInspector<'t, U>
    where
        F: FnOnce(T, &'t Tree) -> U,
    {
        NodeInspector {
            node: f(self.node, self.tree),
            tree: self.tree,
        }
    }

    /// Get the underlying node
    pub fn into_node(self) -> T {
        self.node
    }

    /// Get a reference to the tree
    pub fn tree(&self) -> &'t Tree {
        self.tree
    }

    /// Execute a function with the current node and tree, then return self
    pub fn inspect<F>(self, f: F) -> Self
    where
        F: FnOnce(&T, &'t Tree),
    {
        f(&self.node, self.tree);
        self
    }

    /// Assert a condition on the current node, with a custom message
    pub fn assert<F>(self, f: F, message: &str) -> Self
    where
        F: FnOnce(&T, &'t Tree) -> bool,
    {
        assert!(f(&self.node, self.tree), "{}", message);
        self
    }
}

// Common traits for inspecting nodes with similar properties
pub trait NamedNode {
    fn assert_name(self, expected: &str, node_type: &str) -> Self;
}
