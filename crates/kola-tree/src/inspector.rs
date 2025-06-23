use kola_utils::{convert::TryAsRef, interner::StrInterner};
use std::{fmt::Debug, hash::BuildHasher};

use crate::{id::Id, node::Node, tree::TreeBuilder};

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
    pub fn assert_with<F>(self, f: F, message: &str) -> Self
    where
        F: FnOnce(&T, &'t TreeBuilder) -> bool,
    {
        assert!(f(&self.node, self.tree), "{}", message);
        self
    }
}

impl<'t, T, S> NodeInspector<'t, Id<T>, S>
where
    Node: TryAsRef<T>,
    S: BuildHasher,
{
    /// Assert that the current node is equal to another node
    pub fn assert_eq<U>(self, other: &U) -> Self
    where
        U: Debug,
        T: Debug + PartialEq<U>,
    {
        let node = self.node.get(self.tree);

        assert_eq!(node, other, "Expected {:?} but found {:?}", other, node);
        self
    }

    pub fn assert_ne<U>(self, other: &U) -> Self
    where
        U: Debug,
        T: Debug + PartialEq<U>,
    {
        let node = self.node.get(self.tree);

        assert_ne!(node, other, "Expected not {:?} but found {:?}", other, node);
        self
    }

    pub fn get(self) -> &'t T {
        self.node.get(self.tree)
    }
}

impl<'t, T, S> NodeInspector<'t, Option<T>, S>
where
    S: BuildHasher,
{
    /// Assert that the current node is `None`
    pub fn assert_none(self) -> Self {
        assert!(self.node.is_none(), "Expected None but found Some");
        self
    }

    /// Assert that the current node is `Some`
    pub fn assert_some(self) -> Self {
        assert!(self.node.is_some(), "Expected Some but found None");
        self
    }
}

impl<'t, T, S> NodeInspector<'t, Option<Id<T>>, S>
where
    Node: TryAsRef<T>,
    S: BuildHasher,
{
    /// Assert that the current node is `Some` and equal to another node
    pub fn assert_some_eq<U: PartialEq>(self, other: &U) -> Self
    where
        U: Debug,
        T: Debug + PartialEq<U>,
    {
        if let Some(node) = self.node.map(|id| id.get(self.tree)) {
            assert_eq!(node, other, "Expected {:?} but found {:?}", other, node);
        } else {
            panic!("Expected Some but found None");
        }
        self
    }
}
