use std::rc::Rc;

use crate::{InnerNode, NodeKind};

use super::{Attached, Expr, Meta, Node, NodeId, Phase};

#[derive(Debug)]
pub struct TreeBuilder {
    nodes: Vec<Node>,
}

impl Default for TreeBuilder {
    fn default() -> Self {
        Self { nodes: Vec::new() }
    }
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    // pub fn register<P>(mut self) -> Self
    // where
    //     P: Phase,
    // {
    //     let container = Vec::<Meta<P>>::new();
    //     let container = Box::new(container) as Box<dyn MetaContainer>;
    //     self.metadata.push(container);

    //     self
    // }

    pub fn insert<P, T>(&mut self, node: T) -> NodeId<T>
    where
        T: Into<Node>,
        // P: Phase,
        // T: Attached<P> + Into<Node>,
    {
        let id = self.nodes.len() as u32;

        let node = node.into();
        self.nodes.push(node);

        // let meta = T::into_meta(meta);
        // let container = self
        //     .metadata
        //     .iter_mut()
        //     .find_map(|c| c.as_any_mut().downcast_mut::<Vec<Meta<P>>>())
        //     .expect("You first need to register the Phase");
        // container.push(meta);

        NodeId::new(id)
    }

    pub fn finish(self, root: NodeId<Expr>) -> Tree {
        let Self { nodes } = self;

        let nodes = Rc::new(nodes);

        Tree {
            nodes,
            // metadata,
            root,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tree {
    nodes: Rc<Vec<Node>>,
    // metadata: Vec<Box<dyn MetaContainer>>,
    root: NodeId<Expr>,
}

// TODO map meta for infer

impl Tree {
    // pub fn register_with<P>(&mut self, f: impl Fn(NodeKind) -> Meta<P>)
    // where
    //     P: Phase,
    // {
    //     let container = self.nodes.iter().map(Node::kind).map(f).collect::<Vec<_>>();
    //     let container = Box::new(container) as Box<dyn MetaContainer>;
    //     self.metadata.push(container);
    // }

    pub fn metadata_with<P>(&self, f: impl Fn(NodeKind) -> Meta<P>) -> Vec<Meta<P>>
    where
        P: Phase,
    {
        self.nodes.iter().map(Node::kind).map(f).collect::<Vec<_>>()
    }

    pub fn node<T>(&self, id: NodeId<T>) -> &T
    where
        T: InnerNode,
    {
        let node = &self.nodes[id.as_usize()];
        T::to_inner_ref(node).unwrap()
    }

    // pub fn node_mut<T>(&mut self, id: NodeId<T>) -> &mut T
    // where
    //     T: InnerNode,
    // {
    //     let node = &mut self.nodes[id.as_usize()];
    //     T::to_inner_mut(node).unwrap()
    // }

    // pub fn update_node<T>(&mut self, id: NodeId<T>, node: T) -> T
    // where
    //     T: InnerNode,
    // {
    //     std::mem::replace(self.node_mut(id), node)
    // }

    // pub fn meta<P, T>(&self, id: NodeId<T>) -> &T::Meta
    // where
    //     P: Phase,
    //     T: Attached<P>,
    // {
    //     let container = self.meta_container().unwrap();
    //     let meta = &container[id.as_usize()];
    //     T::to_attached_ref(meta).unwrap()
    // }

    // pub fn meta_mut<P, T>(&mut self, id: NodeId<T>) -> &mut T::Meta
    // where
    //     P: Phase,
    //     T: Attached<P>,
    // {
    //     let container = self.meta_container_mut().unwrap();
    //     let meta = &mut container[id.as_usize()];
    //     T::to_attached_mut(meta).unwrap()
    // }

    // pub fn update_meta<P, T>(&mut self, id: NodeId<T>, meta: T::Meta) -> T::Meta
    // where
    //     P: Phase,
    //     T: Attached<P>,
    // {
    //     std::mem::replace(self.meta_mut(id), meta)
    // }

    pub fn root(&self) -> NodeId<super::Expr> {
        self.root
    }

    pub fn iter_nodes(&self) -> std::slice::Iter<'_, Node> {
        self.nodes.iter()
    }

    // pub fn iter_nodes_mut(&mut self) -> std::slice::IterMut<'_, Node> {
    //     self.nodes.iter_mut()
    // }

    // pub fn iter_meta<P>(&self) -> std::slice::Iter<'_, Meta<P>>
    // where
    //     P: Phase,
    // {
    //     let container = self.meta_container().unwrap();
    //     container.iter()
    // }

    // pub fn iter_meta_mut<P>(&mut self) -> std::slice::IterMut<'_, Meta<P>>
    // where
    //     P: Phase,
    // {
    //     let container = self.meta_container_mut().unwrap();
    //     container.iter_mut()
    // }

    // pub(crate) fn meta_raw<P, T>(&self, id: NodeId<T>) -> &Meta<P>
    // where
    //     P: Phase,
    //     T: Attached<P>,
    // {
    //     let container = self.meta_container().unwrap();
    //     &container[id.as_usize()]
    // }

    // fn meta_container<P>(&self) -> Option<&Vec<Meta<P>>>
    // where
    //     P: Phase,
    // {
    //     self.metadata
    //         .iter()
    //         .find_map(|c| c.as_any_ref().downcast_ref::<Vec<Meta<P>>>())
    // }

    // fn meta_container_mut<P>(&mut self) -> Option<&mut Vec<Meta<P>>>
    // where
    //     P: Phase,
    // {
    //     self.metadata
    //         .iter_mut()
    //         .find_map(|c| c.as_any_mut().downcast_mut::<Vec<Meta<P>>>())
    // }
}
