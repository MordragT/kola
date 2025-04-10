use std::fmt::Debug;

use super::id::{BranchId, LeafId};
use crate::id::NodeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Event {
    Enter(BranchId),
    Exit(BranchId),
    Visit(LeafId),
}

#[derive(Debug, Clone, Default)]
pub struct EventStack {
    stack: Vec<Event>,
}

impl EventStack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn next(&mut self) -> Option<Event> {
        self.stack.pop()
    }

    pub fn push_branch<T>(&mut self, id: NodeId<T>)
    where
        BranchId: From<NodeId<T>>,
    {
        let branch = BranchId::from(id);
        self.stack.push(Event::Exit(branch));
        self.stack.push(Event::Enter(branch));
    }

    pub fn push_leaf<T>(&mut self, id: NodeId<T>)
    where
        LeafId: From<NodeId<T>>,
    {
        let leaf = LeafId::from(id);
        self.stack.push(Event::Visit(leaf));
    }
}
