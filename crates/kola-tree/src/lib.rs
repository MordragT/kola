pub mod id;
#[cfg(test)]
pub mod inspector;
pub mod meta;
pub mod node;
pub mod print;
pub mod tree;
pub mod visit;

pub mod prelude {
    pub use crate::id::NodeId;
    pub use crate::meta::{Meta, MetaCast, MetaContainer, MetaVec, Metadata, Phase};
    pub use crate::node::{self, Handler, Node, NodeKind};
    pub use crate::print::{Decorator, TreePrinter};
    pub use crate::tree::{NodeContainer, Tree, TreeBuilder};
    pub use crate::visit::{Event, EventStack, Visitor};
}
