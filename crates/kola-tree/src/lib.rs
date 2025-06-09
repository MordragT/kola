pub mod fold;
pub mod id;
pub mod inspector;
pub mod meta;
pub mod node;
pub mod print;
pub mod tree;
pub mod visit;

pub mod prelude {
    pub use crate::fold::Folder;
    pub use crate::id::Id;
    pub use crate::meta::{Empty, Meta, MetaCast, MetaVec, MetaView, Phase, Stub, UniformPhase};
    pub use crate::node::{self, Node, NodeId, NodeKind};
    pub use crate::print::{
        Decorator, Decorators, IdPrinter, NodePrinter, SlicePrinter, TreePrinter,
    };
    pub use crate::tree::{Query2, Query3, Query4, Tree, TreeBuilder, TreeView};
    pub use crate::visit::{Visitable, Visitor};
}
