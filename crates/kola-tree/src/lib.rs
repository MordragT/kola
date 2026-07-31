pub mod col;
pub mod id;
pub mod inspector;
pub mod meta;
pub mod node;
pub mod print;
pub mod slice;
pub mod tree;
pub mod visit;

pub mod prelude {
    pub use crate::col::{Col, Get, GetOpt};
    pub use crate::id::Id;
    pub use crate::meta::{MetaMap, MetaSet, MetaSetCheckpoint, MetaVec};
    pub use crate::node::{
        self, AnyId, Node, NodeKind, NodeStorage, StorageCheckpoint, UniversalStorage,
    };
    pub use crate::print::{
        Decorator, Decorators, IdPrinter, NodePrinter, SlicePrinter, TreePrinter,
    };
    pub use crate::slice::{SliceBuilder, SliceId, SliceStorage};
    pub use crate::tree::{Tree, TreeBuilder, TreeCheckpoint, TreeView};
    pub use crate::visit::{Visitable, Visitor};
}
