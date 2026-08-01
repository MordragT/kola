pub mod attrs;
pub mod id;
pub mod inspect;
pub mod node;
pub mod print;
pub mod query;
pub mod slice;
pub mod tree;
pub mod visit;

pub use kola_subst as subst;

pub mod prelude {
    pub use crate::attrs::{NodeAttrs, NodeAttrsCheckpoint, SideMap, SideVec};
    pub use crate::id::Id;
    pub use crate::node::{
        self, AnyId, Node, NodeKind, NodeStorage, StorageCheckpoint, UniversalStorage,
    };
    pub use crate::print::{Decorator, Decorators, TreePrinter};
    pub use crate::query::{Col, Get, GetOpt};
    pub use crate::slice::{SliceBuilder, SliceId, SliceStorage};
    pub use crate::tree::{Tree, TreeBuilder, TreeCheckpoint, TreeView};
    pub use crate::visit::{Visitable, Visitor};
}
