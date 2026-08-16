pub mod id;
pub mod node;
pub mod print;
pub mod tree;
pub mod visit;

pub use kola_subst as subst;

pub mod prelude {
    pub use crate::id::IdExt;
    pub use crate::node;
    pub use crate::print::{Decorator, Decorators, TreePrinter};
    pub use crate::tree::{AnyId, Node, NodeKind, Tree, TreeCheckpoint};
    pub use crate::visit::{Visitable, Visitor};
    pub use kola_relations::side_table::{SideMap, SideVec};
    pub use kola_relations::slice::{SliceBuilder, SliceId, SliceStorage};
    pub use kola_relations::{
        Allocate, Id, IdIter, KeyedQuery, PartialRelation, QueryPlan, RelationIds, RelationQuery,
        SliceRelation, TotalRelation, TotalRelationMut, query, query_one, query_relation,
        query_slice,
    };
}
