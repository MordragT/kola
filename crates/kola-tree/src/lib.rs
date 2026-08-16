pub mod inspect;
pub mod node;
pub mod print;
pub mod tree;
pub mod visit;

pub use kola_subst as subst;

pub mod prelude {
    pub use crate::inspect::{AssertEqExt, AssertNameExt, IdExt, SliceIdExt, assert_name};
    pub use crate::node;
    pub use crate::print::{Decorator, Decorators, TreePrinter};
    pub use crate::tree::{AnyId, Node, NodeKind, TotalSpan, Tree, TreeCheckpoint};
    pub use crate::visit::{Visitable, Visitor};
    pub use kola_relations::side_table::{SideMap, SideVec};
    pub use kola_relations::slice::{SliceBuilder, SliceId, SliceStorage};
    pub use kola_relations::{
        Allocate, Id, IdIter, Inspector, KeyedQuery, PartialRelation, QueryPlan, RelationIds,
        RelationQuery, SliceRelation, TotalRelation, TotalRelationMut, query, query_items,
        query_one, query_relation, query_slice,
    };
}
