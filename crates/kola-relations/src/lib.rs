pub mod id;
pub mod query;
pub mod relation;
pub mod side_table;
pub mod slice;

// Re-exports
pub use id::{Id, IdIter};
pub use kola_relations_macro::{
    RelationProjections, RelationStorage, TryProject as TryProjectDerive,
};
pub use query::{
    KeyedQuery, QueryPlan, RelationQuery, TryProject, query, query_one, query_relation, query_slice,
};
pub use relation::{
    Allocate, PartialRelation, RelationIds, SliceRelation, TotalRelation, TotalRelationMut,
};
pub use side_table::{SideMap, SideVec};
pub use slice::{SliceBuilder, SliceId, SliceStorage};
