pub mod id;
pub mod query;
pub mod relation;
pub mod slice;

// Re-exports
pub use id::{Id, IdIter};
pub use query::{
    KeyedQuery, QueryPlan, RelationQuery, TryProject, query, query_one, query_relation, query_slice,
};
pub use relation::{
    Allocate, PartialRelation, RelationIds, SliceRelation, TotalRelation, TotalRelationMut,
};
pub use slice::SliceId;
