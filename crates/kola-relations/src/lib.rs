pub mod id;
pub mod overlay;
pub mod query;
pub mod relation;
pub mod slice;

// Re-exports
pub use id::{Id, IdIter};
pub use overlay::Overlay;
pub use query::{KeyedQuery, RelationQuery, TryProject, query, query_one, query_slice};
pub use relation::{
    Allocate, PartialRelation, RelationIds, SliceRelation, TotalRelation, TotalRelationMut,
};
pub use slice::SliceId;
