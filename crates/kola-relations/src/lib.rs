pub mod id;
pub mod query;
pub mod relation;
pub mod side_table;
pub mod slice;

// Re-exports
pub use id::{Id, IdIter};
pub use kola_relations_macro::{RelationStorage, TryProject};
pub use query::*;
pub use relation::{
    Allocate, PartialRelation, RelationIds, SliceRelation, TotalRelation, TotalRelationMut,
};
pub use side_table::{SideMap, SideVec};
pub use slice::{SliceBuilder, SliceId, SliceStorage};
