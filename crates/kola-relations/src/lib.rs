pub mod id;
pub mod presence;
pub mod query;
pub mod relation;
pub mod side_table;
pub mod slice;

// Re-exports
pub use id::{Id, IdIter};
pub use kola_relations_macro::{RelationStorage, TryCast};
pub use presence::{Optional, Presence, Required};
pub use query::*;
pub use relation::{
    Allocate, PartialRelation, PartialRelationMut, RelationIds, SliceRelation, SliceRelationMut,
    TotalRelation, TotalRelationMut,
};
pub use side_table::{SideMap, SideVec};
pub use slice::{SliceBuilder, SliceId, SliceStorage};
