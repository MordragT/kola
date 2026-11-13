pub mod id;
pub mod presence;
pub mod query;
pub mod relation;
pub mod side_table;
pub mod slice;

// Re-exports
pub use id::{Id, IdIter};
pub use kola_relations_macro::{Merge, RelationStorage, TryCast};
pub use presence::{Optional, Presence, Required};
pub use query::*;
pub use relation::{
    Allocate, Merge, PartialRelation, PartialRelationMut, PartialRelationSet, RelationIds,
    SliceRelation, SliceRelationMut, TotalRelation, TotalRelationMut,
};
pub use side_table::{MaybeVec, SideMap, SideVec};
pub use slice::{SliceBuilder, SliceId, SliceStorage};
