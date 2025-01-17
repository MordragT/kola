use serde::{Deserialize, Serialize};

use super::MonoType;

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Collection {
    pub collection: CollectionType,
    pub arg: MonoType,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum CollectionType {
    Array,
    Vector,
    Stream,
}
