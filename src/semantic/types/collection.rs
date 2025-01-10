use super::MonoType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Collection {
    pub collection: CollectionType,
    pub arg: MonoType,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum CollectionType {
    Array,
    Vector,
    Stream,
}
