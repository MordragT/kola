use kola_protocol::TypeProtocol;
use std::borrow::Cow;

use crate::arenas::Idx;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapWitness(pub(super) Idx<TypeProtocol>);

#[derive(Debug, Clone, PartialEq)]
pub struct RawWitness<'a>(pub(super) Cow<'a, TypeProtocol>);

impl RawWitness<'static> {
    pub fn new(proto: TypeProtocol) -> Self {
        Self(Cow::Owned(proto))
    }
}
