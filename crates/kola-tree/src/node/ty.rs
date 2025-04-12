use derive_more::From;
use kola_utils::as_variant;
use serde::{Deserialize, Serialize};

use super::{Name, Symbol};
use crate::id::NodeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeError;

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[from(forward)]
pub struct TypeIdent(pub Symbol);

impl TypeIdent {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl PartialEq<Symbol> for TypeIdent {
    fn eq(&self, other: &Symbol) -> bool {
        &self.0 == other
    }
}

impl PartialEq<str> for TypeIdent {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PropertyType {
    pub name: NodeId<Name>,
    pub ty: NodeId<MonoType>,
}

// TODO maybe resemble the type def in kola-semantic
#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordType(pub Vec<NodeId<PropertyType>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FuncType {
    pub input: NodeId<MonoType>,
    pub output: NodeId<MonoType>,
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum MonoType {
    Error(NodeId<TypeError>),
    Ident(NodeId<TypeIdent>),
    Record(NodeId<RecordType>),
    Func(NodeId<FuncType>),
}

impl MonoType {
    #[inline]
    pub fn to_error(self) -> Option<NodeId<TypeError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_type_ident(self) -> Option<NodeId<TypeIdent>> {
        as_variant!(self, Self::Ident)
    }

    #[inline]
    pub fn to_record_type(self) -> Option<NodeId<RecordType>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_func_type(self) -> Option<NodeId<FuncType>> {
        as_variant!(self, Self::Func)
    }

    #[inline]
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error(_))
    }

    #[inline]
    pub fn is_type_ident(self) -> bool {
        matches!(self, Self::Ident(_))
    }

    #[inline]
    pub fn is_record_type(self) -> bool {
        matches!(self, Self::Record(_))
    }

    #[inline]
    pub fn is_func_type(self) -> bool {
        matches!(self, Self::Func(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PolyType {
    pub vars: Vec<NodeId<TypeIdent>>,
    pub ty: NodeId<MonoType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeAlias {
    pub alias: NodeId<TypeIdent>,
    pub ty: NodeId<PolyType>,
}

// TODO implement derives for all nodes better
