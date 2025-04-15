use derive_more::From;
use kola_utils::as_variant;
use serde::{Deserialize, Serialize};

use super::Name;
use crate::{id::NodeId, tree::NodeContainer};

/*
type Option = forall a . [ Some : a, None ]
type OptionResult  = forall a e . [ Option a | +Error : e ]
type AlwaysSome = forall a . [ Option a | -None ]

type Person = { name : Str }
type Member = { Person | +id : Num }
type Id = { Member | -id }

map : forall a b . (a -> b) -> List a -> List b
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeError;

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypePath(pub Vec<NodeId<Name>>);

impl TypePath {
    pub fn get<'a>(&self, index: usize, tree: &'a impl NodeContainer) -> &'a Name {
        self.0[index].get(tree)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordFieldType {
    pub name: NodeId<Name>,
    pub ty: NodeId<TypeExpr>,
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordType(pub Vec<NodeId<RecordFieldType>>);

impl RecordType {
    pub fn get(&self, index: usize, tree: &impl NodeContainer) -> RecordFieldType {
        *self.0[index].get(tree)
    }
}

// TODO better name it Tag ??
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct VariantCaseType {
    pub name: NodeId<Name>,
    pub ty: Option<NodeId<TypeExpr>>,
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct VariantType(pub Vec<NodeId<VariantCaseType>>);

impl VariantType {
    pub fn get(&self, index: usize, tree: &impl NodeContainer) -> VariantCaseType {
        *self.0[index].get(tree)
    }
}

// TODO this needs to be disambiguated with parentheses if a function should be one argument
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FuncType {
    pub input: NodeId<TypeExpr>,
    pub output: NodeId<TypeExpr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeApplication {
    pub constructor: NodeId<TypeExpr>,
    pub arg: NodeId<TypeExpr>,
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum TypeExpr {
    Error(NodeId<TypeError>),
    Path(NodeId<TypePath>),
    Record(NodeId<RecordType>),
    Variant(NodeId<VariantType>),
    Func(NodeId<FuncType>),
    Application(NodeId<TypeApplication>),
}

impl TypeExpr {
    #[inline]
    pub fn to_error(self) -> Option<NodeId<TypeError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_type_path(self) -> Option<NodeId<TypePath>> {
        as_variant!(self, Self::Path)
    }

    #[inline]
    pub fn to_record_type(self) -> Option<NodeId<RecordType>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_variant_type(self) -> Option<NodeId<VariantType>> {
        as_variant!(self, Self::Variant)
    }

    #[inline]
    pub fn to_func_type(self) -> Option<NodeId<FuncType>> {
        as_variant!(self, Self::Func)
    }

    #[inline]
    pub fn to_type_application(self) -> Option<NodeId<TypeApplication>> {
        as_variant!(self, Self::Application)
    }

    #[inline]
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error(_))
    }

    #[inline]
    pub fn is_type_path(self) -> bool {
        matches!(self, Self::Path(_))
    }

    #[inline]
    pub fn is_record_type(self) -> bool {
        matches!(self, Self::Record(_))
    }

    #[inline]
    pub fn is_variant_type(self) -> bool {
        matches!(self, Self::Variant(_))
    }

    #[inline]
    pub fn is_func_type(self) -> bool {
        matches!(self, Self::Func(_))
    }

    #[inline]
    pub fn is_type_application(self) -> bool {
        matches!(self, Self::Application(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Type {
    pub vars: Vec<NodeId<Name>>,
    pub ty: NodeId<TypeExpr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeBind {
    pub name: NodeId<Name>,
    pub ty: NodeId<Type>,
}
