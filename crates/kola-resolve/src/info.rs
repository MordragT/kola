use std::{fmt, hash::Hash};

use derive_more::From;
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{self, Vis},
};

use crate::symbol::ModuleSymbol;

pub struct BindInfo<T> {
    pub node_id: Id<T>,
    pub loc: Loc,
    pub vis: Vis,
}

impl<T> BindInfo<T> {
    pub const fn new(node_id: Id<T>, loc: Loc, vis: Vis) -> Self {
        Self { node_id, loc, vis }
    }

    pub const fn node_id(&self) -> Id<T> {
        self.node_id
    }

    pub const fn loc(&self) -> Loc {
        self.loc
    }

    pub const fn vis(&self) -> Vis {
        self.vis
    }
}

impl<T> fmt::Debug for BindInfo<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BindInfo")
            .field("node_id", &self.node_id)
            .field("loc", &self.loc)
            .field("vis", &self.vis)
            .finish()
    }
}

impl<T> Clone for BindInfo<T> {
    fn clone(&self) -> Self {
        Self {
            node_id: self.node_id,
            loc: self.loc,
            vis: self.vis,
        }
    }
}

impl<T> Copy for BindInfo<T> {}

impl<T> PartialEq for BindInfo<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node_id == other.node_id && self.loc == other.loc
    }
}

impl<T> Eq for BindInfo<T> {}

impl<T> PartialOrd for BindInfo<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for BindInfo<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.node_id
            .cmp(&other.node_id)
            .then_with(|| self.loc.cmp(&other.loc))
    }
}

impl<T> Hash for BindInfo<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.node_id.hash(state);
        self.loc.hash(state);
    }
}

pub type ModuleInfo = BindInfo<node::ModuleBind>;
pub type TypeInfo = BindInfo<node::TypeBind>;
pub type ValueInfo = BindInfo<node::ValueBind>;

pub enum BindKind {
    Module,
    Type,
    Value,
    Local,
}

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnyInfo {
    Module(ModuleInfo),
    Type(TypeInfo),
    Value(ValueInfo),
}

impl AnyInfo {
    pub const fn kind(&self) -> BindKind {
        match self {
            AnyInfo::Module(_) => BindKind::Module,
            AnyInfo::Type(_) => BindKind::Type,
            AnyInfo::Value(_) => BindKind::Value,
        }
    }

    pub const fn location(&self) -> Loc {
        match self {
            AnyInfo::Module(info) => info.loc,
            AnyInfo::Type(info) => info.loc,
            AnyInfo::Value(info) => info.loc,
        }
    }

    pub const fn visibility(&self) -> Vis {
        match self {
            AnyInfo::Module(info) => info.vis,
            AnyInfo::Type(info) => info.vis,
            AnyInfo::Value(info) => info.vis,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleDef {
    IsScope,
    IsLink(ModuleSymbol),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueDef {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDef {}
