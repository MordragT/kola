
use indexmap::IndexMap;
use kola_span::Loc;
use kola_tree::source::ModuleKey;

use crate::{
    name::NameMap,
    phase::NodeMap,
    symbol::ModuleSym,
};

pub type ModuleMap = IndexMap<ModuleSym, Module>;

#[derive(Debug, Clone)]
pub struct Module {
    pub loc: Loc,
    /// Which module tree this module's syntax lives in.
    pub key: ModuleKey,
    pub names: NameMap,
    pub nodes: NodeMap,
}

impl Module {
    pub fn new(loc: Loc, key: ModuleKey) -> Self {
        Self {
            loc,
            key,
            names: NameMap::new(),
            nodes: NodeMap::new(),
        }
    }
}

