
use indexmap::IndexMap;
use kola_span::Loc;
use kola_tree::source::ModuleKey;

use crate::{
    name::NameMap,
    phase::NodeMap,
    surface::ModuleSurface,
    symbol::ModuleSym,
};

pub type ModuleMap = IndexMap<ModuleSym, ModuleBuilder>;

#[derive(Debug, Clone)]
pub struct ModuleBuilder {
    pub loc: Loc,
    /// Which module tree this module's syntax lives in.
    pub key: ModuleKey,
    pub names: NameMap,
    pub nodes: NodeMap,
    pub surface: ModuleSurface,
}

impl ModuleBuilder {
    pub fn new(loc: Loc, key: ModuleKey) -> Self {
        Self {
            loc,
            key,
            names: NameMap::new(),
            nodes: NodeMap::new(),
            surface: ModuleSurface::new(),
        }
    }
}

