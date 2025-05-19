use std::ops::Index;

use kola_tree::{id::Id, node};
use kola_utils::{bimap::BiMap, interner::PathKey};

use super::ModuleKey;

#[derive(Debug, Clone, Default)]
pub struct ModuleKeyMapping(BiMap<ModuleKey, (PathKey, Id<node::Module>)>);

impl ModuleKeyMapping {
    pub fn new() -> Self {
        Self(BiMap::new())
    }

    pub fn insert(&mut self, module_key: ModuleKey, path_key: PathKey, node_id: Id<node::Module>) {
        self.0.insert(module_key, (path_key, node_id));
    }

    pub fn get_by_value(&self, value: &(PathKey, Id<node::Module>)) -> Option<ModuleKey> {
        self.0.get_by_value(value).copied()
    }

    pub fn get_by_key(&self, key: &ModuleKey) -> Option<&(PathKey, Id<node::Module>)> {
        self.0.get_by_key(key)
    }
}

impl Index<ModuleKey> for ModuleKeyMapping {
    type Output = (PathKey, Id<node::Module>);

    fn index(&self, key: ModuleKey) -> &Self::Output {
        self.0.get_by_key(&key).unwrap()
    }
}
