use std::collections::HashMap;

use kola_tree::{id::Id, node, tree::TreeAccess};

use crate::module::{ModuleId, ModuleInfo};

pub struct Explorer {}

impl Explorer {
    // When this is called I already have parsed the tree and can use it's root
    pub fn explore(
        id: ModuleId,
        modules: &HashMap<ModuleId, ModuleInfo>,
        tree: &impl TreeAccess,
    ) -> ModuleInfo {
        todo!()
    }
}
