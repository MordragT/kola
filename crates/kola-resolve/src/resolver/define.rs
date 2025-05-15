use std::{collections::HashMap, marker::PhantomData, ops::Deref};

use kola_span::Diagnostic;
use kola_tree::{
    node::{self, Vis},
    tree::TreeView,
};
use kola_utils::{interner::StrKey, io::FileSystem};

use crate::{
    forest::Forest,
    module::{ModuleBindInfo, ModuleKey, ModuleScope, UnresolvedModuleScope},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Define {
    pub module_key: ModuleKey,
    pub scope: UnresolvedModuleScope,
}

impl Define {
    pub fn new(module_key: ModuleKey, forest: &mut Forest) -> Self {
        let scope = forest.unresolved_scopes.remove(&module_key).unwrap();
        forest.in_progress.insert(module_key);
        Self { module_key, scope }
    }

    pub fn define(self, forest: &mut Forest) {
        let (path_key, node_id) = forest.mappings[self.module_key];
        let span = forest.span(path_key, node_id);
        let tree = forest.tree(path_key);

        let ModuleScope {
            module_key,
            imports,
            mut modules,
            values,
            types,
            state: _,
        } = self.scope;

        dbg!(&imports);

        // TODO I should probably track insertion order, to make the whole algorithm predictable
        let mut paths = HashMap::new();

        for (name, bind) in modules.iter_mut() {
            let module_key = match *bind.value_id.get(tree.deref()) {
                node::ModuleExpr::Import(import_id) => imports[&import_id],
                node::ModuleExpr::Module(module_id) => *forest
                    .mappings
                    .get_by_value(&(path_key, module_id))
                    .unwrap(), // This should never fail
                node::ModuleExpr::Path(path_id) => {
                    let path = tree.node(path_id);
                    let segments = path
                        .0
                        .iter()
                        .copied()
                        .map(|id| tree.node(id).0)
                        .collect::<Vec<_>>();
                    paths.insert(*name, segments);
                    // Handle module paths in the next step
                    continue;
                }
            };

            bind.set_module_key(module_key);
        }

        let dependents = forest.dependencies.dependents_of(module_key);

        let parent = if dependents.is_empty() {
            None
        } else if let [parent] = dependents {
            Some(*parent)
        } else {
            panic!("At this point every module must only have one parent or none")
        };

        for (name, segments) in paths {
            let module_key = match define_path(&segments, parent, &modules, forest).ok_or(
                Diagnostic::error(span, "Module not found")
                    .with_help("Maybe the module is not marked to be exported."),
            ) {
                Ok(key) => key,
                Err(e) => {
                    forest.report.add_diagnostic(e);
                    continue;
                }
            };

            let info = modules.get_mut(&name).unwrap();
            info.set_module_key(module_key);
        }

        forest.in_progress.remove(&module_key);
        forest.scopes.insert(
            module_key,
            ModuleScope {
                module_key,
                imports,
                modules,
                values,
                types,
                state: PhantomData,
            },
        );
    }
}

// TODO This algorithm doesnt work exactly how I want it to.
// It doesnt handle visibility errors, therefore the error is too generic (can be not found or visibilty error).
// Secondly it doesnt handle the case where the target is a alias itsself that,
// was declared after this alias was declared.
fn define_path(
    segments: &[StrKey],
    parent: Option<ModuleKey>,
    modules: &HashMap<StrKey, ModuleBindInfo>,
    forest: &mut Forest,
) -> Option<ModuleKey> {
    let sup = STR_INTERNER.write().unwrap().intern("super");

    let (name, path) = segments.split_first()?;
    let mut module_key = if *name == sup {
        parent?
    } else {
        let info = modules.get(name)?;
        if info.vis == Vis::Export {
            info.module_key? // here a alias could be found that was declared after this one
        } else {
            return None;
        }
    };

    for name in path {
        if let Some(scope) = forest.unresolved_scopes.remove(&module_key) {
            let define = Define { module_key, scope };
            forest.in_progress.insert(module_key);
            define.define(forest);
        }

        if forest.in_progress.contains(&module_key) {
            // TODO this should probably return an cycle error
            return None;
        }

        let scope = forest.scopes.get(&module_key)?;
        let info = scope.get_module(*name)?;
        if info.vis == Vis::Export {
            module_key = info.module_key.unwrap(); // here all members of the scope are already defined so safe to unwrap
        } else {
            return None;
        }
    }

    Some(module_key)
}
