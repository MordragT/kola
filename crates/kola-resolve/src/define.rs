use std::{collections::HashMap, marker::PhantomData, ops::Deref};

use kola_span::Diagnostic;
use kola_tree::{
    node::{self, Vis},
    tree::TreeView,
};
use kola_utils::interner::StrKey;

use crate::{
    context::ResolveContext,
    module::{ModuleBindInfo, ModuleKey, ModuleScope, UnresolvedModuleScope},
};

pub fn define(module_key: ModuleKey, scope: UnresolvedModuleScope, ctx: &mut ResolveContext) {
    let ModuleScope {
        module_key,
        imports,
        mut modules,
        values,
        types,
        state: _,
    } = scope;

    ctx.in_progress.insert(module_key);

    let (path_key, node_id) = ctx.mapping[module_key];
    let span = ctx.topography.span(path_key, node_id);
    let tree = ctx.forest.tree(path_key);

    dbg!(&imports);

    // TODO I should probably track insertion order, to make the whole algorithm predictable
    let mut paths = HashMap::new();

    for (name, bind) in modules.iter_mut() {
        let module_key = match *bind.value_id.get(tree.deref()) {
            node::ModuleExpr::Import(import_id) => imports[&import_id],
            node::ModuleExpr::Module(module_id) => {
                ctx.mapping.get_by_value(&(path_key, module_id)).unwrap()
            } // This should never fail
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

    let dependents = ctx.dependencies.dependents_of(module_key);

    let parent = if dependents.is_empty() {
        None
    } else if let [parent] = dependents {
        Some(*parent)
    } else {
        panic!("At this point every module must only have one parent or none")
    };

    for (name, segments) in paths {
        let module_key = match define_path(&segments, parent, &modules, ctx).ok_or(
            Diagnostic::error(span, "Module not found")
                .with_help("Maybe the module is not marked to be exported."),
        ) {
            Ok(key) => key,
            Err(e) => {
                ctx.report.add_diagnostic(e);
                continue;
            }
        };

        let info = modules.get_mut(&name).unwrap();
        info.set_module_key(module_key);
    }

    ctx.in_progress.remove(&module_key);
    ctx.scopes.insert(
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

// TODO This algorithm doesnt work exactly how I want it to.
// It doesnt handle visibility errors, therefore the error is too generic (can be not found or visibilty error).
// Secondly it doesnt handle the case where the target is a alias itsself that,
// was declared after this alias was declared.
fn define_path(
    segments: &[StrKey],
    parent: Option<ModuleKey>,
    modules: &HashMap<StrKey, ModuleBindInfo>,
    ctx: &mut ResolveContext,
) -> Option<ModuleKey> {
    let sup = ctx.interner.intern("super");

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
        if let Some(scope) = ctx.unresolved.remove(&module_key) {
            define(module_key, scope, ctx);
        }

        if ctx.in_progress.contains(&module_key) {
            // TODO this should probably return an cycle error
            return None;
        }

        let scope = ctx.scopes.get(module_key)?;
        let info = scope.get_module(*name)?;
        if info.vis == Vis::Export {
            module_key = info.module_key.unwrap(); // here all members of the scope are already defined so safe to unwrap
        } else {
            return None;
        }
    }

    Some(module_key)
}
