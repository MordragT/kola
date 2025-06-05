use kola_span::Diagnostic;
use kola_tree::node::{self, Vis};
use kola_utils::interner::StrKey;

use crate::{
    context::ResolveContext,
    info::ModuleDef,
    scope::{ModuleScope, Rib},
    symbol::ModuleSymbol,
};

pub fn define(scope: ModuleScope, ctx: &mut ResolveContext) {
    ctx.todo.remove(&scope.symbol());
    ctx.active.insert(scope.symbol());

    // TODO I should probably track insertion order, to make the whole algorithm predictable
    for (sym, bind) in &scope.modules {
        if let Some(scope) = ctx.module_scopes.get(sym) {
            ctx.def_table.insert_module(*sym, ModuleDef::IsScope);
        } else if let Some(path) = ctx.module_paths.remove(sym) {
            let dependents = ctx.module_graph.dependents_of(*sym);

            let parent = if dependents.is_empty() {
                None
            } else if let [parent] = dependents {
                Some(*parent)
            } else {
                panic!("At this point every module must only have one parent or none")
            };

            let target_sym = match define_path(&path, parent, &scope.modules, ctx).ok_or(
                Diagnostic::error(bind.loc, "Module not found")
                    .with_help("Maybe the module is not marked to be exported."),
            ) {
                Ok(key) => key,
                Err(e) => {
                    ctx.report.add_diagnostic(e);
                    continue;
                }
            };

            ctx.def_table
                .insert_module(*sym, ModuleDef::IsLink(target_sym));
        } else {
            // This means that the symbol is neither a inline module, a import nor a path,
            // which should not be possible at this point.
            unreachable!()
        }
    }

    ctx.active.remove(&scope.symbol());
    ctx.module_scopes.insert(scope.symbol(), scope);
}

// TODO This algorithm doesnt work exactly how I want it to.
// It doesnt handle visibility errors, therefore the error is too generic (can be not found or visibilty error).
// Secondly it doesnt handle the case where the target is a alias itsself that,
// was declared after this alias was declared.
fn define_path(
    segments: &[StrKey],
    parent: Option<ModuleSymbol>,
    modules: &Rib<node::ModuleBind>,
    ctx: &mut ResolveContext,
) -> Option<ModuleSymbol> {
    let sup = ctx.interner.intern("super");

    let (name, path) = segments.split_first()?;
    let mut module_sym = if *name == sup {
        parent?
    } else {
        let symbol = modules.lookup_sym(*name)?;
        let info = modules.get_by_sym(symbol)?;

        if info.vis != Vis::Export {
            return None; // if the module is not exported, we cannot resolve it
        }

        symbol
    };

    for name in path {
        if ctx.todo.contains(&module_sym) {
            let scope = ctx.module_scopes.remove(&module_sym)?;
            define(scope, ctx);
        }

        if ctx.active.contains(&module_sym) {
            // TODO this should probably return an cycle error
            return None;
        }

        let scope = ctx.module_scopes.get(&module_sym)?;
        module_sym = scope.modules.lookup_sym(*name)?;

        let info = scope.modules.get_by_sym(module_sym)?;
        if info.vis != Vis::Export {
            return None; // if the module is not exported, we cannot resolve ito safe to unwrap
        }
    }

    Some(module_sym)
}
