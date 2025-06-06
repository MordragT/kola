use std::rc::Rc;

use kola_span::Diagnostic;
use kola_tree::node::Vis;
use kola_utils::interner::StrKey;

use crate::{context::ResolveContext, scope::ModuleScope, symbol::ModuleSymbol};

pub fn define_module_scope(scope: Rc<ModuleScope>, ctx: &mut ResolveContext) {
    ctx.pending.remove(&scope.symbol());
    ctx.active.insert(scope.symbol());

    // TODO It is a bit confusing that Symbol<ModuleBind> might have no ModuleBind (if it is a ModulePath) node.
    // Maybe I should really make the Symbol abstraction independent of the node type.
    for (sym, path) in &scope.paths {
        define_module_path(*sym, path, scope.clone(), ctx);
    }

    ctx.active.remove(&scope.symbol);
}

// TODO return error type instead of on the fly reporting.
pub fn define_module_path<'s>(
    path_sym: ModuleSymbol,
    mut path: &[StrKey],
    mut scope: Rc<ModuleScope>,
    ctx: &'s mut ResolveContext,
) {
    let source_sym = scope.symbol();

    ctx.pending.remove(&path_sym);
    ctx.active.insert(path_sym);

    while let Some((name, rest)) = path.split_first() {
        // TODO do not special case super here, but rather insert it in the scope itsself.

        let Some(sym) = scope.modules.lookup_sym(*name) else {
            ctx.report
                .add_diagnostic(Diagnostic::error(scope.loc, "Module not found"));
            ctx.active.remove(&path_sym);
            return;
        };

        let bind = scope.modules[sym];

        // Check if the module is exported or part of the current module.
        if bind.vis != Vis::Export && scope.symbol() != path_sym {
            ctx.report.add_diagnostic(
                Diagnostic::error(bind.loc, "Module not exported")
                    .with_help("Only exported modules can be used in paths."),
            );
            ctx.active.remove(&path_sym);
            return;
        }

        if ctx.active.contains(&sym) {
            ctx.report
                .add_diagnostic(Diagnostic::error(bind.loc, "Module cycle detected"));
            ctx.active.remove(&path_sym);
            return;
        }

        if ctx.pending.contains(&sym) {
            if let Some(scope) = ctx.module_scopes.get(&sym) {
                define_module_scope(scope.clone(), ctx);
            } else if let Some(path) = scope.paths.get_by_key(&sym) {
                define_module_path(sym, &path, scope.clone(), ctx);
            } else {
                unreachable!()
            }
        }

        scope = if let Some(scope) = ctx.module_scopes.get(&sym) {
            scope.clone()
        } else {
            ctx.report
                .add_diagnostic(Diagnostic::error(bind.loc, "Module scope not found"));
            ctx.active.remove(&path_sym);
            return;
        };

        path = rest;
    }

    ctx.active.remove(&path_sym);
    ctx.module_graph.add_dependency(source_sym, scope.symbol());
    ctx.module_scopes.insert(path_sym, scope);
}
