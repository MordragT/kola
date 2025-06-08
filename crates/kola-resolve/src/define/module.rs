use std::rc::Rc;

use kola_span::Diagnostic;
use kola_tree::node::Vis;
use kola_utils::{interner::StrKey, visit::VisitState};

use crate::{context::ResolveContext, info::ModuleInfo, scope::ModuleScope, symbol::ModuleSym};

pub fn define_modules(ctx: &mut ResolveContext) {
    let super_name = ctx.interner.intern("super");

    let order = ctx.module_graph.topological_sort().unwrap();

    for sym in order.into_iter().rev() {
        let mut scope = ctx.unresolved_scopes.remove(&sym).unwrap();

        if let [parent, rest @ ..] = ctx.module_graph.dependents_of(scope.symbol()) {
            let parent_scope = &ctx.module_scopes[parent];
            scope.insert_module(
                super_name,
                *parent,
                ModuleInfo::new(parent_scope.id, parent_scope.loc, Vis::Export),
            );

            if !rest.is_empty() {
                panic!(
                    "Multiple parent modules found for module: {}",
                    scope.symbol()
                );
            }
        }

        ctx.module_scopes.insert(scope.symbol(), Rc::new(scope));
    }

    for (sym, scope) in ctx.module_scopes.clone() {
        match ctx.module_visit[sym] {
            VisitState::Unvisited => define_module_scope(scope, ctx),
            VisitState::Visiting => {
                ctx.report
                    .add_diagnostic(Diagnostic::error(scope.loc, "Module cycle detected"));
                ctx.module_visit[sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }
}

pub fn define_module_scope(scope: Rc<ModuleScope>, ctx: &mut ResolveContext) {
    ctx.module_visit[scope.symbol()] = VisitState::Visiting;

    for (sym, path) in &scope.paths {
        match ctx.module_visit[*sym] {
            VisitState::Unvisited => define_module_path(*sym, path, scope.clone(), ctx),
            VisitState::Visiting => {
                ctx.report
                    .add_diagnostic(Diagnostic::error(scope.loc, "Module cycle detected"));
                ctx.module_visit[*sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    ctx.module_visit[scope.symbol()] = VisitState::Visited;
}

// TODO return error type instead of on the fly reporting.
pub fn define_module_path<'s>(
    path_sym: ModuleSym,
    mut path: &[StrKey],
    mut scope: Rc<ModuleScope>,
    ctx: &'s mut ResolveContext,
) {
    ctx.module_visit[path_sym] = VisitState::Visiting;

    let source_sym = scope.symbol();

    while let Some((name, rest)) = path.split_first() {
        // TODO do not special case super here, but rather insert it in the scope itsself.

        let Some(sym) = scope.modules.lookup_sym(*name) else {
            ctx.report
                .add_diagnostic(Diagnostic::error(scope.loc, "Module not found"));
            ctx.module_visit[path_sym] = VisitState::Visited;
            return;
        };

        let bind = scope.modules[sym];

        // Check if the module is exported or part of the current module.
        if bind.vis != Vis::Export && scope.symbol() != source_sym {
            ctx.report.add_diagnostic(
                Diagnostic::error(bind.loc, "Module not exported")
                    .with_help("Only exported modules can be used in paths."),
            );
            ctx.module_visit[path_sym] = VisitState::Visited;
            return;
        }

        if ctx.module_visit[sym] == VisitState::Visiting {
            ctx.report
                .add_diagnostic(Diagnostic::error(bind.loc, "Module cycle detected"));
            ctx.module_visit[sym] = VisitState::Visited;
            return;
        }

        if ctx.module_visit[sym] == VisitState::Unvisited {
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
            ctx.module_visit[path_sym] = VisitState::Visited;
            return;
        };

        path = rest;
    }

    ctx.module_visit[path_sym] = VisitState::Visited;
    ctx.module_graph.add_dependency(source_sym, scope.symbol());
    ctx.module_scopes.insert(path_sym, scope);
}
