use std::rc::Rc;

use kola_span::{Diagnostic, Report};
use kola_tree::node::Vis;
use kola_utils::{
    interner::{StrInterner, StrKey},
    visit::{VisitMap, VisitState},
};

use super::{ModuleGraph, ModuleScopes};
use crate::{info::ModuleInfo, resolver::MutModuleScopes, scope::ModuleScope, symbol::ModuleSym};

pub struct ModuleResolution {
    pub module_scopes: ModuleScopes,
}

pub fn resolve_modules(
    scopes: MutModuleScopes,
    interner: &mut StrInterner,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
) -> ModuleResolution {
    let mut module_visit = VisitMap::new();
    let mut module_scopes = ModuleScopes::new();

    let super_name = interner.intern("super");

    // By using a IndexMap, we ensure that the order of modules is preserved,
    // meaning parents are resolved before children.
    for mut scope in scopes.into_values().rev() {
        if let [parent, rest @ ..] = module_graph.dependents_of(scope.symbol()) {
            let parent_scope = &module_scopes[parent];

            if let Err(e) = scope.insert_module(
                super_name,
                *parent,
                ModuleInfo::new(parent_scope.loc, Vis::Export),
            ) {
                report.add_diagnostic(e.into());
            }

            if !rest.is_empty() {
                panic!(
                    "Multiple parent modules found for module: {}",
                    scope.symbol()
                );
            }
        }

        module_scopes.insert(scope.symbol(), Rc::new(scope));
    }

    for (sym, scope) in module_scopes.clone() {
        match module_visit[sym] {
            VisitState::Unvisited => resolve_module_scope(
                scope,
                report,
                module_graph,
                &mut module_visit,
                &mut module_scopes,
            ),
            VisitState::Visiting => {
                report.add_diagnostic(Diagnostic::error(scope.loc, "Module cycle detected"));
                module_visit[sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    ModuleResolution { module_scopes }
}

pub fn resolve_module_scope(
    scope: Rc<ModuleScope>,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    module_visit: &mut VisitMap<ModuleSym>,
    module_scopes: &mut ModuleScopes,
) {
    module_visit[scope.symbol()] = VisitState::Visiting;

    for (sym, path) in &scope.paths {
        match module_visit[*sym] {
            VisitState::Unvisited => resolve_module_path(
                *sym,
                path,
                scope.clone(),
                report,
                module_graph,
                module_visit,
                module_scopes,
            ),
            VisitState::Visiting => {
                report.add_diagnostic(Diagnostic::error(scope.loc, "Module cycle detected"));
                module_visit[*sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    module_visit[scope.symbol()] = VisitState::Visited;
}

// TODO return error type instead of on the fly reporting.
pub fn resolve_module_path<'s>(
    path_sym: ModuleSym,
    mut path: &[StrKey],
    mut scope: Rc<ModuleScope>,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    module_visit: &mut VisitMap<ModuleSym>,
    module_scopes: &mut ModuleScopes,
) {
    module_visit[path_sym] = VisitState::Visiting;

    let source_sym = scope.symbol();

    while let Some((name, rest)) = path.split_first() {
        // TODO do not special case super here, but rather insert it in the scope itsself.

        let Some(sym) = scope.modules.lookup_sym(*name) else {
            report.add_diagnostic(Diagnostic::error(scope.loc, "Module not found"));
            module_visit[path_sym] = VisitState::Visited;
            return;
        };

        let bind = scope.modules[sym];

        // Check if the module is exported or part of the current module.
        if bind.vis != Vis::Export && scope.symbol() != source_sym {
            report.add_diagnostic(
                Diagnostic::error(bind.loc, "Module not exported")
                    .with_help("Only exported modules can be used in paths."),
            );
            module_visit[path_sym] = VisitState::Visited;
            return;
        }

        if module_visit[sym] == VisitState::Visiting {
            report.add_diagnostic(Diagnostic::error(bind.loc, "Module cycle detected"));
            module_visit[sym] = VisitState::Visited;
            return;
        }

        if module_visit[sym] == VisitState::Unvisited {
            if let Some(scope) = module_scopes.get(&sym) {
                resolve_module_scope(
                    scope.clone(),
                    report,
                    module_graph,
                    module_visit,
                    module_scopes,
                );
            } else if let Some(path) = scope.paths.get_by_key(&sym) {
                resolve_module_path(
                    sym,
                    &path,
                    scope.clone(),
                    report,
                    module_graph,
                    module_visit,
                    module_scopes,
                );
            } else {
                unreachable!()
            }
        }

        scope = if let Some(scope) = module_scopes.get(&sym) {
            scope.clone()
        } else {
            report.add_diagnostic(Diagnostic::error(bind.loc, "Module scope not found"));
            module_visit[path_sym] = VisitState::Visited;
            return;
        };

        path = rest;
    }

    module_visit[path_sym] = VisitState::Visited;
    module_graph.add_dependency(source_sym, scope.symbol());
    module_scopes.insert(path_sym, scope);
}
