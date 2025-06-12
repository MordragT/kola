use std::{cell::RefCell, rc::Rc};

use kola_span::{Diagnostic, Report};
use kola_tree::node::{ModuleName, Vis};
use kola_utils::{
    interner::StrInterner,
    visit::{VisitMap, VisitState},
};

use crate::{
    defs::ModuleDef,
    info::ModuleGraph,
    scope::{ModuleCell, ModuleCells, ModuleScope},
    symbol::ModuleSym,
};

pub struct ModuleResolution {
    pub module_scopes: ModuleCells,
}

/*
 How this works:

1. We start with a list of module scopes, each representing a concrete module in the program.
2. Each module scope contains a list of module references, which are paths to other modules.
3. We take a module scope of the list and mark it as visiting.
4. For every module reference in the scope, we check if it has been visited:
  - If it has not been visited, we resolve the module path recursively.
       1. We take the first name in the path and look it up in the current scope.
       2. If the name is not found, we report an error.
       3. If the name is found, we check if it is exported.
       4. If it is not exported, we report an error.
       5. If it is exported, we check if it has been visited:
           - If it has not been visited, we resolve the module scope recursively.
           - If it is currently being visited, we have a cycle and report an error.
           - If it has been visited, we skip it.
  - If it is currently being visited, we have a cycle and report an error.
  - If it has been visited, we skip it.

*/

pub fn resolve_modules(
    scopes: Vec<ModuleScope>,
    interner: &mut StrInterner,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
) -> ModuleResolution {
    let mut module_visit = VisitMap::new();
    let mut module_scopes = ModuleCells::new();

    let super_name = ModuleName::new(interner.intern("super"));

    // By using a Vec we ensure that the order of modules is preserved,
    // meaning parents are resolved before children.
    for mut scope in scopes.into_iter().rev() {
        if let [parent, rest @ ..] = module_graph.dependents_of(scope.bind.sym()) {
            let parent_scope = &module_scopes[parent];

            if let Err(e) = scope.shape.insert_module(
                super_name,
                *parent,
                ModuleDef::new(parent_scope.borrow().loc, Vis::Export),
            ) {
                report.add_diagnostic(e.into());
            }

            if !rest.is_empty() {
                panic!("Multiple parent modules found for module: {:?}", scope.bind);
            }
        }

        module_scopes.insert(scope.bind.sym(), Rc::new(RefCell::new(scope)));
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
                report.add_diagnostic(Diagnostic::error(
                    scope.borrow().loc,
                    "Module cycle detected",
                ));
                module_visit[sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    ModuleResolution { module_scopes }
}

pub fn resolve_module_scope(
    scope: ModuleCell,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    module_visit: &mut VisitMap<ModuleSym>,
    module_scopes: &mut ModuleCells,
) {
    let sym = scope.borrow().bind.sym();

    module_visit[sym] = VisitState::Visiting;

    for (&sym, module_ref) in scope.borrow().refs.modules() {
        match module_visit[sym] {
            VisitState::Unvisited => resolve_module_path(
                sym,
                module_ref.path(),
                scope.clone(),
                report,
                module_graph,
                module_visit,
                module_scopes,
            ),
            VisitState::Visiting => {
                report.add_diagnostic(Diagnostic::error(
                    scope.borrow().loc,
                    "Module cycle detected",
                ));
                module_visit[sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    module_visit[sym] = VisitState::Visited;
}

// TODO return error type instead of on the fly reporting.
pub fn resolve_module_path<'s>(
    path_sym: ModuleSym,
    mut path: &[ModuleName],
    mut scope: ModuleCell,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    module_visit: &mut VisitMap<ModuleSym>,
    module_scopes: &mut ModuleCells,
) {
    module_visit[path_sym] = VisitState::Visiting;

    let source_sym = scope.borrow().bind.sym();

    while let Some((name, rest)) = path.split_first() {
        let Some(sym) = scope.borrow().shape.lookup_module(*name) else {
            report.add_diagnostic(Diagnostic::error(scope.borrow().loc, "Module not found"));
            module_visit[path_sym] = VisitState::Visited;
            return;
        };

        let def = scope.borrow().shape[sym];

        // Check if the module is exported or part of the current module.
        if def.vis != Vis::Export && scope.borrow().bind.sym() != source_sym {
            report.add_diagnostic(
                Diagnostic::error(def.loc, "Module not exported")
                    .with_help("Only exported modules can be used in paths."),
            );
            module_visit[path_sym] = VisitState::Visited;
            return;
        }

        if module_visit[sym] == VisitState::Visiting {
            report.add_diagnostic(Diagnostic::error(def.loc, "Module cycle detected"));
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
            } else if let Some(module_ref) = scope.borrow().refs.get_module(sym) {
                resolve_module_path(
                    sym,
                    module_ref.path(),
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
            report.add_diagnostic(Diagnostic::error(def.loc, "Module scope not found"));
            module_visit[path_sym] = VisitState::Visited;
            return;
        };

        path = rest;
    }

    module_visit[path_sym] = VisitState::Visited;
    module_graph.add_dependency(source_sym, scope.borrow().bind.sym());
    module_scopes.insert(path_sym, scope);
}
