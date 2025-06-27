use kola_span::{Diagnostic, Report};
use kola_tree::{
    meta::MetaMapExt,
    node::{ModuleName, Vis},
};
use kola_utils::{
    interner::StrInterner,
    visit::{VisitMap, VisitState},
};

use crate::{
    defs::ModuleDef,
    info::{ModuleGraph, ModuleInfos},
    phase::ResolvedModule,
    refs::{ModuleBindRef, ModuleRef},
    scope::{ModuleScope, ModuleScopes},
    symbol::ModuleSym,
};

pub struct ModuleResolution {
    pub module_scopes: ModuleScopes,
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
    input: Vec<ModuleScope>,
    interner: &mut StrInterner,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
) -> ModuleResolution {
    let mut visit = VisitMap::new();
    let mut scopes = ModuleScopes::new();
    let mut infos = ModuleInfos::new();
    let mut symbols = Vec::new();

    let super_name = ModuleName::new(interner.intern("super"));

    // By using a Vec we ensure that the order of modules is preserved,
    // meaning parents are resolved before children.
    // 1. First insert parent modules into the scope as "super".
    for mut scope in input.into_iter().rev() {
        if let [parent, rest @ ..] = module_graph.dependents_of(scope.info.sym) {
            let parent_info = &infos[parent];

            if let Err(e) = scope.insert_module(
                super_name,
                *parent,
                ModuleDef::unbound(parent_info.loc, Vis::Export),
            ) {
                report.add_diagnostic(e.into());
            }

            if !rest.is_empty() {
                panic!("Multiple parent modules found for module: {:?}", scope.info);
            }
        }

        let sym = scope.info.sym;
        symbols.push(sym);
        infos.insert(sym, scope.info);
        scopes.insert(sym, scope);
    }

    // 2. Then resolve all module binds and their scopes.
    for sym in symbols.clone() {
        match visit[sym] {
            VisitState::Unvisited => {
                resolve_module_scope(sym, report, module_graph, &mut visit, &mut scopes)
            }
            VisitState::Visiting => {
                report.add_diagnostic(Diagnostic::error(infos[&sym].loc, "Module cycle detected"));
                visit[sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    // 3. Finally resolve all module references without a bind.
    for sym in symbols {
        let module_refs = scopes[&sym].refs.modules().to_vec();
        for module_ref in module_refs {
            resolve_module_ref(sym, &module_ref, report, module_graph, &mut scopes);
        }
    }

    ModuleResolution {
        module_scopes: scopes,
    }
}

pub fn resolve_module_scope(
    scope_sym: ModuleSym,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    visit: &mut VisitMap<ModuleSym>,
    scopes: &mut ModuleScopes,
) {
    visit[scope_sym] = VisitState::Visiting;

    for (bind_sym, bind_ref) in scopes[&scope_sym].refs.module_binds().clone() {
        match visit[bind_sym] {
            VisitState::Unvisited => resolve_module_bind_ref(
                bind_sym,
                bind_ref,
                scope_sym,
                report,
                module_graph,
                visit,
                scopes,
            ),
            VisitState::Visiting => {
                report.add_diagnostic(Diagnostic::error(
                    scopes[&scope_sym].info.loc,
                    "Module cycle detected",
                ));
                visit[bind_sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    visit[scope_sym] = VisitState::Visited;
}

// TODO return error type instead of on the fly reporting.
pub fn resolve_module_bind_ref(
    bind_sym: ModuleSym,
    bind_ref: ModuleBindRef,
    source_sym: ModuleSym,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    visit: &mut VisitMap<ModuleSym>,
    scopes: &mut ModuleScopes,
) {
    visit[bind_sym] = VisitState::Visiting;

    let mut path = bind_ref.path();
    let mut scope = &scopes[&source_sym].clone();

    while let Some((name, rest)) = path.split_first() {
        let Some(sym) = scope.shape.get_module(*name) else {
            report.add_diagnostic(Diagnostic::error(scope.info.loc, "Module not found"));
            visit[bind_sym] = VisitState::Visited;
            return;
        };

        let def = scope.defs[sym];

        // Check if the module is exported or part of the current module.
        if def.vis != Vis::Export && scope.info.sym != source_sym {
            report.add_diagnostic(
                Diagnostic::error(def.loc, "Module not exported")
                    .with_help("Only exported modules can be used in paths."),
            );
            visit[bind_sym] = VisitState::Visited;
            return;
        }

        if visit[sym] == VisitState::Visiting {
            report.add_diagnostic(Diagnostic::error(def.loc, "Module cycle detected"));
            visit[sym] = VisitState::Visited;
            return;
        }

        if visit[sym] == VisitState::Unvisited {
            if scopes.contains_key(&sym) {
                resolve_module_scope(sym, report, module_graph, visit, scopes);
            } else if let Some(module_ref) = scope.refs.get_module_bind(sym).cloned() {
                resolve_module_bind_ref(
                    sym,
                    module_ref,
                    scope.info.sym,
                    report,
                    module_graph,
                    visit,
                    scopes,
                );
            } else {
                unreachable!()
            }
        }

        scope = if let Some(scope) = scopes.get(&sym) {
            scope
        } else {
            report.add_diagnostic(Diagnostic::error(def.loc, "Module scope not found"));
            visit[bind_sym] = VisitState::Visited;
            return;
        };

        path = rest;
    }

    visit[bind_sym] = VisitState::Visited;
    module_graph.add_dependency(source_sym, scope.info.sym);
    scopes.insert(bind_sym, scope.clone());
}

pub fn resolve_module_ref(
    source_sym: ModuleSym,
    module_ref: &ModuleRef,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    scopes: &mut ModuleScopes,
) {
    let ModuleRef {
        path,
        id,
        source,
        loc,
    } = module_ref;

    let mut path = path.as_slice();
    let mut scope = &scopes[&source_sym];

    while let Some((name, rest)) = path.split_first() {
        let Some(sym) = scope.shape.get_module(*name) else {
            report.add_diagnostic(Diagnostic::error(scope.info.loc, "Module not found"));
            return;
        };

        let def = scope.defs[sym];

        // Check if the module is exported or part of the current module.
        if def.vis != Vis::Export && scope.info.sym != source_sym {
            report.add_diagnostic(
                Diagnostic::error(def.loc, "Module not exported")
                    .with_help("Only exported modules can be used in paths."),
                // TODO loc of module ref in here
            );
            return;
        }

        scope = if let Some(scope) = scopes.get(&sym) {
            scope
        } else {
            report.add_diagnostic(Diagnostic::error(def.loc, "Module scope not found"));
            return;
        };

        path = rest;
    }

    let target = scope.info.sym;

    module_graph.add_dependency(source_sym, target);
    scopes[&source_sym]
        .resolved
        .insert_meta(*id, ResolvedModule(target));
}
