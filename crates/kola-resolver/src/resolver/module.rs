use std::collections::HashMap;

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
    constraints::{ModuleBindConstraint, ModuleRef},
    defs::ModuleDef,
    functor::Functor,
    info::{ModuleGraph, ModuleInfos},
    phase::ResolvedModule,
    scope::{ModuleScope, ModuleScopes},
    symbol::{FunctorSym, ModuleSym},
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
    functors: &HashMap<FunctorSym, Functor>,
    interner: &mut StrInterner,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
) -> ModuleResolution {
    let mut visit = VisitMap::new();
    let mut scopes = ModuleScopes::new();
    let mut infos = ModuleInfos::new();

    let super_name = ModuleName::new(interner.intern("super"));

    // By using a Vec we ensure that the order of modules is preserved,
    // meaning parents are resolved before children.
    // 1. First insert parent modules into the scope as "super".
    for mut scope in input.into_iter().rev() {
        let mut dependents = module_graph.dependents_of(scope.info.sym);

        if let Some(parent) = dependents.next() {
            let parent_info = &infos[parent];

            if let Err(e) = scope.insert_module(
                super_name,
                *parent,
                ModuleDef::unbound(parent_info.loc, Vis::Export),
            ) {
                report.add_diagnostic(e.into());
            }

            if !dependents.next().is_none() {
                panic!("Multiple parent modules found for module: {:?}", scope.info);
            }
        }

        let sym = scope.info.sym;
        infos.insert(sym, scope.info);
        scopes.insert(sym, scope);
    }

    // 2. Then resolve all module binds and their scopes.
    for sym in scopes.keys().copied().collect::<Vec<_>>() {
        match visit[sym] {
            VisitState::Unvisited => resolve_module_scope(
                sym,
                functors,
                report,
                module_graph,
                &mut visit,
                &mut scopes,
                interner,
            ),
            VisitState::Visiting => {
                report.add_diagnostic(
                    Diagnostic::error(infos[&sym].loc, "Module cycle detected")
                        .with_notes(["While trying to resolve module bindings.".into()]),
                );
                visit[sym] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    // 3. Finally resolve all module references without a bind.
    for sym in scopes.keys().copied().collect::<Vec<_>>() {
        let module_refs = scopes[&sym].cons.modules().to_vec();
        for module_ref in module_refs {
            resolve_module_ref(sym, &module_ref, report, module_graph, &mut scopes);
        }
    }

    ModuleResolution {
        module_scopes: scopes,
    }
}

pub fn resolve_module_scope(
    scope: ModuleSym,
    functors: &HashMap<FunctorSym, Functor>,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    visit: &mut VisitMap<ModuleSym>,
    scopes: &mut ModuleScopes,
    interner: &StrInterner,
) {
    visit[scope] = VisitState::Visiting;

    for (bind, constraint) in scopes[&scope].cons.module_binds().clone() {
        match visit[bind] {
            VisitState::Unvisited => resolve_module_bind(
                constraint,
                scope,
                functors,
                report,
                module_graph,
                visit,
                scopes,
                interner,
            ),
            VisitState::Visiting => {
                report.add_diagnostic(
                    Diagnostic::error(scopes[&scope].info.loc, "Module cycle detected")
                        .with_notes(["While trying to resolve module bindings.".into()]),
                );
                visit[bind] = VisitState::Visited;
            }
            VisitState::Visited => (),
        }
    }

    visit[scope] = VisitState::Visited;
}

// TODO return error type instead of on the fly reporting.
pub fn resolve_module_bind(
    constraint: ModuleBindConstraint,
    scope_sym: ModuleSym,
    functors: &HashMap<FunctorSym, Functor>,
    report: &mut Report,
    module_graph: &mut ModuleGraph,
    visit: &mut VisitMap<ModuleSym>,
    scopes: &mut ModuleScopes,
    interner: &StrInterner,
) {
    match constraint {
        ModuleBindConstraint::Functor {
            id,
            bind,
            loc,
            functor,
            arg,
        } => {
            visit[bind] = VisitState::Visiting;

            let scope = &scopes[&scope_sym];

            let Some(functor_sym) = scope.shape.get_functor(functor) else {
                report.add_diagnostic(
                    Diagnostic::error(loc, "Functor not found").with_trace([(
                        "While trying to resolve a functor application".into(),
                        loc,
                    )]),
                );
                visit[bind] = VisitState::Visited;
                return;
            };

            let def = scope.defs[functor_sym];

            if def.vis != Vis::Export && scope.info.sym != scope_sym {
                report.add_diagnostic(
                    Diagnostic::error(def.loc, "Functor not exported")
                        .with_help("Only exported functors can be used in functor applications.")
                        .with_trace([(
                            "While trying to resolve a functor application".into(),
                            loc,
                        )]),
                );
                visit[bind] = VisitState::Visited;
                return;
            }

            let functor = functors[&functor_sym].clone();

            if visit[arg] == VisitState::Visiting {
                report.add_diagnostic(Diagnostic::error(loc, "Module cycle detected").with_notes(
                    ["While trying to resolve a module path inside a functor application".into()],
                ));
                visit[arg] = VisitState::Visited;
                // TODO should also visit[bind] = VisitState::Visited; ?
                return;
            }

            if visit[arg] == VisitState::Unvisited {
                if scopes.contains_key(&arg) {
                    resolve_module_scope(
                        arg,
                        functors,
                        report,
                        module_graph,
                        visit,
                        scopes,
                        interner,
                    );
                } else if let Some(constraint) = scope.cons.get_module_bind(arg).cloned() {
                    resolve_module_bind(
                        constraint,
                        scope.info.sym,
                        functors,
                        report,
                        module_graph,
                        visit,
                        scopes,
                        interner,
                    );
                } else {
                    unreachable!()
                }
            }

            let mut scope = functor.apply(arg);
            // module_graph.add_dependency(bind, scope.info.sym); // Add the functor body as dependency on the applied module.
            scope.info.sym = bind;
            let id = scope.info.id;
            scope.resolved.insert_meta(id, bind);
            // TODO unsure if maybe also other info fields need updating.

            visit[bind] = VisitState::Visited;
            module_graph.add_dependency(scope_sym, bind);
            scopes.insert(bind, scope);

            // Do a resolve of the now applied functor scope.
            // TODO this will not work properly if module binds depend on the defining scope.
            // E.g.:
            // module alpha = { ... },
            // module functor f a => { module depend_on_outer = alpha, }
            resolve_module_scope(
                bind,
                functors,
                report,
                module_graph,
                visit,
                scopes,
                interner,
            );
        }
        ModuleBindConstraint::Path {
            id,
            bind,
            loc,
            path,
        } => {
            visit[bind] = VisitState::Visiting;

            let mut scope = &scopes[&scope_sym];
            let mut path = path.as_slice();

            while let Some((name, rest)) = path.split_first() {
                let Some(sym) = scope.shape.get_module(*name) else {
                    report.add_diagnostic(Diagnostic::error(scope.info.loc, "Module not found"));
                    visit[bind] = VisitState::Visited;
                    return;
                };

                let def = scope.defs[sym];

                // Check if the module is exported or part of the current module.
                if def.vis != Vis::Export && scope.info.sym != scope_sym {
                    report.add_diagnostic(
                        Diagnostic::error(def.loc, "Module not exported")
                            .with_help("Only exported modules can be used in paths.")
                            .with_trace([("Within this module path".into(), loc)]),
                    );
                    visit[bind] = VisitState::Visited;
                    return;
                }

                if visit[sym] == VisitState::Visiting {
                    report.add_diagnostic(
                        Diagnostic::error(def.loc, "Module cycle detected")
                            .with_notes([
                                "While trying to resolve a module path inside a binding".into()
                            ])
                            .with_trace([("Within this module path".into(), loc)]),
                    );
                    visit[sym] = VisitState::Visited;
                    // TODO should also visit[bind] = VisitState::Visited; ?
                    return;
                }

                if visit[sym] == VisitState::Unvisited {
                    if scopes.contains_key(&sym) {
                        resolve_module_scope(
                            sym,
                            functors,
                            report,
                            module_graph,
                            visit,
                            scopes,
                            interner,
                        );
                    } else if let Some(constraint) = scope.cons.get_module_bind(sym).cloned() {
                        resolve_module_bind(
                            constraint,
                            scope.info.sym,
                            functors,
                            report,
                            module_graph,
                            visit,
                            scopes,
                            interner,
                        );
                    } else {
                        unreachable!()
                    }
                }

                scope = if let Some(scope) = scopes.get(&sym) {
                    scope
                } else {
                    report.add_diagnostic(
                        Diagnostic::error(def.loc, "Module scope not found")
                            .with_notes([
                                "While trying to resolve a module path inside a binding".into()
                            ])
                            .with_trace([("Within this module path".into(), loc)]),
                    );
                    visit[bind] = VisitState::Visited;
                    return;
                };

                path = rest;
            }

            visit[bind] = VisitState::Visited;
            module_graph.add_dependency(scope_sym, scope.info.sym);
            scopes.insert(bind, scope.clone());
        }
    }
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
            report.add_diagnostic(
                Diagnostic::error(scope.info.loc, "Module not found")
                    .with_trace([("Within this module path".into(), *loc)]),
            );
            return;
        };

        let def = scope.defs[sym];

        // Check if the module is exported or part of the current module.
        if def.vis != Vis::Export && scope.info.sym != source_sym {
            report.add_diagnostic(
                Diagnostic::error(def.loc, "Module not exported")
                    .with_help("Only exported modules can be used in paths.")
                    .with_trace([("Within this module path".into(), *loc)]),
                // TODO loc of module ref in here
            );
            return;
        }

        scope = if let Some(scope) = scopes.get(&sym) {
            scope
        } else {
            report.add_diagnostic(
                Diagnostic::error(def.loc, "Module scope not found")
                    .with_trace([("Within this module path".into(), *loc)]),
            );
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
