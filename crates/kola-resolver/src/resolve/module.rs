use std::collections::HashMap;

use kola_span::{Diagnostic, Report};
use kola_tree::{meta::MetaMapExt, node::Vis};

use crate::{
    constraints::{LocalConstraints, ModuleConst},
    def::DefMap,
    env::ModuleMap,
    phase::ResolvedModule,
    resolve::ModuleGraph,
    symbol::ModuleSym,
};

pub fn resolve_modules(
    modules: &mut ModuleMap,
    local_cons_map: &HashMap<ModuleSym, LocalConstraints>,
    defs: &DefMap,
    module_graph: &mut ModuleGraph,
    report: &mut Report,
) {
    // 1. Iterate over every module that has local constraints to check
    for (&source_sym, local_cons) in local_cons_map {
        for ModuleConst {
            path,
            id,
            source: _, // already in source_sym ?
            loc,
        } in local_cons.modules()
        {
            let mut current_sym = source_sym;
            let mut failed = false;

            // 2. Walk down the path segments linearly
            for name in path {
                // Safe to direct index because the macro-structure is fully resolved!
                let current_module = &modules[&current_sym];

                if let Some(next_sym) = current_module.names.get_module(*name) {
                    let def = defs[next_sym];

                    // 3. Enforce export visibility rules
                    if def.vis != Vis::Export && current_sym != source_sym {
                        report.add_diagnostic(
                            Diagnostic::error(def.loc, "Module not exported")
                                .with_help("Only exported modules can be used in paths.")
                                .with_trace([("Within this module path".into(), *loc)]),
                        );
                        failed = true;
                        break;
                    }
                    current_sym = next_sym;
                } else {
                    report.add_diagnostic(
                        Diagnostic::error(*loc, "Module not found")
                            .with_trace([("Within this module path".into(), *loc)]),
                    );
                    failed = true;
                    break;
                }
            }

            // 4. If the path successfully resolved, record the target metadata
            if !failed {
                module_graph.add_dependency(source_sym, current_sym);

                // Mutate the source module to cache the resolution metadata for downstream passes
                if let Some(source_module) = modules.get_mut(&source_sym) {
                    source_module
                        .nodes
                        .insert_meta(*id, ResolvedModule(current_sym));
                }
            }
        }
    }
}
