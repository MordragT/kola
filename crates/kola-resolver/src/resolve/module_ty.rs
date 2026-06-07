use kola_span::{Diagnostic, Issue, Report};
use kola_tree::meta::MetaMapExt;
use log::debug;
use std::collections::HashMap;

use crate::{
    constraints::{LocalConstraints, ModuleTypeBindConst, ModuleTypeConst},
    env::ModuleMap,
    phase::ResolvedModuleType,
    resolve::{ModuleTypeGraph, ModuleTypeOrders}, // Assuming ModuleTypeGraph lives here
    symbol::ModuleSym,
};

pub struct ModuleTypeResolution {
    pub module_type_orders: ModuleTypeOrders,
}

pub fn resolve_module_types(
    modules: &mut ModuleMap,
    local_cons_map: &HashMap<ModuleSym, LocalConstraints>,
    module_order: &[ModuleSym],
    report: &mut Report,
) -> ModuleTypeResolution {
    let mut module_type_orders = ModuleTypeOrders::new();

    // Process each module following the verified global topological order
    for &sym in module_order {
        // Not all modules (like empty files or re-exports) necessarily have local constraints
        if let Some(local_cons) = local_cons_map.get(&sym) {
            // Build a localized dependency graph to determine declaration order inside this specific module
            let mut module_type_graph = ModuleTypeGraph::new();

            resolve_module_types_in_module(
                sym,
                modules,
                local_cons,
                &mut module_type_graph,
                report,
            );

            // Sort the internal module type definitions to catch cyclic type signatures
            match module_type_graph.topological_sort() {
                Ok(order) => {
                    module_type_orders.insert(sym, order);
                }
                Err(cycle) => {
                    report.add_issue(
                        Issue::error(cycle.to_string(), 0).with_help(
                            "Check for circular dependencies in module type definitions.",
                        ),
                    );

                    debug!(
                        "Internal module type cycle detected inside module symbol {:?}:\n{}",
                        sym,
                        module_type_graph.to_dot()
                    );
                }
            }
        }
    }

    ModuleTypeResolution { module_type_orders }
}

fn resolve_module_types_in_module(
    module_sym: ModuleSym,
    modules: &mut ModuleMap,
    local_cons: &LocalConstraints,
    module_type_graph: &mut ModuleTypeGraph,
    report: &mut Report,
) {
    // Grab the specific target module.
    // Disjoint field borrowing allows us to read from `.names` and write to `.resolved` simultaneously.
    let Some(module) = modules.get_mut(&module_sym) else {
        return;
    };

    // 1. Resolve local module type binding references (e.g., dependencies inside signature declarations)
    for &ModuleTypeBindConst {
        name,
        id,
        source,
        loc,
    } in local_cons.module_type_binds()
    {
        if let Some(target) = module.names.get_module_type(name) {
            module_type_graph.add_dependency(source, target);
            module.nodes.insert_meta(id, ResolvedModuleType(target));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Module type not found")
                    .with_help("Check that the module type is defined in this module."),
            );
        }
    }

    // 2. Resolve flat module type usage references (e.g., value annotations like `val x : MY_SIG`)
    for &ModuleTypeConst { name, id, loc } in local_cons.module_types() {
        if let Some(module_type_sym) = module.names.get_module_type(name) {
            module
                .nodes
                .insert_meta(id, ResolvedModuleType(module_type_sym));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Module type not found in annotation")
                    .with_help("Check that the module type is defined in this module."),
            );
        }
    }
}
