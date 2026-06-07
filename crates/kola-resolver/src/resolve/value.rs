use kola_span::{Diagnostic, Issue, Report};
use kola_tree::meta::MetaMapExt;
use log::debug;
use std::collections::HashMap;

use crate::{
    constraints::{LocalConstraints, ValueConst},
    env::ModuleMap,
    phase::ResolvedValue,
    resolve::{ValueGraph, ValueOrders}, // Assuming ValueGraph lives here
    symbol::ModuleSym,
};

pub struct ValueResolution {
    pub value_orders: ValueOrders,
}

/*
 How this works:

1. During discovery, forward value references are collected as `ValueConst` structs
   containing the reference name, node ID, and source binding symbol.
2. No placeholder symbols or bindings are created during discovery.
3. Resolution resolves each collected `ValueConst` to concrete value definitions.

The process:
1. For each module symbol processed in global topological order, iterate through
   its collected local value references.
2. For each `ValueConst`:
   - Look up the reference name in the module's stable environment (`names`) to find the target symbol.
   - If found, cache the final mapping inside the module's metadata table (node ID → target symbol).
   - Add the internal dependency edge (source → target) to the local `ValueGraph`.
   - If not found, report an error.
3. Perform a topological sort on the localized dependency graph for:
   - Cycle detection (if sort fails, identifying invalid mutual recursion or self-references).
   - Evaluation/Compilation order (if sort succeeds).
4. All bindings are fully tracked in a single pass during resolution.

This approach eliminates placeholder symbols and binding mutations, making each
phase have a single clear responsibility: discovery collects information,
resolution creates relationships.
*/

pub fn resolve_values(
    modules: &mut ModuleMap,
    mut value_graph_map: HashMap<ModuleSym, ValueGraph>,
    local_cons_map: &HashMap<ModuleSym, LocalConstraints>,
    module_order: &[ModuleSym],
    report: &mut Report,
) -> ValueResolution {
    let mut value_orders = ValueOrders::new();

    // Process each module following the verified global topological order
    for &sym in module_order {
        if let Some(local_cons) = local_cons_map.get(&sym) {
            // Localized term graph to sort value definitions and uncover illegal circular expressions
            let mut value_graph = value_graph_map.remove(&sym).unwrap();

            resolve_values_in_module(sym, modules, local_cons, &mut value_graph, report);

            // Sort internal terms to lock down exact compilation or evaluation offsets
            match value_graph.topological_sort() {
                Ok(order) => {
                    value_orders.insert(sym, order);
                }
                Err(cycle) => {
                    report.add_issue(
                        Issue::error(cycle.to_string(), 0)
                            .with_help("Check for circular dependencies in value definitions."),
                    );

                    debug!(
                        "Internal term/value dependency cycle detected inside module symbol {:?}:\n{}",
                        sym,
                        value_graph.to_dot()
                    );
                }
            }
        }
    }

    ValueResolution { value_orders }
}

fn resolve_values_in_module(
    module_sym: ModuleSym,
    modules: &mut ModuleMap,
    local_cons: &LocalConstraints,
    value_graph: &mut ValueGraph,
    report: &mut Report,
) {
    let Some(module) = modules.get_mut(&module_sym) else {
        return;
    };

    // Resolve all forward term references inside local function bodies or bindings
    for &ValueConst {
        name,
        id,
        source,
        loc,
    } in local_cons.values()
    {
        if let Some(target) = module.names.get_value(name) {
            value_graph.add_dependency(source, target);
            module
                .nodes
                .insert_meta(id, ResolvedValue::Reference(target));
        } else {
            report.add_diagnostic(Diagnostic::error(loc, "Value not found").with_help(
                "Check that the value is defined in this module or has been brought into scope.",
            ));
        }
    }
}
