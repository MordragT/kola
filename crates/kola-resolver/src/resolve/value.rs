use indexmap::IndexMap;
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

pub fn resolve_values(
    modules: &mut ModuleMap,
    mut value_graph_map: IndexMap<ModuleSym, ValueGraph>,
    local_cons_map: &HashMap<ModuleSym, LocalConstraints>,
    module_order: &[ModuleSym],
    report: &mut Report,
) -> ValueResolution {
    let mut value_orders = ValueOrders::new();

    // Process each module following the verified global topological order
    for &sym in module_order {
        // Localized term graph to sort value definitions and uncover illegal circular expressions
        let mut value_graph = value_graph_map.swap_remove(&sym).unwrap();

        if let Some(local_cons) = local_cons_map.get(&sym) {
            resolve_values_in_module(sym, modules, local_cons, &mut value_graph, report);
        }

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
