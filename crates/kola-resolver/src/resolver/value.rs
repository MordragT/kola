use kola_span::{Diagnostic, Report};
use kola_tree::meta::MetaMapExt;
use log::debug;

use super::ValueOrders;
use crate::{
    constraints::ValueRef,
    phase::ResolvedValue,
    scope::{ModuleScope, ModuleScopes},
};

pub struct ValueResolution {
    pub value_orders: ValueOrders,
}

/*
 How this works:

1. During discovery, forward value references are collected as `ValueRef` structs
   containing the reference name, global_id, and source binding symbol.
2. No placeholder symbols or bindings are created during discovery.
3. Resolution resolves each collected `ValueRef` to concrete value definitions.

The process:
1. For each module scope, iterate through the collected `Vec<ValueRef>`.
2. For each `ValueRef`:
   - Look up the reference name in the module's shape to find the target symbol
   - If found, create the final binding entry (global_id → target_symbol)
   - Add the dependency edge (source → target) to the value graph
   - If not found, report an error
3. Perform topological sort on the resolved dependency graph for:
   - Cycle detection (if sort fails)
   - Evaluation order (if sort succeeds)
4. All bindings are created in a single pass during resolution.

This approach eliminates placeholder symbols and binding mutations, making each
phase have a single clear responsibility: discovery collects information,
resolution creates relationships.
*/

pub fn resolve_values(scopes: &mut ModuleScopes, report: &mut Report) -> ValueResolution {
    let mut value_orders = ValueOrders::new();

    for (sym, scope) in scopes {
        resolve_values_in_module(scope, report);

        match scope.value_graph.topological_sort() {
            Ok(order) => {
                value_orders.insert(*sym, order);
            }
            Err(cycle) => {
                report.add_diagnostic(
                    Diagnostic::error(scope.info.loc, cycle.to_string())
                        .with_help("Check for circular dependencies in value definitions."),
                );

                debug!("Cycle detected inside:\n{}", scope.value_graph.to_dot());
            }
        }
    }

    ValueResolution { value_orders }
}

fn resolve_values_in_module(scope: &mut ModuleScope, report: &mut Report) {
    // First pass: resolve all forward value references
    for &ValueRef {
        name,
        id,
        source,
        loc,
    } in scope.cons.values()
    {
        if let Some(target) = scope.shape.get_value(name) {
            scope.value_graph.add_dependency(source, target);
            scope
                .resolved
                .insert_meta(id, ResolvedValue::Reference(target));
        } else {
            // Value not found in current module
            report.add_diagnostic(
                Diagnostic::error(loc, "Value not found")
                    .with_help("Check that the value is defined in this module."),
            );
        }
    }
}
