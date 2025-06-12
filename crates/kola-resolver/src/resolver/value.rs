use kola_span::{Diagnostic, Report};

use super::ValueOrders;
use crate::{
    bind::Bindings,
    refs::ValueRef,
    scope::{ModuleCell, ModuleCells},
    symbol::ModuleSym,
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

pub fn resolve_values(
    module_symbols: &[ModuleSym],
    module_scopes: &ModuleCells,
    report: &mut Report,
    bindings: &mut Bindings,
) -> ValueResolution {
    let mut value_orders = ValueOrders::new();

    for module_sym in module_symbols {
        let scope = module_scopes[module_sym].clone();

        resolve_values_in_module(scope.clone(), report, bindings);

        match scope.borrow().value_graph.topological_sort() {
            Ok(order) => {
                value_orders.insert(*module_sym, order);
            }
            Err(_) => {
                report.add_diagnostic(
                    Diagnostic::error(scope.borrow().loc, "Cycle detected in value graph")
                        .with_help("Check for circular dependencies in value definitions."),
                );
            }
        }
    }

    ValueResolution { value_orders }
}

fn resolve_values_in_module(scope: ModuleCell, report: &mut Report, bindings: &mut Bindings) {
    let value_refs: Vec<ValueRef> = scope.borrow().refs.values().to_vec();

    // First pass: resolve all forward value references
    for ValueRef {
        name,
        global_id,
        source,
    } in value_refs
    {
        if let Some(target) = scope.borrow().shape.lookup_value(name) {
            scope
                .borrow_mut()
                .value_graph
                .add_dependency(source, target);
            bindings.insert_path_expr(global_id, target);
        } else {
            // Value not found in current module
            report.add_diagnostic(
                Diagnostic::error(scope.borrow().loc, "Value not found")
                    .with_help("Check that the value is defined in this module."),
            );
        }
    }
}
