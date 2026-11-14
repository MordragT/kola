use indexmap::IndexMap;
use kola_span::{Issue, Report};
use log::debug;

use crate::symbol::{ModuleSym, ModuleTypeGraph, ModuleTypeOrders};

pub fn order_module_types(
    module_type_graph_map: &mut IndexMap<ModuleSym, ModuleTypeGraph>,
    report: &mut Report,
) -> ModuleTypeOrders {
    module_type_graph_map
        .iter()
        .map(|(module_sym, module_type_graph)| {
            // Sort internal module types to lock down exact compilation or evaluation offsets
            let order = match module_type_graph.topological_sort() {
                Ok(order) => order,
                Err(cycle) => {
                    report.add_issue(
                        Issue::error(cycle.to_string(), 0)
                            .with_help("Check for circular dependencies in module type definitions."),
                    );

                    debug!(
                        "Internal module type dependency cycle detected inside module symbol {:?}:\n{}",
                        module_sym,
                        module_type_graph.to_dot()
                    );

                    Vec::new()
                }
            };

            (*module_sym, order)
        })
        .collect()
}
