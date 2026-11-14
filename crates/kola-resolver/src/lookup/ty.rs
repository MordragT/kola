use indexmap::IndexMap;
use kola_span::{Issue, Report};
use log::debug;

use crate::symbol::{ModuleSym, TypeGraph, TypeOrders};

pub fn order_types(
    type_graph_map: &mut IndexMap<ModuleSym, TypeGraph>,
    report: &mut Report,
) -> TypeOrders {
    type_graph_map
        .iter()
        .map(|(module_sym, type_graph)| {
            // Sort internal types to lock down exact compilation or evaluation offsets
            let order = match type_graph.topological_sort() {
                Ok(order) => order,
                Err(cycle) => {
                    report.add_issue(
                        Issue::error(cycle.to_string(), 0)
                            .with_help("Check for circular dependencies in type definitions."),
                    );

                    debug!(
                        "Internal type dependency cycle detected inside module symbol {:?}:\n{}",
                        module_sym,
                        type_graph.to_dot()
                    );

                    Vec::new()
                }
            };

            (*module_sym, order)
        })
        .collect()
}
