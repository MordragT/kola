use indexmap::IndexMap;
use kola_span::{Issue, Report};
use log::debug;

use crate::symbol::{ModuleSym, ValueGraph, ValueOrders};

pub fn order_values(
    value_graph_map: &mut IndexMap<ModuleSym, ValueGraph>,
    report: &mut Report,
) -> ValueOrders {
    value_graph_map
        .iter()
        .map(|(module_sym, value_graph)| {
            // Sort internal terms to lock down exact compilation or evaluation offsets
            let order = match value_graph.topological_sort() {
                Ok(order) => order,
                Err(cycle) => {
                    report.add_issue(
                        Issue::error(cycle.to_string(), 0)
                            .with_help("Check for circular dependencies in value definitions."),
                    );

                    debug!(
                        "Internal value dependency cycle detected inside module symbol {:?}:\n{}",
                        module_sym,
                        value_graph.to_dot()
                    );

                    Vec::new()
                }
            };

            (*module_sym, order)
        })
        .collect()
}
