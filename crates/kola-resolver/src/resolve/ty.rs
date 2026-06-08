use indexmap::IndexMap;
use kola_span::{Diagnostic, Issue, Report};
use kola_tree::meta::MetaMapExt;
use log::debug;
use std::collections::HashMap;

use crate::{
    constraints::{LocalConstraints, TypeBindConst, TypeConst},
    env::ModuleMap,
    phase::ResolvedType,
    resolve::{TypeGraph, TypeOrders}, // Assuming TypeGraph is defined here
    symbol::ModuleSym,
};

pub struct TypeResolution {
    pub type_orders: TypeOrders,
}

pub fn resolve_types(
    modules: &mut ModuleMap,
    mut type_graph_map: IndexMap<ModuleSym, TypeGraph>,
    local_cons_map: &HashMap<ModuleSym, LocalConstraints>,
    module_order: &[ModuleSym],
    report: &mut Report,
) -> TypeResolution {
    let mut type_orders = TypeOrders::new();

    // Process each module safely along the verified global topological order
    for &sym in module_order {
        // Localized dependency graph to determine declaration/mutual-recursion order within this module
        let mut type_graph = type_graph_map.swap_remove(&sym).unwrap();

        if let Some(local_cons) = local_cons_map.get(&sym) {
            resolve_types_in_module(sym, modules, local_cons, &mut type_graph, report);
        }

        // Sort internal type definitions to validate structural ordering and find cycles
        match type_graph.topological_sort() {
            Ok(order) => {
                type_orders.insert(sym, order);
            }
            Err(cycle) => {
                report.add_issue(
                    Issue::error(cycle.to_string(), 0)
                        .with_help("Check for circular dependencies in type definitions."),
                );

                debug!(
                    "Internal type dependency cycle detected inside module symbol {:?}:\n{}",
                    sym,
                    type_graph.to_dot()
                );
            }
        }
    }

    TypeResolution { type_orders }
}

fn resolve_types_in_module(
    module_sym: ModuleSym,
    modules: &mut ModuleMap,
    local_cons: &LocalConstraints,
    type_graph: &mut TypeGraph,
    report: &mut Report,
) {
    let Some(module) = modules.get_mut(&module_sym) else {
        return;
    };

    // 1. Resolve local type binding dependencies (e.g., cross-references in aliases or data variants)
    for &TypeBindConst {
        name,
        id,
        source,
        loc,
    } in local_cons.type_binds()
    {
        if let Some(target) = module.names.get_type(name) {
            type_graph.add_dependency(source, target);
            module
                .nodes
                .insert_meta(id, ResolvedType::Reference(target));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Type not found").with_help(
                    "Check that the type is defined in this module or imported correctly.",
                ),
            );
        }
    }

    // 2. Resolve standalone type occurrences (e.g., inline annotations on parameters and fields)
    for type_const in local_cons.types() {
        match type_const {
            &TypeConst::Qualified { name, id, loc } => {
                if let Some(type_sym) = module.names.get_type(name) {
                    module
                        .nodes
                        .insert_meta(id, ResolvedType::Reference(type_sym));
                } else {
                    report.add_diagnostic(
                        Diagnostic::error(loc, "Type not found in annotation")
                            .with_help("Check that the type is accessible in this module scope."),
                    );
                }
            } // Ready for clean extension if you re-introduce structural TypeRep shapes later
        }
    }
}
