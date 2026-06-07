use std::{collections::HashMap, io};

use indexmap::IndexMap;

use kola_print::prelude::*;
use kola_span::{Issue, Report};
use kola_syntax::loc::LocMap;
use kola_tree::{
    print::{Decorators, TreePrinter},
    tree::TreeMap,
};
use kola_utils::{dependency::DependencyGraph, interner::StrInterner};
use log::{debug, trace};

use crate::{
    constraints::GlobalConstraints,
    def::DefMap,
    env::{FunctorMap, ModuleMap},
    print::ResolutionDecorator,
    symbol::{ModuleSym, ModuleTypeSym, TypeSym, ValueSym},
};

mod discover;
mod module;
mod module_ty;
mod structure;
mod ty;
mod value;

pub use discover::{DiscoverOutput, discover};
pub use module::resolve_modules;
pub use module_ty::{ModuleTypeResolution, resolve_module_types};
pub use structure::{StructureResolution, resolve_structure};
pub use ty::{TypeResolution, resolve_types};
pub use value::{ValueResolution, resolve_values};

pub type ModuleTypeGraph = DependencyGraph<ModuleTypeSym>;
pub type ModuleGraph = DependencyGraph<ModuleSym>;
pub type TypeGraph = DependencyGraph<TypeSym>;
pub type ValueGraph = DependencyGraph<ValueSym>;

pub type ModuleTypeOrders = IndexMap<ModuleSym, Vec<ModuleTypeSym>>;
pub type TypeOrders = IndexMap<ModuleSym, Vec<TypeSym>>;
pub type ValueOrders = IndexMap<ModuleSym, Vec<ValueSym>>;

#[derive(Debug, Clone, Default)]
pub struct ResolveOutput {
    pub modules: ModuleMap,
    pub defs: DefMap,
    pub module_graph: ModuleGraph,
    pub entry_points: Vec<ValueSym>,
    pub module_order: Vec<ModuleSym>,
    pub module_type_orders: ModuleTypeOrders,
    pub type_orders: TypeOrders,
    pub value_orders: ValueOrders,
}

pub fn resolve(
    tree_map: &TreeMap,
    loc_map: &LocMap,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    print_options: PrintOptions,
) -> io::Result<ResolveOutput> {
    let mut modules = ModuleMap::new();
    let mut functors = FunctorMap::new();
    let mut entry_points = Vec::new();
    let mut defs = DefMap::new();
    let mut module_graph = ModuleGraph::new();
    let mut global_cons = GlobalConstraints::new();
    let mut local_cons_map = HashMap::new();
    let mut files = HashMap::new();
    let mut value_graph_map = HashMap::new();
    let mut type_graph_map = HashMap::new();

    for (source_id, tree) in tree_map {
        let DiscoverOutput {
            root,
            cons,
            report: module_report,
            value_graph,
            type_graph,
        } = discover::discover(
            tree,
            &loc_map[source_id],
            interner,
            &mut modules,
            &mut functors,
            &mut entry_points,
            &mut defs,
            &mut module_graph,
            &mut global_cons,
        );

        if !module_report.is_empty() {
            report.append(module_report);

            return Ok(ResolveOutput {
                modules,
                defs,
                module_graph,
                entry_points,
                ..Default::default()
            });
        }

        files.insert(*source_id, root);
        local_cons_map.insert(root, cons);
        value_graph_map.insert(root, value_graph);
        type_graph_map.insert(root, type_graph);
    }

    let StructureResolution {
        mut modules,
        local_cons_map,
    } = resolve_structure(
        modules,
        local_cons_map,
        global_cons,
        &files,
        &functors,
        &defs,
        &mut module_graph,
        interner,
        report,
    );

    if !report.is_empty() {
        return Ok(ResolveOutput {
            modules,
            defs,
            module_graph,
            entry_points,
            ..Default::default()
        });
    }

    resolve_modules(
        &mut modules,
        &local_cons_map,
        &defs,
        &mut module_graph,
        report,
    );

    if !report.is_empty() {
        return Ok(ResolveOutput {
            modules,
            defs,
            module_graph,
            entry_points,
            ..Default::default()
        });
    }

    let module_order = match module_graph.topological_sort() {
        Ok(order) => order,
        Err(cycle) => {
            report.add_issue(
                Issue::error(cycle.to_string(), 0)
                    .with_help("Check for circular dependencies in module definitions."),
            );
            return Ok(ResolveOutput {
                modules,
                defs,
                module_graph,
                entry_points,
                ..Default::default()
            });
        }
    };

    let ModuleTypeResolution { module_type_orders } =
        resolve_module_types(&mut modules, &local_cons_map, &module_order, report);

    if !report.is_empty() {
        return Ok(ResolveOutput {
            modules,
            defs,
            module_graph,
            entry_points,
            module_order,
            ..Default::default()
        });
    }

    let TypeResolution { type_orders } = resolve_types(
        &mut modules,
        type_graph_map,
        &local_cons_map,
        &module_order,
        report,
    );

    if !report.is_empty() {
        return Ok(ResolveOutput {
            modules,
            defs,
            module_graph,
            entry_points,
            module_order,
            module_type_orders,
            ..Default::default()
        });
    }

    let ValueResolution { value_orders } = resolve_values(
        &mut modules,
        value_graph_map,
        &local_cons_map,
        &module_order,
        report,
    );

    if !report.is_empty() {
        return Ok(ResolveOutput {
            modules,
            defs,
            module_graph,
            entry_points,
            module_order,
            module_type_orders,
            type_orders,
            ..Default::default()
        });
    }

    for (source_id, sym) in files {
        let tree = &tree_map[&source_id];

        let resolution_decorator = ResolutionDecorator(&modules[&sym].nodes);
        let decorators = Decorators::new().with(&resolution_decorator);

        let tree_printer = TreePrinter::new(tree, interner, decorators, tree.root_id());

        trace!(
            "{} SourceId {}, ModuleSym {}\n{}",
            "Resolved Abstract Syntax Tree".bold().bright_white(),
            source_id,
            sym,
            tree_printer.render(print_options, arena)
        );
    }

    debug!(
        "{} Module Graph:\n{}",
        "Module Graph".bold().bright_white(),
        module_graph.to_dot()
    );

    let module_order = match module_graph.topological_sort() {
        Ok(order) => order,
        Err(cycle) => {
            report.add_issue(
                Issue::error(cycle.to_string(), 0)
                    .with_help("Check for circular dependencies in module definitions."),
            );
            return Ok(ResolveOutput {
                modules,
                defs,
                module_graph,
                entry_points,
                module_type_orders,
                type_orders,
                value_orders,
                ..Default::default()
            });
        }
    };

    Ok(ResolveOutput {
        modules,
        defs,
        module_graph,
        entry_points,
        module_order,
        module_type_orders,
        type_orders,
        value_orders,
    })
}
