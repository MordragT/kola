use std::{collections::HashMap, io};

use indexmap::IndexMap;

use kola_interner::StrInterner;
use kola_print::prelude::*;
use kola_span::{Issue, Report, SourceId, SourceManager};
use kola_tree::{
    print::{Decorators, TreePrinter},
    source::{ModuleKey, TreeMap},
};
use log::{debug, trace};

use crate::{
    db::Db,
    def::DefMap,
    discover::{DiscoverOutput, discover},
    elaborate::{ElabJobs, elaborate_modules},
    env::ModuleMap,
    lookup::{Lookups, lookup_module_types, lookup_modules, lookup_types, lookup_values},
    print::ResolutionDecorator,
    symbol::{FileMap, ModuleGraph, ModuleSym},
};

pub fn resolve(
    source_manager: SourceManager,
    tree_map: TreeMap,
    arena: &Bump,
    interner: &mut StrInterner,
    report: &mut Report,
    print_options: PrintOptions,
) -> io::Result<Db> {
    let mut modules = ModuleMap::new();
    let mut entry_points = Vec::new();
    let mut defs = DefMap::new();

    let mut elab_jobs = ElabJobs::new();
    let mut lookups = Lookups::new();

    let mut value_graph_map = IndexMap::new();
    let mut type_graph_map = IndexMap::new();
    let mut module_type_graph = IndexMap::new();
    let mut module_graph = ModuleGraph::new();

    // One `ModuleSym` per module tree. `files` still maps a source to its root
    // module, which is what `import` resolves to.
    let mut module_of: HashMap<(SourceId, ModuleKey), ModuleSym> = HashMap::new();
    for (source_id, source) in &tree_map {
        for key in source.trees.keys() {
            module_of.insert((*source_id, *key), ModuleSym::new());
        }
    }

    let files = tree_map
        .iter()
        .map(|(id, source)| (*id, module_of[&(*id, source.root)]))
        .collect::<FileMap>();

    let root = *files.first().unwrap().1;

    for (source_id, source) in &tree_map {
        let mut entries: Vec<_> = source.iter().collect();
        // Discover the root module first; children follow in key order.
        entries.sort_by_key(|(key, _)| *key != source.root);

        for (key, tree) in entries {
            let sym = module_of[&(*source_id, key)];

            let DiscoverOutput {
                report: module_report,
            } = discover(
                sym,
                *source_id,
                key,
                tree,
                &files,
                &module_of,
                interner,
                &mut modules,
                &mut entry_points,
                &mut defs,
                &mut value_graph_map,
                &mut type_graph_map,
                &mut module_type_graph,
                &mut module_graph,
                &mut lookups,
                &mut elab_jobs,
            );

            if !module_report.is_empty() {
                report.append(module_report);

                return Ok(Db {
                    root,
                    source_manager,
                    tree_map,
                    files,
                    modules,
                    defs,
                    module_graph,
                    entry_points,
                    ..Default::default()
                });
            }
        }
    }

    debug!(
        "{} Module Graph:\n{}",
        "Module Graph".bold().bright_white(),
        module_graph.to_dot()
    );

    elaborate_modules(
        elab_jobs,
        &mut lookups,
        &mut modules,
        &mut defs,
        &mut module_graph,
        interner,
        report,
    );

    if !report.is_empty() {
        return Ok(Db {
            root,
            source_manager,
            tree_map,
            files,
            modules,
            defs,
            module_graph,
            entry_points,
            ..Default::default()
        });
    }

    let Lookups {
        module_types: mut module_type_lookups,
        module_type_annots: mut module_type_annot_lookups,
        modules: module_lookups,
        types: mut type_lookups,
        type_annots: mut type_annot_lookups,
        values: mut value_lookups,
    } = lookups;

    lookup_modules(
        &mut modules,
        &module_lookups,
        &defs,
        &mut module_graph,
        report,
    );

    if !report.is_empty() {
        return Ok(Db {
            root,
            source_manager,
            tree_map,
            files,
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
            return Ok(Db {
                root,
                source_manager,
                tree_map,
                files,
                modules,
                defs,
                module_graph,
                entry_points,
                ..Default::default()
            });
        }
    };

    // Precompute the module ranks to efficiently order lookups
    // by their containing module's position in the module graph.
    let max_sym = module_order
        .iter()
        .map(|sym| sym.as_usize())
        .max()
        .unwrap_or_default();
    let mut rank = vec![max_sym; max_sym + 1];
    for (i, sym) in module_order.iter().enumerate() {
        rank[sym.as_usize()] = i;
    }

    module_type_lookups.sort_unstable_by_key(|lookup| rank[lookup.module.as_usize()]);
    module_type_annot_lookups.sort_unstable_by_key(|lookup| rank[lookup.module.as_usize()]);

    let module_type_orders = lookup_module_types(
        &mut modules,
        &module_type_lookups,
        &module_type_annot_lookups,
        &mut IndexMap::new(), // TODO: this should be part of discovery probably
        report,
    );

    if !report.is_empty() {
        return Ok(Db {
            root,
            source_manager,
            tree_map,
            files,
            modules,
            defs,
            module_graph,
            entry_points,
            module_order,
            ..Default::default()
        });
    }

    type_lookups.sort_unstable_by_key(|lookup| rank[lookup.module.as_usize()]);
    type_annot_lookups.sort_unstable_by_key(|lookup| rank[lookup.module.as_usize()]);

    let type_orders = lookup_types(
        &mut modules,
        &type_lookups,
        &type_annot_lookups,
        &mut type_graph_map,
        report,
    );

    if !report.is_empty() {
        return Ok(Db {
            root,
            source_manager,
            tree_map,
            files,
            modules,
            defs,
            module_graph,
            entry_points,
            module_order,
            module_type_orders,
            ..Default::default()
        });
    }

    value_lookups.sort_unstable_by_key(|lookup| rank[lookup.module.as_usize()]);

    let value_orders = lookup_values(&mut modules, &value_lookups, &mut value_graph_map, report);

    if !report.is_empty() {
        return Ok(Db {
            root,
            source_manager,
            tree_map,
            files,
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

    for (source_id, sym) in &files {
        let trees = &tree_map[source_id];
        let tree = trees.root_tree();

        let resolution_decorator = ResolutionDecorator(&modules[sym].nodes);
        let decorators = Decorators::new().with(&resolution_decorator);

        let tree_printer = TreePrinter::new(tree, interner, decorators, false);

        trace!(
            "{} SourceId {}, ModuleSym {}\n{}",
            "Resolved Abstract Syntax Tree".bold().bright_white(),
            source_id,
            sym,
            tree_printer.render(&tree.root_id(), print_options, arena)
        );
    }

    debug!(
        "{} Module Graph:\n{}",
        "Module Graph".bold().bright_white(),
        module_graph.to_dot()
    );

    Ok(Db {
        root,
        source_manager,
        tree_map,
        files,
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
