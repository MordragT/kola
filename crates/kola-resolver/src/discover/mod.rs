use std::collections::HashMap;

use indexmap::IndexMap;
use kola_interner::StrInterner;
use kola_span::{Loc, Report, SourceId};
use kola_tree::prelude::*;

mod bodies;
mod collect;

use crate::{
    def::DefMap,
    elaborate::ElabJobs,
    env::{ModuleBuilder, ModuleMap},
    lookup::Lookups,
    symbol::{
        FileMap, ModuleGraph, ModuleSym, ModuleTypeGraph, TypeGraph, ValueGraph, ValueSym,
    },
};

use bodies::BodyVisitor;
use collect::Collector;

#[derive(Debug, Clone)]
pub struct DiscoverOutput {
    pub report: Report,
}

/// Immutable input shared by both discovery stages.
pub(super) struct Inputs<'a> {
    pub sym: ModuleSym,
    pub source: SourceId,
    pub key: ModuleKey,
    pub loc: Loc,
    pub tree: &'a Tree,
    pub files: &'a FileMap,
    pub module_of: &'a HashMap<(SourceId, ModuleKey), ModuleSym>,
    pub interner: &'a StrInterner,
}

impl Inputs<'_> {
    #[inline]
    pub fn span<T: 'static>(&self, id: Id<T>) -> Loc
    where
        Tree: TotalSpan<T, Loc>,
    {
        *self.tree.get_span(id)
    }
}

/// Per-module state that is built by collection and consumed by body traversal.
pub(super) struct ModuleState {
    pub module: ModuleBuilder,
    pub value_graph: ValueGraph,
    pub type_graph: TypeGraph,
    pub module_type_graph: ModuleTypeGraph,
    pub report: Report,
}

impl ModuleState {
    pub fn new(loc: Loc, key: ModuleKey) -> Self {
        Self {
            module: ModuleBuilder::new(loc, key),
            value_graph: ValueGraph::new(),
            type_graph: TypeGraph::new(),
            module_type_graph: ModuleTypeGraph::new(),
            report: Report::new(),
        }
    }
}

pub fn discover(
    sym: ModuleSym,
    source: SourceId,
    key: ModuleKey,
    tree: &Tree,
    files: &FileMap,
    module_of: &HashMap<(SourceId, ModuleKey), ModuleSym>,
    interner: &StrInterner,
    modules: &mut ModuleMap,
    entry_points: &mut Vec<ValueSym>,
    defs: &mut DefMap,
    value_graph_map: &mut IndexMap<ModuleSym, ValueGraph>,
    type_graph_map: &mut IndexMap<ModuleSym, TypeGraph>,
    module_type_graph_map: &mut IndexMap<ModuleSym, ModuleTypeGraph>,
    module_graph: &mut ModuleGraph,
    lookups: &mut Lookups,
    elab_jobs: &mut ElabJobs,
) -> DiscoverOutput {
    let loc = *tree.get_span(tree.root_id());
    module_graph.add_node(sym);

    let inputs = Inputs {
        sym,
        source,
        key,
        loc,
        tree,
        files,
        module_of,
        interner,
    };

    let collector = Collector::new(&inputs, defs, entry_points, module_graph, elab_jobs);
    let state = collector.collect();

    let mut bodies = BodyVisitor::new(&inputs, state, lookups);
    bodies.visit_bodies();

    let mut state = bodies.into_state();
    state.module.nodes.set(tree.root_id(), sym);

    value_graph_map.insert(sym, state.value_graph);
    type_graph_map.insert(sym, state.type_graph);
    module_type_graph_map.insert(sym, state.module_type_graph);
    modules.insert(sym, state.module);

    DiscoverOutput {
        report: state.report,
    }
}
