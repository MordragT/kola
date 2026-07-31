use std::collections::HashMap;

use indexmap::IndexMap;
use kola_span::{Diagnostic, Issue, Loc, Report};
use kola_tree::{
    col::GetOpt,
    id::Id,
    node::{self, ValueName},
};
use log::debug;

use crate::{
    env::ModuleMap,
    phase::ResolvedValue,
    symbol::{AnySym, ModuleSym, Substitute, ValueGraph, ValueOrders, ValueSym, merge2},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueLookup {
    /// The name of the value reference.
    pub name: ValueName,
    /// The identifier of the path expression that references some other value bind.
    pub id: Id<node::QualifiedExpr>,
    /// The symbol of the value bind, this reference occured inside.
    pub source: ValueSym,
    /// The location of the value reference in the source code.
    pub loc: Loc,
    /// The module this value reference is located in
    pub module: ModuleSym,
}

impl ValueLookup {
    pub fn new(
        name: ValueName,
        id: Id<node::QualifiedExpr>,
        source: ValueSym,
        loc: Loc,
        module: ModuleSym,
    ) -> Self {
        Self {
            name,
            id,
            source,
            loc,
            module,
        }
    }
}

impl Substitute for ValueLookup {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let source_opt = self.source.try_subst(s);
        let module_opt = self.module.try_subst(s);

        merge2(source_opt, || self.source, module_opt, || self.module)
            .map(|(source, module)| Self::new(self.name, self.id, source, self.loc, module))
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        self.source.subst_mut(s);
        self.module.subst_mut(s);
    }
}

pub fn lookup_values(
    modules: &mut ModuleMap,
    queries: &[ValueLookup],
    value_graph_map: &mut IndexMap<ModuleSym, ValueGraph>,
    report: &mut Report,
) -> ValueOrders {
    // Resolve all forward term references inside local function bodies or bindings
    for &ValueLookup {
        name,
        id,
        source,
        loc,
        module: module_sym,
    } in queries
    {
        let module = &mut modules[&module_sym];

        if let Some(target) = module.names.get_value(name) {
            value_graph_map[&module_sym].add_dependency(source, target.sym);
            module.nodes.set(id, ResolvedValue::Reference(target.sym));
        } else {
            report.add_diagnostic(Diagnostic::error(loc, "Value not found").with_help(
                "Check that the value is defined in this module or has been brought into scope.",
            ));
        }
    }

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
