use std::collections::HashMap;

use kola_span::{Diagnostic, Loc, Report};
use kola_subst::Substitutable;
use kola_tree::{
    id::Id,
    node::{self, ModuleName, Vis},
    query::GetOpt,
};

use crate::{
    def::DefMap,
    env::ModuleMap,
    name::Binding,
    phase::ResolvedModule,
    symbol::{AnySym, ModuleGraph, ModuleSym, Substitution},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleLookup {
    /// The path that references some other module bind.
    pub path: Vec<ModuleName>,
    /// The global identifier of the module path that references some other module bind.
    pub id: Id<node::ModulePath>,
    /// The symbol of the module bind, this reference occured inside.
    pub source: ModuleSym,
    /// The location of the module reference in the source code.
    pub loc: Loc,
}

impl ModuleLookup {
    pub fn new(
        path: Vec<ModuleName>,
        id: Id<node::ModulePath>,
        source: ModuleSym,
        loc: Loc,
    ) -> Self {
        Self {
            path,
            id,
            source,
            loc,
        }
    }
}

impl Substitutable<Substitution> for ModuleLookup {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Some(source) = self.source.try_apply(s) {
            Some(Self::new(self.path.clone(), self.id, source, self.loc))
        } else {
            None
        }
    }

    fn apply_mut(&mut self, s: &mut HashMap<AnySym, AnySym>) {
        self.source.apply_mut(s);
    }
}

pub fn lookup_modules(
    modules: &mut ModuleMap,
    queries: &[ModuleLookup],
    defs: &DefMap,
    module_graph: &mut ModuleGraph,
    report: &mut Report,
) {
    'outer: for ModuleLookup {
        path,
        id,
        source,
        loc,
    } in queries
    {
        let mut current_sym = *source;

        // 2. Walk down the path segments linearly
        for name in path {
            // Safe to direct index because the macro-structure is fully resolved!
            let current_module = &modules[&current_sym];

            if let Some(Binding { sym: next_sym, vis }) = current_module.names.get_module(*name) {
                // 3. Enforce export visibility rules
                if vis != Vis::Export && current_sym != *source {
                    let (_, def_loc) = defs[next_sym];

                    report.add_diagnostic(
                        Diagnostic::error(def_loc, "Module not exported")
                            .with_help("Only exported modules can be used in paths.")
                            .with_trace([("Within this module path".into(), *loc)]),
                    );
                    continue 'outer;
                }
                current_sym = next_sym;
            } else {
                report.add_diagnostic(
                    Diagnostic::error(*loc, "Module not found")
                        .with_trace([("Within this module path".into(), *loc)]),
                );
                continue 'outer;
            }
        }

        // 4. If the path successfully resolved, record the target metadata
        module_graph.add_dependency(*source, current_sym);

        // Mutate the source module to cache the resolution metadata for downstream passes
        if let Some(source_module) = modules.get_mut(source) {
            source_module.nodes.set(*id, ResolvedModule(current_sym));
        }
    }
}
