use std::collections::HashMap;

use indexmap::IndexMap;
use kola_span::{Diagnostic, Issue, Loc, Report};
use kola_subst::{Substitutable, merge};
use kola_tree::{
    col::GetOpt,
    id::Id,
    node::{self, ModuleTypeName},
};
use log::debug;

use crate::{
    env::ModuleMap,
    phase::ResolvedModuleType,
    symbol::{AnySym, ModuleSym, ModuleTypeGraph, ModuleTypeOrders, ModuleTypeSym, Substitution},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleTypeLookup {
    /// The name of the module type reference.
    pub name: ModuleTypeName,
    /// The identifier of the qualified module type that references some other module type bind.
    pub id: Id<node::QualifiedModuleType>,
    /// The symbol of the module type bind, this reference occured inside.
    pub source: ModuleTypeSym,
    /// The location of the module type reference in the source code.
    pub loc: Loc,
    /// The module this module type reference is located in
    pub module: ModuleSym,
}

impl ModuleTypeLookup {
    pub fn new(
        name: ModuleTypeName,
        id: Id<node::QualifiedModuleType>,
        source: ModuleTypeSym,
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

impl Substitutable<Substitution> for ModuleTypeLookup {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        let source_opt = self.source.try_apply(s);
        let module_opt = self.module.try_apply(s);

        merge(source_opt, || self.source, module_opt, || self.module)
            .map(|(source, module)| Self::new(self.name, self.id, source, self.loc, module))
    }

    fn apply_mut(&mut self, s: &mut HashMap<AnySym, AnySym>) {
        self.source.apply_mut(s);
        self.module.apply_mut(s);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleTypeAnnotLookup {
    /// The name of the module type reference.
    pub name: ModuleTypeName,
    /// The identifier of the qualified module type that references some other module type bind.
    pub id: Id<node::QualifiedModuleType>,
    /// The location of the type reference in the source code.
    pub loc: Loc,
    /// The module this module type reference is located in
    pub module: ModuleSym,
}

impl ModuleTypeAnnotLookup {
    pub fn new(
        name: ModuleTypeName,
        id: Id<node::QualifiedModuleType>,
        loc: Loc,
        module: ModuleSym,
    ) -> Self {
        Self {
            name,
            id,
            loc,
            module,
        }
    }
}

impl Substitutable<Substitution> for ModuleTypeAnnotLookup {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Some(module) = self.module.try_apply(s) {
            Some(Self::new(self.name, self.id, self.loc, module))
        } else {
            None
        }
    }

    fn apply_mut(&mut self, s: &mut HashMap<AnySym, AnySym>) {
        self.module.apply_mut(s);
    }
}

pub fn lookup_module_types(
    modules: &mut ModuleMap,
    queries: &[ModuleTypeLookup],
    annot_queries: &[ModuleTypeAnnotLookup],
    module_type_graph_map: &mut IndexMap<ModuleSym, ModuleTypeGraph>,
    report: &mut Report,
) -> ModuleTypeOrders {
    // 1. Resolve local module type binding references (e.g., dependencies inside signature declarations)
    for &ModuleTypeLookup {
        name,
        id,
        source,
        loc,
        module: module_sym,
    } in queries
    {
        let module = &mut modules[&module_sym];
        if let Some(target) = module.names.get_module_type(name) {
            module_type_graph_map[&module_sym].add_dependency(source, target.sym);
            module.nodes.set(id, ResolvedModuleType(target.sym));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Module type not found")
                    .with_help("Check that the module type is defined in this module."),
            );
        }
    }

    // 2. Resolve flat module type usage references (e.g., annotations like `module x : MY_SIG`)
    for &ModuleTypeAnnotLookup {
        name,
        id,
        loc,
        module: module_sym,
    } in annot_queries
    {
        let module = &mut modules[&module_sym];
        if let Some(binding) = module.names.get_module_type(name) {
            module.nodes.set(id, ResolvedModuleType(binding.sym));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Module type not found in annotation")
                    .with_help("Check that the module type is defined in this module."),
            );
        }
    }

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
