use std::collections::HashMap;

use indexmap::IndexMap;
use kola_span::{Diagnostic, Issue, Loc, Report};
use kola_tree::{
    col::GetOpt,
    id::Id,
    node::{self, TypeName},
};
use log::debug;

use crate::{
    env::ModuleMap,
    phase::ResolvedType,
    symbol::{AnySym, ModuleSym, Substitute, TypeGraph, TypeOrders, TypeSym, merge2},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeLookup {
    /// The name of the type reference.
    pub name: TypeName,
    /// The identifier of the type path that references some other type bind.
    pub id: Id<node::QualifiedType>,
    /// The symbol of the type bind, this reference occured inside.
    pub source: TypeSym,
    /// The location of the type reference in the source code.
    pub loc: Loc,
    /// The module this type reference is located in
    pub module: ModuleSym,
}

impl TypeLookup {
    pub fn new(
        name: TypeName,
        id: Id<node::QualifiedType>,
        source: TypeSym,
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

impl Substitute for TypeLookup {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAnnotLookup {
    /// The name of the type reference.
    pub name: TypeName,
    /// The identifier of the type path that references some other type bind.
    pub id: Id<node::QualifiedType>,
    /// The location of the type reference in the source code.
    pub loc: Loc,
    /// The module this type annotation is located in
    pub module: ModuleSym,
}

impl TypeAnnotLookup {
    pub fn new(name: TypeName, id: Id<node::QualifiedType>, loc: Loc, module: ModuleSym) -> Self {
        Self {
            name,
            id,
            loc,
            module,
        }
    }
}

impl Substitute for TypeAnnotLookup {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Some(module) = self.module.try_subst(s) {
            Some(Self::new(self.name, self.id, self.loc, module))
        } else {
            None
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        self.module.subst_mut(s);
    }
}

pub fn lookup_types(
    modules: &mut ModuleMap,
    queries: &[TypeLookup],
    annot_queries: &[TypeAnnotLookup],
    type_graph_map: &mut IndexMap<ModuleSym, TypeGraph>,
    report: &mut Report,
) -> TypeOrders {
    // 1. Resolve local type binding dependencies (e.g., cross-references in aliases or data variants)
    for &TypeLookup {
        name,
        id,
        source,
        loc,
        module: module_sym,
    } in queries
    {
        let module = &mut modules[&module_sym];
        if let Some(target) = module.names.get_type(name) {
            type_graph_map[&module_sym].add_dependency(source, target.sym);
            module.nodes.set(id, ResolvedType::Reference(target.sym));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Type not found").with_help(
                    "Check that the type is defined in this module or imported correctly.",
                ),
            );
        }
    }

    // 2. Resolve standalone type occurrences (e.g., inline annotations on parameters and fields)
    for &TypeAnnotLookup {
        name,
        id,
        loc,
        module: module_sym,
    } in annot_queries
    {
        let module = &mut modules[&module_sym];
        if let Some(binding) = module.names.get_type(name) {
            module.nodes.set(id, ResolvedType::Reference(binding.sym));
        } else {
            report.add_diagnostic(
                Diagnostic::error(loc, "Type not found in annotation")
                    .with_help("Check that the type is accessible in this module scope."),
            );
        }
    }

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
