use std::ops::Index;

use kola_span::{SourceId, SourceManager};
use kola_syntax::loc::{LocMap, LocVec};
use kola_tree::{
    id::{Col, Id},
    node,
    tree::{Tree, TreeMap},
};

use crate::{
    def::{AnyDef, DefMap, ModuleDef},
    env::{Functor, FunctorMap, Module, ModuleMap},
    name::Binding,
    phase::NodeMap,
    symbol::{
        AnySym, FileMap, FunctorSym, ModuleGraph, ModuleSym, ModuleTypeOrders, ModuleTypeSym,
        TypeOrders, TypeSym, ValueOrders, ValueSym,
    },
};

/// The resolved program database.
///
/// Single output of the resolver.
#[derive(Default)]
pub struct Db {
    pub root: ModuleSym,
    pub source_manager: SourceManager,
    pub tree_map: TreeMap,
    pub loc_map: LocMap,
    pub files: FileMap,
    pub modules: ModuleMap,
    pub defs: DefMap,
    pub functors: FunctorMap,
    pub module_graph: ModuleGraph,
    pub module_order: Vec<ModuleSym>,
    pub module_type_orders: ModuleTypeOrders,
    pub type_orders: TypeOrders,
    pub value_orders: ValueOrders,
    pub entry_points: Vec<ValueSym>,
}

/// A coherent view into a single module.
#[derive(Clone, Copy)]
pub struct ModuleView<'db> {
    pub is_root: bool,
    pub sym: ModuleSym,
    pub module: &'db Module,
    pub tree_map: &'db TreeMap,
    pub loc_map: &'db LocMap,
    pub defs: &'db DefMap,
    pub value_order: &'db [ValueSym],
    pub type_order: &'db [TypeSym],
    pub module_type_order: &'db [ModuleTypeSym],
}

// ── by-module ──

impl Db {
    pub fn module(&self, sym: ModuleSym) -> ModuleView<'_> {
        let is_root = sym == self.root;

        ModuleView {
            is_root,
            sym,
            tree_map: &self.tree_map,
            loc_map: &self.loc_map,
            module: &self.modules[&sym],
            defs: &self.defs,
            value_order: self
                .value_orders
                .get(&sym)
                .map(|v| v.as_slice())
                .unwrap_or(&[]),
            type_order: self
                .type_orders
                .get(&sym)
                .map(|v| v.as_slice())
                .unwrap_or(&[]),
            module_type_order: self
                .module_type_orders
                .get(&sym)
                .map(|v| v.as_slice())
                .unwrap_or(&[]),
        }
    }

    pub fn all_modules(&self) -> impl DoubleEndedIterator<Item = ModuleView<'_>> + '_ {
        self.module_order.iter().map(|&sym| self.module(sym))
    }

    pub fn module_count(&self) -> usize {
        self.module_order.len()
    }
}

// ── by-source ──

impl Db {
    pub fn source(&self, id: SourceId) -> (&Tree, &LocVec) {
        (&self.tree_map[&id], &self.loc_map[&id])
    }

    pub fn modules_in_source(&self, id: SourceId) -> impl Iterator<Item = ModuleView<'_>> + '_ {
        self.modules
            .iter()
            .filter(move |(_, m)| m.loc.path == id)
            .map(|(&sym, _)| self.module(sym))
    }

    pub fn sources(&self) -> &SourceManager {
        &self.source_manager
    }
}

// ── symbol lookups ──

impl Db {
    pub fn def(&self, sym: impl Into<AnySym>) -> Option<AnyDef> {
        self.defs.get(sym.into())
    }

    pub fn functor(&self, sym: FunctorSym) -> Option<&Functor> {
        self.functors.get(&sym)
    }
}

// ── dependency ──

impl Db {
    pub fn dependencies_of(&self, sym: ModuleSym) -> impl Iterator<Item = ModuleSym> + '_ {
        self.module_graph.dependencies_of(sym).copied()
    }

    pub fn dependents_of(&self, sym: ModuleSym) -> impl Iterator<Item = ModuleSym> + '_ {
        self.module_graph.dependents_of(sym).copied()
    }

    pub fn entry_points(&self) -> &[ValueSym] {
        &self.entry_points
    }
}

// ── helpers ──

impl ModuleView<'_> {
    pub fn lookup(&self, name: impl Into<node::AnyName>) -> Option<Binding<AnySym>> {
        self.module.names.get(name)
    }

    pub fn meta<T, M>(&self, id: Id<T>) -> &M
    where
        NodeMap: Col<T, Item = M>,
    {
        self.module.nodes.get(id)
    }

    /// Get the module definition for this module,
    /// returns None if it is the root module and thus has no definition.
    pub fn module_def(&self) -> Option<ModuleDef> {
        if self.is_root {
            None
        } else {
            self.defs.get_module(self.sym)
        }
    }

    pub fn value_node(&self, sym: ValueSym) -> (Id<node::ValueBind>, &Tree, &LocVec) {
        let (id, loc) = self.defs[sym];
        let tree = &self.tree_map[&loc.path];
        let spans = &self.loc_map[&loc.path];
        (id, tree, spans)
    }

    pub fn type_node(&self, sym: TypeSym) -> (Id<node::TypeBind>, &Tree, &LocVec) {
        let (id, loc) = self.defs[sym];
        let tree = &self.tree_map[&loc.path];
        let spans = &self.loc_map[&loc.path];
        (id, tree, spans)
    }

    pub fn module_type_node(
        &self,
        sym: ModuleTypeSym,
    ) -> (Id<node::ModuleTypeBind>, &Tree, &LocVec) {
        let (id, loc) = self.defs[sym];
        let tree = &self.tree_map[&loc.path];
        let spans = &self.loc_map[&loc.path];
        (id, tree, spans)
    }

    pub fn functor_node(&self, sym: FunctorSym) -> (Id<node::FunctorBind>, &Tree, &LocVec) {
        let (id, loc) = self.defs[sym];
        let tree = &self.tree_map[&loc.path];
        let spans = &self.loc_map[&loc.path];
        (id, tree, spans)
    }
}

impl Index<SourceId> for Db {
    type Output = Tree;
    fn index(&self, id: SourceId) -> &Self::Output {
        &self.tree_map[&id]
    }
}
