use kola_span::{SourceId, SourceManager};
use kola_tree::{
    node,
    prelude::{Id, PartialRelation},
    source::TreeMap,
    tree::Tree,
};

use crate::{
    def::{AnyDef, DefMap, ModuleDef},
    env::{Module, ModuleMap},
    name::Binding,
    phase::NodeMap,
    symbol::{
        AnySym, FileMap, ModuleGraph, ModuleSym, ModuleTypeOrders, ModuleTypeSym, TypeOrders,
        TypeSym, ValueOrders, ValueSym,
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
    pub files: FileMap,
    pub modules: ModuleMap,
    pub defs: DefMap,
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
    pub fn source(&self, id: SourceId) -> &kola_tree::source::SourceTrees {
        &self.tree_map[&id]
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

    pub fn meta<T: 'static, M: 'static>(&self, id: Id<T>) -> &M
    where
        NodeMap: PartialRelation<T, M>,
    {
        PartialRelation::<T, M>::get_opt(&self.module.nodes, id).expect("meta not found")
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

    pub fn value_node(&self, sym: ValueSym) -> (Id<node::ValueBind>, &Tree) {
        (self.defs[sym].0, self.tree())
    }

    pub fn type_node(&self, sym: TypeSym) -> (Id<node::TypeBind>, &Tree) {
        (self.defs[sym].0, self.tree())
    }

    pub fn module_type_node(&self, sym: ModuleTypeSym) -> (Id<node::ModuleTypeBind>, &Tree) {
        (self.defs[sym].0, self.tree())
    }
}

impl ModuleView<'_> {
    /// The tree this module's syntax lives in.
    #[inline]
    pub fn tree(&self) -> &Tree {
        &self.tree_map[&self.module.loc.path].trees[&self.module.key]
    }
}
