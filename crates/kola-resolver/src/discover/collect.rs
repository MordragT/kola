use kola_span::{Diagnostic, Loc};
use kola_tree::{
    node::{self, ModuleName, ModuleTypeName, TypeName, ValueName},
    prelude::*,
};

use crate::{
    compose::ComposeJob,
    def::{AnyDef, DefMap},
    env::ModuleBuilder,
    error::name_collision,
    phase::NodeMap,
    surface::{Definition, Obligation},
    symbol::{ModuleGraph, ModuleSym, ModuleTypeSym, TypeSym, ValueSym},
};

use super::{Inputs, ModuleState};

pub(super) struct Collector<'a> {
    input: &'a Inputs<'a>,
    state: ModuleState,
    defs: &'a mut DefMap,
    entry_points: &'a mut Vec<ValueSym>,
    module_graph: &'a mut ModuleGraph,
}

impl<'a> Collector<'a> {
    pub(super) fn new(
        input: &'a Inputs<'a>,
        defs: &'a mut DefMap,
        entry_points: &'a mut Vec<ValueSym>,
        module_graph: &'a mut ModuleGraph,
    ) -> Self {
        Self {
            input,
            state: ModuleState::new(input.loc, input.key),
            defs,
            entry_points,
            module_graph,
        }
    }

    pub(super) fn collect(mut self) -> ModuleState {
        self.collect_top_level();
        self.state
    }

    #[inline]
    fn span<T: 'static>(&self, id: Id<T>) -> Loc
    where
        Tree: TotalSpan<T, Loc>,
    {
        self.input.span(id)
    }

    fn name_collision(&mut self, a: impl Into<AnyDef>, b: impl Into<AnyDef>) {
        let a = a.into();
        let b = b.into();
        let error = name_collision((a.kind(), a.loc()), (b.kind(), b.loc()));
        self.state.report.add_diagnostic(error.into());
    }

    fn insert_symbol<T: 'static, S: 'static>(&mut self, id: Id<T>, sym: S)
    where
        NodeMap: PartialRelationSet<T, S>,
    {
        self.state.module.nodes.set(id, sym);
    }

    pub fn insert_module_type(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        loc: Loc,
        vis: node::Vis,
        name: ModuleTypeName,
        sym: ModuleTypeSym,
    ) -> bool {
        if let Some(bind) = self
            .state
            .module
            .names
            .get(name)
            .and_then(|binding| self.defs.get(binding.sym))
        {
            self.name_collision((id, loc), bind);
            return false;
        }

        self.state.module.names.insert_module_type(name, vis, sym);
        self.defs.insert_module_type(sym, id, loc);
        true
    }

    pub fn insert_module(
        &mut self,
        id: Id<node::ModuleBind>,
        loc: Loc,
        vis: node::Vis,
        name: ModuleName,
        sym: ModuleSym,
    ) -> bool {
        if let Some(bind) = self
            .state
            .module
            .names
            .get(name)
            .and_then(|binding| self.defs.get(binding.sym))
        {
            self.name_collision((id, loc), bind);
            return false;
        }

        self.state.module.names.insert_module(name, vis, sym);
        self.defs.insert_module(sym, id, loc);
        true
    }

    pub fn insert_type(
        &mut self,
        id: Id<node::TypeBind>,
        loc: Loc,
        vis: node::Vis,
        name: TypeName,
        sym: TypeSym,
    ) -> bool {
        if let Some(bind) = self
            .state
            .module
            .names
            .get(name)
            .and_then(|binding| self.defs.get(binding.sym))
        {
            self.name_collision((id, loc), bind);
            return false;
        }

        self.state.module.names.insert_type(name, vis, sym);
        self.defs.insert_type(sym, id, loc);
        true
    }

    pub fn insert_value(
        &mut self,
        id: Id<node::ValueBind>,
        loc: Loc,
        vis: node::Vis,
        name: ValueName,
        sym: ValueSym,
    ) -> bool {
        if let Some(bind) = self
            .state
            .module
            .names
            .get(name)
            .and_then(|binding| self.defs.get(binding.sym))
        {
            self.name_collision((id, loc), bind);
            return false;
        }

        self.state.module.names.insert_value(name, vis, sym);
        self.defs.insert_value(sym, id, loc);
        true
    }

    /// Collect every top-level declaration before any body is visited.
    fn collect_top_level(&mut self) {
        let tree = self.input.tree;

        for (index, (bind, _)) in tree.value_bind.iter().enumerate() {
            let id: Id<node::ValueBind> = Id::new(index as u32);
            self.collect_value(id, bind, tree);
        }

        for (index, (bind, _)) in tree.type_bind.iter().enumerate() {
            let id: Id<node::TypeBind> = Id::new(index as u32);
            self.collect_type(id, bind, tree);
        }

        for (index, (bind, _)) in tree.module_bind.iter().enumerate() {
            let id: Id<node::ModuleBind> = Id::new(index as u32);
            self.collect_module(id, bind, tree);
        }

        for (index, (bind, _)) in tree.module_type_bind.iter().enumerate() {
            let id: Id<node::ModuleTypeBind> = Id::new(index as u32);
            self.collect_module_type(id, bind, tree);
        }
    }

    fn collect_value(&mut self, id: Id<node::ValueBind>, bind: &node::ValueBind, tree: &Tree) {
        let name = *tree.get(bind.name);
        let vis = *tree.get(bind.vis);
        let loc = self.span(id);

        let sym = ValueSym::new();
        self.insert_symbol(id, sym);
        self.state.value_graph.add_node(sym);

        if self.input.interner.get(name.0) == Some("main") {
            self.entry_points.push(sym);
        }

        if self.insert_value(id, loc, vis, name, sym) && vis == node::Vis::Public {
            match bind.value {
                Some(_) => self.state.module.members.definitions.values.define(
                    name,
                    Definition {
                        sym,
                        origin: sym,
                        site: (self.input.sym, id),
                    },
                ),
                None => {
                    if let Some(ty_scheme) = bind.ty_scheme {
                        self.state.module.members.obligations.values.obligate(
                            name,
                            Obligation {
                                loc,
                                declared: ty_scheme,
                            },
                        );
                    }
                }
            }
        }
    }

    fn collect_type(&mut self, id: Id<node::TypeBind>, bind: &node::TypeBind, tree: &Tree) {
        let name = *tree.get(bind.name);
        let vis = *tree.get(bind.vis);
        let loc = self.span(id);

        let sym = TypeSym::new();
        self.insert_symbol(id, sym);
        self.state.type_graph.add_node(sym);

        if self.insert_type(id, loc, vis, name, sym) && vis == node::Vis::Public {
            self.state.module.members.definitions.types.define(
                name,
                Definition {
                    sym,
                    origin: sym,
                    site: (self.input.sym, id),
                },
            );
        }
    }

    fn collect_module_type(
        &mut self,
        id: Id<node::ModuleTypeBind>,
        bind: &node::ModuleTypeBind,
        tree: &Tree,
    ) {
        let name = *tree.get(bind.name);
        let vis = *tree.get(bind.vis);
        let loc = self.span(id);

        let sym = ModuleTypeSym::new();
        self.insert_symbol(id, sym);
        self.state.module_type_graph.add_node(sym);

        if self.insert_module_type(id, loc, vis, name, sym) && vis == node::Vis::Public {
            self.state.module.members.definitions.module_types.define(
                name,
                Definition {
                    sym,
                    origin: sym,
                    site: (self.input.sym, id),
                },
            );
        }
    }

    fn collect_module(&mut self, id: Id<node::ModuleBind>, bind: &node::ModuleBind, tree: &Tree) {
        let name = *tree.get(bind.name);
        let vis = *tree.get(bind.vis);
        let loc = self.span(id);

        let Some(value) = bind.value else {
            if let Some(ty) = bind.ty
                && vis == node::Vis::Public
            {
                self.state
                    .module
                    .members
                    .obligations
                    .modules
                    .obligate(name, Obligation { loc, declared: ty });
            }
            return;
        };

        match *tree.get(value) {
            node::ModuleExpr::Import(import_id) => {
                let target = import_id.get(tree).0;
                let sym = self.input.files[&target];

                self.insert_symbol(id, sym);
                self.insert_symbol(import_id, sym);

                if self.insert_module(id, loc, vis, name, sym) && vis == node::Vis::Public {
                    self.state.module.members.definitions.modules.define(
                        name,
                        Definition {
                            sym,
                            origin: sym,
                            site: (self.input.sym, id),
                        },
                    );
                }

                self.module_graph.add_dependency(self.input.sym, sym);
            }
            node::ModuleExpr::Path(_) => {
                // TODO(phase C): resolve path module aliases in compose.
                self.state.report.add_diagnostic(Diagnostic::error(
                    loc,
                    "Module path aliases are not implemented yet",
                ));
            }
            node::ModuleExpr::Body(module_ref) => {
                let key = module_ref.get(tree).0;
                let sym = self.input.module_of[&(self.input.source, key)];

                self.insert_symbol(id, sym);

                if self.insert_module(id, loc, vis, name, sym) && vis == node::Vis::Public {
                    self.state.module.members.definitions.modules.define(
                        name,
                        Definition {
                            sym,
                            origin: sym,
                            site: (self.input.sym, id),
                        },
                    );
                }

                self.module_graph.add_dependency(self.input.sym, sym);
            }
            node::ModuleExpr::Merge(merge_id) => {
                let Some(sym) = self.collect_merge_expr(merge_id, tree) else {
                    return;
                };

                self.insert_symbol(id, sym);

                if self.insert_module(id, loc, vis, name, sym) && vis == node::Vis::Public {
                    self.state.module.members.definitions.modules.define(
                        name,
                        Definition {
                            sym,
                            origin: sym,
                            site: (self.input.sym, id),
                        },
                    );
                }

                self.module_graph.add_dependency(self.input.sym, sym);
            }
            node::ModuleExpr::Error(_) => {}
        }
    }

    fn collect_module_expr(
        &mut self,
        expr: Id<node::ModuleExpr>,
        tree: &Tree,
    ) -> Option<ModuleSym> {
        match *tree.get(expr) {
            node::ModuleExpr::Merge(merge_id) => self.collect_merge_expr(merge_id, tree),
            node::ModuleExpr::Import(import_id) => {
                let target = import_id.get(tree).0;
                self.input.files.get(&target).copied()
            }
            node::ModuleExpr::Body(module_ref) => {
                let key = module_ref.get(tree).0;
                self.input.module_of.get(&(self.input.source, key)).copied()
            }
            node::ModuleExpr::Path(_) | node::ModuleExpr::Error(_) => None,
        }
    }

    fn collect_merge_expr(
        &mut self,
        merge_id: Id<node::ModuleMergeExpr>,
        tree: &Tree,
    ) -> Option<ModuleSym> {
        if let Some(sym) = self.state.module.nodes.get_opt(merge_id) {
            return Some(*sym);
        }

        let loc = self.span(merge_id);
        let sym = ModuleSym::new();

        self.state.module.nodes.set(merge_id, sym);
        self.state
            .composed
            .push((sym, ModuleBuilder::new(loc, self.input.key)));
        self.module_graph.add_node(sym);
        self.state.compose_jobs.push(ComposeJob {
            sym,
            parent: self.input.sym,
            source: self.input.source,
            key: self.input.key,
            merge: merge_id,
            loc,
        });

        let merge = *tree.get(merge_id);
        let _ = self.collect_module_expr(merge.lhs, tree);
        let _ = self.collect_module_expr(merge.rhs, tree);

        Some(sym)
    }
}
