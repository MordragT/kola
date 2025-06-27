use indexmap::IndexMap;
use kola_tree::node::{ModuleName, ModuleTypeName, TypeName, ValueName};
use kola_utils::scope::LinearScope;

use crate::{
    defs::{AnyDef, Definitions, ModuleDef, ModuleTypeDef, TypeDef, ValueDef},
    error::NameCollision,
    info::{ModuleInfo, ModuleTypeGraph, TypeGraph, ValueGraph},
    phase::ResolvedNodes,
    refs::References,
    shape::Shape,
    symbol::{ModuleSym, ModuleTypeSym, TypeSym, ValueSym},
};

pub type ModuleScopes = IndexMap<ModuleSym, ModuleScope>;

pub type ValueScope = LinearScope<ValueName, ValueSym>;
pub type TypeScope = LinearScope<TypeName, TypeSym>;

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub info: ModuleInfo,
    pub shape: Shape,
    pub defs: Definitions,
    pub refs: References,
    pub resolved: ResolvedNodes,
    pub value_scope: ValueScope,
    pub value_graph: ValueGraph,
    pub type_scope: TypeScope,
    pub type_graph: TypeGraph,
    pub module_type_graph: ModuleTypeGraph,
}

impl ModuleScope {
    pub fn new(info: ModuleInfo) -> Self {
        Self {
            info,
            shape: Shape::new(),
            defs: Definitions::new(),
            refs: References::new(),
            resolved: ResolvedNodes::new(),
            value_scope: ValueScope::new(),
            value_graph: ValueGraph::new(),
            type_scope: TypeScope::new(),
            type_graph: TypeGraph::new(),
            module_type_graph: ModuleTypeGraph::new(),
        }
    }

    pub fn insert_module_type(
        &mut self,
        name: ModuleTypeName,
        sym: ModuleTypeSym,
        def: ModuleTypeDef,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.shape.get(name).and_then(|sym| self.defs.get(sym)) {
            return Err(name_collision(def.into(), bind));
        }

        self.shape.insert_module_type(name, sym);
        self.defs.insert_module_type(sym, def);
        Ok(())
    }

    pub fn insert_module(
        &mut self,
        name: ModuleName,
        sym: ModuleSym,
        def: ModuleDef,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.shape.get(name).and_then(|sym| self.defs.get(sym)) {
            return Err(name_collision(def.into(), bind));
        }

        self.shape.insert_module(name, sym);
        self.defs.insert_module(sym, def);
        Ok(())
    }

    pub fn insert_value(
        &mut self,
        name: ValueName,
        sym: ValueSym,
        def: ValueDef,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.shape.get(name).and_then(|sym| self.defs.get(sym)) {
            return Err(name_collision(def.into(), bind));
        }

        self.shape.insert_value(name, sym);
        self.defs.insert_value(sym, def);
        Ok(())
    }

    pub fn insert_type(
        &mut self,
        name: TypeName,
        sym: TypeSym,
        def: TypeDef,
    ) -> Result<(), NameCollision> {
        if let Some(bind) = self.shape.get(name).and_then(|sym| self.defs.get(sym)) {
            return Err(name_collision(def.into(), bind));
        }

        self.shape.insert_type(name, sym);
        self.defs.insert_type(sym, def);
        Ok(())
    }
}

const fn name_collision(this: AnyDef, other: AnyDef) -> NameCollision {
    use kola_tree::node::NamespaceKind::*;

    let help = match (this.kind(), other.kind()) {
        (ModuleType, ModuleType) => {
            "Module type bindings must have distinct names from Module type bindings."
        }
        (Module, Module) => "Module bindings must have distinct names from Module bindings.",
        (Module, Value) => "Module bindings must have distinct names from Value bindings.",
        (Module, Type) => "Module bindings must have distinct names from Type bindings.",
        (Value, Module) => "Value bindings must have distinct names from Module bindings.",
        (Value, Value) => "Value bindings must have distinct names from Value bindings.",
        (Value, Type) => "Value bindings must have distinct names from Type bindings.",
        (Type, Module) => "Type bindings must have distinct names from Module bindings.",
        (Type, Value) => "Type bindings must have distinct names from Value bindings.",
        (Type, Type) => "Type bindings must have distinct names from Type bindings.",
        (_, _) => "TODO Bindings must have distinct names.",
    };

    match this {
        AnyDef::ModuleType(this) => {
            NameCollision::module_type_bind(this.loc, other.location(), help)
        }
        AnyDef::Module(this) => NameCollision::module_bind(this.loc, other.location(), help),
        AnyDef::Value(this) => NameCollision::value_bind(this.loc, other.location(), help),
        AnyDef::Type(this) => NameCollision::type_bind(this.loc, other.location(), help),
    }
}

#[derive(Debug, Clone)]
pub struct ModuleScopeStack {
    todo: Vec<ModuleScope>,
    done: Vec<ModuleScope>,
}

impl ModuleScopeStack {
    #[inline]
    pub fn new() -> Self {
        Self {
            todo: Vec::new(),
            done: Vec::new(),
        }
    }

    #[inline]
    pub fn start(&mut self, scope: impl Into<ModuleScope>) {
        self.todo.push(scope.into());
    }

    #[inline]
    pub fn finish(&mut self) {
        if let Some(current) = self.todo.pop() {
            self.done.push(current);
        }
    }

    pub fn insert_module_type(
        &mut self,
        name: ModuleTypeName,
        sym: ModuleTypeSym,
        def: ModuleTypeDef,
    ) -> Result<(), NameCollision> {
        self.todo
            .last_mut()
            .unwrap()
            .insert_module_type(name, sym, def)
    }

    pub fn insert_module(
        &mut self,
        name: ModuleName,
        sym: ModuleSym,
        def: ModuleDef,
    ) -> Result<(), NameCollision> {
        self.todo.last_mut().unwrap().insert_module(name, sym, def)
    }

    pub fn insert_type(
        &mut self,
        name: TypeName,
        sym: TypeSym,
        def: TypeDef,
    ) -> Result<(), NameCollision> {
        self.todo.last_mut().unwrap().insert_type(name, sym, def)
    }

    pub fn insert_value(
        &mut self,
        name: ValueName,
        sym: ValueSym,
        def: ValueDef,
    ) -> Result<(), NameCollision> {
        self.todo.last_mut().unwrap().insert_value(name, sym, def)
    }

    #[inline]
    pub fn try_info(&self) -> Option<&ModuleInfo> {
        self.todo.last().map(|scope| &scope.info)
    }

    #[inline]
    pub fn info(&self) -> &ModuleInfo {
        self.try_info().unwrap()
    }

    #[inline]
    pub fn try_shape(&self) -> Option<&Shape> {
        self.todo.last().map(|scope| &scope.shape)
    }

    #[inline]
    pub fn shape(&self) -> &Shape {
        self.try_shape().unwrap()
    }

    #[inline]
    pub fn try_shape_mut(&mut self) -> Option<&mut Shape> {
        self.todo.last_mut().map(|scope| &mut scope.shape)
    }

    #[inline]
    pub fn shape_mut(&mut self) -> &mut Shape {
        self.try_shape_mut().unwrap()
    }

    #[inline]
    pub fn try_defs(&self) -> Option<&Definitions> {
        self.todo.last().map(|scope| &scope.defs)
    }

    #[inline]
    pub fn defs(&self) -> &Definitions {
        self.try_defs().unwrap()
    }

    #[inline]
    pub fn try_defs_mut(&mut self) -> Option<&mut Definitions> {
        self.todo.last_mut().map(|scope| &mut scope.defs)
    }

    #[inline]
    pub fn defs_mut(&mut self) -> &mut Definitions {
        self.try_defs_mut().unwrap()
    }

    #[inline]
    pub fn try_refs(&self) -> Option<&References> {
        self.todo.last().map(|scope| &scope.refs)
    }

    #[inline]
    pub fn refs(&self) -> &References {
        self.try_refs().unwrap()
    }

    #[inline]
    pub fn try_refs_mut(&mut self) -> Option<&mut References> {
        self.todo.last_mut().map(|scope| &mut scope.refs)
    }

    #[inline]
    pub fn refs_mut(&mut self) -> &mut References {
        self.try_refs_mut().unwrap()
    }

    #[inline]
    pub fn try_resolved(&self) -> Option<&ResolvedNodes> {
        self.todo.last().map(|scope| &scope.resolved)
    }

    #[inline]
    pub fn resolved(&self) -> &ResolvedNodes {
        self.try_resolved().unwrap()
    }

    #[inline]
    pub fn try_resolved_mut(&mut self) -> Option<&mut ResolvedNodes> {
        self.todo.last_mut().map(|scope| &mut scope.resolved)
    }

    #[inline]
    pub fn resolved_mut(&mut self) -> &mut ResolvedNodes {
        self.try_resolved_mut().unwrap()
    }

    #[inline]
    pub fn try_value_scope(&self) -> Option<&ValueScope> {
        self.todo.last().map(|scope| &scope.value_scope)
    }

    #[inline]
    pub fn value_scope(&self) -> &ValueScope {
        self.try_value_scope().unwrap()
    }

    #[inline]
    pub fn try_value_scope_mut(&mut self) -> Option<&mut ValueScope> {
        self.todo.last_mut().map(|scope| &mut scope.value_scope)
    }

    #[inline]
    pub fn value_scope_mut(&mut self) -> &mut ValueScope {
        self.try_value_scope_mut().unwrap()
    }

    #[inline]
    pub fn try_type_scope(&self) -> Option<&TypeScope> {
        self.todo.last().map(|scope| &scope.type_scope)
    }

    #[inline]
    pub fn type_scope(&self) -> &TypeScope {
        self.try_type_scope().unwrap()
    }

    #[inline]
    pub fn try_type_scope_mut(&mut self) -> Option<&mut TypeScope> {
        self.todo.last_mut().map(|scope| &mut scope.type_scope)
    }

    #[inline]
    pub fn type_scope_mut(&mut self) -> &mut TypeScope {
        self.try_type_scope_mut().unwrap()
    }

    #[inline]
    pub fn try_value_graph(&self) -> Option<&ValueGraph> {
        self.todo.last().map(|scope| &scope.value_graph)
    }

    #[inline]
    pub fn value_graph(&self) -> &ValueGraph {
        self.try_value_graph().unwrap()
    }

    #[inline]
    pub fn try_value_graph_mut(&mut self) -> Option<&mut ValueGraph> {
        self.todo.last_mut().map(|scope| &mut scope.value_graph)
    }

    #[inline]
    pub fn value_graph_mut(&mut self) -> &mut ValueGraph {
        self.try_value_graph_mut().unwrap()
    }
    #[inline]
    pub fn try_type_graph(&self) -> Option<&TypeGraph> {
        self.todo.last().map(|scope| &scope.type_graph)
    }

    #[inline]
    pub fn type_graph(&self) -> &TypeGraph {
        self.try_type_graph().unwrap()
    }

    #[inline]
    pub fn try_type_graph_mut(&mut self) -> Option<&mut TypeGraph> {
        self.todo.last_mut().map(|scope| &mut scope.type_graph)
    }

    #[inline]
    pub fn type_graph_mut(&mut self) -> &mut TypeGraph {
        self.try_type_graph_mut().unwrap()
    }

    #[inline]
    pub fn try_module_type_graph(&self) -> Option<&ModuleTypeGraph> {
        self.todo.last().map(|scope| &scope.module_type_graph)
    }

    #[inline]
    pub fn module_type_graph(&self) -> &ModuleTypeGraph {
        self.try_module_type_graph().unwrap()
    }

    #[inline]
    pub fn try_module_type_graph_mut(&mut self) -> Option<&mut ModuleTypeGraph> {
        self.todo
            .last_mut()
            .map(|scope| &mut scope.module_type_graph)
    }

    #[inline]
    pub fn module_type_graph_mut(&mut self) -> &mut ModuleTypeGraph {
        self.try_module_type_graph_mut().unwrap()
    }

    #[inline]
    pub fn into_completed(self) -> Vec<ModuleScope> {
        self.done
    }
}

impl From<ModuleInfo> for ModuleScope {
    fn from(info: ModuleInfo) -> Self {
        Self::new(info)
    }
}
