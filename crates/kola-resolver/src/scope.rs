use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use kola_span::Loc;
use kola_tree::node::{self, ModuleName, ModuleNamespace, TypeName, ValueName};
use kola_utils::scope::LinearScope;

use crate::{
    bind::Bind,
    defs::{AnyDef, Definitions, ModuleDef, TypeDef, ValueDef},
    error::NameCollision,
    info::ValueGraph,
    refs::References,
    shape::Shape,
    symbol::{ModuleSym, TypeSym, ValueSym},
};

pub type LexicalScope = LinearScope<ValueName, ValueSym>;

pub type ModuleCell = Rc<RefCell<ModuleScope>>;
pub type ModuleCells = IndexMap<ModuleSym, ModuleCell>;

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub bind: Bind<ModuleNamespace, node::Module>,
    pub shape: Shape,
    pub defs: Definitions,
    pub refs: References,
    pub lexical: LexicalScope,
    pub value_graph: ValueGraph,
    pub loc: Loc,
}

impl ModuleScope {
    pub fn new(bind: Bind<ModuleNamespace, node::Module>, loc: Loc) -> Self {
        Self {
            bind,
            shape: Shape::new(),
            defs: Definitions::new(),
            refs: References::new(),
            lexical: LexicalScope::new(),
            value_graph: ValueGraph::new(),
            loc,
        }
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
        (Module, Module) => "Module bindings must have distinct names from Module bindings.",
        (Module, Value) => "Module bindings must have distinct names from Value bindings.",
        (Module, Type) => "Module bindings must have distinct names from Type bindings.",
        (Value, Module) => "Value bindings must have distinct names from Module bindings.",
        (Value, Value) => "Value bindings must have distinct names from Value bindings.",
        (Value, Type) => "Value bindings must have distinct names from Type bindings.",
        (Type, Module) => "Type bindings must have distinct names from Module bindings.",
        (Type, Value) => "Type bindings must have distinct names from Value bindings.",
        (Type, Type) => "Type bindings must have distinct names from Type bindings.",
    };

    match this {
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
    pub fn start(&mut self, scope: ModuleScope) {
        self.todo.push(scope);
    }

    #[inline]
    pub fn finish(&mut self) {
        if let Some(current) = self.todo.pop() {
            self.done.push(current);
        }
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
    pub fn try_bind(&self) -> Option<&Bind<ModuleNamespace, node::Module>> {
        self.todo.last().map(|scope| &scope.bind)
    }

    #[inline]
    pub fn bind(&self) -> &Bind<ModuleNamespace, node::Module> {
        self.try_bind().unwrap()
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
    pub fn try_lexical(&self) -> Option<&LexicalScope> {
        self.todo.last().map(|scope| &scope.lexical)
    }

    #[inline]
    pub fn lexical(&self) -> &LexicalScope {
        self.try_lexical().unwrap()
    }

    #[inline]
    pub fn try_lexical_mut(&mut self) -> Option<&mut LexicalScope> {
        self.todo.last_mut().map(|scope| &mut scope.lexical)
    }

    #[inline]
    pub fn lexical_mut(&mut self) -> &mut LexicalScope {
        self.try_lexical_mut().unwrap()
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
    pub fn try_loc(&self) -> Option<&Loc> {
        self.todo.last().map(|scope| &scope.loc)
    }

    #[inline]
    pub fn loc(&self) -> &Loc {
        self.try_loc().unwrap()
    }

    #[inline]
    pub fn into_completed(self) -> Vec<ModuleScope> {
        self.done
    }
}
