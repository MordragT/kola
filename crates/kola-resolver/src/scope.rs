use std::{cell::RefCell, rc::Rc};

use indexmap::IndexMap;
use kola_span::Loc;
use kola_tree::node::{self, ModuleNamespace, ValueName};
use kola_utils::scope::LinearScope;

use crate::{
    bind::Bind,
    info::ValueGraph,
    refs::ModuleRefs,
    shape::ModuleShape,
    symbol::{ModuleSym, ValueSym},
};

pub type LexicalScope = LinearScope<ValueName, ValueSym>;

pub type ModuleCell = Rc<RefCell<ModuleScope>>;
pub type ModuleCells = IndexMap<ModuleSym, ModuleCell>;

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub bind: Bind<ModuleNamespace, node::Module>,
    pub shape: ModuleShape,
    pub refs: ModuleRefs,
    pub lexical: LexicalScope,
    pub value_graph: ValueGraph,
    pub loc: Loc,
}

impl ModuleScope {
    pub fn new(bind: Bind<ModuleNamespace, node::Module>, loc: Loc) -> Self {
        Self {
            bind,
            shape: ModuleShape::new(),
            refs: ModuleRefs::new(),
            lexical: LexicalScope::new(),
            value_graph: ValueGraph::new(),
            loc,
        }
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

    #[inline]
    pub fn try_bind(&self) -> Option<&Bind<ModuleNamespace, node::Module>> {
        self.todo.last().map(|scope| &scope.bind)
    }

    #[inline]
    pub fn bind(&self) -> &Bind<ModuleNamespace, node::Module> {
        self.try_bind().unwrap()
    }

    #[inline]
    pub fn try_shape(&self) -> Option<&ModuleShape> {
        self.todo.last().map(|scope| &scope.shape)
    }

    #[inline]
    pub fn shape(&self) -> &ModuleShape {
        self.try_shape().unwrap()
    }

    #[inline]
    pub fn try_shape_mut(&mut self) -> Option<&mut ModuleShape> {
        self.todo.last_mut().map(|scope| &mut scope.shape)
    }

    #[inline]
    pub fn shape_mut(&mut self) -> &mut ModuleShape {
        self.try_shape_mut().unwrap()
    }

    #[inline]
    pub fn try_refs(&self) -> Option<&ModuleRefs> {
        self.todo.last().map(|scope| &scope.refs)
    }

    #[inline]
    pub fn refs(&self) -> &ModuleRefs {
        self.try_refs().unwrap()
    }

    #[inline]
    pub fn try_refs_mut(&mut self) -> Option<&mut ModuleRefs> {
        self.todo.last_mut().map(|scope| &mut scope.refs)
    }

    #[inline]
    pub fn refs_mut(&mut self) -> &mut ModuleRefs {
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
