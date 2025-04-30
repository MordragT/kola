// *   For functors like `functor (S : SIG) => Body`, `S` acts precisely like a **Module Variable**.
//     It's a name that represents an unknown module *argument* that is required to conform to the signature `SIG`.
// *   When you *apply* this functor to a concrete module, say `MyFunctor(MyModule)`,
//     conceptually, you are indeed "substituting" `S` with `MyModule` inside the `Body`.
//     The most common and elegant way to implement this is not through literal text or AST substitution,
//     but by **extending the environment**.
//     When you process the `Body` of the functor application `MyFunctor(MyModule)`,
//     you perform this processing in an environment where the name `S` is bound to the module `MyModule`.
// *   This environment binding means that any path like `S.x` or `S.T` encountered within the `Body`
//     is resolved by first looking up `S` in the environment (finding `MyModule`)
//     and then looking up `x` or `T` within `MyModule`.
// */
use std::{
    collections::HashMap,
    fmt,
    hash::Hash,
    path::{Path, PathBuf},
    rc::Rc,
};

use kola_syntax::prelude::*;
use kola_tree::{
    node::{Symbol, Vis},
    prelude::*,
};
use kola_vfs::error::SourceDiagnostic;

pub type ModuleInfoTable = HashMap<ModuleId, ModuleInfo>;

pub trait ModuleInfoView {
    fn module(&self, symbol: &Symbol) -> Option<&ModuleBind>;
    fn value(&self, symbol: &Symbol) -> Option<&ValueBind>;
    fn ty(&self, symbol: &Symbol) -> Option<&TypeBind>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInfoBuilder {
    pub modules: HashMap<Symbol, ModuleBind>,
    pub values: HashMap<Symbol, ValueBind>,
    pub types: HashMap<Symbol, TypeBind>,
}

impl ModuleInfoBuilder {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            values: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn insert_module(
        &mut self,
        symbol: Symbol,
        bind: ModuleBind,
    ) -> Result<(), SourceDiagnostic> {
        let span = bind.span;

        if let Some(bind) = self.values.get(&symbol) {
            return Err(SourceDiagnostic::error(
                span,
                "A value bind with the same name was defined before",
            )
            .with_trace([("This value bind here".to_owned(), bind.span)])
            .with_help(
                "Module bindings must have distinct names from value bindings. Try to rename it.",
            ));
        }

        if let Some(other) = self.modules.insert(symbol, bind) {
            Err(SourceDiagnostic::error(
                span,
                "A module bind with the same name was defined before",
            )
            .with_trace([("This module bind here".to_owned(), other.span)])
            .with_help("Module bindings must have distinct names. Try to rename it."))
        } else {
            Ok(())
        }
    }

    pub fn insert_value(
        &mut self,
        symbol: Symbol,
        bind: ValueBind,
    ) -> Result<(), SourceDiagnostic> {
        let span = bind.span;

        if let Some(bind) = self.modules.get(&symbol) {
            return Err(SourceDiagnostic::error(
                span,
                "A module bind with the same name was defined before",
            )
            .with_trace([("This module bind here".to_owned(), bind.span)])
            .with_help(
                "Value bindings must have distinct names from module bindings. Try to rename it.",
            ));
        }

        if let Some(other) = self.values.insert(symbol, bind) {
            Err(
                SourceDiagnostic::error(span, "A value bind with the same name was defined before")
                    .with_trace([("This value bind here".to_owned(), other.span)])
                    .with_help("Value bindings must have distinct names. Try to rename it."),
            )
        } else {
            Ok(())
        }
    }

    pub fn insert_type(&mut self, symbol: Symbol, bind: TypeBind) -> Result<(), SourceDiagnostic> {
        todo!()
    }

    pub fn finish(self) -> ModuleInfo {
        let Self {
            modules,
            values,
            types,
        } = self;

        ModuleInfo {
            modules: Rc::new(modules),
            values: Rc::new(values),
            types: Rc::new(types),
        }
    }
}

impl ModuleInfoView for ModuleInfoBuilder {
    fn module(&self, symbol: &Symbol) -> Option<&ModuleBind> {
        self.modules.get(symbol)
    }

    fn value(&self, symbol: &Symbol) -> Option<&ValueBind> {
        self.values.get(symbol)
    }

    fn ty(&self, symbol: &Symbol) -> Option<&TypeBind> {
        self.types.get(symbol)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInfo {
    pub modules: Rc<HashMap<Symbol, ModuleBind>>,
    pub values: Rc<HashMap<Symbol, ValueBind>>,
    pub types: Rc<HashMap<Symbol, TypeBind>>,
}

impl ModuleInfoView for ModuleInfo {
    fn module(&self, symbol: &Symbol) -> Option<&ModuleBind> {
        self.modules.get(symbol)
    }

    fn value(&self, symbol: &Symbol) -> Option<&ValueBind> {
        self.values.get(symbol)
    }

    fn ty(&self, symbol: &Symbol) -> Option<&TypeBind> {
        self.types.get(symbol)
    }
}

// TODO I could include type information for values and modules
// if they are speficically written out. Not sure if there is a huge benefit though
// May be a bit more performant than to do determine it later in the Typer
// But is not strictly the main goal here so might as well not do it.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueBind {
    pub id: Id<node::ValueBind>,
    pub vis: Vis,
    pub span: Span,
}

impl ValueBind {
    pub fn new(id: Id<node::ValueBind>, vis: Vis, span: Span) -> Self {
        Self { id, vis, span }
    }
}

// TODO visibilty
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeBind {
    pub id: Id<node::TypeBind>,
    pub span: Span,
}

impl TypeBind {
    pub fn new(id: Id<node::TypeBind>, span: Span) -> Self {
        Self { id, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleBind {
    pub id: ModuleId,
    pub vis: Vis,
    pub span: Span,
}

impl ModuleBind {
    pub fn new(id: ModuleId, vis: Vis, span: Span) -> Self {
        Self { id, vis, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Repr {
    parent: Option<ModuleId>,
    path: PathBuf,
    id: Id<node::Module>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(Rc<Repr>);

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let parent = if let Some(parent) = &self.0.parent {
            Some(&parent.0.path)
        } else {
            None
        };

        f.debug_struct("ModuleId")
            .field("id", &self.0.id.as_usize())
            .field("path", &self.0.path)
            .field("parent", &parent)
            .finish()
    }
}

impl ModuleId {
    // pub fn root(path: impl Into<PathBuf>, id: Id<node::Module>) -> Self {
    //     let repr = Repr {
    //         parent: None,
    //         path: path.into(),
    //         id,
    //     };

    //     Self(Rc::new(repr))
    // }

    pub fn new(parent: Option<Self>, path: impl Into<PathBuf>, id: Id<node::Module>) -> Self {
        let repr = Repr {
            parent,
            path: path.into(),
            id,
        };

        Self(Rc::new(repr))
    }

    pub fn parent(&self) -> Option<&Self> {
        self.0.parent.as_ref()
    }

    pub fn path(&self) -> &Path {
        &self.0.path
    }

    pub fn id(&self) -> Id<node::Module> {
        self.0.id
    }

    pub fn is_parent_of(&self, other: &Self) -> bool {
        other.parent().is_some_and(|parent| parent == self)
    }

    pub fn is_child_of(&self, other: &Self) -> bool {
        self.parent().is_some_and(|parent| parent == other)
    }
}
