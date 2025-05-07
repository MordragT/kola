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
use std::{collections::HashMap, fmt, hash::Hash, rc::Rc};

use kola_syntax::prelude::*;
use kola_tree::{
    node::{Symbol, Vis},
    prelude::*,
};
use kola_vfs::{diag::SourceDiagnostic, file::FileInfo, path::FilePath};

pub type ModuleInfoTable = HashMap<ModulePath, ModuleInfo>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
    pub id: Id<node::Module>,
    pub file_path: FilePath,
    pub span: Option<Span>,
}

impl ModulePath {
    pub fn new(id: Id<node::Module>, file_path: FilePath, span: Option<Span>) -> Self {
        Self {
            id,
            file_path,
            span,
        }
    }

    pub fn from_file(file: &FileInfo) -> Self {
        Self {
            id: file.tree.root_id(),
            file_path: file.source.file_path(),
            span: None,
        }
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ModulePath")
            .field("id", &self.id.as_usize())
            .field("path", &self.file_path)
            .field("span", &self.span)
            .finish()
    }
}

pub trait ModuleInfoView {
    fn module(&self, symbol: &Symbol) -> Option<&ModuleBind>;
    fn value(&self, symbol: &Symbol) -> Option<&ValueBind>;
    fn ty(&self, symbol: &Symbol) -> Option<&TypeBind>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameCollision {
    ValueBind {
        span: Span,
        bind: Span,
        help: &'static str,
    },
    ModuleBind {
        span: Span,
        bind: Span,
        help: &'static str,
    },
    TypeBind {
        span: Span,
        bind: Span,
        help: &'static str,
    },
}

impl NameCollision {
    pub fn value_bind(span: Span, bind: Span, help: &'static str) -> Self {
        NameCollision::ValueBind { span, bind, help }
    }
    pub fn module_bind(span: Span, bind: Span, help: &'static str) -> Self {
        NameCollision::ModuleBind { span, bind, help }
    }
    pub fn type_bind(span: Span, bind: Span, help: &'static str) -> Self {
        NameCollision::TypeBind { span, bind, help }
    }
}

impl From<NameCollision> for SourceDiagnostic {
    fn from(value: NameCollision) -> Self {
        match value {
            NameCollision::ValueBind { span, bind, help } => {
                SourceDiagnostic::error(span, "A value bind with the same name was defined before")
                    .with_trace([("This value bind here".to_owned(), bind)])
                    .with_help(help)
            }
            NameCollision::ModuleBind { span, bind, help } => {
                SourceDiagnostic::error(span, "A module bind with the same name was defined before")
                    .with_trace([("This module bind here".to_owned(), bind)])
                    .with_help(help)
            }
            NameCollision::TypeBind { span, bind, help } => {
                SourceDiagnostic::error(span, "A type bind with the same name was defined before")
                    .with_trace([("This type bind here".to_owned(), bind)])
                    .with_help(help)
            }
        }
    }
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
    pub fn insert_module(&mut self, symbol: Symbol, bind: ModuleBind) -> Result<(), NameCollision> {
        let span = bind.span;

        if let Some(bind) = self.values.get(&symbol) {
            return Err(NameCollision::value_bind(
                span,
                bind.span,
                "Module bindings must have distinct names from value bindings. Try to rename it.",
            ));
        }

        if let Some(other) = self.modules.insert(symbol, bind) {
            Err(NameCollision::module_bind(
                span,
                other.span,
                "Module bindings must have distinct names. Try to rename it.",
            ))
        } else {
            Ok(())
        }
    }

    pub fn insert_value(&mut self, symbol: Symbol, bind: ValueBind) -> Result<(), NameCollision> {
        let span = bind.span;

        if let Some(bind) = self.modules.get(&symbol) {
            return Err(NameCollision::module_bind(
                span,
                bind.span,
                "Value bindings must have distinct names from module bindings. Try to rename it.",
            ));
        }

        if let Some(other) = self.values.insert(symbol, bind) {
            Err(NameCollision::value_bind(
                span,
                other.span,
                "Value bindings must have distinct names. Try to rename it.",
            ))
        } else {
            Ok(())
        }
    }

    pub fn insert_type(&mut self, symbol: Symbol, bind: TypeBind) -> Result<(), NameCollision> {
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
    pub id: Id<node::ModuleBind>,
    pub vis: Vis,
    pub span: Span,
}

impl ModuleBind {
    pub fn new(id: Id<node::ModuleBind>, vis: Vis, span: Span) -> Self {
        Self { id, vis, span }
    }
}
