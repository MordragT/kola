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
use std::{collections::HashMap, hash::Hash, ops::ControlFlow, rc::Rc};

use kola_syntax::prelude::*;
use kola_tree::{node::Vis, prelude::*};
use kola_vfs::{
    diag::{SourceDiagnostic, SourceReport},
    file::FileInfo,
};

use crate::error::NameCollision;

pub struct BindDiscoverer {
    builder: BindInfoBuilder,
    errors: Vec<SourceDiagnostic>,
    file: FileInfo,
}

impl BindDiscoverer {
    pub fn new(file: FileInfo) -> Self {
        Self {
            builder: BindInfoBuilder::new(),
            errors: Vec::new(),
            file,
        }
    }

    #[inline]
    fn span<T>(&self, id: Id<T>) -> Span
    where
        T: MetaCast<LocPhase, Meta = Span>,
    {
        self.file.span(id)
    }

    #[inline]
    fn report(&mut self, diag: impl Into<SourceDiagnostic>) {
        self.errors.push(diag.into())
    }

    pub fn finish(self) -> Result<BindInfo, SourceReport> {
        let Self {
            builder,
            errors,
            file,
        } = self;

        if errors.is_empty() {
            Ok(builder.finish())
        } else {
            let report = SourceReport::new(file.source.clone(), errors);
            Err(report)
        }
    }
}

impl<T> Visitor<T> for BindDiscoverer
where
    T: TreeView,
{
    type BreakValue = ();

    // This will only traverse the current module's scope,
    // because the different module branches are not visited any further under `visit_module_bind`
    fn visit_value_bind(
        &mut self,
        id: Id<node::ValueBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::ValueBind { vis, name, .. } = *id.get(tree);

        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();

        if let Err(diag) = self
            .builder
            .insert_value(name, ValueBind::new(id, vis, span))
        {
            self.report(diag);
        }

        ControlFlow::Continue(())
    }

    fn visit_type_bind(
        &mut self,
        id: Id<node::TypeBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::TypeBind { name, .. } = *id.get(tree);

        let name = name.get(tree).0.clone();

        if let Err(diag) = self.builder.insert_type(name, TypeBind::new(id, span)) {
            self.report(diag);
        }
        ControlFlow::Continue(())
    }

    fn visit_module_bind(
        &mut self,
        id: Id<node::ModuleBind>,
        tree: &T,
    ) -> ControlFlow<Self::BreakValue> {
        let span = self.span(id);

        let node::ModuleBind { vis, name, .. } = *id.get(tree);

        let vis = *vis.get(tree);
        let name = name.get(tree).0.clone();

        if let Err(diag) = self
            .builder
            .insert_module(name, ModuleBind::new(id, vis, span))
        {
            self.report(diag);
        }
        ControlFlow::Continue(())
    }
}

pub trait BindInfoView {
    fn module(&self, symbol: StrKey) -> Option<&ModuleBind>;
    fn value(&self, symbol: StrKey) -> Option<&ValueBind>;
    fn ty(&self, symbol: StrKey) -> Option<&TypeBind>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindInfoBuilder {
    pub modules: HashMap<StrKey, ModuleBind>,
    pub values: HashMap<StrKey, ValueBind>,
    pub types: HashMap<StrKey, TypeBind>,
}

impl BindInfoBuilder {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            values: HashMap::new(),
            types: HashMap::new(),
        }
    }
    pub fn insert_module(&mut self, symbol: StrKey, bind: ModuleBind) -> Result<(), NameCollision> {
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

    pub fn insert_value(&mut self, symbol: StrKey, bind: ValueBind) -> Result<(), NameCollision> {
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

    pub fn insert_type(&mut self, symbol: StrKey, bind: TypeBind) -> Result<(), NameCollision> {
        todo!()
    }

    pub fn finish(self) -> BindInfo {
        let Self {
            modules,
            values,
            types,
        } = self;

        BindInfo {
            modules: Rc::new(modules),
            values: Rc::new(values),
            types: Rc::new(types),
        }
    }
}

impl BindInfoView for BindInfoBuilder {
    fn module(&self, symbol: StrKey) -> Option<&ModuleBind> {
        self.modules.get(symbol)
    }

    fn value(&self, symbol: StrKey) -> Option<&ValueBind> {
        self.values.get(symbol)
    }

    fn ty(&self, symbol: StrKey) -> Option<&TypeBind> {
        self.types.get(symbol)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindInfo {
    pub modules: Rc<HashMap<StrKey, ModuleBind>>,
    pub values: Rc<HashMap<StrKey, ValueBind>>,
    pub types: Rc<HashMap<StrKey, TypeBind>>,
}

impl BindInfoView for BindInfo {
    fn module(&self, symbol: StrKey) -> Option<&ModuleBind> {
        self.modules.get(symbol)
    }

    fn value(&self, symbol: StrKey) -> Option<&ValueBind> {
        self.values.get(symbol)
    }

    fn ty(&self, symbol: StrKey) -> Option<&TypeBind> {
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
