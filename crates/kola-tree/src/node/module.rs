use super::{Expr, FunctorName, ModuleName, ModuleTypeName, TypeName, TypeScheme, ValueName};
use crate::prelude::*;
use crate::print::TreePrinter;
use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_interner::PathKey;
use kola_print::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "red")]
pub struct BindError;

impl BindError {
    pub fn new_in<M>(builder: &mut Tree<M>) -> Id<Self>
    where
        M: Clone + Default + 'static,
    {
        builder.alloc(Self)
    }
}

#[derive(
    Debug,
    EnumAsInner,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum Bind {
    Value(Id<ValueBind>),
    Type(Id<TypeBind>),
    Module(Id<ModuleBind>),
    ModuleType(Id<ModuleTypeBind>),
    Functor(Id<FunctorBind>),
    Error(Id<BindError>),
}

impl<'a> Notate<'a, Bind> for TreePrinter<'a> {
    fn notate(self, value: &Bind, arena: &'a Bump) -> Notation<'a> {
        match value {
            Bind::Value(v) => self.notate(v, arena),
            Bind::Type(t) => self.notate(t, arena),
            Bind::Module(m) => self.notate(m, arena),
            Bind::ModuleType(mt) => self.notate(mt, arena),
            Bind::Functor(f) => self.notate(f, arena),
            Bind::Error(e) => self.notate(e, arena),
        }
    }
}

impl Bind {
    pub fn value_in<M>(
        vis: Vis,
        name: ValueName,
        ty_scheme: Option<TypeScheme>,
        value: Expr,
        builder: &mut Tree<M>,
    ) -> Id<Self>
    where
        M: Clone + Default + 'static,
    {
        let bind = ValueBind::new_in(vis, name, ty_scheme, value, builder);

        builder.alloc(Self::Value(bind))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Vis {
    Export,
    None,
}

impl<'a> Notate<'a, Vis> for TreePrinter<'a> {
    fn notate(self, value: &Vis, arena: &'a Bump) -> Notation<'a> {
        match value {
            Vis::Export => "Export".purple().display_in(arena),
            Vis::None => arena.empty(),
        }
    }
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ValueBind {
    pub vis: Id<Vis>,
    pub name: Id<ValueName>,
    pub ty_scheme: Option<Id<TypeScheme>>,
    pub value: Id<Expr>,
}

impl ValueBind {
    pub fn new_in<M>(
        vis: Vis,
        name: ValueName,
        ty_scheme: Option<TypeScheme>,
        value: Expr,
        builder: &mut Tree<M>,
    ) -> Id<Self>
    where
        M: Clone + Default + 'static,
    {
        let vis = builder.alloc(vis);
        let name = builder.alloc(name);
        let ty_scheme = ty_scheme.map(|ty| builder.alloc(ty));
        let value = builder.alloc(value);

        builder.alloc(Self {
            vis,
            name,
            ty_scheme,
            value,
        })
    }
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct TypeBind {
    pub vis: Id<Vis>,
    pub name: Id<TypeName>,
    pub ty_scheme: Id<TypeScheme>,
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ModuleBind {
    pub vis: Id<Vis>,
    pub name: Id<ModuleName>,
    pub ty: Option<Id<ModuleType>>,
    pub value: Id<ModuleExpr>,
}

impl ModuleBind {
    pub fn new_in<M>(
        vis: Vis,
        name: ModuleName,
        ty: Option<ModuleType>,
        value: ModuleExpr,
        builder: &mut Tree<M>,
    ) -> Id<Self>
    where
        M: Clone + Default + 'static,
    {
        let vis = builder.alloc(vis);
        let name = builder.alloc(name);
        let ty = ty.map(|sig| builder.alloc(sig));
        let value = builder.alloc(value);

        builder.alloc(Self {
            vis,
            name,
            ty,
            value,
        })
    }
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct FunctorParam {
    pub name: Id<ModuleName>,
    pub ty: Id<ModuleType>,
}

#[derive(Debug, Notate, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct FunctorBind {
    pub vis: Id<Vis>,
    pub name: Id<FunctorName>,
    pub params: SliceId<FunctorParam>,
    pub body: Id<ModuleBody>,
}

impl FunctorBind {
    pub fn new_in<M>(
        vis: Vis,
        name: FunctorName,
        params: impl IntoIterator<Item = FunctorParam>,
        body: ModuleBody,
        builder: &mut Tree<M>,
    ) -> Id<Self>
    where
        M: Clone + Default + 'static,
    {
        let vis = builder.alloc(vis);
        let name = builder.alloc(name);

        let params: Vec<_> = params
            .into_iter()
            .map(|param| builder.alloc(param))
            .collect();
        let params = builder.slices.alloc(params);
        let body = builder.alloc(body);

        builder.alloc(Self {
            vis,
            name,
            params,
            body,
        })
    }
}

#[derive(
    Debug,
    EnumAsInner,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum ModuleExpr {
    Error(Id<ModuleError>),
    Body(Id<ModuleBody>),
    Import(Id<ModuleImport>),
    Path(Id<ModulePath>),
    FunctorApp(Id<FunctorApp>),
}

impl<'a> Notate<'a, ModuleExpr> for TreePrinter<'a> {
    fn notate(self, value: &ModuleExpr, arena: &'a Bump) -> Notation<'a> {
        match value {
            ModuleExpr::Error(id) => self.notate(id, arena),
            ModuleExpr::Body(id) => self.notate(id, arena),
            ModuleExpr::Import(id) => self.notate(id, arena),
            ModuleExpr::Path(id) => self.notate(id, arena),
            ModuleExpr::FunctorApp(id) => self.notate(id, arena),
        }
    }
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "red")]
pub struct ModuleError;

// TODO rename to ModuleBody ?
#[derive(
    Debug, Notate, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ModuleBody(pub SliceId<Bind>);

#[derive(
    Debug, Notate, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "cyan")]
pub struct ModulePath(pub SliceId<ModuleName>);

impl ModulePath {
    pub fn get(&self, index: usize, tree: &Tree) -> ModuleName {
        *tree.get_slice(self.0).get(index).unwrap().get(tree)
    }
}

#[derive(
    Notate, Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ModuleImport(#[notate(display)] pub PathKey);

#[derive(
    Debug, Notate, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct FunctorArgs(pub SliceId<ModulePath>);

#[derive(Debug, Notate, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct FunctorApp {
    pub path: Option<Id<ModulePath>>,
    pub func: Id<FunctorName>,
    pub args: Id<FunctorArgs>,
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ModuleTypeBind {
    pub vis: Id<Vis>,
    pub name: Id<ModuleTypeName>,
    pub ty: Id<ModuleType>,
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum ModuleType {
    Qualified(Id<QualifiedModuleType>),
    Concrete(Id<ConcreteModuleType>),
}

impl<'a> Notate<'a, ModuleType> for TreePrinter<'a> {
    fn notate(self, value: &ModuleType, arena: &'a Bump) -> Notation<'a> {
        match value {
            ModuleType::Qualified(q) => self.notate(q, arena),
            ModuleType::Concrete(c) => self.notate(c, arena),
        }
    }
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct QualifiedModuleType {
    pub path: Option<Id<ModulePath>>,
    pub ty: Id<ModuleTypeName>,
}

#[derive(
    Debug, Notate, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ConcreteModuleType(pub SliceId<Spec>);

#[derive(
    Debug,
    EnumAsInner,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum Spec {
    Value(Id<ValueSpec>),
    Module(Id<ModuleSpec>),
    Error(Id<SpecError>),
}

impl<'a> Notate<'a, Spec> for TreePrinter<'a> {
    fn notate(self, value: &Spec, arena: &'a Bump) -> Notation<'a> {
        match value {
            Spec::Value(v) => self.notate(v, arena),
            Spec::Module(m) => self.notate(m, arena),
            Spec::Error(e) => self.notate(e, arena),
        }
    }
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "red")]
pub struct SpecError;

// f : Num -> Num
#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ValueSpec {
    pub name: Id<ValueName>,
    pub ty: Id<TypeScheme>,
}

// module M : { ... }
#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ModuleSpec {
    pub name: Id<ModuleName>,
    pub ty: Id<ModuleType>,
}
