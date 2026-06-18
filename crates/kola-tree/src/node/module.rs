use derive_more::{From, IntoIterator};
use enum_as_inner::EnumAsInner;
use kola_macros::{Inspector, Notate};
use kola_utils::interner::PathKey;
use serde::{Deserialize, Serialize};

use kola_print::prelude::*;

use super::{Expr, ModuleName, TypeScheme};
use crate::{
    id::Id,
    node::{FunctorName, ModuleTypeName, TypeName, ValueName},
    print::NodePrinter,
    tree::{TreeBuilder, TreeView},
};

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "red")]
pub struct BindError;

impl BindError {
    pub fn new_in(builder: &mut TreeBuilder) -> Id<Self> {
        builder.insert(Self)
    }
}

#[derive(
    Debug,
    EnumAsInner,
    Inspector,
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

impl<'a> Notate<'a> for NodePrinter<'a, Bind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Bind::Value(v) => self.to(v).notate(arena),
            Bind::Type(t) => self.to(t).notate(arena),
            Bind::Module(m) => self.to(m).notate(arena),
            Bind::ModuleType(mt) => self.to(mt).notate(arena),
            Bind::Functor(f) => self.to(f).notate(arena),
            Bind::Error(e) => self.to(e).notate(arena),
        }
    }
}

impl Bind {
    pub fn value_in(
        vis: Vis,
        name: ValueName,
        ty_scheme: Option<TypeScheme>,
        value: Expr,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let bind = ValueBind::new_in(vis, name, ty_scheme, value, builder);

        builder.insert(Self::Value(bind))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Vis {
    Export,
    None,
}

impl<'a> Notate<'a> for NodePrinter<'a, Vis> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Vis::Export => "Export".purple().display_in(arena),
            Vis::None => arena.empty(),
        }
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
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
#[notate(color = "green")]
pub struct ValueBind {
    pub vis: Id<Vis>,
    pub name: Id<ValueName>,
    pub ty_scheme: Option<Id<TypeScheme>>,
    pub value: Id<Expr>,
}

impl ValueBind {
    pub fn new_in(
        vis: Vis,
        name: ValueName,
        ty_scheme: Option<TypeScheme>,
        value: Expr,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let vis = builder.insert(vis);
        let name = builder.insert(name);
        let ty_scheme = ty_scheme.map(|ty| builder.insert(ty));
        let value = builder.insert(value);

        builder.insert(Self {
            vis,
            name,
            ty_scheme,
            value,
        })
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
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
#[notate(color = "green")]
pub struct TypeBind {
    pub vis: Id<Vis>,
    pub name: Id<TypeName>,
    pub ty_scheme: Id<TypeScheme>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
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
#[notate(color = "green")]
pub struct ModuleBind {
    pub vis: Id<Vis>,
    pub name: Id<ModuleName>,
    pub ty: Option<Id<ModuleType>>,
    pub value: Id<ModuleExpr>,
}

impl ModuleBind {
    pub fn new_in(
        vis: Vis,
        name: ModuleName,
        ty: Option<ModuleType>,
        value: ModuleExpr,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let vis = builder.insert(vis);
        let name = builder.insert(name);
        let ty = ty.map(|sig| builder.insert(sig));
        let value = builder.insert(value);

        builder.insert(Self {
            vis,
            name,
            ty,
            value,
        })
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
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
#[notate(color = "green")]
pub struct FunctorParam {
    pub name: Id<ModuleName>,
    pub ty: Id<ModuleType>,
}

#[derive(
    Debug, Notate, Inspector, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "green")]
pub struct FunctorBind {
    pub vis: Id<Vis>,
    pub name: Id<FunctorName>,
    pub params: Vec<Id<FunctorParam>>,
    pub body: Id<ModuleBody>,
}

impl FunctorBind {
    pub fn new_in(
        vis: Vis,
        name: FunctorName,
        params: impl IntoIterator<Item = FunctorParam>,
        body: ModuleBody,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let vis = builder.insert(vis);
        let name = builder.insert(name);
        let params = params
            .into_iter()
            .map(|param| builder.insert(param))
            .collect();
        let body = builder.insert(body);

        builder.insert(Self {
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
    Inspector,
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

impl<'a> Notate<'a> for NodePrinter<'a, ModuleExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            ModuleExpr::Error(id) => self.to(id).notate(arena),
            ModuleExpr::Body(id) => self.to(id).notate(arena),
            ModuleExpr::Import(id) => self.to(id).notate(arena),
            ModuleExpr::Path(id) => self.to(id).notate(arena),
            ModuleExpr::FunctorApp(id) => self.to(id).notate(arena),
        }
    }
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "red")]
pub struct ModuleError;

// TODO rename to ModuleBody ?
#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "green")]
#[into_iterator(owned, ref)]
pub struct ModuleBody(pub Vec<Id<Bind>>);

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "cyan")]
#[into_iterator(owned, ref)]
pub struct ModulePath(pub Vec<Id<ModuleName>>);

impl ModulePath {
    pub fn get(&self, index: usize, tree: &impl TreeView) -> ModuleName {
        *self.0[index].get(tree)
    }
}

#[derive(
    Notate,
    Inspector,
    Debug,
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
#[notate(color = "green")]
pub struct ModuleImport(pub PathKey);

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "green")]
#[into_iterator(owned, ref)]
pub struct FunctorArgs(pub Vec<Id<ModulePath>>);

#[derive(
    Debug, Notate, Inspector, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "green")]
pub struct FunctorApp {
    pub path: Option<Id<ModulePath>>,
    pub func: Id<FunctorName>,
    pub args: Id<FunctorArgs>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
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
#[notate(color = "green")]
pub struct ModuleTypeBind {
    pub vis: Id<Vis>,
    pub name: Id<ModuleTypeName>,
    pub ty: Id<ModuleType>,
}

#[derive(
    Debug,
    Inspector,
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
pub enum ModuleType {
    Qualified(Id<QualifiedModuleType>),
    Concrete(Id<ConcreteModuleType>),
}

impl<'a> Notate<'a> for NodePrinter<'a, ModuleType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            ModuleType::Qualified(q) => self.to(q).notate(arena),
            ModuleType::Concrete(c) => self.to(c).notate(arena),
        }
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
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
#[notate(color = "green")]
pub struct QualifiedModuleType {
    pub path: Option<Id<ModulePath>>,
    pub ty: Id<ModuleTypeName>,
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "green")]
#[into_iterator(owned, ref)]
pub struct ConcreteModuleType(pub Vec<Id<Spec>>);

#[derive(
    Debug,
    EnumAsInner,
    Inspector,
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

impl<'a> Notate<'a> for NodePrinter<'a, Spec> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Spec::Value(v) => self.to(v).notate(arena),
            Spec::Module(m) => self.to(m).notate(arena),
            Spec::Error(e) => self.to(e).notate(arena),
        }
    }
}

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "red")]
pub struct SpecError;

// f : Num -> Num
#[derive(
    Debug,
    Notate,
    Inspector,
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
#[notate(color = "green")]
pub struct ValueSpec {
    pub name: Id<ValueName>,
    pub ty: Id<TypeScheme>,
}

// module M : { ... }
#[derive(
    Debug,
    Notate,
    Inspector,
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
#[notate(color = "green")]
pub struct ModuleSpec {
    pub name: Id<ModuleName>,
    pub ty: Id<ModuleType>,
}
