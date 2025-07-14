//! # Module and Functor Abstract Syntax Tree (AST) Nodes
//!
//! This module defines the Abstract Syntax Tree (AST) nodes that compose Kola's module system,
//! including explicit support for SML-style functors. The design within this layer is structured
//! to enable first-class module expressions and integrate functor constructs into the existing
//! module binding mechanisms.
//!
//! A `Functor` definition is implemented as a direct variant of `ModuleExpr`. This design treats
//! functors as first-class entities within the module system, allowing them to be bound to names
//! via `ModuleBind` or used directly as expressions. The `body` of a `Functor` is also a
//! `ModuleExpr`, which inherently supports curried, multi-argument functors through nested
//! `Functor` expressions.
//!
//! `FunctorCall` is similarly defined as a `ModuleExpr`. This represents the application of a
//! functor as an expression, where its evaluation during a later compilation phase yields a
//! concrete module.
//!
//! The `Functor` struct includes a `param_ty` field of type `Id<ModuleType>`. This structural
//! constraint requires all functor definitions to explicitly declare the concrete `ModuleType`
//! (signature) expected for their input module parameter. This mandates a clear interface
//! specification for functor arguments at the AST level. This means that there is no module-level
//! polymorphism. Functor types are implicitly modeled as transformations from one concrete
//! `ModuleType` to another, with their precise semantic representation and inference handled
//! in subsequent type checking stages.
use derive_more::{From, IntoIterator};
use enum_as_inner::EnumAsInner;
use kola_macros::{Inspector, Notate};
use serde::{Deserialize, Serialize};

use kola_print::prelude::*;

use super::{Expr, ModuleName, TypeScheme};
use crate::{
    id::Id,
    node::{EffectName, EffectRowType, FunctorName, ModuleTypeName, TypeName, ValueName},
    print::NodePrinter,
    tree::{TreeBuilder, TreeView},
};

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
    OpaqueType(Id<OpaqueTypeBind>),
    EffectType(Id<EffectTypeBind>),
    Module(Id<ModuleBind>),
    ModuleType(Id<ModuleTypeBind>),
    Functor(Id<FunctorBind>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Bind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Bind::Value(v) => self.to(v).notate(arena),
            Bind::Type(t) => self.to(t).notate(arena),
            Bind::OpaqueType(o) => self.to(o).notate(arena),
            Bind::EffectType(e) => self.to(e).notate(arena),
            Bind::Module(m) => self.to(m).notate(arena),
            Bind::ModuleType(mt) => self.to(mt).notate(arena),
            Bind::Functor(f) => self.to(f).notate(arena),
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
pub struct OpaqueTypeBind {
    // Opaque types are always public,
    // because opaque private types do not make sense
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
pub struct EffectTypeBind {
    pub vis: Id<Vis>,
    pub name: Id<EffectName>,
    pub ty: Id<EffectRowType>,
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
    pub body: Id<Module>,
}

impl FunctorBind {
    pub fn new_in(
        vis: Vis,
        name: FunctorName,
        params: impl IntoIterator<Item = FunctorParam>,
        body: Module,
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
    Module(Id<Module>),
    Import(Id<ModuleImport>),
    Path(Id<ModulePath>),
    FunctorApp(Id<FunctorApp>),
}

impl<'a> Notate<'a> for NodePrinter<'a, ModuleExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            ModuleExpr::Error(id) => self.to(id).notate(arena),
            ModuleExpr::Module(id) => self.to(id).notate(arena),
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
pub struct Module(pub Vec<Id<Bind>>);

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
pub struct ModuleImport(pub Id<ModuleName>);

#[derive(
    Debug, Notate, Inspector, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "green")]
pub struct FunctorApp {
    pub func: Id<FunctorName>,
    pub args: Vec<Id<ModulePath>>,
}

// TODO should I only allow ModuleTypes to be bound or should I change this to a ModuleSigBind ?
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
    OpaqueType(Id<OpaqueTypeSpec>),
    Module(Id<ModuleSpec>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Spec> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Spec::Value(v) => self.to(v).notate(arena),
            Spec::OpaqueType(o) => self.to(o).notate(arena),
            Spec::Module(m) => self.to(m).notate(arena),
        }
    }
}

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

// opaque type T : * -> *
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
pub struct OpaqueTypeSpec {
    pub name: Id<TypeName>,
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
