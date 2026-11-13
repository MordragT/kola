use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_interner::PathKey;
use kola_print::prelude::*;
use kola_relations::TryCast;
use serde::{Deserialize, Serialize};

use super::{Expr, ModuleName, ModuleTypeName, TypeName, TypeScheme, ValueName};
use crate::prelude::*;
use crate::print::TreePrinter;

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "red")]
pub struct BindError;

impl BindError {
    pub fn new_in<M>(meta: M, builder: &mut Tree<M>) -> Id<Self>
    where
        M: Clone + 'static,
    {
        builder.alloc_located((Self, meta.clone()))
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
    TryCast,
)]
pub enum Bind {
    Value(Id<ValueBind>),
    Type(Id<TypeBind>),
    Module(Id<ModuleBind>),
    ModuleType(Id<ModuleTypeBind>),
    Error(Id<BindError>),
}

impl<'a> Notate<'a, Bind> for TreePrinter<'a> {
    fn notate(self, value: &Bind, arena: &'a Bump) -> Notation<'a> {
        match value {
            Bind::Value(v) => self.notate(v, arena),
            Bind::Type(t) => self.notate(t, arena),
            Bind::Module(m) => self.notate(m, arena),
            Bind::ModuleType(mt) => self.notate(mt, arena),
            Bind::Error(e) => self.notate(e, arena),
        }
    }
}

impl Bind {
    pub fn value_in<M>(
        vis: Vis,
        name: ValueName,
        ty_scheme: Option<TypeScheme>,
        value: Option<Expr>,
        meta: M,
        builder: &mut Tree<M>,
    ) -> Id<Self>
    where
        M: Clone + 'static,
    {
        let bind = ValueBind::new_in(vis, name, ty_scheme, value, meta.clone(), builder);

        builder.alloc_located((Self::Value(bind), meta.clone()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Vis {
    Public,
    Private,
}

impl<'a> Notate<'a, Vis> for TreePrinter<'a> {
    fn notate(self, value: &Vis, arena: &'a Bump) -> Notation<'a> {
        match value {
            Vis::Public => arena.empty(),
            Vis::Private => "Private".purple().display_in(arena),
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
    pub value: Option<Id<Expr>>,
}

impl ValueBind {
    pub fn new_in<M>(
        vis: Vis,
        name: ValueName,
        ty_scheme: Option<TypeScheme>,
        value: Option<Expr>,
        meta: M,
        builder: &mut Tree<M>,
    ) -> Id<Self>
    where
        M: Clone + 'static,
    {
        let vis = builder.alloc_located((vis, meta.clone()));
        let name = builder.alloc_located((name, meta.clone()));
        let ty_scheme = ty_scheme.map(|ty| builder.alloc_located((ty, meta.clone())));
        let value = value.map(|value| builder.alloc_located((value, meta.clone())));

        builder.alloc_located((Self {
            vis,
            name,
            ty_scheme,
            value,
        }, meta.clone()))
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
    pub value: Option<Id<ModuleExpr>>,
}

impl ModuleBind {
    pub fn new_in<M>(
        vis: Vis,
        name: ModuleName,
        ty: Option<ModuleType>,
        value: Option<ModuleExpr>,
        meta: M,
        builder: &mut Tree<M>,
    ) -> Id<Self>
    where
        M: Clone + 'static,
    {
        let vis = builder.alloc_located((vis, meta.clone()));
        let name = builder.alloc_located((name, meta.clone()));
        let ty = ty.map(|sig| builder.alloc_located((sig, meta.clone())));
        let value = value.map(|value| builder.alloc_located((value, meta.clone())));

        builder.alloc_located((Self {
            vis,
            name,
            ty,
            value,
        }, meta.clone()))
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ModuleRef(pub ModuleKey);

impl<'a> Notate<'a, ModuleRef> for TreePrinter<'a> {
    fn notate(self, value: &ModuleRef, arena: &'a Bump) -> Notation<'a> {
        value.0.to_string().display_in(arena)
    }
}

/// A module expression.
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
    TryCast,
)]
pub enum ModuleExpr {
    Error(Id<ModuleError>),
    Body(Id<ModuleRef>),
    Import(Id<ModuleImport>),
    Path(Id<ModulePath>),
    Merge(Id<ModuleMergeExpr>),
}

impl<'a> Notate<'a, ModuleExpr> for TreePrinter<'a> {
    fn notate(self, value: &ModuleExpr, arena: &'a Bump) -> Notation<'a> {
        match value {
            ModuleExpr::Error(id) => self.notate(id, arena),
            ModuleExpr::Body(id) => self.notate(id, arena),
            ModuleExpr::Import(id) => self.notate(id, arena),
            ModuleExpr::Path(id) => self.notate(id, arena),
            ModuleExpr::Merge(id) => self.notate(id, arena),
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

/// Module composition operator.
#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub enum ModuleOp {
    With,
    Over,
    Using,
}

/// `lhs <op> rhs`
#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "green")]
pub struct ModuleMergeExpr {
    pub lhs: Id<ModuleExpr>,
    pub rhs: Id<ModuleExpr>,
    pub op: ModuleOp,
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
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, TryCast,
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
    TryCast,
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
