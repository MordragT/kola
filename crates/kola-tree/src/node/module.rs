use derive_more::{From, IntoIterator};
use serde::{Deserialize, Serialize};

use kola_print::prelude::*;
use kola_utils::as_variant;

use super::{Expr, ModuleName, TypeScheme};
use crate::{
    id::Id,
    node::{TypeName, ValueName},
    print::NodePrinter,
    tree::{TreeBuilder, TreeView},
};

/*
Nice to haves:
- be able to pull submodules without evaluating parent modules
- module's can define test's which are automatically run

module type Stack = {
    opaque type Stack : Type -> Type

    push : forall a . a -> Stack a -> Stack a
    pop : forall a . Stack a -> a ~ Undefined
}

module list : Stack = {
    opaque type Stack = List

    push = ...
    pop = ...
}

module safe-stack = functor (s : Stack) => {
    type SafeStack = s.Stack

    pop_or_default : forall a . SafeStack a -> a -> a
        = fn stack => fn default => handle (s.pop stack) with ...
}
*/

#[derive(
    Debug, From, IntoIterator, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[into_iterator(owned, ref)]
pub struct Module(pub Vec<Id<Bind>>); // TODO maybe should know its parent ?

impl<'a> Notate<'a> for NodePrinter<'a, Module> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "Module".green().display_in(arena);

        let binds = self.to_slice(&self.value.0).gather(arena);

        let single = binds.clone().concat_map(
            |bind| arena.just(' ').then(bind.flatten(arena), arena),
            arena,
        );

        let multi = binds
            .concat_map(|bind| arena.newline().then(bind, arena), arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, From, IntoIterator, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[into_iterator(owned, ref)]
pub struct ModulePath(pub Vec<Id<ModuleName>>);

impl ModulePath {
    pub fn get(&self, index: usize, tree: &impl TreeView) -> ModuleName {
        *self.0[index].get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, ModulePath> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "ModulePath".cyan().display_in(arena);

        let path = self.to_slice(&self.value.0).gather(arena);

        let single = path
            .clone()
            .concat_map(|s| arena.just(' ').then(s, arena), arena)
            .flatten(arena);
        let multi = path
            .concat_map(|s| arena.newline().then(s, arena), arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ModuleImport(pub Id<ModuleName>);

impl<'a> Notate<'a> for NodePrinter<'a, ModuleImport> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "ModuleImport".green().display_in(arena);

        let path = self.to_id(self.value.0).notate(arena);

        let single = arena.just(' ').then(path.clone().flatten(arena), arena);
        let multi = arena.newline().then(path, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// TODO rename Module to ModuleImport and create wrapper ?
#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum ModuleExpr {
    Module(Id<Module>),
    Import(Id<ModuleImport>),
    Path(Id<ModulePath>),
}

impl<'a> Notate<'a> for NodePrinter<'a, ModuleExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            ModuleExpr::Module(id) => self.to(id).notate(arena),
            ModuleExpr::Import(id) => self.to(id).notate(arena),
            ModuleExpr::Path(id) => self.to(id).notate(arena),
        }
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Bind {
    Value(Id<ValueBind>),
    Type(Id<TypeBind>),
    OpaqueType(Id<OpaqueTypeBind>),
    Module(Id<ModuleBind>),
    ModuleType(Id<ModuleTypeBind>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Bind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Bind::Value(v) => self.to(v).notate(arena),
            Bind::Type(t) => self.to(t).notate(arena),
            Bind::OpaqueType(o) => self.to(o).notate(arena),
            Bind::Module(m) => self.to(m).notate(arena),
            Bind::ModuleType(mt) => self.to(mt).notate(arena),
        }
    }
}

impl Bind {
    pub fn value_in(
        vis: Vis,
        name: ValueName,
        ty: Option<TypeScheme>,
        value: Expr,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let bind = ValueBind::new_in(vis, name, ty, value, builder);

        builder.insert(Self::Value(bind))
    }

    pub fn to_value(self) -> Option<Id<ValueBind>> {
        as_variant!(self, Self::Value)
    }

    pub fn to_type(self) -> Option<Id<TypeBind>> {
        as_variant!(self, Self::Type)
    }

    pub fn to_opaque_type(self) -> Option<Id<OpaqueTypeBind>> {
        as_variant!(self, Self::OpaqueType)
    }

    pub fn to_module(self) -> Option<Id<ModuleBind>> {
        as_variant!(self, Self::Module)
    }

    pub fn to_module_type(self) -> Option<Id<ModuleTypeBind>> {
        as_variant!(self, Self::ModuleType)
    }

    pub fn is_value(self) -> bool {
        matches!(self, Self::Value(_))
    }

    pub fn is_type(self) -> bool {
        matches!(self, Self::Type(_))
    }

    pub fn is_opaque_type(self) -> bool {
        matches!(self, Self::OpaqueType(_))
    }

    pub fn is_module(self) -> bool {
        matches!(self, Self::Module(_))
    }

    pub fn is_module_type(self) -> bool {
        matches!(self, Self::ModuleType(_))
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ValueBind {
    pub vis: Id<Vis>,
    pub name: Id<ValueName>,
    pub ty: Option<Id<TypeScheme>>,
    pub value: Id<Expr>,
}

impl<'a> Notate<'a> for NodePrinter<'a, ValueBind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ValueBind {
            vis,
            name,
            ty,
            value,
        } = *self.value;

        let head = "ValueBind".green().display_in(arena);

        let vis = self.to_id(vis).notate(arena);
        let name = self.to_id(name).notate(arena);
        let ty = ty.map(|ty| self.to_id(ty).notate(arena));
        let value = self.to_id(value).notate(arena);

        let single = [
            arena.just(' ').then("vis = ".display_in(arena), arena),
            vis.clone(),
            ", name = ".display_in(arena),
            name.clone(),
            ty.clone()
                .map(|ty| ", ty = ".display_in(arena).then(ty, arena))
                .or_not(arena),
            ", value = ".display_in(arena),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            "vis = ".display_in(arena),
            vis,
            arena.newline(),
            "name = ".display_in(arena),
            name,
            ty.map(|ty| [arena.newline(), "ty = ".display_in(arena), ty].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            "value = ".display_in(arena),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl ValueBind {
    pub fn new_in(
        vis: Vis,
        name: ValueName,
        ty: Option<TypeScheme>,
        value: Expr,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let vis = builder.insert(vis);
        let name = builder.insert(name);
        let ty = ty.map(|ty| builder.insert(ty));
        let value = builder.insert(value);

        builder.insert(Self {
            vis,
            name,
            ty,
            value,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeBind {
    pub name: Id<TypeName>,
    pub ty: Id<TypeScheme>,
}

impl<'a> Notate<'a> for NodePrinter<'a, TypeBind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let TypeBind { name, ty } = self.value;

        let head = "TypeBind".green().display_in(arena);

        let name = self.to_id(*name).notate(arena);
        let ty = self.to_id(*ty).notate(arena);

        let single = [
            arena.just(' ').then("name = ".display_in(arena), arena),
            name.clone().flatten(arena),
            ", ty = ".display_in(arena),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            "name = ".display_in(arena),
            name,
            arena.newline(),
            "ty = ".display_in(arena),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct OpaqueTypeBind {
    pub name: Id<TypeName>,
    pub ty: Id<TypeScheme>,
}

impl<'a> Notate<'a> for NodePrinter<'a, OpaqueTypeBind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let OpaqueTypeBind { name, ty } = self.value;

        let head = "OpaqueTypeBind".green().display_in(arena);

        let name = self.to_id(*name).notate(arena);
        let ty = self.to_id(*ty).notate(arena);

        let single = [
            arena.just(' ').then("name = ".display_in(arena), arena),
            name.clone().flatten(arena),
            ", ty = ".display_in(arena),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            "name = ".display_in(arena),
            name,
            arena.newline(),
            "ty = ".display_in(arena),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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
        let ty = ty.map(|ty| builder.insert(ty));
        let value = builder.insert(value);

        builder.insert(Self {
            vis,
            name,
            ty,
            value,
        })
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, ModuleBind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ModuleBind {
            vis,
            name,
            ty,
            value,
        } = self.value;

        let head = "ModuleBind".green().display_in(arena);

        let vis = self.to_id(*vis).notate(arena);
        let name = self.to_id(*name).notate(arena);
        let ty = ty.as_ref().map(|ty| self.to_id(*ty).notate(arena));
        let value = self.to_id(*value).notate(arena);

        let single = [
            arena.just(' ').then("vis = ".display_in(arena), arena),
            vis.clone(),
            ", name = ".display_in(arena),
            name.clone(),
            ty.clone()
                .map(|ty| ", ty = ".display_in(arena).then(ty, arena))
                .or_not(arena),
            ", value = ".display_in(arena),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            "vis = ".display_in(arena),
            vis,
            arena.newline(),
            "name = ".display_in(arena),
            name,
            ty.map(|ty| [arena.newline(), "ty = ".display_in(arena), ty].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            "value = ".display_in(arena),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModuleTypeBind {
    pub name: Id<ModuleName>,
    pub ty: Id<ModuleType>,
}

impl<'a> Notate<'a> for NodePrinter<'a, ModuleTypeBind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ModuleTypeBind { name, ty } = self.value;

        let head = "ModuleTypeBind".green().display_in(arena);

        let name = self.to_id(*name).notate(arena);
        let ty = self.to_id(*ty).notate(arena);

        let single = [
            arena.just(' ').then("name = ".display_in(arena), arena),
            name.clone().flatten(arena),
            ", ty = ".display_in(arena),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            "name = ".display_in(arena),
            name,
            arena.newline(),
            "ty = ".display_in(arena),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// cannot infact contain another module type bind
// only submodules can be defined
#[derive(
    Debug, From, IntoIterator, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[into_iterator(owned, ref)]
pub struct ModuleType(pub Vec<Id<Spec>>); // TODO functor ?

impl<'a> Notate<'a> for NodePrinter<'a, ModuleType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "ModuleType".green().display_in(arena);

        let specs = self.to_slice(&self.value.0).gather(arena);

        let single = specs.clone().concat_map(
            |spec| arena.just(' ').then(spec.flatten(arena), arena),
            arena,
        );

        let multi = specs
            .concat_map(|spec| arena.newline().then(spec, arena), arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Spec {
    Value(Id<ValueSpec>),
    TypeBind(Id<TypeBind>),
    OpaqueType(Id<OpaqueTypeSpec>),
    Module(Id<ModuleSpec>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Spec> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Spec::Value(v) => self.to(v).notate(arena),
            Spec::TypeBind(t) => self.to(t).notate(arena),
            Spec::OpaqueType(o) => self.to(o).notate(arena),
            Spec::Module(m) => self.to(m).notate(arena),
        }
    }
}

impl Spec {
    pub fn to_value(self) -> Option<Id<ValueSpec>> {
        as_variant!(self, Self::Value)
    }

    pub fn to_type_bind(self) -> Option<Id<TypeBind>> {
        as_variant!(self, Self::TypeBind)
    }

    pub fn to_opaque_type(self) -> Option<Id<OpaqueTypeSpec>> {
        as_variant!(self, Self::OpaqueType)
    }

    pub fn to_module(self) -> Option<Id<ModuleSpec>> {
        as_variant!(self, Self::Module)
    }

    pub fn is_value(self) -> bool {
        matches!(self, Self::Value(_))
    }

    pub fn is_type_bind(self) -> bool {
        matches!(self, Self::TypeBind(_))
    }

    pub fn is_opaque_type(self) -> bool {
        matches!(self, Self::OpaqueType(_))
    }

    pub fn is_module(self) -> bool {
        matches!(self, Self::Module(_))
    }
}

// f : Num -> Num
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ValueSpec {
    pub name: Id<ValueName>,
    pub ty: Id<TypeScheme>,
}

impl<'a> Notate<'a> for NodePrinter<'a, ValueSpec> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ValueSpec { name, ty } = self.value;

        let head = "ValueSpec".green().display_in(arena);

        let name = self.to_id(*name).notate(arena);
        let ty = self.to_id(*ty).notate(arena);

        let single = [
            arena.just(' ').then("name = ".display_in(arena), arena),
            name.clone().flatten(arena),
            ", ty = ".display_in(arena),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            "name = ".display_in(arena),
            name,
            arena.newline(),
            "ty = ".display_in(arena),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// module M : { ... }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModuleSpec {
    pub name: Id<ModuleName>,
    pub ty: Id<ModuleType>,
}

impl<'a> Notate<'a> for NodePrinter<'a, ModuleSpec> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ModuleSpec { name, ty } = self.value;

        let head = "ModuleSpec".green().display_in(arena);

        let name = self.to_id(*name).notate(arena);
        let ty = self.to_id(*ty).notate(arena);

        let single = [
            arena.just(' ').then("name = ".display_in(arena), arena),
            name.clone().flatten(arena),
            ", ty = ".display_in(arena),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            "name = ".display_in(arena),
            name,
            arena.newline(),
            "ty = ".display_in(arena),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// opaque type T : * -> *
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct OpaqueTypeSpec {
    pub name: Id<TypeName>,
    pub kind: Id<OpaqueTypeKind>,
}

impl<'a> Notate<'a> for NodePrinter<'a, OpaqueTypeSpec> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let OpaqueTypeSpec { name, kind } = self.value;

        let head = "OpaqueTypeSpec".green().display_in(arena);

        let name = self.to_id(*name).notate(arena);
        let kind = self.to_id(*kind).notate(arena);

        let single = [
            arena.just(' ').then("name = ".display_in(arena), arena),
            name.clone().flatten(arena),
            ", kind = ".display_in(arena),
            kind.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            "name = ".display_in(arena),
            name,
            arena.newline(),
            "kind = ".display_in(arena),
            kind,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// * -> * = arity of 2
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct OpaqueTypeKind {
    pub arity: usize,
}

impl<'a> Notate<'a> for NodePrinter<'a, OpaqueTypeKind> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "OpaqueTypeKind".green().display_in(arena);

        let arity = format!("arity = {}", self.value.arity).display_in(arena);

        head.then(arena.just(' ').then(arity, arena), arena)
    }
}

/*
------------------------------------------------------
New idea for a more structural module system

# module signature explicit for reuse
module stack : [
    forall t . exists Stack .
    push : Stack -> t -> Stack,
    pop : Stack -> t ~ Undefined,
]

# module type explicitly set to check if compatible
module list : stack[Stack = List] # explicit setting existential types ??

# module structure
module list = forall t . exists List . (
  # module array : ... inferred
  module array = import std.array

  List := {...}

  push : List -> t -> List
  push = ...

  pop : List -> t ~ Undefined
  pop = ...
)

# module functor signature can be inferred
module safe-stack : stack -> [ # is the same as: ... [forall t . exists Stack . push...] -> [...
  forall t . exists SafeStack .
  pop_or_default : SafeStack -> t -> t
]

# module functor
module safe-stack =
    functor (S : stack) => forall t exists SafeStack .
(
  SafeStack := S.Stack

  pop_or_default = \stack => ... => ...
)
-----------------------------------
Subset to implement first:

module list : [ forall t . List : { ... }, push : List -> t -> List, ... ]

module list = (

  module array = import std.array

  List : forall t . { ... }

  push : forall t . List t -> t -> List t
  push = ...
)
------------------------------------
Then implement functors

module safe-stack = functor (S : stack) => (...)

------------------------------------
Some probably bad ideas:

parent modules could define what is exported from a module through
ascription ?

module num : [pi] = import "num.kl" # only exports num ?
*/
mod inspector {
    use std::hash::BuildHasher;

    use super::*;
    use crate::inspector::*;

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<Module>, S> {
        pub fn has_binds(self, count: usize) -> Self {
            let binds_len = self.node.get(self.tree).0.len();
            assert_eq!(
                binds_len, count,
                "Expected {} binds but found {}",
                count, binds_len
            );
            self
        }

        pub fn bind_at(self, index: usize) -> NodeInspector<'t, Id<Bind>, S> {
            let module = self.node.get(self.tree);
            assert!(
                index < module.0.len(),
                "Bind index {} out of bounds (max {})",
                index,
                module.0.len() - 1
            );
            let bind_id = module.0[index];
            NodeInspector::new(bind_id, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ModulePath>, S> {
        pub fn has_segments(self, count: usize) -> Self {
            let segments_len = self.node.get(self.tree).0.len();
            assert_eq!(
                segments_len, count,
                "Expected {} segments but found {}",
                count, segments_len
            );
            self
        }

        pub fn segment_at_is(self, index: usize, expected: &str) -> Self {
            let path = self.node.get(self.tree);
            assert!(
                index < path.0.len(),
                "Segment index {} out of bounds (max {})",
                index,
                path.0.len() - 1
            );
            let segment = path.get(index, self.tree);
            let name = self.interner.get(segment.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected segment '{}' but found '{}'",
                expected, name
            );
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ModuleImport>, S> {
        pub fn as_name(self) -> NodeInspector<'t, Id<ModuleName>, S> {
            let name_id = self.node.get(self.tree).0;

            NodeInspector::new(name_id, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ModuleExpr>, S> {
        pub fn as_import(self) -> Option<NodeInspector<'t, Id<ModuleImport>, S>> {
            match *self.node.get(self.tree) {
                ModuleExpr::Import(id) => Some(NodeInspector::new(id, self.tree, self.interner)),
                _ => None,
            }
        }

        pub fn as_module(self) -> Option<NodeInspector<'t, Id<Module>, S>> {
            match *self.node.get(self.tree) {
                ModuleExpr::Module(id) => Some(NodeInspector::new(id, self.tree, self.interner)),
                _ => None,
            }
        }

        pub fn as_path(self) -> Option<NodeInspector<'t, Id<ModulePath>, S>> {
            match *self.node.get(self.tree) {
                ModuleExpr::Path(id) => Some(NodeInspector::new(id, self.tree, self.interner)),
                _ => None,
            }
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<Bind>, S> {
        pub fn as_module(self) -> Option<NodeInspector<'t, Id<ModuleBind>, S>> {
            let bind = self.node.get(self.tree);
            bind.to_module()
                .map(|module_id| NodeInspector::new(module_id, self.tree, self.interner))
        }

        pub fn as_value(self) -> Option<NodeInspector<'t, Id<ValueBind>, S>> {
            let bind = self.node.get(self.tree);
            bind.to_value()
                .map(|value_id| NodeInspector::new(value_id, self.tree, self.interner))
        }

        pub fn as_type(self) -> Option<NodeInspector<'t, Id<TypeBind>, S>> {
            let bind = self.node.get(self.tree);
            bind.to_type()
                .map(|type_id| NodeInspector::new(type_id, self.tree, self.interner))
        }

        pub fn as_module_type(self) -> Option<NodeInspector<'t, Id<ModuleTypeBind>, S>> {
            let bind = self.node.get(self.tree);
            bind.to_module_type()
                .map(|mt_id| NodeInspector::new(mt_id, self.tree, self.interner))
        }

        pub fn as_opaque_type(self) -> Option<NodeInspector<'t, Id<OpaqueTypeBind>, S>> {
            let bind = self.node.get(self.tree);
            bind.to_opaque_type()
                .map(|ot_id| NodeInspector::new(ot_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ModuleBind>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected module name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn value(self) -> NodeInspector<'t, Id<ModuleExpr>, S> {
            let module_bind = self.node.get(self.tree);
            NodeInspector::new(module_bind.value, self.tree, self.interner)
        }

        pub fn module_type(self) -> Option<NodeInspector<'t, Id<ModuleType>, S>> {
            let module_bind = self.node.get(self.tree);
            module_bind
                .ty
                .map(|ty_id| NodeInspector::new(ty_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<OpaqueTypeBind>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected opaque type name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn type_node(self) -> NodeInspector<'t, Id<TypeScheme>, S> {
            let opaque_type_bind = self.node.get(self.tree);
            NodeInspector::new(opaque_type_bind.ty, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ModuleTypeBind>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected module type name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn type_node(self) -> NodeInspector<'t, Id<ModuleType>, S> {
            let module_type_bind = self.node.get(self.tree);
            NodeInspector::new(module_type_bind.ty, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ModuleType>, S> {
        pub fn has_specs(self, count: usize) -> Self {
            let specs_len = self.node.get(self.tree).0.len();
            assert_eq!(
                specs_len, count,
                "Expected {} specs but found {}",
                count, specs_len
            );
            self
        }

        pub fn spec_at(self, index: usize) -> NodeInspector<'t, Id<Spec>, S> {
            let module_type = self.node.get(self.tree);
            assert!(
                index < module_type.0.len(),
                "Spec index {} out of bounds (max {})",
                index,
                module_type.0.len() - 1
            );
            let spec_id = module_type.0[index];
            NodeInspector::new(spec_id, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<Spec>, S> {
        pub fn as_value(self) -> Option<NodeInspector<'t, Id<ValueSpec>, S>> {
            let spec = self.node.get(self.tree);
            spec.to_value()
                .map(|value_id| NodeInspector::new(value_id, self.tree, self.interner))
        }

        pub fn as_type_bind(self) -> Option<NodeInspector<'t, Id<TypeBind>, S>> {
            let spec = self.node.get(self.tree);
            spec.to_type_bind()
                .map(|type_id| NodeInspector::new(type_id, self.tree, self.interner))
        }

        pub fn as_module(self) -> Option<NodeInspector<'t, Id<ModuleSpec>, S>> {
            let spec = self.node.get(self.tree);
            spec.to_module()
                .map(|module_id| NodeInspector::new(module_id, self.tree, self.interner))
        }

        pub fn as_opaque_type(self) -> Option<NodeInspector<'t, Id<OpaqueTypeSpec>, S>> {
            let spec = self.node.get(self.tree);
            spec.to_opaque_type()
                .map(|opaque_id| NodeInspector::new(opaque_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ValueSpec>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected value spec name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn type_node(self) -> NodeInspector<'t, Id<TypeScheme>, S> {
            let value_spec = self.node.get(self.tree);
            NodeInspector::new(value_spec.ty, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ValueBind>, S> {
        pub fn vis(self) -> NodeInspector<'t, Id<Vis>, S> {
            let value_bind = self.node.get(self.tree);
            NodeInspector::new(value_bind.vis, self.tree, self.interner)
        }

        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected value name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn type_node(self) -> Option<NodeInspector<'t, Id<TypeScheme>, S>> {
            let value_bind = self.node.get(self.tree);
            value_bind
                .ty
                .map(|ty_id| NodeInspector::new(ty_id, self.tree, self.interner))
        }

        pub fn value(self) -> NodeInspector<'t, Id<Expr>, S> {
            let value_bind = self.node.get(self.tree);
            NodeInspector::new(value_bind.value, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<TypeBind>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected type name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn type_scheme(self) -> NodeInspector<'t, Id<TypeScheme>, S> {
            let type_bind = self.node.get(self.tree);
            NodeInspector::new(type_bind.ty, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ModuleSpec>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected module spec name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn module_type(self) -> NodeInspector<'t, Id<ModuleType>, S> {
            let module_spec = self.node.get(self.tree);
            NodeInspector::new(module_spec.ty, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<OpaqueTypeSpec>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected opaque type spec name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn kind(self) -> NodeInspector<'t, Id<OpaqueTypeKind>, S> {
            let opaque_type_spec = self.node.get(self.tree);
            NodeInspector::new(opaque_type_spec.kind, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<OpaqueTypeKind>, S> {
        pub fn has_arity(self, expected: usize) -> Self {
            let arity = self.node.get(self.tree).arity;
            assert_eq!(
                arity, expected,
                "Expected arity {} but found {}",
                expected, arity
            );
            self
        }
    }
}
