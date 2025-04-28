use derive_more::From;
use kola_print::prelude::*;
use kola_utils::as_variant;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Name, Type};
use crate::{
    id::Id,
    print::TreePrinter,
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

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Module(pub Vec<Id<Bind>>); // TODO maybe should know its parent ?

impl Printable<TreePrinter> for Module {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "Module".green().display_in(arena);

        let binds = self.0.gather(with, arena);

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

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModulePath(pub Vec<Id<Name>>);

impl ModulePath {
    pub fn get<'a>(&self, index: usize, tree: &'a impl TreeView) -> &'a Name {
        self.0[index].get(tree)
    }
}

impl Printable<TreePrinter> for ModulePath {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "ModulePath".cyan().display_in(arena);

        let path = self.0.gather(with, arena);

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
pub struct ModuleImport(pub Id<Name>);

impl Printable<TreePrinter> for ModuleImport {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "ModuleImport".green().display_in(arena);

        let path = self.0.notate(with, arena);

        let single = arena.just(' ').then(path.clone().flatten(arena), arena);
        let multi = arena.newline().then(path, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum ModuleExpr {
    Module(Id<Module>),
    Import(Id<ModuleImport>),
    Path(Id<ModulePath>),
}

impl Printable<TreePrinter> for ModuleExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match *self {
            Self::Module(id) => id.get(&with.tree).notate(with, arena),
            Self::Import(id) => id.get(&with.tree).notate(with, arena),
            Self::Path(id) => id.get(&with.tree).notate(with, arena),
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

impl Printable<TreePrinter> for Bind {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Value(v) => v.get(&with.tree).notate(with, arena),
            Self::Type(t) => t.get(&with.tree).notate(with, arena),
            Self::OpaqueType(o) => o.get(&with.tree).notate(with, arena),
            Self::Module(m) => m.get(&with.tree).notate(with, arena),
            Self::ModuleType(mt) => mt.get(&with.tree).notate(with, arena),
        }
    }
}

impl Bind {
    pub fn value_in(
        vis: Vis,
        name: Name,
        ty: Option<Type>,
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

impl Printable<TreePrinter> for Vis {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Export => "Export".purple().display_in(arena),
            Self::None => arena.empty(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ValueBind {
    pub vis: Id<Vis>,
    pub name: Id<Name>,
    pub ty: Option<Id<Type>>,
    pub value: Id<Expr>,
}

impl Printable<TreePrinter> for ValueBind {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            vis,
            name,
            ty,
            value,
        } = self;

        let head = "ValueBind".green().display_in(arena);

        let vis = vis.notate(with, arena);
        let name = name.notate(with, arena);
        let ty = ty.as_ref().map(|ty| ty.notate(with, arena));
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" vis = "),
            vis.clone(),
            arena.notate(" name = "),
            name.clone(),
            ty.clone()
                .map(|ty| arena.notate(", ty = ").then(ty, arena))
                .or_not(arena),
            arena.notate(", value = "),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("vis = "),
            vis,
            arena.newline(),
            arena.notate("name = "),
            name,
            ty.map(|ty| [arena.newline(), arena.notate("ty = "), ty].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("value = "),
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
        name: Name,
        ty: Option<Type>,
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
    pub name: Id<Name>,
    pub ty: Id<Type>,
}

impl Printable<TreePrinter> for TypeBind {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty } = self;

        let head = "TypeBind".green().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", ty = "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("ty = "),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct OpaqueTypeBind {
    pub name: Id<Name>,
    pub ty: Id<Type>,
}

impl Printable<TreePrinter> for OpaqueTypeBind {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty } = self;

        let head = "OpaqueTypeBind".green().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", ty = "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("ty = "),
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
    pub name: Id<Name>,
    pub ty: Option<Id<ModuleType>>,
    pub value: Id<ModuleExpr>,
}

impl Printable<TreePrinter> for ModuleBind {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self {
            vis,
            name,
            ty,
            value,
        } = self;

        let head = "ModuleBind".green().display_in(arena);

        let vis = vis.notate(with, arena);
        let name = name.notate(with, arena);
        let ty = ty.as_ref().map(|ty| ty.notate(with, arena));
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" vis = "),
            vis.clone(),
            arena.notate(" name = "),
            name.clone(),
            ty.clone()
                .map(|ty| arena.notate(", ty = ").then(ty, arena))
                .or_not(arena),
            arena.notate(", value = "),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("vis = "),
            vis,
            arena.newline(),
            arena.notate("name = "),
            name,
            ty.map(|ty| [arena.newline(), arena.notate("ty = "), ty].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModuleTypeBind {
    pub name: Id<Name>,
    pub ty: Id<ModuleType>,
}

impl Printable<TreePrinter> for ModuleTypeBind {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty } = self;

        let head = "ModuleTypeBind".green().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", ty = "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("ty = "),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// cannot infact contain another module type bind
// only submodules can be defined
#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModuleType(pub Vec<Id<Spec>>); // TODO functor ?

impl Printable<TreePrinter> for ModuleType {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "ModuleType".green().display_in(arena);

        let specs = self.0.gather(with, arena);

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

impl Printable<TreePrinter> for Spec {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Value(v) => v.get(&with.tree).notate(with, arena),
            Self::TypeBind(t) => t.get(&with.tree).notate(with, arena),
            Self::OpaqueType(o) => o.get(&with.tree).notate(with, arena),
            Self::Module(m) => m.get(&with.tree).notate(with, arena),
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
    pub name: Id<Name>,
    pub ty: Id<Type>,
}

impl Printable<TreePrinter> for ValueSpec {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty } = self;

        let head = "ValueSpec".green().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", ty = "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("ty = "),
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
    pub name: Id<Name>,
    pub ty: Id<ModuleType>,
}

impl Printable<TreePrinter> for ModuleSpec {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty } = self;

        let head = "ModuleSpec".green().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", ty = "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("ty = "),
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
    pub name: Id<Name>,
    pub kind: Id<OpaqueTypeKind>,
}

impl Printable<TreePrinter> for OpaqueTypeSpec {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, kind } = self;

        let head = "OpaqueTypeSpec".green().display_in(arena);

        let name = name.notate(with, arena);
        let kind = kind.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", kind = "),
            kind.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("kind = "),
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

impl Printable<TreePrinter> for OpaqueTypeKind {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "OpaqueTypeKind".green().display_in(arena);

        let arity = format!("arity = {}", self.arity).display_in(arena);

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
    use super::*;
    use crate::inspector::*;

    impl<'t> NodeInspector<'t, Id<Module>> {
        /// Assert the module has the specified number of binds
        pub fn has_binds(self, count: usize) -> Self {
            let binds_len = self.node.get(self.tree).0.len();
            assert_eq!(
                binds_len, count,
                "Expected {} binds but found {}",
                count, binds_len
            );
            self
        }

        /// Get an inspector for the bind at the given index
        pub fn bind_at(self, index: usize) -> NodeInspector<'t, Id<Bind>> {
            let module = self.node.get(self.tree);
            assert!(
                index < module.0.len(),
                "Bind index {} out of bounds (max {})",
                index,
                module.0.len() - 1
            );
            let bind_id = module.0[index];
            NodeInspector::new(bind_id, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, Id<ModulePath>> {
        /// Assert the path has the specified number of segments
        pub fn has_segments(self, count: usize) -> Self {
            let segments_len = self.node.get(self.tree).0.len();
            assert_eq!(
                segments_len, count,
                "Expected {} segments but found {}",
                count, segments_len
            );
            self
        }

        /// Assert the path segment at the given index has the expected name
        pub fn segment_at_is(self, index: usize, expected: &str) -> Self {
            let path = self.node.get(self.tree);
            assert!(
                index < path.0.len(),
                "Segment index {} out of bounds (max {})",
                index,
                path.0.len() - 1
            );
            let segment = path.get(index, self.tree);
            assert_eq!(
                segment.as_str(),
                expected,
                "Expected segment '{}' but found '{}'",
                expected,
                segment.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<ModuleImport>> {
        pub fn as_name(self) -> NodeInspector<'t, Id<Name>> {
            let name_id = self.node.get(self.tree).0;

            NodeInspector::new(name_id, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, Id<ModuleExpr>> {
        pub fn as_import(self) -> Option<NodeInspector<'t, Id<ModuleImport>>> {
            match *self.node.get(self.tree) {
                ModuleExpr::Import(id) => Some(NodeInspector::new(id, self.tree)),
                _ => None,
            }
        }

        pub fn as_module(self) -> Option<NodeInspector<'t, Id<Module>>> {
            match *self.node.get(self.tree) {
                ModuleExpr::Module(id) => Some(NodeInspector::new(id, self.tree)),
                _ => None,
            }
        }

        pub fn as_path(self) -> Option<NodeInspector<'t, Id<ModulePath>>> {
            match *self.node.get(self.tree) {
                ModuleExpr::Path(id) => Some(NodeInspector::new(id, self.tree)),
                _ => None,
            }
        }
    }

    impl<'t> NodeInspector<'t, Id<Bind>> {
        /// Check if this bind is a module bind and return an inspector for it
        pub fn as_module(self) -> Option<NodeInspector<'t, Id<ModuleBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_module()
                .map(|module_id| NodeInspector::new(module_id, self.tree))
        }

        /// Check if this bind is a value bind and return an inspector for it
        pub fn as_value(self) -> Option<NodeInspector<'t, Id<ValueBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_value()
                .map(|value_id| NodeInspector::new(value_id, self.tree))
        }

        /// Check if this bind is a type bind and return an inspector for it
        pub fn as_type(self) -> Option<NodeInspector<'t, Id<TypeBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_type()
                .map(|type_id| NodeInspector::new(type_id, self.tree))
        }

        /// Check if this bind is a module type bind and return an inspector for it
        pub fn as_module_type(self) -> Option<NodeInspector<'t, Id<ModuleTypeBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_module_type()
                .map(|mt_id| NodeInspector::new(mt_id, self.tree))
        }

        /// Check if this bind is an opaque type bind and return an inspector for it
        pub fn as_opaque_type(self) -> Option<NodeInspector<'t, Id<OpaqueTypeBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_opaque_type()
                .map(|ot_id| NodeInspector::new(ot_id, self.tree))
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<ModuleBind>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<ModuleBind>> {
        /// Assert the module bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "module")
        }

        /// Get an inspector for the module's implementation
        pub fn value(self) -> NodeInspector<'t, Id<ModuleExpr>> {
            let module_bind = self.node.get(self.tree);
            NodeInspector::new(module_bind.value, self.tree)
        }

        /// Get an inspector for the module's interface if it has one
        pub fn module_type(self) -> Option<NodeInspector<'t, Id<ModuleType>>> {
            let module_bind = self.node.get(self.tree);
            module_bind
                .ty
                .map(|ty_id| NodeInspector::new(ty_id, self.tree))
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<OpaqueTypeBind>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<OpaqueTypeBind>> {
        /// Assert the opaque type bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "opaque type")
        }

        /// Get an inspector for the opaque type's implementation
        pub fn type_node(self) -> NodeInspector<'t, Id<Type>> {
            let opaque_type_bind = self.node.get(self.tree);
            NodeInspector::new(opaque_type_bind.ty, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<ModuleTypeBind>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<ModuleTypeBind>> {
        /// Assert the module type bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "module type")
        }

        /// Get an inspector for the module type
        pub fn type_node(self) -> NodeInspector<'t, Id<ModuleType>> {
            let module_type_bind = self.node.get(self.tree);
            NodeInspector::new(module_type_bind.ty, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, Id<ModuleType>> {
        /// Assert the module type has the specified number of specifications
        pub fn has_specs(self, count: usize) -> Self {
            let specs_len = self.node.get(self.tree).0.len();
            assert_eq!(
                specs_len, count,
                "Expected {} specs but found {}",
                count, specs_len
            );
            self
        }

        /// Get an inspector for the spec at the given index
        pub fn spec_at(self, index: usize) -> NodeInspector<'t, Id<Spec>> {
            let module_type = self.node.get(self.tree);
            assert!(
                index < module_type.0.len(),
                "Spec index {} out of bounds (max {})",
                index,
                module_type.0.len() - 1
            );
            let spec_id = module_type.0[index];
            NodeInspector::new(spec_id, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, Id<Spec>> {
        /// Check if this spec is a value spec and return an inspector for it
        pub fn as_value(self) -> Option<NodeInspector<'t, Id<ValueSpec>>> {
            let spec = self.node.get(self.tree);
            spec.to_value()
                .map(|value_id| NodeInspector::new(value_id, self.tree))
        }

        /// Check if this spec is a type bind and return an inspector for it
        pub fn as_type_bind(self) -> Option<NodeInspector<'t, Id<TypeBind>>> {
            let spec = self.node.get(self.tree);
            spec.to_type_bind()
                .map(|type_id| NodeInspector::new(type_id, self.tree))
        }

        /// Check if this spec is a module spec and return an inspector for it
        pub fn as_module(self) -> Option<NodeInspector<'t, Id<ModuleSpec>>> {
            let spec = self.node.get(self.tree);
            spec.to_module()
                .map(|module_id| NodeInspector::new(module_id, self.tree))
        }

        /// Check if this spec is an opaque type spec and return an inspector for it
        pub fn as_opaque_type(self) -> Option<NodeInspector<'t, Id<OpaqueTypeSpec>>> {
            let spec = self.node.get(self.tree);
            spec.to_opaque_type()
                .map(|opaque_id| NodeInspector::new(opaque_id, self.tree))
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<ValueSpec>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<ValueSpec>> {
        /// Assert the value spec has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "value spec")
        }

        pub fn type_node(self) -> NodeInspector<'t, Id<Type>> {
            let value_spec = self.node.get(self.tree);
            NodeInspector::new(value_spec.ty, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<ValueBind>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<ValueBind>> {
        pub fn vis(self) -> NodeInspector<'t, Id<Vis>> {
            let value_bind = self.node.get(self.tree);
            NodeInspector::new(value_bind.vis, self.tree)
        }

        /// Assert the value bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "value")
        }

        /// Get an inspector for the value's type if it has one
        pub fn type_node(self) -> Option<NodeInspector<'t, Id<Type>>> {
            let value_bind = self.node.get(self.tree);
            value_bind
                .ty
                .map(|ty_id| NodeInspector::new(ty_id, self.tree))
        }

        /// Get an inspector for the value's expression
        pub fn value(self) -> NodeInspector<'t, Id<Expr>> {
            let value_bind = self.node.get(self.tree);
            NodeInspector::new(value_bind.value, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<TypeBind>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<TypeBind>> {
        /// Assert the type bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "type")
        }

        /// Get an inspector for the type definition
        pub fn type_node(self) -> NodeInspector<'t, Id<Type>> {
            let type_bind = self.node.get(self.tree);
            NodeInspector::new(type_bind.ty, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<ModuleSpec>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<ModuleSpec>> {
        /// Assert the module spec has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "module spec")
        }

        /// Get an inspector for the module spec's type
        pub fn module_type(self) -> NodeInspector<'t, Id<ModuleType>> {
            let module_spec = self.node.get(self.tree);
            NodeInspector::new(module_spec.ty, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<OpaqueTypeSpec>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<OpaqueTypeSpec>> {
        /// Assert the opaque type spec has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "opaque type spec")
        }

        /// Get an inspector for the opaque type's kind
        pub fn kind(self) -> NodeInspector<'t, Id<OpaqueTypeKind>> {
            let opaque_type_spec = self.node.get(self.tree);
            NodeInspector::new(opaque_type_spec.kind, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, Id<OpaqueTypeKind>> {
        /// Assert the opaque type kind has the specified arity
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
