use derive_more::From;
use kola_print::prelude::*;
use kola_utils::as_variant;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Expr, Name, Type};
use crate::{
    id::NodeId,
    print::TreePrinter,
    tree::{NodeContainer, TreeBuilder},
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
pub struct Module(pub Vec<NodeId<Bind>>); // TODO maybe should know its parent ?

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

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Bind {
    Value(NodeId<ValueBind>),
    Type(NodeId<TypeBind>),
    OpaqueType(NodeId<OpaqueTypeBind>),
    Module(NodeId<ModuleBind>),
    ModuleType(NodeId<ModuleTypeBind>),
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
        name: Name,
        ty: Option<Type>,
        value: Expr,
        builder: &mut TreeBuilder,
    ) -> NodeId<Self> {
        let bind = ValueBind::new_in(name, ty, value, builder);

        builder.insert(Self::Value(bind))
    }

    pub fn to_value(self) -> Option<NodeId<ValueBind>> {
        as_variant!(self, Self::Value)
    }

    pub fn to_type(self) -> Option<NodeId<TypeBind>> {
        as_variant!(self, Self::Type)
    }

    pub fn to_opaque_type(self) -> Option<NodeId<OpaqueTypeBind>> {
        as_variant!(self, Self::OpaqueType)
    }

    pub fn to_module(self) -> Option<NodeId<ModuleBind>> {
        as_variant!(self, Self::Module)
    }

    pub fn to_module_type(self) -> Option<NodeId<ModuleTypeBind>> {
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
pub struct ValueBind {
    pub name: NodeId<Name>,
    pub ty: Option<NodeId<Type>>,
    pub value: NodeId<Expr>,
}

impl Printable<TreePrinter> for ValueBind {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty, value } = self;

        let head = "ValueBind".green().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.as_ref().map(|ty| ty.notate(with, arena)).or_not(arena);
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", ty = "),
            ty.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("ty = "),
            ty,
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
        name: Name,
        ty: Option<Type>,
        value: Expr,
        builder: &mut TreeBuilder,
    ) -> NodeId<Self> {
        let name = builder.insert(name);
        let ty = ty.map(|ty| builder.insert(ty));
        let value = builder.insert(value);

        builder.insert(Self { name, ty, value })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeBind {
    pub name: NodeId<Name>,
    pub ty: NodeId<Type>,
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
    pub name: NodeId<Name>,
    pub ty: NodeId<Type>,
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
    pub name: NodeId<Name>,
    pub ty: Option<NodeId<ModuleType>>,
    pub value: NodeId<Module>,
}

impl Printable<TreePrinter> for ModuleBind {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty, value } = self;

        let head = "ModuleBind".green().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.as_ref().map(|ty| ty.notate(with, arena)).or_not(arena);
        let value = value.notate(with, arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", ty = "),
            ty.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("ty = "),
            ty,
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
    pub name: NodeId<Name>,
    pub ty: NodeId<ModuleType>,
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
pub struct ModuleType(pub Vec<NodeId<Spec>>); // TODO functor ?

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
    Value(NodeId<ValueSpec>),
    TypeBind(NodeId<TypeBind>),
    OpaqueType(NodeId<OpaqueTypeSpec>),
    Module(NodeId<ModuleSpec>),
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
    pub fn to_value(self) -> Option<NodeId<ValueSpec>> {
        as_variant!(self, Self::Value)
    }

    pub fn to_type_bind(self) -> Option<NodeId<TypeBind>> {
        as_variant!(self, Self::TypeBind)
    }

    pub fn to_opaque_type(self) -> Option<NodeId<OpaqueTypeSpec>> {
        as_variant!(self, Self::OpaqueType)
    }

    pub fn to_module(self) -> Option<NodeId<ModuleSpec>> {
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
    pub name: NodeId<Name>,
    pub ty: NodeId<Type>,
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
    pub name: NodeId<Name>,
    pub ty: NodeId<ModuleType>,
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
    pub name: NodeId<Name>,
    pub kind: NodeId<OpaqueTypeKind>,
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

#[cfg(test)]
mod inspector {
    use super::*;
    use crate::inspector::*;

    impl<'t> NodeInspector<'t, NodeId<Module>> {
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
        pub fn bind_at(self, index: usize) -> NodeInspector<'t, NodeId<Bind>> {
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

    impl<'t> NodeInspector<'t, NodeId<Bind>> {
        /// Check if this bind is a module bind and return an inspector for it
        pub fn as_module(self) -> Option<NodeInspector<'t, NodeId<ModuleBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_module()
                .map(|module_id| NodeInspector::new(module_id, self.tree))
        }

        /// Check if this bind is a value bind and return an inspector for it
        pub fn as_value(self) -> Option<NodeInspector<'t, NodeId<ValueBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_value()
                .map(|value_id| NodeInspector::new(value_id, self.tree))
        }

        /// Check if this bind is a type bind and return an inspector for it
        pub fn as_type(self) -> Option<NodeInspector<'t, NodeId<TypeBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_type()
                .map(|type_id| NodeInspector::new(type_id, self.tree))
        }

        /// Check if this bind is a module type bind and return an inspector for it
        pub fn as_module_type(self) -> Option<NodeInspector<'t, NodeId<ModuleTypeBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_module_type()
                .map(|mt_id| NodeInspector::new(mt_id, self.tree))
        }

        /// Check if this bind is an opaque type bind and return an inspector for it
        pub fn as_opaque_type(self) -> Option<NodeInspector<'t, NodeId<OpaqueTypeBind>>> {
            let bind = self.node.get(self.tree);
            bind.to_opaque_type()
                .map(|ot_id| NodeInspector::new(ot_id, self.tree))
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<ModuleBind>> {
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

    impl<'t> NodeInspector<'t, NodeId<ModuleBind>> {
        /// Assert the module bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "module")
        }

        /// Get an inspector for the module's implementation
        pub fn value(self) -> NodeInspector<'t, NodeId<Module>> {
            let module_bind = self.node.get(self.tree);
            NodeInspector::new(module_bind.value, self.tree)
        }

        /// Get an inspector for the module's interface if it has one
        pub fn module_type(self) -> Option<NodeInspector<'t, NodeId<ModuleType>>> {
            let module_bind = self.node.get(self.tree);
            module_bind
                .ty
                .map(|ty_id| NodeInspector::new(ty_id, self.tree))
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<OpaqueTypeBind>> {
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

    impl<'t> NodeInspector<'t, NodeId<OpaqueTypeBind>> {
        /// Assert the opaque type bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "opaque type")
        }

        /// Get an inspector for the opaque type's implementation
        pub fn type_node(self) -> NodeInspector<'t, NodeId<Type>> {
            let opaque_type_bind = self.node.get(self.tree);
            NodeInspector::new(opaque_type_bind.ty, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<ModuleTypeBind>> {
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

    impl<'t> NodeInspector<'t, NodeId<ModuleTypeBind>> {
        /// Assert the module type bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "module type")
        }

        /// Get an inspector for the module type
        pub fn type_node(self) -> NodeInspector<'t, NodeId<ModuleType>> {
            let module_type_bind = self.node.get(self.tree);
            NodeInspector::new(module_type_bind.ty, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<ModuleType>> {
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
        pub fn spec_at(self, index: usize) -> NodeInspector<'t, NodeId<Spec>> {
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

    impl<'t> NodeInspector<'t, NodeId<Spec>> {
        /// Check if this spec is a value spec and return an inspector for it
        pub fn as_value(self) -> Option<NodeInspector<'t, NodeId<ValueSpec>>> {
            let spec = self.node.get(self.tree);
            spec.to_value()
                .map(|value_id| NodeInspector::new(value_id, self.tree))
        }

        /// Check if this spec is a type bind and return an inspector for it
        pub fn as_type_bind(self) -> Option<NodeInspector<'t, NodeId<TypeBind>>> {
            let spec = self.node.get(self.tree);
            spec.to_type_bind()
                .map(|type_id| NodeInspector::new(type_id, self.tree))
        }

        /// Check if this spec is a module spec and return an inspector for it
        pub fn as_module(self) -> Option<NodeInspector<'t, NodeId<ModuleSpec>>> {
            let spec = self.node.get(self.tree);
            spec.to_module()
                .map(|module_id| NodeInspector::new(module_id, self.tree))
        }

        /// Check if this spec is an opaque type spec and return an inspector for it
        pub fn as_opaque_type(self) -> Option<NodeInspector<'t, NodeId<OpaqueTypeSpec>>> {
            let spec = self.node.get(self.tree);
            spec.to_opaque_type()
                .map(|opaque_id| NodeInspector::new(opaque_id, self.tree))
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<ValueSpec>> {
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

    impl<'t> NodeInspector<'t, NodeId<ValueSpec>> {
        /// Assert the value spec has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "value spec")
        }

        pub fn type_node(self) -> NodeInspector<'t, NodeId<Type>> {
            let value_spec = self.node.get(self.tree);
            NodeInspector::new(value_spec.ty, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<ValueBind>> {
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

    impl<'t> NodeInspector<'t, NodeId<ValueBind>> {
        /// Assert the value bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "value")
        }

        /// Get an inspector for the value's type if it has one
        pub fn type_node(self) -> Option<NodeInspector<'t, NodeId<Type>>> {
            let value_bind = self.node.get(self.tree);
            value_bind
                .ty
                .map(|ty_id| NodeInspector::new(ty_id, self.tree))
        }

        /// Get an inspector for the value's expression
        pub fn value(self) -> NodeInspector<'t, NodeId<Expr>> {
            let value_bind = self.node.get(self.tree);
            NodeInspector::new(value_bind.value, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<TypeBind>> {
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

    impl<'t> NodeInspector<'t, NodeId<TypeBind>> {
        /// Assert the type bind has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "type")
        }

        /// Get an inspector for the type definition
        pub fn type_node(self) -> NodeInspector<'t, NodeId<Type>> {
            let type_bind = self.node.get(self.tree);
            NodeInspector::new(type_bind.ty, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<ModuleSpec>> {
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

    impl<'t> NodeInspector<'t, NodeId<ModuleSpec>> {
        /// Assert the module spec has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "module spec")
        }

        /// Get an inspector for the module spec's type
        pub fn module_type(self) -> NodeInspector<'t, NodeId<ModuleType>> {
            let module_spec = self.node.get(self.tree);
            NodeInspector::new(module_spec.ty, self.tree)
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<OpaqueTypeSpec>> {
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

    impl<'t> NodeInspector<'t, NodeId<OpaqueTypeSpec>> {
        /// Assert the opaque type spec has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            self.assert_name(expected, "opaque type spec")
        }

        /// Get an inspector for the opaque type's kind
        pub fn kind(self) -> NodeInspector<'t, NodeId<OpaqueTypeKind>> {
            let opaque_type_spec = self.node.get(self.tree);
            NodeInspector::new(opaque_type_spec.kind, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<OpaqueTypeKind>> {
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
