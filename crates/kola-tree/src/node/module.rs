use derive_more::{From, IntoIterator};
use enum_as_inner::EnumAsInner;
use kola_macros::{Inspector, Notate};
use serde::{Deserialize, Serialize};

use kola_print::prelude::*;

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
    Debug,
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
#[into_iterator(owned, ref)]
pub struct Module(pub Vec<Id<Bind>>);

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

// pub struct ModulePathIter<'a, T: TreeView> {
//     tree: &'a T,
//     current: Option<ModulePath>,
// }

// impl<'a, T: TreeView> ModulePathIter<'a, T> {
//     pub fn new(tree: &'a T, path: ModulePath) -> Self {
//         Self {
//             tree,
//             current: Some(path),
//         }
//     }
// }

// impl<T: TreeView> Iterator for ModulePathIter<'_, T> {
//     type Item = Id<ModuleName>;

//     fn next(&mut self) -> Option<Self::Item> {
//         match self.current {
//             Some(ModulePath::Next(first, next)) => {
//                 self.current = Some(*first.get(self.tree));
//                 Some(next)
//             }
//             Some(ModulePath::First(name)) => {
//                 self.current = None;
//                 Some(name)
//             }
//             None => None,
//         }
//     }
// }

// #[derive(
//     Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
// )]
// pub enum ModulePath {
//     Next(Id<Self>, Id<ModuleName>),
//     First(Id<ModuleName>),
// }

// impl ModulePath {
//     pub fn new_in(name: impl Into<ModuleName>, tree: &mut impl TreeView) -> Id<Self> {
//         let name = tree.insert(name.into());
//         tree.insert(ModulePath::First(name))
//     }

//     pub fn from_iter<T>(iter: impl IntoIterator<Item = ModuleName>, tree: &mut T) -> Id<Self>
//     where
//         T: TreeView,
//     {
//         let mut iter = iter.into_iter();
//         let first = iter
//             .next()
//             .map(|name| tree.insert(name))
//             .expect("ModulePath must have at least one name");

//         let path = iter.fold(ModulePath::First(first), |path, name| {
//             let next = tree.insert(name);
//             ModulePath::Next(tree.insert(path), next)
//         });

//         tree.insert(path)
//     }

//     pub fn iter_rev<T>(self, tree: &T) -> ModulePathIter<'_, T>
//     where
//         T: TreeView,
//     {
//         ModulePathIter::new(tree, self)
//     }

//     pub fn get_from_back(self, index: usize, tree: &impl TreeView) -> Option<Id<ModuleName>> {
//         self.iter_rev(tree).nth(index)
//     }

//     pub fn last(self, tree: &impl TreeView) -> Option<Id<ModuleName>> {
//         self.iter_rev(tree).next()
//     }

//     pub fn len(self, tree: &impl TreeView) -> usize {
//         self.iter_rev(tree).count()
//     }
// }

// impl<'a> Notate<'a> for NodePrinter<'a, ModulePath> {
//     fn notate(&self, arena: &'a Bump) -> Notation<'a> {
//         match *self.value {
//             ModulePath::Next(first, next) => {
//                 let first = self.to(first).notate(arena);
//                 let next = self.to(next).notate(arena);

//                 let single = [first.clone(), arena.just(' '), next.clone()]
//                     .concat_in(arena)
//                     .flatten(arena);
//                 let multi = [first, arena.newline(), next]
//                     .concat_in(arena)
//                     .indent(arena);

//                 single.or(multi, arena)
//             }
//             ModulePath::First(name) => {
//                 let head = "ModulePath".cyan().display_in(arena);
//                 let name = self.to(name).notate(arena);

//                 [head, arena.just(' '), name]
//                     .concat_in(arena)
//                     .flatten(arena)
//             }
//         }
//     }
// }

#[derive(
    Debug,
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
            .concat_by(arena.just(' '), arena)
            .flatten(arena);
        let multi = path.concat_by(arena.newline(), arena).indent(arena);

        head.then(single.or(multi, arena), arena)
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
    pub ty: Option<Id<TypeScheme>>,
    pub value: Id<Expr>,
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
    pub name: Id<TypeName>,
    pub ty: Id<TypeScheme>,
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
    pub name: Id<TypeName>,
    pub ty: Id<TypeScheme>,
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
    pub name: Id<ModuleName>,
    pub ty: Id<ModuleType>,
}

// cannot infact contain another module type bind
// only submodules can be defined
#[derive(
    Debug,
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
    pub kind: Id<OpaqueTypeKind>,
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
