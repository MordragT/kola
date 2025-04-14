use derive_more::From;
use serde::{Deserialize, Serialize};

use super::{Expr, Name, Type, TypeBind};
use crate::id::NodeId;

/*
Nice to haves:
- be able to pull submodules without evaluating parent modules

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

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Bind {
    Value(NodeId<ValueBind>),
    Type(NodeId<TypeBind>),
    Module(NodeId<ModuleBind>),
    ModuleType(NodeId<ModuleTypeBind>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ValueBind {
    pub name: NodeId<Name>,
    pub ty: Option<NodeId<Type>>,
    pub value: NodeId<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModuleBind {
    pub name: NodeId<Name>,
    pub ty: Option<NodeId<ModuleType>>,
    pub value: NodeId<Module>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModuleTypeBind {
    pub name: NodeId<Name>,
    pub ty: NodeId<ModuleType>,
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ModuleType(pub Vec<NodeId<Spec>>); // TODO functor ?

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Spec {
    Type(NodeId<TypeBind>), // type binds & value bind types
    // OpaqueType(NodeId<OpaqueType>)
    ModuleType(NodeId<ModuleTypeBind>),
}

// modules are nominal
// they should allow hiding (somewhat opaque)
// they should be a first class citizen

// essentially records but records allow parametric polymorphism
// what if modules then also allow parametric polymorphism ?
// so the type of a module is essentially its record representation type ?
// what if modules other than records allow existential types and opaque types ?
// what are the pro's con's of module headers vs inline visibilty modifiers ?
// How could they be written syntactically ?

/*

module <module-name>
    imports [Std.Str, ] # could here parametric polymorphism/existential types be declared
    exports [...]

some_function : Str -> Str
some_function = /str => ...

SomeAlias : { a : Str, b : Str }

SomeOpaqueType := Str

new_opaque : Str -> SomeOpaqueType
new_opaque = \str => (SomeOpaqueType str) # functions must be lower case ? then opaque type constructors does not need extra syntax if no other constructors are implicitley defined colliding with this syntax
*/

/*
module List forall a
    imports []
    exports []

List : { }
*/

/*
module List = (
    length : .. -> Num
    length = ..
)

module List : (length : .. -> Num)

module NonEmptyList : List = (
...
)
*/

/*
- if I implement ML-style modules then I get opaque types
- I need signature, structure and functors.

A module structure has some signature but I'd like to define a signature apart from the module
I'd also want some sort of validation mechanism for modules maybe module's can define test's
which are automatically run?

contract STACK
forall t
exists Stack
: (
  push : Stack -> t -> Stack
  pop : Stack -> t # TODO missing effect here
)

module list
forall t
exists (
    List := { ... }
)
imports (std.array)
exports (List, push, pop, new)
implements (std.stack.STACK)
where Stack : List
= (
    new : List
    new = (List { ... }) # private opaque type constructor for list

    push : List -> t -> List
    push = ...

    pop : List -> t ~ Undefined # use undefined effect here for simplicity I don't have sum types yet
    pop = ...
)


module safe-stack
imports (std.stack.STACK)
with (S : STACK)
forall t
exists (
    SafeStack := S.Stack
)
exports (pop_or_default, SafeStack)
= (
   pop_or_default : SafeStack -> t -> t
   pop_or_default = ...
)
*/

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
