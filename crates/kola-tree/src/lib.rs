pub mod handle;
pub mod id;
pub mod kind;
pub mod meta;
pub mod node;
pub mod print;
pub mod tree;
pub mod visit;

pub mod prelude {
    pub use crate::Phase;
    pub use crate::handle::Handler;
    pub use crate::id::NodeId;
    pub use crate::kind::NodeKind;
    pub use crate::meta::{Meta, MetaCast, MetaContainer, MetaVec, Metadata};
    pub use crate::node::{self, Node};
    pub use crate::print::{Decorator, TreePrinter};
    pub use crate::tree::{NodeContainer, Tree, TreeBuilder};
    pub use crate::visit::{Event, EventStack, Visitor};
}

use std::fmt::Debug;

pub trait Phase: 'static + Debug + Copy {
    type Name: Debug + Clone;
    type Ident: Debug + Clone;
    type Literal: Debug + Clone;
    type List: Debug + Clone;
    type Property: Debug + Clone;
    type Record: Debug + Clone;
    type RecordSelect: Debug + Clone;
    type RecordExtend: Debug + Clone;
    type RecordRestrict: Debug + Clone;
    type RecordUpdate: Debug + Clone;
    type UnaryOp: Debug + Clone;
    type Unary: Debug + Clone;
    type BinaryOp: Debug + Clone;
    type Binary: Debug + Clone;
    type Let: Debug + Clone;
    type PatError: Debug + Clone;
    type Wildcard: Debug + Clone;
    type LiteralPat: Debug + Clone;
    type IdentPat: Debug + Clone;
    type PropertyPat: Debug + Clone;
    type RecordPat: Debug + Clone;
    type Pat: Debug + Clone;
    type Branch: Debug + Clone;
    type Case: Debug + Clone;
    type If: Debug + Clone;
    type Func: Debug + Clone;
    type Call: Debug + Clone;
    type ExprError: Debug + Clone;
    type Expr: Debug + Clone;
}

pub trait UniformPhase: 'static + Debug + Copy {
    type Meta: Debug + Clone;
}

impl<T: UniformPhase> Phase for T {
    type Name = T::Meta;
    type Ident = T::Meta;
    type Literal = T::Meta;
    type List = T::Meta;
    type Property = T::Meta;
    type Record = T::Meta;
    type RecordSelect = T::Meta;
    type RecordExtend = T::Meta;
    type RecordRestrict = T::Meta;
    type RecordUpdate = T::Meta;
    type UnaryOp = T::Meta;
    type Unary = T::Meta;
    type BinaryOp = T::Meta;
    type Binary = T::Meta;
    type Let = T::Meta;
    type PatError = T::Meta;
    type Wildcard = T::Meta;
    type LiteralPat = T::Meta;
    type IdentPat = T::Meta;
    type PropertyPat = T::Meta;
    type RecordPat = T::Meta;
    type Pat = T::Meta;
    type Branch = T::Meta;
    type Case = T::Meta;
    type If = T::Meta;
    type Func = T::Meta;
    type Call = T::Meta;
    type ExprError = T::Meta;
    type Expr = T::Meta;
}
