use std::fmt::Debug;

pub mod id;
pub mod kind;
pub mod meta;
pub mod node;
pub mod print;
pub mod tree;

pub mod prelude {
    pub use crate::id::NodeId;
    pub use crate::kind::NodeKind;
    pub use crate::meta::{Attached, Meta, MetaContainer, MetaVec, Metadata};
    pub use crate::node::{self, InnerNode, Node};
    pub use crate::print::{Decorator, TreePrinter};
    pub use crate::tree::{Tree, TreeBuilder};
    pub use crate::{Phase, Symbol};
}

pub type Symbol = ecow::EcoString;

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
