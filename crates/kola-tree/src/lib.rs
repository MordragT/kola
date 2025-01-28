use std::fmt::Debug;

pub use id::NodeId;
pub use kind::NodeKind;
pub use meta::{Attached, Meta, MetaContainer, MetaVec, Metadata};
pub use node::*;
pub use tree::{Tree, TreeBuilder};

mod id;
mod kind;
mod meta;
mod node;
pub mod print;
mod tree;

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
