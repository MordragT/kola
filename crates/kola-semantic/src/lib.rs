#![feature(never_type)]

//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

pub mod env;
pub mod error;
pub mod infer;
pub mod meta;
pub mod substitute;
pub mod types;
pub mod unify;

pub mod prelude {
    pub use crate::env::{KindEnv, TypeEnv};
    pub use crate::error::{SemanticError, SemanticErrors, SemanticReport};
    pub use crate::infer::{Constraint, Constraints, Inferer};
    pub use crate::meta::{TypeDecorator, TypeMetadata};
    pub use crate::substitute::{Substitutable, Substitution};
    pub use crate::types::*;
    pub use crate::unify::Unifiable;
}

use kola_tree::Phase;
use types::MonoType;

#[derive(Clone, Copy, Debug)]
pub struct SemanticPhase;

impl Phase for SemanticPhase {
    type Name = ();
    type Ident = MonoType;
    type Literal = MonoType;
    type List = MonoType;
    type Property = ();
    type Record = MonoType;
    type RecordSelect = MonoType;
    type RecordExtend = MonoType;
    type RecordRestrict = MonoType;
    type RecordUpdate = MonoType;
    type UnaryOp = MonoType;
    type Unary = MonoType;
    type BinaryOp = MonoType;
    type Binary = MonoType;
    type Let = MonoType;
    type PatError = ();
    type Wildcard = MonoType;
    type LiteralPat = MonoType;
    type IdentPat = MonoType;
    type PropertyPat = MonoType;
    type RecordPat = MonoType;
    type Pat = MonoType;
    type Branch = ();
    type Case = MonoType;
    type If = MonoType;
    type Func = MonoType;
    type Call = MonoType;
    type ExprError = ();
    type Expr = MonoType;
}
