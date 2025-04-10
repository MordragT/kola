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

use kola_tree::{Phase, node::Symbol};

#[derive(Clone, Copy, Debug)]
pub struct SemanticPhase;

impl Phase for SemanticPhase {
    type Name = Symbol;
    type Ident = types::MonoType;
    type Literal = types::MonoType;
    type List = types::MonoType;
    type Property = types::Property;
    type Record = types::MonoType;
    type RecordSelect = types::MonoType;
    type RecordExtend = types::MonoType;
    type RecordRestrict = types::MonoType;
    type RecordUpdate = types::MonoType;
    type UnaryOp = types::MonoType;
    type Unary = types::MonoType;
    type BinaryOp = types::MonoType;
    type Binary = types::MonoType;
    type Let = types::MonoType;
    type PatError = ();
    type Wildcard = types::MonoType;
    type LiteralPat = types::MonoType;
    type IdentPat = types::MonoType;
    type PropertyPat = types::MonoType;
    type RecordPat = types::MonoType;
    type Pat = types::MonoType;
    type Branch = ();
    type Case = types::MonoType;
    type If = types::MonoType;
    type Func = types::MonoType;
    type Call = types::MonoType;
    type ExprError = ();
    type Expr = types::MonoType;
}
