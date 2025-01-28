#![feature(never_type)]

//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

pub use env::*;
pub use infer::*;
pub use substitute::*;
pub use unify::*;

pub mod error;
pub mod types;

mod env;
mod infer;
mod substitute;
mod unify;

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
    type ExprError = MonoType;
    type Expr = MonoType;
}
