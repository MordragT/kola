#![feature(never_type)]
#![feature(path_add_extension)]

//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

pub mod elaborate;
pub mod env;
pub mod error;
pub mod infer;
pub mod meta;
pub mod module;
pub mod substitute;
pub mod types;
pub mod unify;
pub mod world;

pub mod prelude {
    pub use crate::env::{KindEnv, TypeEnv};
    pub use crate::error::{SemanticError, SemanticErrors, SemanticReport};
    pub use crate::infer::{Constraint, Constraints, Inferer};
    pub use crate::meta::{TypeDecorator, TypeMetadata};
    pub use crate::substitute::{Substitutable, Substitution};
    pub use crate::types::*;
    pub use crate::unify::Unifiable;
}

use kola_tree::{meta::Phase, node::Symbol};

#[derive(Clone, Copy, Debug)]
pub struct SemanticPhase;

impl Phase for SemanticPhase {
    type Name = Symbol;
    type AnyPat = types::MonoType;
    type LiteralPat = types::MonoType;
    type IdentPat = types::MonoType;
    type RecordFieldPat = types::MonoType;
    type RecordPat = types::MonoType;
    type VariantCasePat = types::MonoType;
    type VariantPat = types::MonoType;
    type PatError = ();
    type Pat = types::MonoType;
    type LiteralExpr = types::MonoType;
    type PathExpr = types::MonoType;
    type ListExpr = types::MonoType;
    type RecordField = types::Property;
    type RecordExpr = types::MonoType;
    type RecordFieldPath = types::MonoType;
    type RecordExtendExpr = types::MonoType;
    type RecordRestrictExpr = types::MonoType;
    type RecordUpdateOp = types::MonoType;
    type RecordUpdateExpr = types::MonoType;
    type UnaryOp = types::MonoType;
    type UnaryExpr = types::MonoType;
    type BinaryOp = types::MonoType;
    type BinaryExpr = types::MonoType;
    type LetExpr = types::MonoType;
    type CaseBranch = ();
    type CaseExpr = types::MonoType;
    type IfExpr = types::MonoType;
    type LambdaExpr = types::MonoType;
    type CallExpr = types::MonoType;
    type ExprError = ();
    type Expr = types::MonoType;
    type TypePath = types::MonoType;
    type TypeVar = types::MonoType;
    type RecordFieldType = types::MonoType;
    type RecordType = types::MonoType;
    type VariantCaseType = types::MonoType;
    type VariantType = types::MonoType;
    type FuncType = types::MonoType;
    type TypeApplication = types::MonoType;
    type TypeExpr = types::MonoType;
    type TypeError = ();
    type Type = types::MonoType;
    type ValueBind = types::MonoType;
    type TypeBind = types::MonoType;
    type OpaqueTypeBind = types::MonoType;
    type ModuleBind = types::MonoType;
    type ModuleTypeBind = types::MonoType;
    type Bind = types::MonoType;
    type Module = types::MonoType;
    type ValueSpec = types::MonoType;
    type OpaqueTypeKind = types::MonoType;
    type OpaqueTypeSpec = types::MonoType;
    type ModuleSpec = types::MonoType;
    type Spec = types::MonoType;
    type ModuleType = types::MonoType;
}
