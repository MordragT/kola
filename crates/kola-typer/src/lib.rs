//! # Constraint-Based Type Inference
//!
//! This crate implements a constraint-based Hindley-Milner type system with:
//! - **Immediate generalization**: Top-level bindings generalized immediately after constraint solving
//! - **Incremental processing**: Each binding fully processed before proceeding to the next
//! - **Monomorphic let-bindings**: Local let-binds are not generalized for simplicity
//! - **Row polymorphism**: Extensible records with proper constraint handling
//! - **Module-aware inference**: Types resolved respecting module boundaries
//!
//! ## Algorithm Overview
//! 1. **Per-binding constraint generation and solving**: Each bind generates and solves constraints immediately
//! 2. **Immediate generalization**: Generalize each binding as soon as it's processed
//! 3. **Incremental type environment**: Generalized types immediately available to subsequent bindings
//! 4. **Cross-module consistency**: Polymorphic types available both within and across modules

#![feature(never_type)]
#![feature(path_add_extension)]
#![feature(box_into_inner)]
#![feature(let_chains)]

//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

pub mod check;
pub mod constraints;
pub mod env;
pub mod error;
pub mod pattern_typer;
pub mod phase;
pub mod print;
pub mod substitute;
pub mod typer;
pub mod types;
pub mod unify;

pub mod prelude {
    pub use crate::constraints::{Constraint, Constraints};
    pub use crate::env::{BoundVars, GlobalTypeEnv, KindEnv, LocalTypeEnv, ModuleTypeEnv};
    pub use crate::error::{TypeError, TypeErrors};
    pub use crate::phase::{TypeAnnotations, TypePhase};
    pub use crate::print::TypeDecorator;
    pub use crate::substitute::{Substitutable, Substitution};
    pub use crate::typer::Typer;
    pub use crate::types::*;
    pub use crate::unify::Unifiable;
}
