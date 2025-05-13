#![feature(never_type)]
#![feature(path_add_extension)]
#![feature(box_into_inner)]
#![feature(let_chains)]

//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

pub mod env;
pub mod error;
pub mod substitute;
pub mod typer;
pub mod types;
pub mod unify;

pub mod prelude {
    pub use crate::env::{KindEnv, TypeEnv};
    pub use crate::error::{SemanticError, SemanticErrors};
    pub use crate::substitute::{Substitutable, Substitution};
    pub use crate::typer::{
        Constraint, Constraints, TypeDecorator, TypeInfo, TypeInfoTable, TypePhase, Typer,
    };
    pub use crate::types::*;
    pub use crate::unify::Unifiable;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VisitState {
    Unvisited,
    Visiting,
    Visited,
}
