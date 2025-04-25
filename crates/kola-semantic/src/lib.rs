#![feature(never_type)]
#![feature(path_add_extension)]

//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

pub mod env;
pub mod error;
pub mod infer;
pub mod module;
pub mod substitute;
pub mod types;
pub mod unify;
pub mod world;

pub mod prelude {
    pub use crate::env::{KindEnv, TypeEnv};
    pub use crate::error::{SemanticError, SemanticErrors, SemanticReport};
    pub use crate::infer::{
        Constraint, Constraints, InferDecorator, InferMetadata, InferPhase, Inferer,
    };
    pub use crate::substitute::{Substitutable, Substitution};
    pub use crate::types::*;
    pub use crate::unify::Unifiable;
    pub use crate::world::{DiscoverOptions, DiscoverVerboseOptions, World};
}
