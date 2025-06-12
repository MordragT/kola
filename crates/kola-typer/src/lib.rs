#![feature(never_type)]
#![feature(path_add_extension)]
#![feature(box_into_inner)]
#![feature(let_chains)]

//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

pub mod check;
pub mod env;
pub mod error;
pub mod phase;
pub mod print;
pub mod scope;
pub mod substitute;
pub mod typer;
pub mod types;
pub mod unify;

pub mod prelude {
    pub use crate::env::{KindEnv, TypeEnv};
    pub use crate::error::{TypeError, TypeErrors};
    pub use crate::phase::{TypeAnnotations, TypePhase};
    pub use crate::print::TypeDecorator;
    pub use crate::scope::{BoundVars, TypeScope};
    pub use crate::substitute::{Substitutable, Substitution};
    pub use crate::typer::{Constraint, Constraints, Typer};
    pub use crate::types::*;
    pub use crate::unify::Unifiable;
}
