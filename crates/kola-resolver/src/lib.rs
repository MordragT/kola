//! # Kola Resolver: Forward Reference Symbol Resolution
//!
//! This crate implements a **forward reference symbol system** for resolving module dependencies,
//! imports, and symbol bindings in the Kola language.

#![feature(never_type)]
#![feature(exhaustive_patterns)]

pub mod constraints;
pub mod def;
pub mod env;
pub mod error;
pub mod name;
pub mod phase;
pub mod print;
pub mod resolve;
pub mod symbol;

pub mod prelude {
    pub use crate::resolve::{ResolveOutput, resolve};
}
