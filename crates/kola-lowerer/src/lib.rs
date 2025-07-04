#![feature(never_type)]
#![feature(exhaustive_patterns)]

// https://github.com/AntonPing/norem-draft-1/blob/e5a5d677b17cf966f9a19d6a88c094980cc6f6d4/src/backend/normalize.rs

pub mod module;
pub mod normalizer;
pub mod symbol;

pub mod prelude {
    pub use crate::module::{LoweredModule, lower, lower_module};
}
