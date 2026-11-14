//! # Kola Resolver: Forward Reference Symbol Resolution
//!
//! This crate implements a **forward reference symbol system** for resolving module dependencies,
//! imports, and symbol bindings in the Kola language.

pub mod db;
pub mod def;
pub mod discover;
pub mod env;
pub mod error;
pub mod lookup;
pub mod name;
pub mod phase;
pub mod print;
pub mod resolve;
pub mod surface;
pub mod symbol;

mod compose;

pub mod prelude {
    pub use crate::{
        db::{Db, ModuleView},
        resolve::resolve,
    };
}
