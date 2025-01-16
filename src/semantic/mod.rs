//! Algorithm W with addition in Polymoprhic Type System for Extensible Records
//! https://github.com/miguel-nascimento/algorithm-j-rs/
//! https://github.com/nwoeanhinnogaehr/algorithmw-rust/

pub use constrain::*;
// pub use context::*;
pub use infer::*;
pub use scope::*;
pub use substitute::*;
pub use unify::*;

mod constrain;
// mod context;
pub mod error;
mod infer;
mod scope;
mod substitute;
pub mod types;
mod unify;
