#![feature(never_type)]

pub mod error;
pub mod forest;
pub mod info;
pub mod resolver;
pub mod scope;
pub mod symbol;
pub mod topography;

pub mod prelude {
    pub use crate::forest::Forest;
    pub use crate::resolver::{ResolveOutput, resolve};
    pub use crate::topography::Topography;
}

pub type QualId<T> = (kola_utils::interner::PathKey, kola_tree::id::Id<T>);
