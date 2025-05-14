#![feature(never_type)]

pub mod error;
pub mod forest;
pub mod module;
pub mod resolver;

pub mod prelude {
    pub use crate::forest::Forest;
    pub use crate::resolver::Resolver;
}
