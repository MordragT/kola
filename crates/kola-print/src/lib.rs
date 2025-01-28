#![feature(allocator_api)]

pub use notation::{Arena, Notation};
pub use printable::Printable;
pub use printer::PrintOptions;

pub mod combinator;
mod notation;
mod printable;
mod printer;

pub mod prelude {
    pub use super::combinator::{ConcatBy, ConcatIn, ConcatMap, DisplayIn, Gather, OrNot};
    pub use super::notation::{Arena, Notation};
    pub use super::printable::Printable;
    pub use super::printer::PrintOptions;
    pub use bumpalo::{self, Bump};
}

// https://justinpombrio.net/2024/02/23/a-twist-on-Wadlers-printer.html
