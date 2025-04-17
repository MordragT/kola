#![feature(allocator_api)]

pub use notation::{Arena, Notation};
pub use printable::Printable;
pub use printer::PrintOptions;

pub mod combinator;
mod notation;
mod printable;
mod printer;

pub mod prelude {
    pub use crate::combinator::{ConcatBy, ConcatIn, ConcatMap, DisplayIn, Gather, OrNot};
    pub use crate::layout;
    pub use crate::notation::{Arena, Notation};
    pub use crate::printable::Printable;
    pub use crate::printer::PrintOptions;
    pub use bumpalo::{self, Bump};
}

// https://justinpombrio.net/2024/02/23/a-twist-on-Wadlers-printer.html

/// Creates a layout choice between single-line and multi-line formatting
/// for a series of notations.
///
/// Usage:
/// layout!(head,
///     field1, field2, field3, ...
/// )
#[macro_export]
macro_rules! layout {
    ($arena:ident, $head:expr, $($field:expr),+ $(,)?) => {{
        // Single line version with spaces
        let single_line = [
            $arena.just(' '),
            $($field.clone(), $arena.just(' '),)+
        ].concat_in(arena);

        // Multi-line version with newlines and indentation
        let multi_line = [
            $($arena.newline().then($field, $arena),)+
        ].concat_in($arena).indent($arena);

        // Let the choice be made at rendering time
        $head.then(single_line.or(multi_line, $arena), $arena)
    }};
}
