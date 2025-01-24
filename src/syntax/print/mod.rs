use bumpalo::{
    collections::{CollectIn, String, Vec},
    Bump,
};
use owo_colors::{OwoColorize, Style};
use std::fmt::{self, Write};

pub use notation::{Arena, Notation};
pub use printable::Printable;
pub use printer::PrintOptions;

mod notation;
mod printable;
mod printer;

pub mod prelude {
    pub use super::notation::{Arena, Notation};
    pub use super::printable::Printable;
    pub use super::printer::PrintOptions;
    pub use super::{ConcatBy, ConcatIn, ConcatMap, DisplayIn, Gather, OrNot};
    pub use bumpalo::Bump;
}

// https://justinpombrio.net/2024/02/23/a-twist-on-Wadlers-printer.html

pub trait OrNot<'a> {
    fn or_not(self, arena: &'a Bump) -> Notation<'a>;
}

// impl<'a, T> OrNot<'a> for Option<&'a T>
// where
//     T: Printable,
// {
//     fn or_not(self, arena: &'a Bump) -> Notation<'a> {
//         match self {
//             Some(t) => t.notate(arena),
//             None => arena.empty(),
//         }
//     }
// }

impl<'a> OrNot<'a> for Option<Notation<'a>> {
    fn or_not(self, arena: &'a Bump) -> Notation<'a> {
        match self {
            Some(n) => n,
            None => arena.empty(),
        }
    }
}

pub trait ConcatIn<'a> {
    /// Combine all notations into one notation,
    fn concat_in(self, arena: &'a Bump) -> Notation<'a>;
}

impl<'a> ConcatIn<'a> for &'a [Notation<'a>] {
    fn concat_in(self, arena: &'a Bump) -> Notation<'a> {
        arena.concat(self)
    }
}

impl<'a> ConcatIn<'a> for Vec<'a, Notation<'a>> {
    fn concat_in(self, arena: &'a Bump) -> Notation<'a> {
        arena.concat(self.into_bump_slice())
    }
}

impl<'a, const N: usize> ConcatIn<'a> for [Notation<'a>; N] {
    fn concat_in(self, arena: &'a Bump) -> Notation<'a> {
        let slice = arena.alloc_slice_clone(&self);
        arena.concat(slice)
    }
}

pub trait ConcatMap<'a> {
    /// Combine all notations into one notation, mapping each element with f.
    fn concat_map<F>(self, f: F, arena: &'a Bump) -> Notation<'a>
    where
        F: FnMut(Notation<'a>) -> Notation<'a>;
}

impl<'a, I> ConcatMap<'a> for I
where
    I: IntoIterator<Item = Notation<'a>>,
{
    fn concat_map<F>(self, f: F, arena: &'a Bump) -> Notation<'a>
    where
        F: FnMut(Notation<'a>) -> Notation<'a>,
    {
        let buf = self.into_iter().map(f).collect_in::<Vec<_>>(arena);
        arena.concat(buf.into_bump_slice())
    }
}

pub trait ConcatBy<'a> {
    /// Combine all notations into one notation, separated by sep.
    fn concat_by(self, sep: Notation<'a>, arena: &'a Bump) -> Notation<'a>;
}

impl<'a, I> ConcatBy<'a> for I
where
    I: IntoIterator<Item = Notation<'a>>,
{
    fn concat_by(self, sep: Notation<'a>, arena: &'a Bump) -> Notation<'a> {
        let mut buf = self
            .into_iter()
            .flat_map(|n| [n, sep.clone()])
            .collect_in::<Vec<_>>(arena);

        if !buf.is_empty() {
            buf.pop();
        }

        arena.concat(buf.into_bump_slice())
    }
}

pub trait Gather<'a, W> {
    fn gather(self, with: &'a W, arena: &'a Bump) -> Vec<'a, Notation<'a>>;
}

impl<'a, I, T, W> Gather<'a, W> for I
where
    T: Printable<W> + 'a,
    I: IntoIterator<Item = &'a T>,
{
    fn gather(self, with: &'a W, arena: &'a Bump) -> Vec<'a, Notation<'a>> {
        self.into_iter()
            .map(|p| p.notate(with, arena))
            .collect_in(arena)
    }
}

pub trait DisplayIn {
    fn display_in<'a>(&self, arena: &'a Bump) -> Notation<'a>
    where
        Self: fmt::Display,
    {
        let mut s = String::new_in(arena);
        s.write_fmt(format_args!("{self}")).unwrap();
        arena.notate(s.into_bump_str())
    }

    fn display_with_in<'a>(&self, style: Style, arena: &'a Bump) -> Notation<'a>
    where
        Self: fmt::Display,
    {
        let mut s = String::new_in(arena);
        s.write_fmt(format_args!("{}", self.style(style))).unwrap();
        arena.notate(s.into_bump_str())
    }
}

impl<T> DisplayIn for T where T: fmt::Display {}

// intersperse == separated_by
// padded
