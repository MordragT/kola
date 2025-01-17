use bumpalo::{
    collections::{String, Vec},
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
    pub use super::{JoinIn, NotateIn, OrNot, TransformIn};
    pub use bumpalo::Bump;
}

// https://justinpombrio.net/2024/02/23/a-twist-on-Wadlers-printer.html

pub trait OrNot {
    fn or_not<'a>(self, arena: &'a Bump) -> Notation<'a>
    where
        Self: 'a;
}

impl OrNot for Option<Notation<'_>> {
    fn or_not<'a>(self, arena: &'a Bump) -> Notation<'a>
    where
        Self: 'a,
    {
        match self {
            Some(n) => n,
            None => arena.empty(),
        }
    }
}

pub trait JoinIn {
    fn join_in<'a>(self, arena: &'a Bump) -> Notation<'a>
    where
        Self: IntoIterator<Item = Notation<'a>> + Sized + 'a,
    {
        arena.join(self)
    }
}

impl<'a, I> JoinIn for I where I: IntoIterator<Item = Notation<'a>> + Sized + 'a {}

pub trait TransformIn {
    fn transform_in<'a, T>(self, arena: &'a Bump) -> &'a [Notation<'a>]
    where
        T: Printable + 'a,
        Self: IntoIterator<Item = &'a T> + Sized + 'a,
    {
        let mut buf = Vec::new_in(arena);
        buf.extend(self.into_iter().map(|item| item.notate(arena)));

        buf.into_bump_slice()
    }
}

impl<'a, T, I> TransformIn for I
where
    T: Printable + 'a,
    I: IntoIterator<Item = &'a T> + Sized + 'a,
{
}

pub trait NotateIn {
    fn notate_in<'a>(&self, arena: &'a Bump) -> Notation<'a>
    where
        Self: fmt::Display,
    {
        let mut s = String::new_in(arena);
        s.write_fmt(format_args!("{self}")).unwrap();
        arena.notate(s.into_bump_str())
    }

    fn notate_with_in<'a>(&self, style: Style, arena: &'a Bump) -> Notation<'a>
    where
        Self: fmt::Display,
    {
        let mut s = String::new_in(arena);
        s.write_fmt(format_args!("{}", self.style(style))).unwrap();
        arena.notate(s.into_bump_str())
    }
}

impl<T> NotateIn for T where T: fmt::Display {}
