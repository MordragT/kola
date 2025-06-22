use std::{fmt, mem};

mod atom;
mod expr;
mod pattern;

pub use atom::{Atom, Func, Tag};
use derive_more::From;
pub use expr::*;
use kola_utils::impl_try_as;
pub use pattern::*;

// TODO Symbol scoping
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(pub u32);

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let mut display = self.0.to_string();
        // display.truncate(3);
        // write!(f, "s{display}")
        write!(f, "s{}", self.0)
    }
}

#[derive(Debug, From, Clone, Copy, PartialEq)]
pub enum Instr {
    Atom(Atom),
    Expr(Expr),
    Field(RecordField),
    Item(ListItem),
    Path(FieldPath),
    PatternMatcher(PatternMatcher),
}

impl Instr {
    pub const BITS: usize = mem::size_of::<Self>();
}

// Ensure that the size of Instr is not too large
const _: () = assert!(mem::size_of::<Instr>() <= 32);

impl<T> From<&T> for Instr
where
    T: Into<Instr> + Copy,
{
    fn from(value: &T) -> Self {
        (*value).into()
    }
}

impl_try_as!(
    Instr,
    Atom(Atom),
    Expr(Expr),
    Field(RecordField),
    Item(ListItem),
    Path(FieldPath),
    PatternMatcher(PatternMatcher)
);
