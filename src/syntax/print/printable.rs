use bumpalo::{
    collections::{CollectIn, Vec},
    Bump,
};
use ecow::EcoString;

use crate::syntax::{
    token::{Token, Tokens},
    Span, Spanned,
};

use super::{
    notation::{Arena, Notation},
    printer::{PrintOptions, Printer},
    DisplayIn,
};

pub trait Printable<With> {
    fn notate<'a>(&'a self, with: &'a With, arena: &'a Bump) -> Notation<'a>;

    fn render(&self, with: &With, options: PrintOptions) -> std::string::String {
        let arena = Bump::new();

        let notation = self.notate(with, &arena);
        let mut printer = Printer::new(&notation, options, &arena);

        let mut output = std::string::String::new();
        printer.print(&mut output, &arena).unwrap();

        return output;
    }
}

impl<T> Printable<T> for EcoString {
    fn notate<'a>(&'a self, _with: &'a T, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

impl Printable<()> for Tokens<'_> {
    fn notate<'a>(&'a self, with: &'a (), arena: &'a Bump) -> Notation<'a> {
        let items = self
            .iter()
            .flat_map(|t| [t.notate(with, arena), arena.newline()])
            .collect_in::<Vec<_>>(arena);

        arena.concat(items.into_bump_slice())
    }
}

impl<T> Printable<T> for Span {
    fn notate<'a>(&'a self, _with: &T, arena: &'a Bump) -> Notation<'a> {
        self.display_in(arena)
    }
}

impl Printable<()> for Spanned<Token<'_>> {
    fn notate<'a>(&'a self, _with: &'a (), arena: &'a Bump) -> Notation<'a> {
        let (token, span) = self;
        let kind = token.kind();

        format_args!("\"{token}\"\t\t({kind}, {span})").display_in(arena)
    }
}
