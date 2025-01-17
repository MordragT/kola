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
    NotateIn,
};

pub trait Printable {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a>;

    fn render(&self, options: PrintOptions) -> std::string::String {
        let arena = Bump::new();

        let notation = self.notate(&arena);
        let mut printer = Printer::new(&notation, 100, &arena);

        let mut output = std::string::String::new();
        printer.print(options, &mut output, &arena).unwrap();

        return output;
    }
}

impl Printable for EcoString {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        self.notate_in(arena)
    }
}

impl Printable for Tokens<'_> {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let items = self
            .iter()
            .flat_map(|t| [t.notate(arena), arena.break_line()])
            .collect_in::<Vec<_>>(arena);

        arena.join_slice(items.into_bump_slice())
    }
}

impl Printable for Span {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        self.notate_in(arena)
    }
}

impl Printable for Spanned<Token<'_>> {
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        let (token, span) = self;
        let kind = token.kind();

        format_args!("\"{token}\"\t\t({kind}, {span})").notate_in(arena)
    }
}

impl<T> Printable for Option<T>
where
    T: Printable,
{
    fn notate<'a>(&'a self, arena: &'a Bump) -> Notation<'a> {
        match self {
            Some(t) => t.notate(arena),
            None => arena.empty(),
        }
    }
}
