use bumpalo::{
    collections::{CollectIn, String, Vec},
    Bump,
};
use ecow::EcoString;
use owo_colors::{OwoColorize, Style};
use std::{
    fmt::{self, Write},
    ops::Deref,
    rc::Rc,
};

use super::{
    token::{Token, Tokens},
    Span, Spanned,
};

// https://justinpombrio.net/2024/02/23/a-twist-on-Wadlers-printer.html

pub trait Printable {
    fn notate<'a>(&'a self, arena: &'a Arena<'a>) -> Notation<'a>;

    fn notate_flat<'a>(&'a self, arena: &'a Arena<'a>) -> Notation<'a> {
        self.notate(arena).flatten(arena)
    }

    fn render(&self) -> std::string::String {
        let alloc = Bump::new();
        let arena = Arena(&alloc);

        let notation = self.notate(&arena);
        let mut printer = Printer::new(&notation, 100, &arena);

        let mut output = std::string::String::new();
        printer.print(&mut output, &arena).unwrap();

        return output;
    }
}

impl Printable for EcoString {
    fn notate<'a>(&'a self, arena: &'a Arena<'a>) -> Notation<'a> {
        self.notate_in(arena)
    }
}

impl Printable for Tokens<'_> {
    fn notate<'a>(&'a self, arena: &'a Arena<'a>) -> Notation<'a> {
        let items = self
            .iter()
            .flat_map(|t| [t.notate(arena), arena.break_line()])
            .collect_in::<Vec<_>>(arena);

        arena.join_slice(items.into_bump_slice())
    }
}

impl Printable for Span {
    fn notate<'a>(&'a self, arena: &'a Arena<'a>) -> Notation<'a> {
        self.notate_in(arena)
    }
}

impl Printable for Spanned<Token<'_>> {
    fn notate<'a>(&'a self, arena: &'a Arena<'a>) -> Notation<'a> {
        let (token, span) = self;
        let kind = token.kind();

        format_args!("\"{token}\"\t\t({kind}, {span})").notate_in(arena)
    }
}

impl<T> Printable for Option<T>
where
    T: Printable,
{
    fn notate<'a>(&'a self, arena: &'a Arena<'a>) -> Notation<'a> {
        match self {
            Some(t) => t.notate(arena),
            None => arena.empty(),
        }
    }
}

pub trait OrNot {
    fn or_not<'a>(self, arena: &'a Arena<'a>) -> Notation<'a>
    where
        Self: 'a;
}

impl OrNot for Option<Notation<'_>> {
    fn or_not<'a>(self, arena: &'a Arena<'a>) -> Notation<'a>
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
    fn join_in<'a>(self, arena: &'a Arena<'a>) -> Notation<'a>
    where
        Self: IntoIterator<Item = Notation<'a>> + Sized + 'a,
    {
        arena.join(self)
    }
}

impl<'a, I: IntoIterator<Item = Notation<'a>> + Sized + 'a> JoinIn for I {}

pub trait NotateIn {
    fn notate_in<'a>(&self, arena: &'a Arena<'a>) -> Notation<'a>
    where
        Self: fmt::Display,
    {
        let mut s = String::new_in(arena);
        s.write_fmt(format_args!("{self}")).unwrap();
        arena.notate(s.into_bump_str())
    }
}

impl<T: fmt::Display> NotateIn for T {}

#[derive(Clone, Copy)]
pub struct Arena<'a>(&'a Bump);

impl<'a> Deref for Arena<'a> {
    type Target = Bump;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> Arena<'a> {
    pub fn empty(&self) -> Notation<'a> {
        let repr = NotationRepr::Empty;
        let inner = Rc::new_in(repr, self.0);

        Notation { inner }
    }

    pub fn break_line(&self) -> Notation<'a> {
        let repr = NotationRepr::Linebreak;
        let inner = Rc::new_in(repr, self.0);

        Notation { inner }
    }

    /// Display text exactly as-is. The text should not contain a newline!
    pub fn notate(&self, s: &'a str) -> Notation<'a> {
        self.notate_with(s, Style::new())
    }

    /// Display text exactly as-is with some style. The text should not contain a newline!
    pub fn notate_with(&self, s: &'a str, style: Style) -> Notation<'a> {
        let width = s.len() as u32; // TODO proper width calculation

        let text = TextNotation::new(s, style, width);
        let repr = NotationRepr::Text(text);
        let inner = Rc::new_in(repr, self.0);

        Notation { inner }
    }

    /// Display the specified notations immediately following each other.
    pub fn join<I>(&self, items: I) -> Notation<'a>
    where
        I: IntoIterator<Item = Notation<'a>> + 'a,
    {
        let buf = items.into_iter().collect_in::<Vec<_>>(self.0);

        self.join_slice(buf.into_bump_slice())
    }

    /// Display the specified notations immediately following each other.
    pub fn join_slice(&self, items: &'a [Notation<'a>]) -> Notation<'a> {
        let repr = NotationRepr::Join(items);
        let inner = Rc::new_in(repr, self.0);

        Notation { inner }
    }

    /// Concatenate a vector of printable items into a single notation.
    pub fn join_into<T, I>(&'a self, items: I) -> Notation<'a>
    where
        I: IntoIterator<Item = &'a T> + 'a,
        T: Printable + 'a,
    {
        self.join_slice(self.process(items))
    }

    pub fn process<T, I>(&'a self, items: I) -> &'a [Notation<'a>]
    where
        I: IntoIterator<Item = &'a T> + 'a,
        T: Printable + 'a,
    {
        let mut buf = Vec::new_in(self.0);
        buf.extend(items.into_iter().map(|item| item.notate(self)));

        buf.into_bump_slice()
    }
}

#[derive(Debug, Clone)]
struct TextNotation<'a> {
    inner: &'a str,
    style: Style,
    width: u32,
}

impl<'a> TextNotation<'a> {
    fn new(inner: &'a str, style: Style, width: u32) -> Self {
        Self {
            inner,
            style,
            width,
        }
    }
}

#[derive(Debug, Clone)]
enum NotationRepr<'a> {
    Empty,
    Linebreak,
    Text(TextNotation<'a>),
    Flat(Notation<'a>),
    Indent(Notation<'a>),
    Dedent(Notation<'a>),
    Join(&'a [Notation<'a>]),
    Group(Notation<'a>, Notation<'a>), // TODO better name to not confuse with wadler style group ?
    Choice(Notation<'a>, Notation<'a>),
}

#[derive(Debug, Clone)]
pub struct Notation<'a> {
    inner: Rc<NotationRepr<'a>, &'a Bump>,
}

impl<'a> Notation<'a> {
    /// Use the leftmost option of every choice in the contained Notation.
    /// If the contained Notation follows the recommendation of not
    /// putting newlines in the left-most options of choices, then this
    /// `flat` will be displayed all on one line.
    pub fn flatten(self, arena: &'a Arena<'a>) -> Self {
        let repr = NotationRepr::Flat(self);
        let inner = Rc::new_in(repr, arena.0);

        Self { inner }
    }

    /// Increase the indentation level of the contained notation.
    /// The indentation level determines the number of spaces put after Linebreak's
    /// It therefore doesn't affect the first line of a notation.
    pub fn indent(self, arena: &'a Arena<'a>) -> Self {
        let repr = NotationRepr::Indent(self);
        let inner = Rc::new_in(repr, arena.0);

        Self { inner }
    }

    pub fn dedent(self, arena: &'a Arena<'a>) -> Self {
        let repr = NotationRepr::Dedent(self);
        let inner = Rc::new_in(repr, arena.0);

        Self { inner }
    }

    /// If the first line of the left notation fits within the required width,
    /// then display the left notation. Otherwise, display the right notation.
    pub fn or(self, other: Self, arena: &'a Arena<'a>) -> Self {
        let repr = NotationRepr::Choice(self, other);
        let inner = Rc::new_in(repr, arena.0);

        Self { inner }
    }

    pub fn then(self, other: Self, arena: &'a Arena<'a>) -> Self {
        let repr = NotationRepr::Group(self, other);
        let inner = Rc::new_in(repr, arena.0);

        Self { inner }
    }
}

#[derive(Clone, Copy)]
struct Chunk<'a> {
    notation: &'a Notation<'a>,
    indent: u32,
    flat: bool,
}

impl<'a> Chunk<'a> {
    fn new(notation: &'a Notation<'a>) -> Self {
        Self {
            notation,
            indent: 0,
            flat: false,
        }
    }

    fn with_notation(mut self, n: &'a Notation<'a>) -> Self {
        self.notation = n;
        self
    }

    fn flatten(mut self) -> Self {
        self.flat = true;
        self
    }

    fn indent(mut self) -> Self {
        self.indent += 1;
        self
    }
    fn dedent(mut self) -> Self {
        self.indent -= 1;
        self
    }
}

struct Printer<'a> {
    /// Maximum line width that we'll try to stay within
    width: u32,
    /// Current column position
    pos: u32,
    /// A stack of chunks to print. The _top_ of the stack is the
    /// _end_ of the vector, which represents the _earliest_ part
    /// of the document to print.
    chunks: Vec<'a, Chunk<'a>>,
}

impl<'a> Printer<'a> {
    fn new(notation: &'a Notation<'a>, width: u32, arena: &'a Arena<'a>) -> Self {
        let chunk = Chunk::new(notation);
        let mut chunks = Vec::new_in(arena);
        chunks.push(chunk);

        Self {
            width,
            pos: 0,
            chunks,
        }
    }

    fn print(&mut self, output: &mut impl fmt::Write, arena: &'a Arena<'a>) -> fmt::Result {
        use NotationRepr::*;

        while let Some(chunk) = self.chunks.pop() {
            match chunk.notation.inner.as_ref() {
                Empty => (),
                Linebreak => {
                    output.write_char('\n')?;
                    for _ in 0..chunk.indent {
                        output.write_char(' ')?;
                    }
                    self.pos = chunk.indent;
                }
                Text(t) => {
                    output.write_fmt(format_args!("{}", t.inner.style(t.style)))?;
                    self.pos += t.width;
                }
                Flat(n) => self.chunks.push(chunk.with_notation(n).flatten()),
                Indent(n) => self.chunks.push(chunk.with_notation(n).indent()),
                Dedent(n) => self.chunks.push(chunk.with_notation(n).dedent()),
                // reverse so that items are in correct order
                Join(ns) => self
                    .chunks
                    .extend(ns.into_iter().rev().map(|n| chunk.with_notation(n))),
                Group(a, b) => {
                    // Push b first so it is on top of the stack
                    self.chunks.push(chunk.with_notation(b));
                    self.chunks.push(chunk.with_notation(a));
                }
                Choice(a, b) => {
                    if chunk.flat || self.fits(chunk.with_notation(a), arena) {
                        self.chunks.push(chunk.with_notation(a));
                    } else {
                        self.chunks.push(chunk.with_notation(b))
                    }
                }
            }
        }

        Ok(())
    }

    fn fits(&self, chunk: Chunk<'a>, arena: &'a Arena<'a>) -> bool {
        use NotationRepr::*;

        let mut stack = bumpalo::vec![in arena; chunk];
        let mut spilled = &self.chunks as &[_];

        let mut rem = match self.width.checked_sub(self.pos) {
            Some(r) => r,
            None => return false,
        };

        while let Some(chunk) = stack.pop().or_else(|| {
            spilled.split_last().map(|(chunk, chunks)| {
                spilled = chunks;
                *chunk
            })
        }) {
            match chunk.notation.inner.as_ref() {
                Empty => (),
                Linebreak => return true,
                Text(t) => match rem.checked_sub(t.width) {
                    Some(r) => rem = r,
                    None => return false,
                },
                Flat(n) => stack.push(chunk.with_notation(n).flatten()),
                Indent(n) => stack.push(chunk.with_notation(n).indent()),
                Dedent(n) => stack.push(chunk.with_notation(n).dedent()),
                Join(ns) => stack.extend(ns.into_iter().map(|n| chunk.with_notation(n))),
                Group(a, b) => {
                    stack.push(chunk.with_notation(a));
                    stack.push(chunk.with_notation(b));
                }
                Choice(a, b) => {
                    if chunk.flat {
                        stack.push(chunk.with_notation(a))
                    } else {
                        stack.push(chunk.with_notation(b))
                    }
                }
            }
        }

        true
    }
}
