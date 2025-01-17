use bumpalo::{
    collections::{CollectIn, Vec},
    Bump,
};
use owo_colors::Style;
use std::rc::Rc;

pub trait Arena<'a> {
    fn empty(&'a self) -> Notation<'a>;
    fn break_line(&'a self) -> Notation<'a>;
    /// Display text exactly as-is. The text should not contain a newline!
    fn notate(&'a self, s: &'a str) -> Notation<'a>;
    /// Display text exactly as-is with some style. The text should not contain a newline!
    fn notate_with(&'a self, s: &'a str, style: Style) -> Notation<'a>;
    /// Display the specified notations immediately following each other.
    fn join<I>(&'a self, items: I) -> Notation<'a>
    where
        I: IntoIterator<Item = Notation<'a>> + 'a;
    /// Display the specified notations immediately following each other.
    fn join_slice(&'a self, items: &'a [Notation<'a>]) -> Notation<'a>;
}

impl<'a> Arena<'a> for Bump {
    fn empty(&'a self) -> Notation<'a> {
        let repr = NotationRepr::Empty;
        let inner = Rc::new_in(repr, self);

        Notation { inner }
    }

    fn break_line(&'a self) -> Notation<'a> {
        let repr = NotationRepr::Linebreak;
        let inner = Rc::new_in(repr, self);

        Notation { inner }
    }

    fn notate(&'a self, s: &'a str) -> Notation<'a> {
        self.notate_with(s, Style::new())
    }

    fn notate_with(&'a self, s: &'a str, style: Style) -> Notation<'a> {
        let width = s.len() as u32; // TODO proper width calculation

        let text = TextNotation::new(s, style, width);
        let repr = NotationRepr::Text(text);
        let inner = Rc::new_in(repr, self);

        Notation { inner }
    }

    fn join<I>(&'a self, items: I) -> Notation<'a>
    where
        I: IntoIterator<Item = Notation<'a>> + 'a,
    {
        let buf = items.into_iter().collect_in::<Vec<_>>(self);

        self.join_slice(buf.into_bump_slice())
    }

    fn join_slice(&'a self, items: &'a [Notation<'a>]) -> Notation<'a> {
        let repr = NotationRepr::Join(items);
        let inner = Rc::new_in(repr, self);

        Notation { inner }
    }
}

#[derive(Debug, Clone)]
pub struct TextNotation<'a> {
    pub(super) inner: &'a str,
    pub(super) style: Style,
    pub(super) width: u32,
}

impl<'a> TextNotation<'a> {
    pub fn new(inner: &'a str, style: Style, width: u32) -> Self {
        Self {
            inner,
            style,
            width,
        }
    }
}

#[derive(Debug, Clone)]
pub enum NotationRepr<'a> {
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
    pub(super) inner: Rc<NotationRepr<'a>, &'a Bump>,
}

impl<'a> Notation<'a> {
    /// Use the leftmost option of every choice in the contained Notation.
    /// If the contained Notation follows the recommendation of not
    /// putting newlines in the left-most options of choices, then this
    /// `flat` will be displayed all on one line.
    pub fn flatten(self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Flat(self);
        let inner = Rc::new_in(repr, arena);

        Self { inner }
    }

    /// Increase the indentation level of the contained notation.
    /// The indentation level determines the number of spaces put after Linebreak's
    /// It therefore doesn't affect the first line of a notation.
    pub fn indent(self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Indent(self);
        let inner = Rc::new_in(repr, arena);

        Self { inner }
    }

    pub fn dedent(self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Dedent(self);
        let inner = Rc::new_in(repr, arena);

        Self { inner }
    }

    /// If the first line of the left notation fits within the required width,
    /// then display the left notation. Otherwise, display the right notation.
    pub fn or(self, other: Self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Choice(self, other);
        let inner = Rc::new_in(repr, arena);

        Self { inner }
    }

    pub fn then(self, other: Self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Group(self, other);
        let inner = Rc::new_in(repr, arena);

        Self { inner }
    }
}
