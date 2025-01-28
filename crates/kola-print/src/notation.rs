use bumpalo::Bump;
use std::rc::Rc;

pub trait Arena<'a> {
    fn empty(&'a self) -> Notation<'a>;
    fn newline(&'a self) -> Notation<'a>;
    fn just(&'a self, c: char) -> Notation<'a>;
    /// Display text exactly as-is. The text should not contain a newline!
    fn notate(&'a self, s: &'a str) -> Notation<'a>;
    /// Display the specified notations immediately following each other.
    fn concat(&'a self, items: &'a [Notation<'a>]) -> Notation<'a>;
}

impl<'a> Arena<'a> for Bump {
    fn empty(&'a self) -> Notation<'a> {
        let repr = NotationRepr::Empty;
        Notation::new(repr, self)
    }

    fn newline(&'a self) -> Notation<'a> {
        let repr = NotationRepr::Newline;
        Notation::new(repr, self)
    }

    fn just(&'a self, c: char) -> Notation<'a> {
        let repr = NotationRepr::Char(c);
        Notation::new(repr, self)
    }

    fn notate(&'a self, s: &'a str) -> Notation<'a> {
        let width = s.len() as u32; // TODO proper width calculation

        let text = TextNotation::new(s, width);
        let repr = NotationRepr::Text(text);
        Notation::new(repr, self)
    }

    fn concat(&'a self, items: &'a [Notation<'a>]) -> Notation<'a> {
        let repr = NotationRepr::Concat(items);
        Notation::new(repr, self)
    }
}

#[derive(Debug, Clone)]
pub struct TextNotation<'a> {
    pub(super) inner: &'a str,
    pub(super) width: u32,
}

impl<'a> TextNotation<'a> {
    pub fn new(inner: &'a str, width: u32) -> Self {
        Self { inner, width }
    }
}

#[derive(Debug, Clone)]
pub enum NotationRepr<'a> {
    Empty,
    Newline,
    Char(char),
    Text(TextNotation<'a>),
    Flat(Notation<'a>),
    Indent(Notation<'a>),
    Dedent(Notation<'a>),
    Concat(&'a [Notation<'a>]),
    Group(Notation<'a>, Notation<'a>), // TODO better name to not confuse with wadler style group ?
    Choice(Notation<'a>, Notation<'a>),
}

#[derive(Debug, Clone)]
pub struct Notation<'a> {
    pub(super) inner: Rc<NotationRepr<'a>, &'a Bump>,
}

impl<'a> Notation<'a> {
    fn new(repr: NotationRepr<'a>, arena: &'a Bump) -> Self {
        let inner = Rc::new_in(repr, arena);

        Self { inner }
    }

    /// Use the leftmost option of every choice in the contained Notation.
    /// If the contained Notation follows the recommendation of not
    /// putting newlines in the left-most options of choices, then this
    /// `flat` will be displayed all on one line.
    pub fn flatten(self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Flat(self);
        Self::new(repr, arena)
    }

    /// Increase the indentation level of the contained notation.
    /// The indentation level determines the number of spaces put after Linebreak's
    /// It therefore doesn't affect the first line of a notation.
    pub fn indent(self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Indent(self);
        Self::new(repr, arena)
    }

    /// Decrease the indentation level of the contained notation.
    /// The indentation level determines the number of spaces put after Linebreak's
    /// It therefore doesn't affect the first line of a notation.
    pub fn dedent(self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Dedent(self);
        Self::new(repr, arena)
    }

    /// If the first line of the left notation fits within the required width,
    /// then display the left notation. Otherwise, display the right notation.
    pub fn or(self, other: Self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Choice(self, other);
        Self::new(repr, arena)
    }

    pub fn then(self, other: Self, arena: &'a Bump) -> Self {
        let repr = NotationRepr::Group(self, other);
        Self::new(repr, arena)
    }

    pub fn enclose(self, left: Self, right: Self, arena: &'a Bump) -> Self {
        let buf = bumpalo::vec![in arena; left, self, right];
        arena.concat(buf.into_bump_slice())
    }

    pub fn enclose_by(self, delim: Self, arena: &'a Bump) -> Self {
        let buf = bumpalo::vec![in arena; delim.clone(), self, delim];
        arena.concat(buf.into_bump_slice())
    }
}
