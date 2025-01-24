use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Literal, Metadata, Name, Node, NodeId, Symbol, Tree};
use crate::syntax::print::prelude::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PatError;

impl<M> Printable<Tree<M>> for PatError {
    fn notate<'a>(&'a self, _with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        "PatError".red().display_in(arena)
    }
}

impl TryFrom<Node> for PatError {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::PatError(e) => Ok(e),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Wildcard;

impl<M> Printable<Tree<M>> for Wildcard {
    fn notate<'a>(&'a self, _with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        "Wildcard".blue().display_in(arena)
    }
}

impl TryFrom<Node> for Wildcard {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::Wildcard(w) => Ok(w),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct LiteralPat(pub Literal);

impl<M> Printable<Tree<M>> for LiteralPat {
    fn notate<'a>(&'a self, _with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let kind = "LiteralPat".purple().display_in(arena);

        let lit = match &self.0 {
            Literal::Bool(b) => b.yellow().display_in(arena),
            Literal::Num(n) => n.yellow().display_in(arena),
            Literal::Char(c) => c.yellow().display_in(arena),
            Literal::Str(s) => s.yellow().display_in(arena),
        }
        .enclose_by(arena.just('"'), arena);

        let single = arena.just(' ').then(lit.clone(), arena);
        let multi = arena.newline().then(lit, arena);

        kind.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for LiteralPat {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::LiteralPat(l) => Ok(l),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct IdentPat(pub Symbol);

impl<M> Printable<Tree<M>> for IdentPat {
    fn notate<'a>(&'a self, _with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let head = "IdentPat".cyan().display_in(arena);

        let ident = self
            .0
            .yellow()
            .display_in(arena)
            .enclose_by(arena.just('"'), arena);

        let single = [arena.just(' '), ident.clone()].concat_in(arena);
        let multi = [arena.newline(), ident].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for IdentPat {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::IdentPat(i) => Ok(i),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PropertyPat {
    pub key: NodeId<Name>,
    pub value: Option<NodeId<Pat>>,
}

impl<M> Printable<Tree<M>> for PropertyPat
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let Self { key, value } = self;

        let head = "PropertyPat".blue().display_in(arena);

        let key = key.notate(with, arena);
        let value = value.as_ref().map(|v| v.notate(with, arena));

        let single = [
            arena.notate(" key = "),
            key.clone().flatten(arena),
            value
                .clone()
                .map(|v| arena.notate(", value = ").then(v, arena))
                .or_not(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("key = "),
            key,
            value
                .map(|v| [arena.newline(), arena.notate("value = "), v].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for PropertyPat {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::PropertyPat(p) => Ok(p),
            _ => Err(()),
        }
    }
}

// impl PropertyPat {
//     pub fn value(&self) -> Option<&Pat> {
//         self.value.as_ref()
//     }
// }

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordPat {
    pub fields: Vec<NodeId<PropertyPat>>,
}

impl<M> Printable<Tree<M>> for RecordPat
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        let head = "RecordPat".blue().display_in(arena);

        let fields = self.fields.gather(with, arena);

        let single = fields.clone().concat_map(
            |field| arena.notate(" ").then(field.flatten(arena), arena),
            arena,
        );
        let multi = fields.concat_map(|field| arena.newline().then(field, arena), arena);

        head.then(single.or(multi, arena), arena)
    }
}

impl TryFrom<Node> for RecordPat {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, Self::Error> {
        match value {
            Node::RecordPat(r) => Ok(r),
            _ => Err(()),
        }
    }
}

// impl RecordPat {
//     pub fn get(&self, name: impl AsRef<str>) -> Option<&PropertyPat> {
//         self.fields.iter().find(|p| &p.key.name == name.as_ref())
//     }
// }

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Pat {
    Error(PatError),
    Wildcard(Wildcard),
    Literal(LiteralPat),
    Ident(IdentPat),
    Record(RecordPat),
}

impl<M> Printable<Tree<M>> for Pat
where
    M: Metadata,
{
    fn notate<'a>(&'a self, with: &'a Tree<M>, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.notate(with, arena),
            Self::Wildcard(w) => w.notate(with, arena),
            Self::Literal(l) => l.notate(with, arena),
            Self::Ident(i) => i.notate(with, arena),
            Self::Record(r) => r.notate(with, arena),
        }
    }
}

impl TryFrom<Node> for Pat {
    type Error = ();

    fn try_from(value: Node) -> Result<Self, ()> {
        match value {
            Node::Pat(p) => Ok(p),
            _ => Err(()),
        }
    }
}

impl Pat {
    // pub fn ty(&self) -> Result<&MonoType, PatError> {
    //     let ty = match self {
    //         Self::Error(e) => return Err(*e),
    //         Self::Wildcard(w) => w.ty(),
    //         Self::Literal(l) => l.ty(),
    //         Self::Ident(i) => i.ty(),
    //         Self::Record(r) => r.ty(),
    //     };
    //     Ok(ty)
    // }

    // pub fn ty_mut(&mut self) -> Result<&mut MonoType, PatError> {
    //     let ty = match self {
    //         Self::Error(e) => return Err(*e),
    //         Self::Wildcard(w) => w.ty_mut(),
    //         Self::Literal(l) => l.ty_mut(),
    //         Self::Ident(i) => i.ty_mut(),
    //         Self::Record(r) => r.ty_mut(),
    //     };
    //     Ok(ty)
    // }

    pub fn as_error(&self) -> Option<&PatError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn as_wildcard(&self) -> Option<&Wildcard> {
        match self {
            Self::Wildcard(w) => Some(w),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&LiteralPat> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn as_ident(&self) -> Option<&IdentPat> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&RecordPat> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }

    pub fn is_error(&self) -> bool {
        self.as_error().is_some()
    }

    pub fn is_wildcard(&self) -> bool {
        self.as_wildcard().is_some()
    }

    pub fn is_literal(&self) -> bool {
        self.as_literal().is_some()
    }

    pub fn is_ident(&self) -> bool {
        self.as_ident().is_some()
    }

    pub fn is_record(&self) -> bool {
        self.as_record().is_some()
    }

    pub fn into_error(self) -> Option<PatError> {
        match self {
            Self::Error(e) => Some(e),
            _ => None,
        }
    }

    pub fn into_wildcard(self) -> Option<Wildcard> {
        match self {
            Self::Wildcard(w) => Some(w),
            _ => None,
        }
    }

    pub fn into_literal(self) -> Option<LiteralPat> {
        match self {
            Self::Literal(l) => Some(l),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<IdentPat> {
        match self {
            Self::Ident(i) => Some(i),
            _ => None,
        }
    }

    pub fn into_record(self) -> Option<RecordPat> {
        match self {
            Self::Record(r) => Some(r),
            _ => None,
        }
    }
}

impl From<Wildcard> for Pat {
    fn from(value: Wildcard) -> Self {
        Self::Wildcard(value)
    }
}

impl From<LiteralPat> for Pat {
    fn from(value: LiteralPat) -> Self {
        Self::Literal(value)
    }
}

impl From<IdentPat> for Pat {
    fn from(value: IdentPat) -> Self {
        Self::Ident(value)
    }
}

impl From<RecordPat> for Pat {
    fn from(value: RecordPat) -> Self {
        Self::Record(value)
    }
}
