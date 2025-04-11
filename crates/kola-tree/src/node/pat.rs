use derive_more::From;
use kola_print::prelude::*;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Literal, Name, Symbol};
use crate::{id::NodeId, print::TreePrinter, tree::NodeContainer};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct PatError;

impl Printable<TreePrinter> for PatError {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "PatError".red().display_in(arena)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Wildcard;

impl Printable<TreePrinter> for Wildcard {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "Wildcard".blue().display_in(arena)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct LiteralPat(pub Literal);

impl Printable<TreePrinter> for LiteralPat {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct IdentPat(pub Symbol);

impl PartialEq<Symbol> for IdentPat {
    fn eq(&self, other: &Symbol) -> bool {
        &self.0 == other
    }
}

impl PartialEq<str> for IdentPat {
    fn eq(&self, other: &str) -> bool {
        self.0.as_str() == other
    }
}

impl Printable<TreePrinter> for IdentPat {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PropertyPat {
    pub key: NodeId<Name>,
    pub value: Option<NodeId<Pat>>,
}

impl PropertyPat {
    pub fn value<'a>(&self, tree: &'a impl NodeContainer) -> Option<&'a Pat> {
        self.value.map(|id| id.get(tree))
    }
}

impl Printable<TreePrinter> for PropertyPat {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RecordPat {
    pub fields: Vec<NodeId<PropertyPat>>,
}

impl RecordPat {
    pub fn get<'a>(
        &self,
        name: impl AsRef<str>,
        tree: &'a impl NodeContainer,
    ) -> Option<&'a PropertyPat> {
        self.fields.iter().find_map(|p| {
            let p = p.get(tree);
            (p.key.get(tree) == name.as_ref()).then_some(p)
        })
    }
}

impl Printable<TreePrinter> for RecordPat {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize, From)]
pub enum Pat {
    Error(PatError),
    Wildcard(Wildcard),
    Literal(LiteralPat),
    Ident(IdentPat),
    Record(RecordPat),
}

impl Printable<TreePrinter> for Pat {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.notate(with, arena),
            Self::Wildcard(w) => w.notate(with, arena),
            Self::Literal(l) => l.notate(with, arena),
            Self::Ident(i) => i.notate(with, arena),
            Self::Record(r) => r.notate(with, arena),
        }
    }
}

impl Pat {
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
