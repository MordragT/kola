use derive_more::From;
use kola_print::prelude::*;
use kola_utils::as_variant;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{LiteralExpr, Name, Symbol};
use crate::{id::NodeId, print::TreePrinter, tree::NodeContainer};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PatError;

impl Printable<TreePrinter> for PatError {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "PatError".red().display_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AnyPat;

impl Printable<TreePrinter> for AnyPat {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "AnyPat".blue().display_in(arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct LiteralPat(pub LiteralExpr);

impl Printable<TreePrinter> for LiteralPat {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let kind = "LiteralPat".purple().display_in(arena);

        let lit = match &self.0 {
            LiteralExpr::Bool(b) => b.yellow().display_in(arena),
            LiteralExpr::Num(n) => n.yellow().display_in(arena),
            LiteralExpr::Char(c) => c.yellow().display_in(arena),
            LiteralExpr::Str(s) => s.yellow().display_in(arena),
        }
        .enclose_by(arena.just('"'), arena);

        let single = arena.just(' ').then(lit.clone(), arena);
        let multi = arena.newline().then(lit, arena);

        kind.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[from(forward)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordFieldPat {
    pub field: NodeId<Name>,
    pub pat: Option<NodeId<Pat>>,
}

impl RecordFieldPat {
    pub fn field(self, tree: &impl NodeContainer) -> &Name {
        self.field.get(tree)
    }

    pub fn pat(self, tree: &impl NodeContainer) -> Option<Pat> {
        self.pat.map(|id| id.get(tree)).copied()
    }
}

impl Printable<TreePrinter> for RecordFieldPat {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { field, pat } = self;

        let head = "RecordFieldPat".blue().display_in(arena);

        let field = field.notate(with, arena);
        let pat = pat.as_ref().map(|v| v.notate(with, arena));

        let single = [
            arena.notate(" field = "),
            field.clone().flatten(arena),
            pat.clone()
                .map(|v| arena.notate(", pat = ").then(v, arena))
                .or_not(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("field = "),
            field,
            pat.map(|v| [arena.newline(), arena.notate("pat = "), v].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordPat(pub Vec<NodeId<RecordFieldPat>>);

impl RecordPat {
    pub fn get(&self, name: impl AsRef<str>, tree: &impl NodeContainer) -> Option<RecordFieldPat> {
        self.0.iter().find_map(|id| {
            let field = id.get(tree);
            (field.field(tree) == name.as_ref())
                .then_some(field)
                .copied()
        })
    }
}

impl Printable<TreePrinter> for RecordPat {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "RecordPat".blue().display_in(arena);

        let fields = self.0.gather(with, arena);

        let single = fields.clone().concat_map(
            |field| arena.notate(" ").then(field.flatten(arena), arena),
            arena,
        );
        let multi = fields.concat_map(|field| arena.newline().then(field, arena), arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct VariantCasePat {
    pub case: NodeId<Name>,
    pub pat: Option<NodeId<Pat>>,
}

impl VariantCasePat {
    pub fn case(self, tree: &impl NodeContainer) -> &Name {
        self.case.get(tree)
    }

    pub fn pat(self, tree: &impl NodeContainer) -> Option<Pat> {
        self.pat.map(|id| id.get(tree)).copied()
    }
}

impl Printable<TreePrinter> for VariantCasePat {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { case, pat } = self;

        let head = "VariantCasePat".blue().display_in(arena);

        let case = case.notate(with, arena);
        let pat = pat.as_ref().map(|v| v.notate(with, arena));

        let single = [
            arena.notate(" case = "),
            case.clone().flatten(arena),
            pat.clone()
                .map(|v| arena.notate(", pat = ").then(v, arena))
                .or_not(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("case = "),
            case,
            pat.map(|v| [arena.newline(), arena.notate("pat = "), v].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct VariantPat(pub Vec<NodeId<VariantCasePat>>);

impl VariantPat {
    pub fn get(&self, name: impl AsRef<str>, tree: &impl NodeContainer) -> Option<VariantCasePat> {
        self.0.iter().find_map(|id| {
            let case = id.get(tree);
            (case.case(tree) == name.as_ref()).then_some(case).copied()
        })
    }
}

impl Printable<TreePrinter> for VariantPat {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "VariantPat".blue().display_in(arena);

        let cases = self.0.gather(with, arena);

        let single = cases.clone().concat_map(
            |case| arena.notate(" ").then(case.flatten(arena), arena),
            arena,
        );
        let multi = cases.concat_map(|case| arena.newline().then(case, arena), arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, From, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum Pat {
    Error(NodeId<PatError>),
    Any(NodeId<AnyPat>),
    Literal(NodeId<LiteralPat>),
    Ident(NodeId<IdentPat>),
    Record(NodeId<RecordPat>),
    Variant(NodeId<VariantPat>),
}

impl Printable<TreePrinter> for Pat {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.notate(with, arena),
            Self::Any(w) => w.notate(with, arena),
            Self::Literal(l) => l.notate(with, arena),
            Self::Ident(i) => i.notate(with, arena),
            Self::Record(r) => r.notate(with, arena),
            Self::Variant(v) => v.notate(with, arena),
        }
    }
}

impl Pat {
    #[inline]
    pub fn to_error(self) -> Option<NodeId<PatError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_wildcard(self) -> Option<NodeId<AnyPat>> {
        as_variant!(self, Self::Any)
    }

    #[inline]
    pub fn to_literal(self) -> Option<NodeId<LiteralPat>> {
        as_variant!(self, Self::Literal)
    }

    #[inline]
    pub fn to_ident(self) -> Option<NodeId<IdentPat>> {
        as_variant!(self, Self::Ident)
    }

    #[inline]
    pub fn to_record(self) -> Option<NodeId<RecordPat>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error(_))
    }

    #[inline]
    pub fn is_wildcard(self) -> bool {
        matches!(self, Self::Any(_))
    }

    #[inline]
    pub fn is_literal(self) -> bool {
        matches!(self, Self::Literal(_))
    }

    #[inline]
    pub fn is_ident(self) -> bool {
        matches!(self, Self::Ident(_))
    }

    #[inline]
    pub fn is_record(self) -> bool {
        matches!(self, Self::Record(_))
    }
}
