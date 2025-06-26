use derive_more::{From, IntoIterator};
use enum_as_inner::EnumAsInner;
use kola_macros::{Inspector, Notate};
use serde::{Deserialize, Serialize};

use kola_print::prelude::*;
use kola_utils::interner::StrKey;

use super::LiteralExpr;
use crate::{
    id::Id,
    node::ValueName,
    print::NodePrinter,
    tree::{TreeBuilder, TreeView},
};

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "magenta")]
pub struct PatError;

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(color = "magenta")]
pub struct AnyPat;

#[derive(Debug, EnumAsInner, From, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum LiteralPat {
    Unit,
    Bool(bool),
    Num(f64),
    Char(char),
    Str(StrKey),
}

impl From<LiteralExpr> for LiteralPat {
    fn from(expr: LiteralExpr) -> Self {
        match expr {
            LiteralExpr::Unit => Self::Unit,
            LiteralExpr::Bool(b) => Self::Bool(b),
            LiteralExpr::Num(n) => Self::Num(n),
            LiteralExpr::Char(c) => Self::Char(c),
            LiteralExpr::Str(s) => Self::Str(s),
        }
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, LiteralPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let kind = "LiteralPat".magenta().display_in(arena);

        let lit = match *self.value {
            LiteralPat::Unit => "Unit".yellow().display_in(arena),
            LiteralPat::Bool(b) => b.yellow().display_in(arena),
            LiteralPat::Num(n) => n.yellow().display_in(arena),
            LiteralPat::Char(c) => c.yellow().display_in(arena),
            LiteralPat::Str(s) => self.interner[s].yellow().display_in(arena),
        }
        .enclose_by(arena.just('"'), arena);

        let single = arena.just(' ').then(lit.clone(), arena);
        let multi = arena.newline().then(lit, arena);

        kind.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "magenta")]
#[from(forward)]
pub struct BindPat(pub Id<ValueName>);

impl BindPat {
    pub fn new_in(name: impl Into<ValueName>, builder: &mut TreeBuilder) -> Id<Self> {
        let name_id = builder.insert(name.into());
        builder.insert(Self(name_id))
    }
}

#[derive(
    Debug,
    EnumAsInner,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum ListElPat {
    Pat(Id<Pat>),
    Spread(Option<Id<ValueName>>),
}

impl ListElPat {
    pub fn pat(pat: impl Into<Pat>, builder: &mut TreeBuilder) -> Id<Self> {
        let pat_id = builder.insert(pat.into());
        builder.insert(Self::Pat(pat_id))
    }

    pub fn spread(name: Option<ValueName>, builder: &mut TreeBuilder) -> Id<Self> {
        let name = name.map(|n| builder.insert(n));
        builder.insert(Self::Spread(name))
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, ListElPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            ListElPat::Pat(pat) => {
                let head = "ListElPat".magenta().display_in(arena);
                let pat = self.to_id(pat).notate(arena);

                let single = arena.just(' ').then(pat.clone(), arena);
                let multi = arena.newline().then(pat, arena).indent(arena);

                head.then(single.or(multi, arena), arena)
            }
            ListElPat::Spread(name) => {
                let head = "ListSpread".green().display_in(arena);

                let name_notation = name.map(|n| self.to_id(n).notate(arena));

                let single =
                    [arena.notate(" ..."), name_notation.clone().or_not(arena)].concat_in(arena);

                let multi = [
                    arena.newline(),
                    arena.notate("..."),
                    name_notation.or_not(arena),
                ]
                .concat_in(arena)
                .indent(arena);

                head.then(single.or(multi, arena), arena)
            }
        }
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "magenta")]
pub struct ListPat(pub Vec<Id<ListElPat>>);

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "magenta")]
pub struct RecordFieldPat {
    pub field: Id<ValueName>,
    pub pat: Option<Id<Pat>>,
}

impl RecordFieldPat {
    pub fn field(self, tree: &impl TreeView) -> ValueName {
        *self.field.get(tree)
    }

    pub fn pat(self, tree: &impl TreeView) -> Option<Pat> {
        self.pat.map(|id| id.get(tree)).copied()
    }
}

#[derive(
    Debug, Inspector, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct RecordPat {
    pub fields: Vec<Id<RecordFieldPat>>,
    pub polymorph: bool,
}

impl RecordPat {
    pub fn new_in<I>(fields: I, polymorph: bool, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator<Item = RecordFieldPat>,
    {
        let fields = fields
            .into_iter()
            .map(|field| builder.insert(field))
            .collect();
        builder.insert(Self { fields, polymorph })
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "RecordPat".magenta().display_in(arena);

        let fields = self.to_slice(&self.value.fields).gather(arena);
        let polymorph = self.value.polymorph;

        let single = [
            fields.clone().concat_map(
                |field| arena.notate(" ").then(field.flatten(arena), arena),
                arena,
            ),
            if polymorph {
                arena.notate(" ...").into()
            } else {
                arena.notate("").into()
            },
        ]
        .concat_in(arena);

        let multi = [
            fields.concat_map(|field| arena.newline().then(field, arena), arena),
            if polymorph {
                arena.newline().then(arena.notate("..."), arena)
            } else {
                arena.notate("").into()
            },
        ]
        .concat_in(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct VariantTagPat {
    pub tag: Id<ValueName>,
    pub pat: Option<Id<Pat>>,
}

impl VariantTagPat {
    pub fn case(self, tree: &impl TreeView) -> ValueName {
        *self.tag.get(tree)
    }

    pub fn pat(self, tree: &impl TreeView) -> Option<Pat> {
        self.pat.map(|id| id.get(tree)).copied()
    }
}

#[derive(
    Debug,
    Notate,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[notate(color = "blue")]
#[into_iterator(owned, ref)]
pub struct VariantPat(pub Vec<Id<VariantTagPat>>);

#[derive(
    Debug, EnumAsInner, Inspector, From, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize,
)]
pub enum Pat {
    Error(Id<PatError>),
    Any(Id<AnyPat>),
    Literal(Id<LiteralPat>),
    Bind(Id<BindPat>),
    List(Id<ListPat>),
    Record(Id<RecordPat>),
    Variant(Id<VariantPat>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Pat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Pat::Error(e) => self.to_id(e).notate(arena),
            Pat::Any(w) => self.to_id(w).notate(arena),
            Pat::Literal(l) => self.to_id(l).notate(arena),
            Pat::Bind(i) => self.to_id(i).notate(arena),
            Pat::List(l) => self.to_id(l).notate(arena),
            Pat::Record(r) => self.to_id(r).notate(arena),
            Pat::Variant(v) => self.to_id(v).notate(arena),
        }
    }
}
