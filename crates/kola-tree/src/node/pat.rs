use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_tree_macro::Inspector;
use serde::{Deserialize, Serialize};

use kola_print::prelude::*;
use kola_utils::interner::StrKey;

use super::{LiteralExpr, ValueName};
use crate::{
    id::Id,
    print::TreePrinter,
    slice::SliceId,
    tree::{TreeBuilder, TreeView},
};

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "magenta")]
pub struct PatError;

#[derive(
    Debug, Notate, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[notate(with = TreePrinter<'a>, color = "magenta")]
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

impl<'a> Notate<'a, LiteralPat> for TreePrinter<'a> {
    fn notate(self, value: &LiteralPat, arena: &'a Bump) -> Notation<'a> {
        let kind = "LiteralPat".magenta().display_in(arena);

        let lit = match *value {
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
#[notate(with = TreePrinter<'a>, color = "magenta")]
#[from(forward)]
pub struct BindPat(pub Id<ValueName>);

impl BindPat {
    pub fn new_in(name: impl Into<ValueName>, builder: &mut TreeBuilder) -> Id<Self> {
        let name_id = builder.alloc(name.into());
        builder.alloc(Self(name_id))
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
        let pat_id = builder.alloc(pat.into());
        builder.alloc(Self::Pat(pat_id))
    }

    pub fn spread(name: Option<ValueName>, builder: &mut TreeBuilder) -> Id<Self> {
        let name = name.map(|n| builder.alloc(n));
        builder.alloc(Self::Spread(name))
    }
}

impl<'a> Notate<'a, ListElPat> for TreePrinter<'a> {
    fn notate(self, value: &ListElPat, arena: &'a Bump) -> Notation<'a> {
        match value {
            ListElPat::Pat(pat) => {
                let head = "ListElPat".magenta().display_in(arena);
                let pat = self.notate(pat, arena);

                let single = arena.just(' ').then(pat.clone(), arena);
                let multi = arena.newline().then(pat, arena).indent(arena);

                head.then(single.or(multi, arena), arena)
            }
            ListElPat::Spread(name) => {
                let head = "ListSpread".green().display_in(arena);

                let name_notation = name.as_ref().map(|n| self.notate(n, arena));

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
#[notate(with = TreePrinter<'a>, color = "magenta")]
pub struct ListPat(pub SliceId<ListElPat>);

impl ListPat {
    pub fn new_in(
        items: impl IntoIterator<Item = ListElPat>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let ids = items.into_iter().map(|item| builder.nodes.alloc(item));
        let slice_id = builder.slices.alloc(ids);
        builder.alloc(Self(slice_id))
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
#[notate(with = TreePrinter<'a>, color = "magenta")]
pub struct RecordFieldPat {
    pub field: Id<ValueName>,
    pub pat: Option<Id<Pat>>,
}

impl RecordFieldPat {
    pub fn field(self, storage: &impl TreeView) -> ValueName {
        *self.field.get(storage)
    }

    pub fn pat(self, storage: &impl TreeView) -> Option<Pat> {
        self.pat.map(|id| id.get(storage)).copied()
    }
}

#[derive(
    Debug, Inspector, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct RecordPat {
    pub fields: SliceId<RecordFieldPat>,
    pub polymorph: bool,
}

impl RecordPat {
    pub fn new_in<I>(fields: I, polymorph: bool, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator<Item = RecordFieldPat>,
    {
        let ids = fields.into_iter().map(|field| builder.nodes.alloc(field));

        let fields = builder.slices.alloc(ids);

        builder.alloc(Self { fields, polymorph })
    }
}

impl<'a> Notate<'a, RecordPat> for TreePrinter<'a> {
    fn notate(self, value: &RecordPat, arena: &'a Bump) -> Notation<'a> {
        let head = "RecordPat".magenta().display_in(arena);

        let fields = value
            .fields
            .get(self.tree)
            .iter()
            .map(|field| self.notate(field, arena))
            .collect_in::<BumpVec<_>>(arena);
        let polymorph = value.polymorph;

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
#[notate(with = TreePrinter<'a>)]
pub struct VariantTagPat {
    pub tag: Id<ValueName>,
    pub pat: Option<Id<Pat>>,
}

impl VariantTagPat {
    pub fn case(self, storage: &impl TreeView) -> ValueName {
        *self.tag.get(storage)
    }

    pub fn pat(self, storage: &impl TreeView) -> Option<Pat> {
        self.pat.map(|id| id.get(storage)).copied()
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
#[notate(with = TreePrinter<'a>, color = "blue")]
pub struct VariantPat(pub SliceId<VariantTagPat>);

impl VariantPat {
    pub fn new_in(
        items: impl IntoIterator<Item = VariantTagPat>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let ids = items.into_iter().map(|item| builder.nodes.alloc(item));

        let slice_id = builder.slices.alloc(ids);
        builder.alloc(Self(slice_id))
    }
}

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

impl<'a> Notate<'a, Pat> for TreePrinter<'a> {
    fn notate(self, value: &Pat, arena: &'a Bump) -> Notation<'a> {
        match value {
            Pat::Error(e) => self.notate(e, arena),
            Pat::Any(w) => self.notate(w, arena),
            Pat::Literal(l) => self.notate(l, arena),
            Pat::Bind(i) => self.notate(i, arena),
            Pat::List(l) => self.notate(l, arena),
            Pat::Record(r) => self.notate(r, arena),
            Pat::Variant(v) => self.notate(v, arena),
        }
    }
}
