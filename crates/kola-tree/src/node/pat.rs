use derive_more::{From, IntoIterator};
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, ops::Deref};

use kola_print::prelude::*;
use kola_utils::{as_variant, interner::StrKey};

use super::LiteralExpr;
use crate::{
    id::Id,
    node::ValueName,
    print::NodePrinter,
    tree::{TreeBuilder, TreeView},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PatError;

impl<'a> Notate<'a> for NodePrinter<'a, PatError> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        "PatError".red().display_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AnyPat;

impl<'a> Notate<'a> for NodePrinter<'a, AnyPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        "AnyPat".blue().display_in(arena)
    }
}

#[derive(Debug, From, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum LiteralPat {
    Unit,
    Bool(bool),
    Num(f64),
    Char(char),
    Str(StrKey),
}

impl LiteralPat {
    pub fn to_bool(&self) -> Option<bool> {
        as_variant!(self, Self::Bool).copied()
    }

    pub fn to_num(&self) -> Option<f64> {
        as_variant!(self, Self::Num).copied()
    }

    pub fn to_char(&self) -> Option<char> {
        as_variant!(self, Self::Char).copied()
    }

    pub fn to_str(&self) -> Option<&StrKey> {
        as_variant!(self, Self::Str)
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Self::Str(_))
    }
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
        let kind = "LiteralPat".purple().display_in(arena);

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
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[from(forward)]
pub struct BindPat(pub StrKey);

impl BindPat {
    #[inline]
    pub fn as_str_key(&self) -> &StrKey {
        &self.0
    }
}

impl Deref for BindPat {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<StrKey> for BindPat {
    #[inline]
    fn as_ref(&self) -> &StrKey {
        &self.0
    }
}

impl Borrow<StrKey> for BindPat {
    #[inline]
    fn borrow(&self) -> &StrKey {
        &self.0
    }
}

impl PartialEq<StrKey> for BindPat {
    #[inline]
    fn eq(&self, other: &StrKey) -> bool {
        self == other
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, BindPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "BindPat".cyan().display_in(arena);

        let bind = self
            .interner
            .get(self.value.0)
            .expect("Symbol not found")
            .yellow()
            .display_in(arena)
            .enclose_by(arena.just('"'), arena);

        let single = [arena.just(' '), bind.clone()].concat_in(arena);
        let multi = [arena.newline(), bind].concat_in(arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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
                let head = "ListElPat".green().display_in(arena);
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

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ListPat(pub Vec<Id<ListElPat>>);

impl<'a> Notate<'a> for NodePrinter<'a, ListPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "ListPat".blue().display_in(arena);

        let elements = self.to_slice(&self.value.0).gather(arena);

        let single = elements.clone().concat_map(
            |element| arena.notate(" ").then(element.flatten(arena), arena),
            arena,
        );
        let multi = elements.concat_map(|element| arena.newline().then(element, arena), arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

impl<'a> Notate<'a> for NodePrinter<'a, RecordFieldPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordFieldPat { field, pat } = *self.value;

        let head = "RecordFieldPat".blue().display_in(arena);

        let field = self.to_id(field).notate(arena);
        let pat = pat.map(|v| self.to_id(v).notate(arena));

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

    // pub fn get(&self, name: impl AsRef<str>, tree: &impl TreeView) -> Option<RecordFieldPat> {
    //     self.fields.iter().find_map(|id| {
    //         let field = id.get(tree);
    //         (field.field(tree) == name.as_ref())
    //             .then_some(field)
    //             .copied()
    //     })
    // }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "RecordPat".blue().display_in(arena);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

impl<'a> Notate<'a> for NodePrinter<'a, VariantTagPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let VariantTagPat { tag, pat } = *self.value;

        let head = "VariantCasePat".blue().display_in(arena);

        let case = self.to_id(tag).notate(arena);
        let pat = pat.map(|v| self.to_id(v).notate(arena));

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

#[derive(
    Debug, From, IntoIterator, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[into_iterator(owned, ref)]
pub struct VariantPat(pub Vec<Id<VariantTagPat>>);

// impl VariantPat {
//     pub fn get(&self, name: impl AsRef<str>, tree: &impl TreeView) -> Option<VariantCasePat> {
//         self.0.iter().find_map(|id| {
//             let case = id.get(tree);
//             (case.case(tree) == name.as_ref()).then_some(case).copied()
//         })
//     }
// }

impl<'a> Notate<'a> for NodePrinter<'a, VariantPat> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "VariantPat".blue().display_in(arena);

        let cases = self.to_slice(&self.value.0).gather(arena);

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

impl Pat {
    #[inline]
    pub fn to_error(self) -> Option<Id<PatError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_wildcard(self) -> Option<Id<AnyPat>> {
        as_variant!(self, Self::Any)
    }

    #[inline]
    pub fn to_literal(self) -> Option<Id<LiteralPat>> {
        as_variant!(self, Self::Literal)
    }

    #[inline]
    pub fn to_bind(self) -> Option<Id<BindPat>> {
        as_variant!(self, Self::Bind)
    }

    #[inline]
    pub fn to_list(self) -> Option<Id<ListPat>> {
        as_variant!(self, Self::List)
    }

    #[inline]
    pub fn to_record(self) -> Option<Id<RecordPat>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_variant(self) -> Option<Id<VariantPat>> {
        as_variant!(self, Self::Variant)
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
    pub fn is_bind(self) -> bool {
        matches!(self, Self::Bind(_))
    }

    #[inline]
    pub fn is_list(self) -> bool {
        matches!(self, Self::List(_))
    }

    #[inline]
    pub fn is_record(self) -> bool {
        matches!(self, Self::Record(_))
    }

    #[inline]
    pub fn is_variant(self) -> bool {
        matches!(self, Self::Variant(_))
    }
}

mod inspector {
    use std::hash::BuildHasher;

    use super::*;
    use crate::inspector::*;
    impl<'t, S: BuildHasher> NodeInspector<'t, Id<Pat>, S> {
        pub fn as_error(self) -> Option<NodeInspector<'t, Id<PatError>, S>> {
            let pat = self.node.get(self.tree);
            pat.to_error()
                .map(|err_id| NodeInspector::new(err_id, self.tree, self.interner))
        }

        pub fn as_any(self) -> Option<NodeInspector<'t, Id<AnyPat>, S>> {
            let pat = self.node.get(self.tree);
            pat.to_wildcard()
                .map(|wild_id| NodeInspector::new(wild_id, self.tree, self.interner))
        }

        pub fn as_literal(self) -> Option<NodeInspector<'t, Id<LiteralPat>, S>> {
            let pat = self.node.get(self.tree);
            pat.to_literal()
                .map(|lit_id| NodeInspector::new(lit_id, self.tree, self.interner))
        }

        pub fn as_bind(self) -> Option<NodeInspector<'t, Id<BindPat>, S>> {
            let pat = self.node.get(self.tree);
            pat.to_bind()
                .map(|id_id| NodeInspector::new(id_id, self.tree, self.interner))
        }

        pub fn as_record(self) -> Option<NodeInspector<'t, Id<RecordPat>, S>> {
            let pat = self.node.get(self.tree);
            pat.to_record()
                .map(|rec_id| NodeInspector::new(rec_id, self.tree, self.interner))
        }

        pub fn as_list(self) -> Option<NodeInspector<'t, Id<ListPat>, S>> {
            let pat = self.node.get(self.tree);
            pat.to_list()
                .map(|list_id| NodeInspector::new(list_id, self.tree, self.interner))
        }

        pub fn as_variant(self) -> Option<NodeInspector<'t, Id<VariantPat>, S>> {
            let pat = self.node.get(self.tree);
            pat.to_variant()
                .map(|var_id| NodeInspector::new(var_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<AnyPat>, S> {
        pub fn is_any(self) -> Self {
            let _ = self.node.get(self.tree);
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<LiteralPat>, S> {
        pub fn is_unit(self) -> Self {
            let lit_pat = self.node.get(self.tree);
            match lit_pat {
                LiteralPat::Unit => {}
                _ => panic!("Expected unit literal pattern but found {:?}", lit_pat),
            }
            self
        }

        pub fn is_bool(self, expected: bool) -> Self {
            let lit_pat = self.node.get(self.tree);
            match lit_pat {
                LiteralPat::Bool(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected bool {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected bool literal pattern but found {:?}", lit_pat),
            }
            self
        }

        pub fn is_num(self, expected: f64) -> Self {
            let lit_pat = self.node.get(self.tree);
            match lit_pat {
                LiteralPat::Num(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected num {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected num literal pattern but found {:?}", lit_pat),
            }
            self
        }

        pub fn is_char(self, expected: char) -> Self {
            let lit_pat = self.node.get(self.tree);
            match lit_pat {
                LiteralPat::Char(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected char {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected char literal pattern but found {:?}", lit_pat),
            }
            self
        }

        pub fn is_string(self, expected: &str) -> Self {
            let lit_pat = self.node.get(self.tree);
            match lit_pat {
                LiteralPat::Str(value) => {
                    let value = self.interner.get(*value).expect("Symbol not found");

                    assert_eq!(
                        value, expected,
                        "Expected string \"{}\" but found \"{}\"",
                        expected, value
                    );
                }
                _ => panic!("Expected string literal pattern but found {:?}", lit_pat),
            }
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<BindPat>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let bind = self.node.get(self.tree);
            let s = self.interner.get(bind.0).expect("Symbol not found");

            assert_eq!(
                s, expected,
                "Expected bind pattern '{}' but found '{}'",
                expected, s
            );
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ListPat>, S> {
        pub fn has_elements(self, count: usize) -> Self {
            let elements_len = self.node.get(self.tree).0.len();
            assert_eq!(
                elements_len, count,
                "Expected {} elements but found {}",
                count, elements_len
            );
            self
        }

        pub fn element_at(self, index: usize) -> NodeInspector<'t, Id<ListElPat>, S> {
            let list_pat = self.node.get(self.tree);
            assert!(
                index < list_pat.0.len(),
                "Element index {} out of bounds (max {})",
                index,
                list_pat.0.len() - 1
            );
            let element_id = list_pat.0[index];
            NodeInspector::new(element_id, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ListElPat>, S> {
        pub fn as_pattern(self) -> Option<NodeInspector<'t, Id<Pat>, S>> {
            let element = self.node.get(self.tree);
            match element {
                ListElPat::Pat(pat_id) => {
                    Some(NodeInspector::new(*pat_id, self.tree, self.interner))
                }
                _ => None,
            }
        }

        pub fn as_spread(self) -> Option<Option<NodeInspector<'t, Id<ValueName>, S>>> {
            let element = self.node.get(self.tree);
            match element {
                ListElPat::Spread(name_opt) => Some(
                    name_opt.map(|name_id| NodeInspector::new(name_id, self.tree, self.interner)),
                ),
                _ => None,
            }
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordPat>, S> {
        pub fn has_fields(self, count: usize) -> Self {
            let fields_len = self.node.get(self.tree).fields.len();
            assert_eq!(
                fields_len, count,
                "Expected {} fields but found {}",
                count, fields_len
            );
            self
        }

        pub fn field_at(self, index: usize) -> NodeInspector<'t, Id<RecordFieldPat>, S> {
            let record_pat = self.node.get(self.tree);
            assert!(
                index < record_pat.fields.len(),
                "Field index {} out of bounds (max {})",
                index,
                record_pat.fields.len() - 1
            );
            let field_id = record_pat.fields[index];
            NodeInspector::new(field_id, self.tree, self.interner)
        }

        pub fn field_named(self, name: &str) -> Option<NodeInspector<'t, Id<RecordFieldPat>, S>> {
            let record_pat = self.node.get(self.tree);
            record_pat
                .fields
                .iter()
                .find(|id| {
                    let field = id.get(self.tree).field(self.tree);
                    self.interner.get(field.0).expect("Symbol not found") == name
                })
                .map(|field_id| NodeInspector::new(*field_id, self.tree, self.interner))
        }

        pub fn is_polymorphic(self) -> Self {
            let record_pat = self.node.get(self.tree);
            assert!(
                record_pat.polymorph,
                "Expected polymorphic record pattern but found non-polymorphic"
            );
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordFieldPat>, S> {
        pub fn has_field_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).field(self.tree);
            let value = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                value, expected,
                "Expected field pattern name '{}' but found '{}'",
                expected, value
            );
            self
        }

        pub fn pattern(self) -> Option<NodeInspector<'t, Id<Pat>, S>> {
            let field_pat = self.node.get(self.tree);
            field_pat
                .pat
                .map(|pat_id| NodeInspector::new(pat_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<VariantPat>, S> {
        pub fn has_cases(self, count: usize) -> Self {
            let cases_len = self.node.get(self.tree).0.len();
            assert_eq!(
                cases_len, count,
                "Expected {} cases but found {}",
                count, cases_len
            );
            self
        }

        pub fn case_at(self, index: usize) -> NodeInspector<'t, Id<VariantTagPat>, S> {
            let variant_pat = self.node.get(self.tree);
            assert!(
                index < variant_pat.0.len(),
                "Case index {} out of bounds (max {})",
                index,
                variant_pat.0.len() - 1
            );
            let case_id = variant_pat.0[index];
            NodeInspector::new(case_id, self.tree, self.interner)
        }

        pub fn case_named(self, name: &str) -> Option<NodeInspector<'t, Id<VariantTagPat>, S>> {
            let variant_pat = self.node.get(self.tree);
            variant_pat
                .0
                .iter()
                .find(|id| {
                    let variant = id.get(self.tree).case(self.tree);
                    self.interner.get(variant.0).expect("Symbol not found") == name
                })
                .map(|case_id| NodeInspector::new(*case_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<VariantTagPat>, S> {
        pub fn has_case_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).case(self.tree);
            let value = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                value, expected,
                "Expected variant case name '{}' but found '{}'",
                expected, value
            );
            self
        }

        pub fn pattern(self) -> Option<NodeInspector<'t, Id<Pat>, S>> {
            let case_pat = self.node.get(self.tree);
            case_pat
                .pat
                .map(|pat_id| NodeInspector::new(pat_id, self.tree, self.interner))
        }
    }
}
