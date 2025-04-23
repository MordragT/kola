use derive_more::From;
use kola_print::prelude::*;
use kola_utils::as_variant;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{LiteralExpr, Name, Symbol};
use crate::{id::Id, print::TreePrinter, tree::TreeAccess};

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

// impl LiteralPat {
//     pub fn to_bool(&self) -> Option<bool> {
//         as_variant!(self, Self::Bool).copied()
//     }

//     pub fn to_num(&self) -> Option<f64> {
//         as_variant!(self, Self::Num).copied()
//     }

//     pub fn to_char(&self) -> Option<char> {
//         as_variant!(self, Self::Char).copied()
//     }

//     pub fn to_str(&self) -> Option<&Symbol> {
//         as_variant!(self, Self::Str)
//     }

//     pub fn is_bool(&self) -> bool {
//         matches!(self, Self::Bool(_))
//     }

//     pub fn is_num(&self) -> bool {
//         matches!(self, Self::Num(_))
//     }

//     pub fn is_char(&self) -> bool {
//         matches!(self, Self::Char(_))
//     }

//     pub fn is_str(&self) -> bool {
//         matches!(self, Self::Str(_))
//     }
// }

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
    pub field: Id<Name>,
    pub pat: Option<Id<Pat>>,
}

impl RecordFieldPat {
    pub fn field(self, tree: &impl TreeAccess) -> &Name {
        self.field.get(tree)
    }

    pub fn pat(self, tree: &impl TreeAccess) -> Option<Pat> {
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
pub struct RecordPat(pub Vec<Id<RecordFieldPat>>);

impl RecordPat {
    pub fn get(&self, name: impl AsRef<str>, tree: &impl TreeAccess) -> Option<RecordFieldPat> {
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
    pub case: Id<Name>,
    pub pat: Option<Id<Pat>>,
}

impl VariantCasePat {
    pub fn case(self, tree: &impl TreeAccess) -> &Name {
        self.case.get(tree)
    }

    pub fn pat(self, tree: &impl TreeAccess) -> Option<Pat> {
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
pub struct VariantPat(pub Vec<Id<VariantCasePat>>);

impl VariantPat {
    pub fn get(&self, name: impl AsRef<str>, tree: &impl TreeAccess) -> Option<VariantCasePat> {
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
    Error(Id<PatError>),
    Any(Id<AnyPat>),
    Literal(Id<LiteralPat>),
    Ident(Id<IdentPat>),
    Record(Id<RecordPat>),
    Variant(Id<VariantPat>),
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
    pub fn to_ident(self) -> Option<Id<IdentPat>> {
        as_variant!(self, Self::Ident)
    }

    #[inline]
    pub fn to_record(self) -> Option<Id<RecordPat>> {
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

mod inspector {
    use super::*;
    use crate::inspector::*;

    impl<'t> NodeInspector<'t, Id<Pat>> {
        /// Check if this pattern is an error pattern
        pub fn as_error(self) -> Option<NodeInspector<'t, Id<PatError>>> {
            let pat = self.node.get(self.tree);
            pat.to_error()
                .map(|err_id| NodeInspector::new(err_id, self.tree))
        }

        /// Check if this pattern is a wildcard pattern
        pub fn as_any(self) -> Option<NodeInspector<'t, Id<AnyPat>>> {
            let pat = self.node.get(self.tree);
            pat.to_wildcard()
                .map(|wild_id| NodeInspector::new(wild_id, self.tree))
        }

        /// Check if this pattern is a literal pattern
        pub fn as_literal(self) -> Option<NodeInspector<'t, Id<LiteralPat>>> {
            let pat = self.node.get(self.tree);
            pat.to_literal()
                .map(|lit_id| NodeInspector::new(lit_id, self.tree))
        }

        /// Check if this pattern is an identifier pattern
        pub fn as_ident(self) -> Option<NodeInspector<'t, Id<IdentPat>>> {
            let pat = self.node.get(self.tree);
            pat.to_ident()
                .map(|id_id| NodeInspector::new(id_id, self.tree))
        }

        /// Check if this pattern is a record pattern
        pub fn as_record(self) -> Option<NodeInspector<'t, Id<RecordPat>>> {
            let pat = self.node.get(self.tree);
            pat.to_record()
                .map(|rec_id| NodeInspector::new(rec_id, self.tree))
        }

        /// Check if this pattern is a variant pattern
        pub fn as_variant(self) -> Option<NodeInspector<'t, Id<VariantPat>>> {
            let pat = self.node.get(self.tree);
            match pat {
                Pat::Variant(v) => Some(NodeInspector::new(*v, self.tree)),
                _ => None,
            }
        }
    }

    impl<'t> NodeInspector<'t, Id<AnyPat>> {
        /// Simple assertion that this is indeed a wildcard pattern
        pub fn is_any(self) -> Self {
            // Just verify that we have a valid any pattern
            let _ = self.node.get(self.tree);
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<LiteralPat>> {
        /// Check what kind of literal pattern this is
        pub fn is_bool(self, expected: bool) -> Self {
            let lit_pat = self.node.get(self.tree);
            match &lit_pat.0 {
                LiteralExpr::Bool(value) => {
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
            match &lit_pat.0 {
                LiteralExpr::Num(value) => {
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
            match &lit_pat.0 {
                LiteralExpr::Char(value) => {
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
            match &lit_pat.0 {
                LiteralExpr::Str(value) => {
                    assert_eq!(
                        value.as_str(),
                        expected,
                        "Expected string \"{}\" but found \"{}\"",
                        expected,
                        value
                    );
                }
                _ => panic!("Expected string literal pattern but found {:?}", lit_pat),
            }
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<IdentPat>> {
        /// Assert the identifier pattern has the expected name
        pub fn has_name(self, expected: &str) -> Self {
            let ident = self.node.get(self.tree);
            assert_eq!(
                ident.0.as_str(),
                expected,
                "Expected identifier pattern '{}' but found '{}'",
                expected,
                ident.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<RecordPat>> {
        /// Assert the record pattern has the specified number of fields
        pub fn has_fields(self, count: usize) -> Self {
            let fields_len = self.node.get(self.tree).0.len();
            assert_eq!(
                fields_len, count,
                "Expected {} fields but found {}",
                count, fields_len
            );
            self
        }

        /// Get an inspector for the field at the given index
        pub fn field_at(self, index: usize) -> NodeInspector<'t, Id<RecordFieldPat>> {
            let record_pat = self.node.get(self.tree);
            assert!(
                index < record_pat.0.len(),
                "Field index {} out of bounds (max {})",
                index,
                record_pat.0.len() - 1
            );
            let field_id = record_pat.0[index];
            NodeInspector::new(field_id, self.tree)
        }

        /// Get an inspector for the field with the given name, if it exists
        pub fn field_named(self, name: &str) -> Option<NodeInspector<'t, Id<RecordFieldPat>>> {
            let record_pat = self.node.get(self.tree);
            record_pat.get(name, self.tree).map(|_| {
                let field_id = record_pat
                    .0
                    .iter()
                    .find(|id| id.get(self.tree).field(self.tree).as_str() == name)
                    .unwrap();
                NodeInspector::new(*field_id, self.tree)
            })
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<RecordFieldPat>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).field(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<RecordFieldPat>> {
        /// Assert the record field pattern has the specified name
        pub fn has_field_name(self, expected: &str) -> Self {
            self.assert_name(expected, "field pattern")
        }

        /// Get an inspector for the pattern if it has one
        pub fn pattern(self) -> Option<NodeInspector<'t, Id<Pat>>> {
            let field_pat = self.node.get(self.tree);
            field_pat
                .pat
                .map(|pat_id| NodeInspector::new(pat_id, self.tree))
        }
    }

    impl<'t> NodeInspector<'t, Id<VariantPat>> {
        /// Assert the variant pattern has the specified number of cases
        pub fn has_cases(self, count: usize) -> Self {
            let cases_len = self.node.get(self.tree).0.len();
            assert_eq!(
                cases_len, count,
                "Expected {} cases but found {}",
                count, cases_len
            );
            self
        }

        /// Get an inspector for the case at the given index
        pub fn case_at(self, index: usize) -> NodeInspector<'t, Id<VariantCasePat>> {
            let variant_pat = self.node.get(self.tree);
            assert!(
                index < variant_pat.0.len(),
                "Case index {} out of bounds (max {})",
                index,
                variant_pat.0.len() - 1
            );
            let case_id = variant_pat.0[index];
            NodeInspector::new(case_id, self.tree)
        }

        /// Get an inspector for the case with the given name, if it exists
        pub fn case_named(self, name: &str) -> Option<NodeInspector<'t, Id<VariantCasePat>>> {
            let variant_pat = self.node.get(self.tree);
            variant_pat.get(name, self.tree).map(|_| {
                let case_id = variant_pat
                    .0
                    .iter()
                    .find(|id| id.get(self.tree).case(self.tree).as_str() == name)
                    .unwrap();
                NodeInspector::new(*case_id, self.tree)
            })
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, Id<VariantCasePat>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).case(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, Id<VariantCasePat>> {
        /// Assert the variant case pattern has the specified name
        pub fn has_case_name(self, expected: &str) -> Self {
            self.assert_name(expected, "variant case")
        }

        /// Get an inspector for the pattern if it has one
        pub fn pattern(self) -> Option<NodeInspector<'t, Id<Pat>>> {
            let case_pat = self.node.get(self.tree);
            case_pat
                .pat
                .map(|pat_id| NodeInspector::new(pat_id, self.tree))
        }
    }
}
