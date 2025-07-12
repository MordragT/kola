//! # Pattern Matching Exhaustiveness Analysis
//!
//! This module implements exhaustiveness checking for pattern matching in case expressions.
//! The algorithm determines whether a set of patterns covers all possible values of a given type.
//!
//! ## Algorithm Overview
//!
//! The exhaustiveness checker works by:
//!
//! 1. **Computing Required Sets**: For each type, determine what pattern coverage is needed
//!    for exhaustiveness (via `RequiredSet` trait)
//!
//! 2. **Computing Actual Sets**: For each pattern, determine what values it actually covers
//!    (via `ActualSet` trait)
//!
//! 3. **Union Operation**: Combine all pattern sets using set union to get total coverage
//!
//! 4. **Superset Check**: Verify that actual coverage is a superset of required coverage
//!
//! ## Set Types
//!
//! - **`CoverSet`**: Top-level abstraction representing what values patterns can match
//! - **`AtomSet`**: Finite sets of atomic values (booleans, unit, etc.)
//! - **`ListSet`**: Length-based coverage for lists (exact lengths, at-least constraints)
//! - **`RowSet`**: Label-based coverage for records and variants (with row reordering)
//!
//! ## Key Algorithms
//!
//! ### List Exhaustiveness
//! Lists are analyzed by length constraints:
//! - `[]` covers exactly length 0
//! - `[a, b]` covers exactly length 2
//! - `[a, ...rest]` covers lengths ≥ 1
//! - Union: `{0, 1} ∪ {2+} = Universal`
//!
//! ### Row Exhaustiveness
//! Records/variants use row reordering for label matching:
//! - `{x: Bool, y: Int}` requires coverage of both fields
//! - Row polymorphism (`{x: Bool, ...}`) requires universal tail coverage
//! - Labels can appear in any order due to reordering semantics

use std::fmt;

use derive_more::Display;
use enumset::{EnumSet, EnumSetType};
use kola_collections::OrdSet;
use kola_span::{Diagnostic, Loc};
use kola_syntax::loc::Locations;
use kola_tree::prelude::*;
use kola_utils::{as_variant, errors::Errors};

use crate::{
    phase::TypedNodes,
    types::{Label, LabelOrVar, ListType, MonoType, PrimitiveType, RowType},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExhaustError {
    pub case: Id<node::CaseExpr>,
    pub mono_t: MonoType,
    pub actual: CoverSet,
    pub required: CoverSet,
    pub loc: Loc,
}

impl From<ExhaustError> for Diagnostic {
    fn from(error: ExhaustError) -> Self {
        let ExhaustError {
            mono_t,
            actual,
            required,
            loc,
            ..
        } = error;

        Diagnostic::error(loc, "Exhaustiveness error in case expression").with_notes([
            format!("Type: {}", mono_t),
            format!("Required coverage: {}", required),
            format!("Actual coverage: {}", actual),
        ])
    }
}

pub type ExhaustResult<T> = Result<T, ExhaustError>;

/// Check all tracked case expressions
pub fn exhaust_check_all<T: TreeView>(
    cases: &[Id<node::CaseExpr>],
    tree: &T,
    types: &TypedNodes,
    spans: &Locations,
) -> Result<(), Errors<ExhaustError>> {
    let mut errors = Errors::new();

    for &case_id in cases {
        let loc = *spans.meta(case_id);
        let checker = ExhaustChecker::new(case_id, tree, types, loc);
        if let Err(error) = checker.check() {
            errors.push(error);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[derive(EnumSetType, Display, Debug, Hash)]
pub enum Atom {
    True,
    False,
    Unit,
}

pub type AtomSet = EnumSet<Atom>;

/// Represents pattern matching sets for list types based on length constraints
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ListSet {
    /// Matches no lists
    Empty,
    /// Matches all possible lists
    Universal,
    /// Matches lists with specific exact lengths
    Exact(OrdSet<u32>),
    /// Matches lists with length >= n
    AtLeast(u32),
    /// Matches both exact lengths and lengths >= n
    Combined { exact: OrdSet<u32>, at_least: u32 },
}

impl ListSet {
    /// Create coverage for empty list only
    pub fn empty_list() -> Self {
        Self::Exact([0].into_iter().collect())
    }

    /// Create coverage for exactly one length
    pub fn exact_length(len: u32) -> Self {
        Self::Exact([len].into_iter().collect())
    }

    /// Create coverage for lists of at least N elements
    pub fn at_least(min_len: u32) -> Self {
        if min_len == 0 {
            Self::Universal
        } else {
            Self::AtLeast(min_len)
        }
    }

    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Universal, _) | (_, Self::Universal) => Self::Universal,
            (Self::Empty, other) | (other, Self::Empty) => other,

            // Combine exact sets
            (Self::Exact(mut a), Self::Exact(b)) => {
                a.extend(b);
                Self::Exact(a)
            }

            // Combine at-least (take minimum)
            (Self::AtLeast(a), Self::AtLeast(b)) => Self::AtLeast(a.min(b)),

            // Combine exact with at-least
            (Self::Exact(exact), Self::AtLeast(min)) | (Self::AtLeast(min), Self::Exact(exact)) => {
                // Check if exact lengths fill the gap to make it universal
                if (0..min).all(|len| exact.contains(&len)) {
                    Self::Universal
                } else {
                    Self::Combined {
                        exact,
                        at_least: min,
                    }
                }
            }

            // Other combinations...
            (a, b) => {
                // Normalize to Combined representation
                let (exact_a, at_least_a) = a.to_parts();
                let (exact_b, at_least_b) = b.to_parts();

                let mut combined_exact = exact_a;
                combined_exact.extend(exact_b);

                let combined_at_least = match (at_least_a, at_least_b) {
                    (Some(a), Some(b)) => Some(a.min(b)),
                    (Some(a), None) | (None, Some(a)) => Some(a),
                    (None, None) => None,
                };

                match combined_at_least {
                    Some(min) => {
                        // Check if this becomes universal
                        if min == 0 || (0..min).all(|len| combined_exact.contains(&len)) {
                            Self::Universal
                        } else {
                            Self::Combined {
                                exact: combined_exact,
                                at_least: min,
                            }
                        }
                    }
                    None => Self::Exact(combined_exact),
                }
            }
        }
    }

    /// Check if this set is a superset of another
    pub fn is_superset(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Universal, _) => true,
            (_, Self::Empty) => true,
            (Self::Empty, _) => false,

            (Self::Exact(a), Self::Exact(b)) => b.iter().all(|len| a.contains(len)),

            (Self::AtLeast(min_a), Self::AtLeast(min_b)) => min_a <= min_b,

            (Self::AtLeast(min), Self::Exact(lengths)) => lengths.iter().all(|&len| len >= *min),

            (Self::Exact(_), Self::AtLeast(_)) => false, // Exact can't cover open-ended

            (Self::Combined { exact, at_least }, Self::Exact(other_exact)) => other_exact
                .iter()
                .all(|&len| exact.contains(&len) || len >= *at_least),

            (Self::Combined { exact: _, at_least }, Self::AtLeast(other_min)) => {
                at_least <= other_min
            }

            (
                Self::Combined {
                    exact: a_exact,
                    at_least: a_min,
                },
                Self::Combined {
                    exact: b_exact,
                    at_least: b_min,
                },
            ) => {
                // Check if all exact lengths in b are covered by a
                let exact_covered = b_exact
                    .iter()
                    .all(|&len| a_exact.contains(&len) || len >= *a_min);
                // Check if a's at_least covers b's at_least
                let at_least_covered = a_min <= b_min;

                exact_covered && at_least_covered
            }

            // Conservative for remaining cases
            _ => false,
        }
    }

    fn to_parts(self) -> (OrdSet<u32>, Option<u32>) {
        match self {
            Self::Empty => (OrdSet::new(), None),
            Self::Universal => (OrdSet::new(), Some(0)),
            Self::Exact(set) => (set, None),
            Self::AtLeast(min) => (OrdSet::new(), Some(min)),
            Self::Combined { exact, at_least } => (exact, Some(at_least)),
        }
    }
}

impl fmt::Display for ListSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "[]"),
            Self::Universal => write!(f, "[...]"),
            Self::Exact(set) => {
                let lengths: Vec<_> = set.iter().collect();
                write!(
                    f,
                    "[{}]",
                    lengths
                        .iter()
                        .map(|l| l.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::AtLeast(min) => write!(f, "[{}+]", min),
            Self::Combined { exact, at_least } => {
                let lengths: Vec<_> = exact.iter().collect();
                write!(
                    f,
                    "[{}+ (exact: {})]",
                    at_least,
                    lengths
                        .iter()
                        .map(|l| l.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabelledSet {
    pub label: Label,
    pub set: Box<CoverSet>,
}

impl fmt::Display for LabelledSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, " {}: {} ", self.label, self.set)
    }
}

/// Represents pattern matching sets for row types (records/variants)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RowSet {
    /// Matches empty rows only
    Empty,
    /// Matches all possible rows
    Universal,
    /// Matches rows with specific labeled fields
    Extension {
        head: LabelledSet,
        tail: Box<RowSet>,
    },
}

impl RowSet {
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Universal, _) | (_, Self::Universal) => Self::Universal,
            (Self::Empty, other) | (other, Self::Empty) => other,

            (
                RowSet::Extension {
                    head: head1,
                    tail: tail1,
                },
                RowSet::Extension {
                    head: head2,
                    tail: tail2,
                },
            ) => {
                if head1.label == head2.label {
                    // Same label: union the sets
                    RowSet::Extension {
                        head: LabelledSet {
                            label: head1.label,
                            set: Box::new(head1.set.union(*head2.set)),
                        },
                        tail: Box::new((*tail1).union(*tail2)),
                    }
                } else {
                    // Different labels: reorder by keeping head1 and merging the rest
                    RowSet::Extension {
                        head: head1,
                        tail: Box::new((*tail1).union(RowSet::Extension {
                            head: head2,
                            tail: tail2,
                        })),
                    }
                }
            }
        }
    }

    /// Check if this row set covers all requirements of another row set.
    /// Implements row reordering by recursively searching for required labels.
    pub fn is_superset(&self, other: &Self) -> bool {
        match other {
            RowSet::Empty => true, // Empty row is always covered
            RowSet::Universal => matches!(self, RowSet::Universal), // Only universal covers universal
            RowSet::Extension { head, tail } => self.covers_head(head) && self.is_superset(tail),
        }
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        other.is_superset(self)
    }

    /// Check if this row can match a specific labeled field.
    /// Uses row reordering to search through the structure.
    fn covers_head(&self, other: &LabelledSet) -> bool {
        match self {
            RowSet::Empty => false,
            RowSet::Universal => true,
            RowSet::Extension { head, .. } if head.label == other.label => {
                // Found matching label - check if sets are compatible
                head.set.is_superset(&other.set)
            }
            RowSet::Extension { tail, .. } => {
                // Label not found in head - search in tail
                tail.covers_head(other)
            }
        }
    }
}

impl fmt::Display for RowSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "{{}}"),
            Self::Universal => write!(f, "{{...}}"),
            Self::Extension { head, tail } => {
                write!(f, "{{{}}}", head)?;
                if let RowSet::Extension { .. } = **tail {
                    write!(f, ", ")?;
                }
                write!(f, "{}", tail)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents the set of values that patterns can match against
pub enum CoverSet {
    /// Matches nothing at all (universal empty)
    Empty,
    /// Matches all possible values (wildcard patterns)
    Universal,
    /// Matches a finite set of atomic values (literals)
    Atom(AtomSet),
    /// Matches row types with labeled fields
    Row(RowSet),
    /// Matches list types with length constraints
    List(ListSet),
    /// Unknown or complex matching behavior
    Opaque,
}

impl CoverSet {
    pub fn as_atom_set(&self) -> Option<&AtomSet> {
        as_variant!(self, Self::Atom)
    }

    pub fn as_list_set(&self) -> Option<&ListSet> {
        as_variant!(self, Self::List)
    }

    pub fn as_row_set(&self) -> Option<&RowSet> {
        as_variant!(self, Self::Row)
    }

    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Empty, other) | (other, Self::Empty) => other,
            (Self::Universal, _) | (_, Self::Universal) => Self::Universal,
            (Self::Atom(a), Self::Atom(b)) => Self::Atom(a.union(b)),
            (Self::Row(a), Self::Row(b)) => Self::Row(a.union(b)),
            (Self::List(a), Self::List(b)) => Self::List(a.union(b)),
            (_, _) => Self::Opaque,
        }
    }

    pub fn is_superset(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Empty) => true,
            (Self::Empty, _) => false,
            (Self::Universal, _) => true,
            (Self::Atom(a), Self::Atom(b)) => a.is_superset(*b),
            (Self::Row(a), Self::Row(b)) => a.is_superset(&b),
            (Self::List(a), Self::List(b)) => a.is_superset(b),
            (_, _) => false,
        }
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        other.is_superset(self)
    }
}

impl fmt::Display for CoverSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "∅"),
            Self::Universal => write!(f, "∀"),
            Self::Atom(set) => write!(f, "{{{}}}", set),
            Self::Row(row) => write!(f, "{}", row),
            Self::List(list) => write!(f, "{}", list),
            Self::Opaque => write!(f, "Opaque"),
        }
    }
}

/// Types that can determine what pattern coverage they require for exhaustiveness
pub trait RequiredSet {
    fn required_set(&self) -> CoverSet;
}

impl RequiredSet for PrimitiveType {
    fn required_set(&self) -> CoverSet {
        match self {
            PrimitiveType::Unit => CoverSet::Atom(AtomSet::only(Atom::Unit)),
            PrimitiveType::Bool => CoverSet::Atom(Atom::True | Atom::False),
            PrimitiveType::Num => CoverSet::Universal,
            PrimitiveType::Char => CoverSet::Universal,
            PrimitiveType::Str => CoverSet::Universal,
        }
    }
}

impl RequiredSet for ListType {
    fn required_set(&self) -> CoverSet {
        CoverSet::List(ListSet::Universal)
    }
}

/// ## Record Patterns
///
/// Records support two kinds of patterns:
/// - **Exact records**: `{ a, b }` - must have exactly fields `a` and `b`, no more
/// - **Polymorphic records**: `{ a, b, ... }` - must have at least fields `a` and `b`, may have more
///
/// ## Variant System
///
/// Variants are built from tag constructors where every uppercase identifier in value
/// position becomes a tag constructor with type `forall a r . a -> < Tag a | r >`.
///
/// We distinguish between:
/// - **Open variants**: `< Tag1 a | r >` - polymorphic with row variable `r`
/// - **Closed variants**: `< Some a, None >` - exact set of cases, no row variable
///
/// ## Exhaustiveness Strategy
///
/// For exhaustiveness checking, the presence or absence of a row variable determines
/// the required set:
///
/// 1. **Polymorphic rows** (with row variable): Require `Universal` set
///    because unknown cases/fields may exist
/// 2. **Exact rows** (no row variable): Require set covering all known cases/fields
///    because the set is finite and known
impl RequiredSet for RowType {
    fn required_set(&self) -> CoverSet {
        let row_set = match self {
            RowType::Empty => RowSet::Empty,
            RowType::Extension { head, tail } => {
                let LabelOrVar::Label(label) = head.label else {
                    todo!("Pattern Matching analysis was performed over type function")
                };

                let head_set = LabelledSet {
                    label,
                    set: Box::new(head.ty.required_set()),
                };
                let tail_set = match tail.required_set() {
                    CoverSet::Row(row) => row,
                    CoverSet::Universal => RowSet::Universal,
                    _ => panic!("Expected row set for tail"),
                };

                RowSet::Extension {
                    head: head_set,
                    tail: Box::new(tail_set),
                }
            }
        };

        CoverSet::Row(row_set)
    }
}

impl RequiredSet for MonoType {
    fn required_set(&self) -> CoverSet {
        match self {
            MonoType::Primitive(primitive) => primitive.required_set(),
            MonoType::List(list) => list.required_set(),
            MonoType::Row(row) => row.required_set(),
            MonoType::Var(_) => CoverSet::Universal, // Conservative but correct
            MonoType::Func(_) => CoverSet::Opaque,
            _ => CoverSet::Opaque,
        }
    }
}

/// Patterns that can determine what values they actually cover
pub trait ActualSet {
    fn actual_set(&self, tree: &impl TreeView) -> CoverSet;
}

impl ActualSet for Id<node::LiteralPat> {
    fn actual_set(&self, tree: &impl TreeView) -> CoverSet {
        match *self.get(tree) {
            node::LiteralPat::Bool(true) => CoverSet::Atom(Atom::True.into()),
            node::LiteralPat::Bool(false) => CoverSet::Atom(Atom::False.into()),
            node::LiteralPat::Unit => CoverSet::Atom(Atom::Unit.into()),
            _ => CoverSet::Opaque, // Other literals for now
        }
    }
}

impl ActualSet for Id<node::ListPat> {
    fn actual_set(&self, tree: &impl TreeView) -> CoverSet {
        let elements = &self.get(tree).0;

        if elements.is_empty() {
            // Empty list pattern: [] - covers only the empty list
            return CoverSet::List(ListSet::empty_list());
        }

        // Count non-spread elements and check for spread
        let mut non_spread_count = 0u32;
        let mut has_spread = false;

        for el_id in elements {
            match el_id.get(tree) {
                node::ListElPat::Spread(_) => {
                    has_spread = true;
                }
                node::ListElPat::Pat(_) => {
                    non_spread_count += 1;
                }
            }
        }

        if has_spread {
            // Patterns like `[x, ...rest]` or `[x, y, ...rest]` match lists
            // of at least the number of non-spread elements
            CoverSet::List(ListSet::at_least(non_spread_count))
        } else {
            // Patterns like `[x]` or `[x, y]` without spread match lists
            // of exactly the given length
            CoverSet::List(ListSet::exact_length(non_spread_count))
        }
    }
}

impl ActualSet for Id<node::RecordPat> {
    fn actual_set(&self, tree: &impl TreeView) -> CoverSet {
        let node::RecordPat { fields, polymorph } = self.get(tree);

        let tail = if *polymorph {
            RowSet::Universal
        } else {
            RowSet::Empty
        };

        // Process fields in reverse order to build the row set correctly
        let row_set = fields.iter().rev().fold(tail, |accu, field| {
            let node::RecordFieldPat { field, pat } = field.get(tree);
            let label = Label(field.get(tree).0);
            let set = pat
                .map(|pat| pat.actual_set(tree))
                .unwrap_or(CoverSet::Universal);

            RowSet::Extension {
                head: LabelledSet {
                    label,
                    set: Box::new(set),
                },
                tail: Box::new(accu),
            }
        });

        CoverSet::Row(row_set)
    }
}

impl ActualSet for Id<node::VariantPat> {
    fn actual_set(&self, tree: &impl TreeView) -> CoverSet {
        let tags = &self.get(tree).0;

        let row_set = tags.iter().rev().fold(RowSet::Empty, |accu, tag| {
            let node::VariantTagPat { tag, pat } = tag.get(tree);
            let label = Label(tag.get(tree).0);
            let set = pat
                .map(|pat| pat.actual_set(tree))
                .unwrap_or(CoverSet::Atom(Atom::Unit.into()));

            RowSet::Extension {
                head: LabelledSet {
                    label,
                    set: Box::new(set),
                },
                tail: Box::new(accu),
            }
        });

        CoverSet::Row(row_set)
    }
}

impl ActualSet for Id<node::Pat> {
    fn actual_set(&self, tree: &impl TreeView) -> CoverSet {
        match *self.get(tree) {
            node::Pat::Error(_) => todo!(),
            node::Pat::Any(_) | node::Pat::Bind(_) => CoverSet::Universal,
            node::Pat::Literal(lit) => lit.actual_set(tree),
            node::Pat::List(list) => list.actual_set(tree),
            node::Pat::Record(record) => record.actual_set(tree),
            node::Pat::Variant(variant) => variant.actual_set(tree),
        }
    }
}

// TODO this is essentially a function ? maybe just use a function instead of a struct
// Checker for a single case expression
pub struct ExhaustChecker<'a, T: TreeView> {
    case_id: Id<node::CaseExpr>,
    tree: &'a T,
    types: &'a TypedNodes,
    loc: Loc,
}

impl<'a, T: TreeView> ExhaustChecker<'a, T> {
    pub fn new(case_id: Id<node::CaseExpr>, tree: &'a T, types: &'a TypedNodes, loc: Loc) -> Self {
        Self {
            case_id,
            tree,
            types,
            loc,
        }
    }

    pub fn check(&self) -> ExhaustResult<()> {
        let node::CaseExpr { source, branches } = self.case_id.get(self.tree);
        let source_type = self.types.meta(*source);

        let actual_set = branches
            .iter()
            .map(|&branch_id| {
                let branch = branch_id.get(self.tree);
                branch.pat.actual_set(self.tree)
            })
            .fold(CoverSet::Empty, |acc, set| acc.union(set));

        let required_set = source_type.required_set();

        if actual_set.is_superset(&required_set) {
            Ok(())
        } else {
            Err(self.error(source_type.clone(), actual_set, required_set))
        }
    }

    fn error(&self, mono_t: MonoType, actual: CoverSet, required: CoverSet) -> ExhaustError {
        ExhaustError {
            case: self.case_id,
            mono_t,
            actual,
            required,
            loc: self.loc,
        }
    }
}
