use enumset::{EnumSet, EnumSetType};
use kola_collections::OrdSet;
use kola_span::{Diagnostic, Loc};
use kola_syntax::loc::Locations;
use kola_tree::prelude::*;
use kola_utils::{as_variant, errors::Errors, interner::StrKey};
use thiserror::Error;

use crate::{
    phase::TypedNodes,
    types::{ListType, MonoType, PrimitiveType, RowType},
};

#[derive(Debug, Error, Clone, PartialEq, Eq, Hash)]
#[error("Exhaustiveness error in case expression for {mono_t}")]
pub struct ExhaustError {
    pub case: Id<node::CaseExpr>,
    pub mono_t: MonoType,
    pub loc: Loc,
}

impl From<ExhaustError> for Diagnostic {
    fn from(error: ExhaustError) -> Self {
        Diagnostic::error(error.loc, error.to_string())
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

#[derive(EnumSetType, Debug, Hash)]
pub enum Atom {
    True,
    False,
    Unit,
}

pub type AtomSet = EnumSet<Atom>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ListSet {
    Empty,
    Universal,
    Exact(OrdSet<u32>),
    AtLeast(u32),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabelledSet {
    pub label: StrKey,
    pub set: Box<CoverSet>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RowSet {
    Empty,
    Universal,
    Extension {
        head: LabelledSet,
        tail: Box<RowSet>,
    },
}

impl RowSet {
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            // Universal absorbs everything
            (RowSet::Universal, _) | (_, RowSet::Universal) => RowSet::Universal,
            // Empty is identity
            (RowSet::Empty, other) | (other, RowSet::Empty) => other,
            // Extension ∪ Extension
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
                    // Same label: union the coverages
                    RowSet::Extension {
                        head: LabelledSet {
                            label: head1.label,
                            set: Box::new(head1.set.union(*head2.set)),
                        },
                        tail: Box::new((*tail1).union(*tail2)),
                    }
                } else {
                    // Different labels: need to reorder and merge
                    // Add head1, then try to union tail1 with the full other row
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

    /// Check if this row coverage can cover all requirements of another row coverage.
    /// This implements row reordering by searching for required labels recursively.
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

    /// Check if this row can cover a specific labeled requirement.
    /// Searches through the row structure using reordering semantics.
    fn covers_head(&self, other: &LabelledSet) -> bool {
        match self {
            RowSet::Empty => false,    // Empty row cannot cover anything
            RowSet::Universal => true, // Universal covers everything
            RowSet::Extension { head, .. } if head.label == other.label => {
                head.set.is_superset(&other.set)
            }
            RowSet::Extension { tail, .. } => tail.covers_head(other),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CoverSet {
    /// Covers all possible values, e.g., wildcard patterns
    Universal,
    /// Covers a finite set of values, e.g., specific literals or patterns
    Atom(AtomSet),
    /// Covers a row type with specific labels and their coverage
    Row(RowSet),
    /// Covers list types with length-based coverage
    List(ListSet),
    /// Unknown coverage
    Opaque,
}

impl CoverSet {
    pub const EMPTY: Self = Self::Atom(AtomSet::new());

    pub fn as_atom_set(&self) -> Option<&AtomSet> {
        as_variant!(self, Self::Atom)
    }

    pub fn as_row_set(&self) -> Option<&RowSet> {
        as_variant!(self, Self::Row)
    }

    /// Returns a set containing any elements present in either set.
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Universal, _) | (_, Self::Universal) => Self::Universal,
            (Self::Atom(a), Self::Atom(b)) => Self::Atom(a.union(b)),
            (Self::Row(a), Self::Row(b)) => Self::Row(a.union(b)),
            (Self::List(a), Self::List(b)) => Self::List(a.union(b)),
            (_, _) => Self::Opaque,
        }
    }

    /// Returns true if the set is a superset of another, i.e., self contains at least all the values in other.
    pub fn is_superset(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Universal, _) => true, // Universal covers everything
            (Self::Atom(a), Self::Atom(b)) => a.is_superset(*b), // All required atoms present
            (Self::Row(a), Self::Row(b)) => a.is_superset(&b), // Row coverage must be checked
            (Self::List(a), Self::List(b)) => a.is_superset(b), // List coverage must be checked
            (_, _) => false,              // Conservative
        }
    }

    /// Returns true if the set is a subset of another, i.e., other contains at least all the values in self.
    pub fn is_subset(&self, other: &Self) -> bool {
        other.is_superset(self)
    }
}

pub trait RequiredCoverage {
    fn required_coverage(&self) -> CoverSet;
}

impl RequiredCoverage for PrimitiveType {
    fn required_coverage(&self) -> CoverSet {
        match self {
            PrimitiveType::Unit => CoverSet::Atom(EnumSet::only(Atom::Unit)),
            PrimitiveType::Bool => CoverSet::Atom(Atom::True | Atom::False),
            PrimitiveType::Num => CoverSet::Universal,
            PrimitiveType::Char => CoverSet::Universal,
            PrimitiveType::Str => CoverSet::Universal,
        }
    }
}

impl RequiredCoverage for ListType {
    fn required_coverage(&self) -> CoverSet {
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
/// the required coverage:
///
/// 1. **Polymorphic rows** (with row variable): Require `Coverage::Universal`
///    because unknown cases/fields may exist
/// 2. **Exact rows** (no row variable): Require coverage of all known cases/fields
///    because the set is finite and known
impl RequiredCoverage for RowType {
    fn required_coverage(&self) -> CoverSet {
        let row_coverage = match self {
            RowType::Empty => RowSet::Empty,
            RowType::Extension { head, tail } => {
                let head_coverage = LabelledSet {
                    label: head.label,
                    set: Box::new(head.ty.required_coverage()),
                };
                let tail_coverage = match tail.required_coverage() {
                    CoverSet::Row(row) => row,
                    _ => panic!("Expected row coverage for tail"),
                };

                RowSet::Extension {
                    head: head_coverage,
                    tail: Box::new(tail_coverage),
                }
            }
        };

        CoverSet::Row(row_coverage)
    }
}

impl RequiredCoverage for MonoType {
    fn required_coverage(&self) -> CoverSet {
        match self {
            MonoType::Primitive(primitive) => primitive.required_coverage(),
            MonoType::List(list) => list.required_coverage(),
            MonoType::Row(row) => row.required_coverage(),
            MonoType::Var(_) => CoverSet::Universal, // Type variables are universal
            MonoType::Func(_) => CoverSet::Opaque,
        }
    }
}

pub trait ActualCoverage {
    fn actual_coverage(&self, tree: &impl TreeView) -> CoverSet;
}

impl ActualCoverage for Id<node::LiteralPat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> CoverSet {
        match *self.get(tree) {
            node::LiteralPat::Bool(true) => CoverSet::Atom(Atom::True.into()),
            node::LiteralPat::Bool(false) => CoverSet::Atom(Atom::False.into()),
            node::LiteralPat::Unit => CoverSet::Atom(Atom::Unit.into()),
            _ => CoverSet::Opaque, // Other literals for now
        }
    }
}

impl ActualCoverage for Id<node::ListPat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> CoverSet {
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

impl ActualCoverage for Id<node::RecordPat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> CoverSet {
        let node::RecordPat { fields, polymorph } = self.get(tree);

        // If the record pattern is polymorphic, the tail is universal coverage
        let tail = if *polymorph {
            RowSet::Universal
        } else {
            RowSet::Empty
        };

        // Process fields in reverse order to build the row coverage correctly
        let row_coverage = fields.iter().rev().fold(tail, |accu, field| {
            let node::RecordFieldPat { field, pat } = field.get(tree);
            let label = field.get(tree).0;
            // Lack of a pattern means binding and therefore universal coverage
            let coverage = pat
                .map(|pat| pat.actual_coverage(tree))
                .unwrap_or(CoverSet::Universal);

            RowSet::Extension {
                head: LabelledSet {
                    label,
                    set: Box::new(coverage),
                },
                tail: Box::new(accu),
            }
        });

        CoverSet::Row(row_coverage)
    }
}

impl ActualCoverage for Id<node::VariantPat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> CoverSet {
        let tags = &self.get(tree).0;

        let row_coverage = tags.iter().rev().fold(RowSet::Empty, |accu, tag| {
            let node::VariantTagPat { tag, pat } = tag.get(tree);
            let label = tag.get(tree).0;
            // Lack of pattern means tag has no value and therefore only Unit coverage
            let coverage = pat
                .map(|pat| pat.actual_coverage(tree))
                .unwrap_or(CoverSet::Atom(Atom::Unit.into()));

            RowSet::Extension {
                head: LabelledSet {
                    label,
                    set: Box::new(coverage),
                },
                tail: Box::new(accu),
            }
        });

        CoverSet::Row(row_coverage)
    }
}

impl ActualCoverage for Id<node::Pat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> CoverSet {
        match *self.get(tree) {
            node::Pat::Error(_) => todo!(),
            node::Pat::Any(_) | node::Pat::Bind(_) => CoverSet::Universal,
            node::Pat::Literal(lit) => lit.actual_coverage(tree),
            node::Pat::List(list) => list.actual_coverage(tree),
            node::Pat::Record(record) => record.actual_coverage(tree),
            node::Pat::Variant(variant) => variant.actual_coverage(tree),
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

        let actual_coverage = branches
            .iter()
            .map(|&branch_id| {
                let branch = branch_id.get(self.tree);
                dbg!(branch.pat.actual_coverage(self.tree))
            })
            .fold(CoverSet::EMPTY, |acc, cov| acc.union(cov));
        dbg!(&actual_coverage);

        let required_coverage = source_type.required_coverage();
        dbg!(&required_coverage);

        if dbg!(actual_coverage.is_superset(&required_coverage)) {
            Ok(()) // Exhaustive - actual covers all required
        } else {
            Err(self.error(source_type.clone()))
        }
    }

    fn error(&self, mono_t: MonoType) -> ExhaustError {
        ExhaustError {
            case: self.case_id,
            mono_t,
            loc: self.loc,
        }
    }
}
