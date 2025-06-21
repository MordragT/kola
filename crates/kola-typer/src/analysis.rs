use enumset::{EnumSet, EnumSetType};
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
    EmptyList,
    NonEmptyList,
}

pub type FiniteSet = EnumSet<Atom>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabeledCoverage {
    pub label: StrKey,
    pub coverage: Coverage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RowCoverage {
    Empty,
    Extension {
        head: LabeledCoverage,
        tail: Coverage,
    },
}

impl RowCoverage {
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            // Empty ∪ Empty = Empty
            (RowCoverage::Empty, RowCoverage::Empty) => RowCoverage::Empty,
            // Empty ∪ Extension = Extension (extension covers more)
            (RowCoverage::Empty, ext) | (ext, RowCoverage::Empty) => ext,
            // Extension ∪ Extension
            (
                RowCoverage::Extension {
                    head: head1,
                    tail: tail1,
                },
                RowCoverage::Extension {
                    head: head2,
                    tail: tail2,
                },
            ) => {
                if head1.label == head2.label {
                    // Same label: union the coverages
                    RowCoverage::Extension {
                        head: LabeledCoverage {
                            label: head1.label,
                            coverage: head1.coverage.union(head2.coverage),
                        },
                        tail: tail1.union(tail2),
                    }
                } else {
                    // Different labels: need to reorder and merge
                    // Add head1, then try to union tail1 with the full other row
                    RowCoverage::Extension {
                        head: head1,
                        tail: tail1.union(Coverage::Row(Box::new(RowCoverage::Extension {
                            head: head2,
                            tail: tail2,
                        }))),
                    }
                }
            }
        }
    }

    /// Check if this row coverage can cover all requirements of another row coverage.
    /// This implements row reordering by searching for required labels recursively.
    pub fn is_superset(&self, other: &Self) -> bool {
        match other {
            RowCoverage::Empty => true, // Empty row is always covered
            RowCoverage::Extension { head, tail } => {
                self.covers_head(head) && self.covers_tail(tail)
            }
        }
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        other.is_superset(self)
    }

    fn covers_tail(&self, tail: &Coverage) -> bool {
        match tail {
            Coverage::Row(tail) => self.is_superset(tail),
            Coverage::Universal => true, // Universal tail is always covered
            _ => panic!("Unknown coverage type in row tail {tail:?}"),
        }
    }

    /// Check if this row can cover a specific labeled requirement.
    /// Searches through the row structure using reordering semantics.
    fn covers_head(&self, other: &LabeledCoverage) -> bool {
        match self {
            RowCoverage::Empty => false, // Empty row cannot cover anything
            RowCoverage::Extension { head, .. } if head.label == other.label => {
                head.coverage.is_superset(&other.coverage)
            }
            RowCoverage::Extension {
                tail: Coverage::Row(tail),
                ..
            } => tail.covers_head(other),
            RowCoverage::Extension {
                tail: Coverage::Universal,
                ..
            } => true, // Universal tail covers everything,
            RowCoverage::Extension { tail, .. } => {
                panic!("Unknown coverage type in row tail {tail:?}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Coverage {
    /// Covers all possible values, e.g., wildcard patterns
    Universal,
    /// Covers a finite set of values, e.g., specific literals or patterns
    Finite(FiniteSet),
    /// Covers a row type with specific labels and their coverage
    Row(Box<RowCoverage>),
    /// Unknown coverage
    Opaque,
}

impl Coverage {
    pub const EMPTY: Self = Self::Finite(FiniteSet::new());

    pub fn as_finite_set(&self) -> Option<&FiniteSet> {
        as_variant!(self, Self::Finite)
    }

    pub fn as_row_coverage(&self) -> Option<&RowCoverage> {
        as_variant!(self, Self::Row)
    }

    /// Returns a set containing any elements present in either set.
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Universal, _) | (_, Self::Universal) => Self::Universal,
            (Self::Finite(a), Self::Finite(b)) => Self::Finite(a.union(b)),
            (Self::Row(a), Self::Row(b)) => Self::Row(Box::new(a.union(*b))),
            (_, _) => Self::Opaque,
        }
    }

    /// Returns true if the set is a superset of another, i.e., self contains at least all the values in other.
    pub fn is_superset(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Universal, _) => true, // Universal covers everything
            (Self::Finite(a), Self::Finite(b)) => a.is_superset(*b), // All required atoms present
            (Self::Row(a), Self::Row(b)) => a.is_superset(b), // Row coverage must be checked
            (_, _) => false,              // Conservative
        }
    }

    /// Returns true if the set is a subset of another, i.e., other contains at least all the values in self.
    pub fn is_subset(&self, other: &Self) -> bool {
        other.is_superset(self)
    }
}

pub trait RequiredCoverage {
    fn required_coverage(&self) -> Coverage;
}

impl RequiredCoverage for PrimitiveType {
    fn required_coverage(&self) -> Coverage {
        match self {
            PrimitiveType::Unit => Coverage::Finite(EnumSet::only(Atom::Unit)),
            PrimitiveType::Bool => Coverage::Finite(Atom::True | Atom::False),
            PrimitiveType::Num => Coverage::Universal,
            PrimitiveType::Char => Coverage::Universal,
            PrimitiveType::Str => Coverage::Universal,
        }
    }
}

impl RequiredCoverage for ListType {
    fn required_coverage(&self) -> Coverage {
        Coverage::Finite(Atom::EmptyList | Atom::NonEmptyList)
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
    fn required_coverage(&self) -> Coverage {
        // For now, require universal coverage (wildcard/bind pattern)
        // Later distinguish between variant and record row types
        // TODO check the following
        // For variants: extract all case names and require coverage of each
        // For records: either always exhaustive or require universal coverage
        Coverage::Universal
    }
}

impl RequiredCoverage for MonoType {
    fn required_coverage(&self) -> Coverage {
        match self {
            MonoType::Primitive(primitive) => primitive.required_coverage(),
            MonoType::List(list) => list.required_coverage(),
            MonoType::Row(row) => row.required_coverage(),
            MonoType::Var(_) => Coverage::Universal, // Type variables are universal
            MonoType::Func(_) => Coverage::Opaque,
        }
    }
}

pub trait ActualCoverage {
    fn actual_coverage(&self, tree: &impl TreeView) -> Coverage;
}

impl ActualCoverage for Id<node::LiteralPat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> Coverage {
        match *self.get(tree) {
            node::LiteralPat::Bool(true) => Coverage::Finite(Atom::True.into()),
            node::LiteralPat::Bool(false) => Coverage::Finite(Atom::False.into()),
            node::LiteralPat::Unit => Coverage::Finite(Atom::Unit.into()),
            _ => Coverage::Opaque, // Other literals for now
        }
    }
}

impl ActualCoverage for Id<node::ListPat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> Coverage {
        let elements = &self.get(tree).0;

        if elements.is_empty() {
            // Empty list pattern: []
            Coverage::Finite(Atom::EmptyList.into())
        } else {
            // Non-empty list pattern: [x, y, ...] or [x, ...rest]
            Coverage::Finite(Atom::NonEmptyList.into())
        }
    }
}

impl ActualCoverage for Id<node::RecordPat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> Coverage {
        todo!()
    }
}

impl ActualCoverage for Id<node::VariantPat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> Coverage {
        todo!()
    }
}

impl ActualCoverage for Id<node::Pat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> Coverage {
        match *self.get(tree) {
            node::Pat::Error(_) => todo!(),
            node::Pat::Any(_) | node::Pat::Bind(_) => Coverage::Universal,
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
                branch.pat.actual_coverage(self.tree)
            })
            .fold(Coverage::EMPTY, |acc, cov| acc.union(cov));

        let required_coverage = source_type.required_coverage();

        if actual_coverage.is_superset(&required_coverage) {
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
