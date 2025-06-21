use enumset::{EnumSet, EnumSetType};
use kola_span::{Diagnostic, Loc};
use kola_syntax::loc::Locations;
use kola_tree::prelude::*;
use kola_utils::errors::Errors;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Coverage {
    Universal,
    Finite(FiniteSet),
    Opaque,
}

impl Coverage {
    /// Returns a set containing any elements present in either set.
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Universal, _) | (_, Self::Universal) => Self::Universal,
            (Self::Finite(a), Self::Finite(b)) => Self::Finite(a.union(b)),
            (Self::Opaque, _) | (_, Self::Opaque) => Self::Opaque,
        }
    }

    /// Returns true if the set is a superset of another, i.e., self contains at least all the values in other.
    pub fn is_superset(self, other: Self) -> bool {
        match (self, other) {
            (Self::Universal, _) => true, // Universal covers everything
            (Self::Finite(a), Self::Finite(b)) => a.is_superset(b), // All required atoms present
            (Self::Finite(_), Self::Universal) => false, // Finite can't cover infinite
            (Self::Opaque, _) | (_, Self::Opaque) => false, // Conservative
        }
    }

    /// Returns true if the set is a subset of another, i.e., other contains at least all the values in self.
    pub fn is_subset(self, other: Self) -> bool {
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

impl RequiredCoverage for MonoType {
    fn required_coverage(&self) -> Coverage {
        match self {
            MonoType::Primitive(p) => p.required_coverage(),
            MonoType::Func(_) => todo!(),
            MonoType::List(_) => todo!(),
            MonoType::Row(_) => todo!(),
            MonoType::Var(_) => todo!(),
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

impl ActualCoverage for Id<node::Pat> {
    fn actual_coverage(&self, tree: &impl TreeView) -> Coverage {
        match *self.get(tree) {
            node::Pat::Any(_) | node::Pat::Bind(_) => Coverage::Universal,
            node::Pat::Literal(lit) => lit.actual_coverage(tree),
            node::Pat::List(list) => list.actual_coverage(tree),
            _ => Coverage::Opaque, // Complex patterns for now
        }
    }
}

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
            .fold(Coverage::Finite(FiniteSet::new()), |acc, cov| {
                acc.union(cov)
            });

        let required_coverage = source_type.required_coverage();

        if actual_coverage.is_superset(required_coverage) {
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
