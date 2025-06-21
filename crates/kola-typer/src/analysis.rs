use enumset::{EnumSet, EnumSetType};
use kola_span::{IntoDiagnostic, Loc, Located};
use kola_syntax::loc::Locations;
use kola_tree::prelude::*;
use kola_utils::errors::Errors;

use crate::{
    phase::TypedNodes,
    types::{ListType, MonoType, PrimitiveType, RowType},
};

// TODO impl Error Display and common traits
pub struct ExhaustError {
    pub case: Id<node::CaseExpr>,
    pub message: String, // replace with Kind
    pub loc: Loc,
}

// impl IntoDiagnostic for ExhaustivenessError

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
            PrimitiveType::Num => Coverage::Opaque,
            PrimitiveType::Char => Coverage::Opaque,
            PrimitiveType::Str => Coverage::Opaque,
        }
    }
}

pub fn pattern_coverage(pat: Id<node::Pat>, tree: &impl TreeView) -> Coverage {
    let literal_coverage = |lit: Id<node::LiteralPat>| match lit.get(tree) {
        node::LiteralPat::Bool(true) => Coverage::Finite(Atom::True.into()),
        node::LiteralPat::Bool(false) => Coverage::Finite(Atom::False.into()),
        node::LiteralPat::Unit => Coverage::Finite(Atom::Unit.into()),
        _ => Coverage::Opaque, // Other literals for now
    };

    match *pat.get(tree) {
        node::Pat::Any(_) | node::Pat::Bind(_) => Coverage::Universal,
        node::Pat::Literal(lit) => literal_coverage(lit),
        _ => Coverage::Opaque, // Complex patterns for now
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
                pattern_coverage(branch.pat, self.tree)
            })
            .fold(Coverage::Finite(FiniteSet::new()), |acc, cov| {
                acc.union(cov)
            });

        match source_type {
            MonoType::Primitive(p) => self.check_primitive(p, actual_coverage),
            MonoType::Func(f) => todo!(),
            MonoType::List(l) => self.check_list(l, actual_coverage),
            MonoType::Row(r) => self.check_row(r, actual_coverage),
            MonoType::Var(v) => todo!(),
        }
    }

    pub fn check_primitive(
        &self,
        primitive: &PrimitiveType,
        actual_coverage: Coverage,
    ) -> ExhaustResult<()> {
        let required_coverage = primitive.required_coverage();

        if actual_coverage.is_superset(required_coverage) {
            Ok(()) // Exhaustive - actual covers all required
        } else {
            Err(self.error(format!("Non-exhaustive patterns for {:?}", primitive)))
        }
    }

    pub fn check_list(&self, list: &ListType, actual_coverage: Coverage) -> ExhaustResult<()> {
        todo!()
    }

    pub fn check_row(&self, row: &RowType, actual_coverage: Coverage) -> ExhaustResult<()> {
        todo!()
    }

    fn error(&self, message: impl Into<String>) -> ExhaustError {
        ExhaustError {
            case: self.case_id,
            message: message.into(),
            loc: self.loc,
        }
    }
}
