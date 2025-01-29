use kola_tree::meta::Meta;
use owo_colors::OwoColorize;
use std::{borrow::Cow, collections::HashMap, fmt};

use super::types::{MonoType, TypeVar};
use crate::SemanticPhase;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Substitution {
    table: HashMap<TypeVar, MonoType>,
    /// Path compression is a technique commonly used in Union-Find data structures.
    /// We apply it here so that whenever a chain of substitutions is traversed,
    /// each variable is updated to point to its ultimate value. For example, the
    /// chain:
    ///
    /// `t0 ↦ t1`, `t1 ↦ t2`, and `t2 ↦ int`
    ///
    /// becomes
    ///
    /// `t0 ↦ int`, `t1 ↦ int`, and `t2 ↦ int`
    ///
    /// Rather than updating the actual mappings,
    /// this cache maintains these compressed mappings.
    cache: HashMap<TypeVar, MonoType>,
}

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (var, ty) in &self.table {
            writeln!(f, "{var}\t{}", ty.green())?;
        }

        Ok(())
    }
}

impl Substitution {
    pub fn new(table: HashMap<TypeVar, MonoType>) -> Self {
        Self {
            table,
            cache: HashMap::new(),
        }
    }

    pub fn empty() -> Self {
        Self::default()
    }

    pub fn get(&self, tv: &TypeVar) -> Option<&MonoType> {
        self.cache.get(tv).or_else(|| self.table.get(tv))
    }

    pub fn insert(&mut self, tv: TypeVar, ty: MonoType) -> Option<MonoType> {
        self.table.insert(tv, ty)
    }

    pub fn cache(&mut self, tv: TypeVar, ty: &MonoType) {
        self.cache
            .entry(tv)
            .and_modify(|stored| {
                if stored != ty {
                    *stored = ty.clone();
                }
            })
            .or_insert_with(|| ty.clone());
    }

    pub fn clear(&mut self) {
        self.table.clear();
        self.cache.clear();
    }
}

/// A type is `Substitutable` if a substitution can be applied to it.
pub trait Substitutable: Sized {
    /// Apply a substitution to a type variable.
    fn apply(self, s: &mut Substitution) -> Self {
        self.try_apply(s).unwrap_or(self)
    }

    /// Apply a substitution to a type variable.
    fn apply_mut(&mut self, s: &mut Substitution) {
        if let Some(new) = self.try_apply(s) {
            *self = new;
        }
    }

    /// Apply a substitution to a type variable.
    fn apply_cow(&self, s: &mut Substitution) -> Cow<'_, Self>
    where
        Self: Clone,
    {
        match self.try_apply(s) {
            Some(t) => Cow::Owned(t),
            None => Cow::Borrowed(self),
        }
    }

    /// Apply a non-mutating substitution to a type variable.
    /// Should return `None` if there was nothing to apply
    /// which allows for optimizations.
    fn try_apply(&self, s: &mut Substitution) -> Option<Self>;
}

impl<T> Substitutable for Vec<T>
where
    T: Substitutable + Clone,
{
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        // I think this is only allocated if used but unsure
        let mut collector = Vec::new();
        let mut rem = self.as_slice();

        while let Some((end, el)) = rem
            .iter()
            .enumerate()
            .find_map(|(i, el)| el.try_apply(s).map(|el| (i, el)))
        {
            collector.extend_from_slice(&self[0..end]);
            collector.push(el);

            rem = rem.get(end + 1..).unwrap_or(&[]);
        }

        if !collector.is_empty() {
            collector.extend_from_slice(rem);
            assert_eq!(collector.len(), self.len());
            Some(collector)
        } else {
            None
        }
    }
}

impl Substitutable for Meta<SemanticPhase> {
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Meta::Name(())
            | Meta::Property(())
            | Meta::PatError(())
            | Meta::Branch(())
            | Meta::ExprError(()) => None,

            Meta::Ident(t) => t.try_apply(s).map(Meta::Ident),
            Meta::Literal(t) => t.try_apply(s).map(Meta::Literal),
            Meta::List(t) => t.try_apply(s).map(Meta::List),
            Meta::Record(t) => t.try_apply(s).map(Meta::Record),
            Meta::RecordSelect(t) => t.try_apply(s).map(Meta::RecordSelect),
            Meta::RecordExtend(t) => t.try_apply(s).map(Meta::RecordExtend),
            Meta::RecordRestrict(t) => t.try_apply(s).map(Meta::RecordRestrict),
            Meta::RecordUpdate(t) => t.try_apply(s).map(Meta::RecordUpdate),
            Meta::UnaryOp(t) => t.try_apply(s).map(Meta::UnaryOp),
            Meta::Unary(t) => t.try_apply(s).map(Meta::Unary),
            Meta::BinaryOp(t) => t.try_apply(s).map(Meta::BinaryOp),
            Meta::Binary(t) => t.try_apply(s).map(Meta::Binary),
            Meta::Let(t) => t.try_apply(s).map(Meta::Let),
            Meta::Wildcard(t) => t.try_apply(s).map(Meta::Wildcard),
            Meta::LiteralPat(t) => t.try_apply(s).map(Meta::LiteralPat),
            Meta::IdentPat(t) => t.try_apply(s).map(Meta::IdentPat),
            Meta::PropertyPat(t) => t.try_apply(s).map(Meta::PropertyPat),
            Meta::RecordPat(t) => t.try_apply(s).map(Meta::RecordPat),
            Meta::Pat(t) => t.try_apply(s).map(Meta::Pat),
            Meta::Case(t) => t.try_apply(s).map(Meta::Case),
            Meta::If(t) => t.try_apply(s).map(Meta::If),
            Meta::Func(t) => t.try_apply(s).map(Meta::Func),
            Meta::Call(t) => t.try_apply(s).map(Meta::Call),
            Meta::Expr(t) => t.try_apply(s).map(Meta::Expr),
        }
    }
}

pub fn merge<A, B, DA, DB>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
) -> Option<(A, B)>
where
    DA: FnOnce() -> A,
    DB: FnOnce() -> B,
{
    match (a, b) {
        (Some(a), Some(b)) => Some((a, b)),
        (Some(a), None) => Some((a, default_b())),
        (None, Some(b)) => Some((default_a(), b)),
        (None, None) => None,
    }
}
