use std::{
    fmt,
    sync::atomic::{AtomicU32, Ordering},
};

use super::{
    error::{InferError, InferResult},
    Substitutions,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar {
    id: u32,
    level: u32,
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'t{}", self.id)
    }
}

impl TypeVar {
    const GENERATOR: AtomicU32 = AtomicU32::new(0);

    /// Efficient generalization with levels
    /// https://okmij.org/ftp/ML/generalization.html#levels
    const LEVEL: AtomicU32 = AtomicU32::new(0);

    pub fn new() -> Self {
        let id = Self::GENERATOR.fetch_add(1, Ordering::Relaxed);
        let level = Self::level();
        Self { id, level }
    }

    pub fn level() -> u32 {
        Self::LEVEL.load(Ordering::Relaxed)
    }

    pub fn enter_level() {
        Self::LEVEL.fetch_add(1, Ordering::Relaxed);
    }

    pub fn exit_level() {
        Self::LEVEL.fetch_sub(1, Ordering::Relaxed);
    }

    /// Checks if the type variable can bind to a type.
    /// It is an error to call this function if ty is equal to self
    pub fn bind(&self, ty: &MonoType, s: &mut Substitutions) -> InferResult<()> {
        // TODO allow constraints on type variables ?
        // and then check them here ?

        if ty.contains(self) {
            Err(InferError::Occurs(*self))
        } else {
            s.insert(*self, ty.clone());
            Ok(())
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TypeConst {
    Bool,
    Num,
    Char,
    Str,
}

/// Non-polymorphic types (e.g. `α → β`, `int → bool`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Monotypes
/// τ ::= α | gn τ1 .. τn
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoType {
    Var(TypeVar),
    Const(TypeConst),
    Arrow(Box<Self>, Box<Self>),
}

impl MonoType {
    pub fn variable() -> Self {
        Self::Var(TypeVar::new())
    }

    pub fn constant(ty: TypeConst) -> Self {
        Self::Const(ty)
    }

    pub fn arrow(lhs: Self, rhs: Self) -> Self {
        Self::Arrow(Box::new(lhs), Box::new(rhs))
    }

    pub fn into_var(self) -> Option<TypeVar> {
        match self {
            Self::Var(tv) => Some(tv),
            _ => None,
        }
    }

    pub fn into_const(self) -> Option<TypeConst> {
        match self {
            Self::Const(tc) => Some(tc),
            _ => None,
        }
    }

    pub fn into_arrow(self) -> Option<(Self, Self)> {
        match self {
            Self::Arrow(a, b) => Some((*a, *b)),
            _ => None,
        }
    }

    pub fn as_var(&self) -> Option<&TypeVar> {
        match self {
            Self::Var(tv) => Some(tv),
            _ => None,
        }
    }

    pub fn as_const(&self) -> Option<&TypeConst> {
        match self {
            Self::Const(tc) => Some(tc),
            _ => None,
        }
    }

    pub fn as_arrow(&self) -> Option<(&Self, &Self)> {
        match self {
            Self::Arrow(a, b) => Some((a, b)),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        self.as_var().is_some()
    }

    pub fn is_const(&self) -> bool {
        self.as_const().is_some()
    }

    pub fn is_arrow(&self) -> bool {
        self.as_arrow().is_some()
    }

    pub fn type_vars(&self, buf: &mut Vec<TypeVar>) {
        match self {
            Self::Const(_) => (),
            Self::Var(tv) => {
                if tv.level > TypeVar::level() {
                    buf.push(*tv)
                }
            }
            Self::Arrow(lhs, rhs) => {
                lhs.type_vars(buf);
                rhs.type_vars(buf);
            }
        }
    }

    /// occurs check
    pub fn contains(&self, tv: &TypeVar) -> bool {
        match self {
            Self::Const(_) => false,
            Self::Var(v) => {
                assert_eq!(v.level, tv.level); // TODO must set self's level to min of both ?
                v.id == tv.id
            }
            Self::Arrow(a, b) => a.contains(tv) || b.contains(tv),
        }
    }

    /// Substitute variables for types
    pub fn substitute(&mut self, s: &Substitutions) {
        match self {
            Self::Arrow(a, b) => {
                a.substitute(s);
                b.substitute(s);
            }
            Self::Var(v) => {
                if let Some(t) = s.get(v) {
                    *self = t.clone();
                }
            }
            _ => (),
        }
    }

    /// Takes a type with type vars inside and returns a polytype, with the type vars generalized inside the forall
    // TODO use context instead of slice for bound ?
    pub fn generalize(&self, bound: &[TypeVar]) -> PolyType {
        let mut vars = Vec::new();
        self.type_vars(&mut vars);
        vars.retain(|tv| !bound.contains(tv));
        vars.sort_unstable();
        vars.dedup();

        PolyType {
            vars,
            ty: self.clone(),
        }
    }
}

/// Types that contains variable bound by zero or more forall
/// Polymorphic types (e.g. `∀α. α → α`, `∀α. ∀β. α → β`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Polytypes
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PolyType {
    vars: Vec<TypeVar>,
    ty: MonoType,
}

impl PolyType {
    pub fn new(ty: MonoType) -> Self {
        Self {
            vars: Vec::new(),
            ty,
        }
    }

    pub fn bound_vars(&self) -> &Vec<TypeVar> {
        &self.vars
    }

    // pub fn free_type_vars(&self, buf: &mut Vec<TypeVar>) {
    //     self.ty.type_vars(buf);
    //     buf.retain(|tv| !self.vars.contains(tv));
    // }

    /// The procedure inst(σ) specializes the polytype σ by copying the term
    /// and replacing the bound type variables consistently by new monotype variables.
    pub fn instantiate(&self) -> MonoType {
        let mut ty = self.ty.clone();

        let substitution = self
            .vars
            .iter()
            .copied()
            .map(|tv| (tv, MonoType::variable()))
            .collect();
        ty.substitute(&substitution);

        ty
    }
}

impl From<MonoType> for PolyType {
    fn from(ty: MonoType) -> Self {
        Self::new(ty)
    }
}
