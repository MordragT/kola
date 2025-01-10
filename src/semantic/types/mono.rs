use std::fmt;

use crate::semantic::{
    error::{InferError, InferErrors, InferResult},
    Cache, Constraints, Context, Kind, Substitutable, Substitution, Unify,
};

use super::{BuiltinType, FunctionType, PolyType, TypeVar, Typed};

/// MonoType
/// Non-polymorphic types (e.g. `α → β`, `int → bool`)
/// https://en.wikipedia.org/wiki/Hindley%e2%80%93Milner_type_system#Monotypes
/// τ ::= α | gn τ1 .. τn
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MonoType {
    Builtin(BuiltinType),
    Func(Box<FunctionType>),
    Var(TypeVar),
    // RowVar(TypeVar), // kind preserving unification (see Extensible Records with Scoped Labels)
    // Row(RowType),
}

impl MonoType {
    pub const BOOL: Self = Self::Builtin(BuiltinType::Bool);
    pub const NUM: Self = Self::Builtin(BuiltinType::Num);
    pub const CHAR: Self = Self::Builtin(BuiltinType::Char);
    pub const STR: Self = Self::Builtin(BuiltinType::Str);
}

impl MonoType {
    pub fn variable() -> Self {
        Self::Var(TypeVar::new())
    }

    pub fn func(arg: Self, ret: Self) -> Self {
        Self::Func(Box::new(FunctionType::new(arg, ret)))
    }
}

impl MonoType {
    pub fn into_var(self) -> Option<TypeVar> {
        match self {
            Self::Var(tv) => Some(tv),
            _ => None,
        }
    }

    pub fn into_const(self) -> Option<BuiltinType> {
        match self {
            Self::Builtin(tc) => Some(tc),
            _ => None,
        }
    }

    pub fn into_func(self) -> Option<FunctionType> {
        match self {
            Self::Func(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_var(&self) -> Option<&TypeVar> {
        match self {
            Self::Var(tv) => Some(tv),
            _ => None,
        }
    }

    pub fn as_const(&self) -> Option<&BuiltinType> {
        match self {
            Self::Builtin(tc) => Some(tc),
            _ => None,
        }
    }

    pub fn as_func(&self) -> Option<&FunctionType> {
        match self {
            Self::Func(f) => Some(f),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        self.as_var().is_some()
    }

    pub fn is_const(&self) -> bool {
        self.as_const().is_some()
    }

    pub fn is_func(&self) -> bool {
        self.as_func().is_some()
    }
}

impl MonoType {
    /// Performs unification on the type with another type.
    /// If successful, results in a solution to the unification problem,
    /// in the form of a substitution. If there is no solution to the
    /// unification problem then unification fails and an error is reported.
    pub fn try_unify(&self, with: &Self, ctx: &mut Context) -> Result<MonoType, InferErrors> {
        self.unify(with, ctx);

        let errors = ctx.take_errors();

        if errors.is_empty() {
            Ok(self.clone())
        } else {
            Err(InferErrors { related: errors })
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

impl Substitutable for MonoType {
    fn try_apply(&self, s: &mut Substitution, cache: &mut Cache) -> Option<Self> {
        match self {
            Self::Builtin(_) => None,
            Self::Func(f) => f.try_apply(s, cache).map(Into::into),
            Self::Var(tv) => tv.try_apply(s, cache),
        }
    }
}

impl Unify<&Self> for MonoType {
    fn unify(&self, with: &Self, ctx: &mut Context) {
        match (self, with) {
            (Self::Builtin(l), Self::Builtin(r)) => l.unify(r, ctx),
            (Self::Func(fl), Self::Func(fr)) => fl.unify(fr, ctx),
            (Self::Var(l), r) => l.unify(r, ctx),
            (l, Self::Var(r)) => r.unify(l, ctx),
            (l, r) => {
                ctx.error(InferError::CannotUnify {
                    expected: l.clone(),
                    actual: r.clone(),
                });
            }
        }
    }
}

impl Typed for MonoType {
    fn constrain(&self, with: Kind, constraints: &mut Constraints) -> InferResult<()> {
        match self {
            Self::Builtin(b) => b.constrain(with, constraints),
            Self::Func(f) => f.constrain(with, constraints),
            Self::Var(tv) => tv.constrain(with, constraints),
        }
    }

    fn contains(&self, tv: TypeVar) -> bool {
        match self {
            Self::Builtin(b) => b.contains(tv),
            Self::Func(f) => f.contains(tv),
            Self::Var(t) => t.contains(tv),
        }
    }

    fn type_vars(&self, vars: &mut Vec<TypeVar>) {
        match self {
            Self::Builtin(b) => b.type_vars(vars), // TODO save this call?
            Self::Func(f) => f.type_vars(vars),
            Self::Var(tv) => tv.type_vars(vars),
        }
    }
}

impl fmt::Display for MonoType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(tv) => tv.fmt(f),
            Self::Builtin(tc) => tc.fmt(f),
            Self::Func(func) => func.fmt(f),
            // Self::RowVar(tv) => tv.fmt(f),
            // Self::Row(rt) => rt.fmt(f),
        }
    }
}
impl From<TypeVar> for MonoType {
    fn from(value: TypeVar) -> Self {
        Self::Var(value)
    }
}

impl From<BuiltinType> for MonoType {
    fn from(value: BuiltinType) -> Self {
        Self::Builtin(value)
    }
}

impl From<&BuiltinType> for MonoType {
    fn from(value: &BuiltinType) -> Self {
        Self::Builtin(*value)
    }
}

impl From<FunctionType> for MonoType {
    fn from(value: FunctionType) -> Self {
        Self::Func(Box::new(value))
    }
}

// impl From<RowType> for MonoType {
//     fn from(value: RowType) -> Self {
//         Self::Row(value)
//     }
// }
