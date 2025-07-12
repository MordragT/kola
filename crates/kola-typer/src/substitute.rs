use std::{borrow::Cow, fmt, hash::Hash};

use kola_collections::{HashMap, ImHashMap, ImOrdMap, ImVec, OrdMap};
use kola_print::prelude::OwoColorize;
use kola_tree::prelude::*;

use crate::{
    env::BoundVars,
    types::{MonoType, TypeVar},
};

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
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn unit(var: TypeVar, ty: MonoType) -> Self {
        let mut table = HashMap::new();
        table.insert(var, ty);
        Self {
            table,
            cache: HashMap::new(),
        }
    }

    pub fn new(table: HashMap<TypeVar, MonoType>) -> Self {
        Self {
            table,
            cache: HashMap::new(),
        }
    }

    pub fn get(&self, tv: &TypeVar) -> Option<&MonoType> {
        self.cache.get(tv).or_else(|| self.table.get(tv))
    }

    pub fn contains(&self, tv: &TypeVar) -> bool {
        self.cache.contains_key(tv) || self.table.contains_key(tv)
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

    pub fn is_empty(&self) -> bool {
        self.table.is_empty() && self.cache.is_empty()
    }

    pub fn clear(&mut self) {
        self.table.clear();
        self.cache.clear();
    }
}

impl BoundVars for Substitution {
    fn extend_bound_vars(&self, vars: &mut Vec<TypeVar>) {
        vars.extend(self.table.keys());
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

impl Substitutable for ! {
    fn try_apply(&self, _s: &mut Substitution) -> Option<Self> {
        None
    }
}

impl<T> Substitutable for Option<T>
where
    T: Substitutable + Clone,
{
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        match self {
            Some(t) => t.try_apply(s).map(Some),
            None => None,
        }
    }
}

impl<T> Substitutable for Vec<T>
where
    T: Substitutable + Clone,
{
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let mut result = None;

        for (i, item) in self.iter().enumerate() {
            if let Some(next) = item.try_apply(s) {
                result.get_or_insert_with(|| self.clone())[i] = next;
            }
        }

        result
    }
}

impl<T> Substitutable for ImVec<T>
where
    T: Substitutable + Clone,
{
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let mut result = None;

        for (i, item) in self.iter().enumerate() {
            if let Some(next) = item.try_apply(s) {
                result.get_or_insert_with(|| self.clone()).set(i, next);
            }
        }

        result
    }
}

impl<K, V> Substitutable for HashMap<K, V>
where
    K: Eq + Clone + Hash,
    V: Substitutable + Clone,
{
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let mut result = None;

        for (key, value) in self.iter() {
            if let Some(next) = value.try_apply(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), next);
            }
        }

        result
    }
}

impl<K, V> Substitutable for ImHashMap<K, V>
where
    K: Eq + Clone + Hash,
    V: Substitutable + Clone,
{
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let mut result = None;

        for (key, value) in self.iter() {
            if let Some(next) = value.try_apply(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), next);
            }
        }

        result
    }
}

impl<K, V> Substitutable for OrdMap<K, V>
where
    K: Ord + Clone,
    V: Substitutable + Clone,
{
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let mut result = None;

        for (key, value) in self.iter() {
            if let Some(next) = value.try_apply(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), next);
            }
        }

        result
    }
}

impl<K, V> Substitutable for ImOrdMap<K, V>
where
    K: Ord + Clone,
    V: Substitutable + Clone,
{
    fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
        let mut result = None;

        for (key, value) in self.iter() {
            if let Some(next) = value.try_apply(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), next);
            }
        }

        result
    }
}

macro_rules! impl_meta_substitutable {
    ($($variant:ident),* $(,)?) => {
        impl<P> Substitutable for Meta<P>
        where
        P: Phase<
            $($variant: Substitutable),*
        >,
        {
            fn try_apply(&self, s: &mut Substitution) -> Option<Self> {
                match self {
                    $(Self::$variant(t) => t.try_apply(s).map(Self::$variant)),*
                }
            }

        }
    };
}

impl_meta_substitutable!(
    FunctorName,
    ModuleTypeName,
    ModuleName,
    KindName,
    EffectName,
    TypeName,
    ValueName,
    // Patterns
    AnyPat,
    LiteralPat,
    BindPat,
    ListElPat,
    ListPat,
    RecordFieldPat,
    RecordPat,
    VariantTagPat,
    VariantPat,
    PatError,
    Pat,
    // Expressions
    LiteralExpr,
    ListExpr,
    RecordField,
    RecordExpr,
    RecordExtendExpr,
    RecordRestrictExpr,
    RecordUpdateOp,
    RecordUpdateExpr,
    RecordMergeExpr,
    FieldPath,
    QualifiedExpr,
    UnaryOp,
    UnaryExpr,
    BinaryOp,
    BinaryExpr,
    LetExpr,
    CaseBranch,
    CaseExpr,
    IfExpr,
    LambdaExpr,
    CallExpr,
    HandlerClause,
    HandleExpr,
    DoExpr,
    TagExpr,
    TypeWitnessExpr,
    ExprError,
    Expr,
    // Types
    QualifiedEffectType,
    EffectOpType,
    EffectRowType,
    EffectType,
    QualifiedType,
    TypeVar,
    LabelOrVar,
    RecordFieldType,
    RecordType,
    TagType,
    VariantType,
    FuncType,
    TypeApplication,
    CompType,
    Type,
    TypeError,
    TypeVarBind,
    ForallBinder,
    TypeScheme,
    // Modules
    Vis,
    ValueBind,
    TypeBind,
    OpaqueTypeBind,
    EffectTypeBind,
    ModuleBind,
    ModuleTypeBind,
    FunctorBind,
    Bind,
    ModuleError,
    Module,
    ModulePath,
    ModuleImport,
    FunctorApp,
    ModuleExpr,
    ValueSpec,
    OpaqueTypeKind,
    OpaqueTypeSpec,
    ModuleSpec,
    Spec,
    ConcreteModuleType,
    QualifiedModuleType,
    ModuleType,
);

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
