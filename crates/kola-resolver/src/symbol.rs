use derive_more::From;
use enum_as_inner::EnumAsInner;
use kola_collections::HashMap;
use kola_tree::node::{
    EffectNamespace, FunctorNamespace, ModuleNamespace, ModuleTypeNamespace, Namespace,
    NamespaceKind, TypeNamespace, ValueNamespace,
};
use kola_utils::define_unique_leveled_id;
use std::{
    borrow::Cow,
    fmt,
    hash::Hash,
    marker::PhantomData,
    sync::atomic::{AtomicU32, Ordering},
};

static LEVEL: AtomicU32 = AtomicU32::new(0);
static GENERATOR: AtomicU32 = AtomicU32::new(0);

define_unique_leveled_id!(Sym);

impl<T: ?Sized> Sym<T> {
    pub fn new() -> Self {
        let id = GENERATOR.fetch_add(1, Ordering::Relaxed);
        let level = Self::load_level();
        Self {
            id,
            level,
            t: PhantomData,
        }
    }

    pub fn load_level() -> u32 {
        LEVEL.load(Ordering::Relaxed)
    }

    pub fn enter() {
        LEVEL.fetch_add(1, Ordering::Relaxed);
    }

    pub fn exit() {
        LEVEL.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn branch<U>(mut f: impl FnMut() -> U) -> U {
        LEVEL.fetch_add(1, Ordering::Relaxed);
        let result = f();
        LEVEL.fetch_sub(1, Ordering::Relaxed);
        result
    }
}

impl<T: ?Sized> Default for Sym<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub type FunctorSym = Sym<FunctorNamespace>;
pub type ModuleTypeSym = Sym<ModuleTypeNamespace>;
pub type ModuleSym = Sym<ModuleNamespace>;
pub type EffectSym = Sym<EffectNamespace>;
pub type TypeSym = Sym<TypeNamespace>;
pub type ValueSym = Sym<ValueNamespace>;

impl fmt::Display for FunctorSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f{}", self.id())
    }
}

impl fmt::Display for ModuleTypeSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "mt{}", self.id())
    }
}

impl fmt::Display for ModuleSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "m{}", self.id())
    }
}

impl fmt::Display for EffectSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "e{}", self.id())
    }
}

impl fmt::Display for TypeSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.id())
    }
}

impl fmt::Display for ValueSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.id())
    }
}

#[derive(Debug, EnumAsInner, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnySym {
    Functor(FunctorSym),
    ModuleType(ModuleTypeSym),
    Module(ModuleSym),
    Effect(EffectSym),
    Type(TypeSym),
    Value(ValueSym),
}

impl AnySym {
    pub fn id(&self) -> u32 {
        match self {
            Self::Functor(symbol) => symbol.id(),
            Self::ModuleType(symbol) => symbol.id(),
            Self::Module(symbol) => symbol.id(),
            Self::Effect(symbol) => symbol.id(),
            Self::Type(symbol) => symbol.id(),
            Self::Value(symbol) => symbol.id(),
        }
    }

    pub fn as_usize(&self) -> usize {
        self.id() as usize
    }

    pub fn level(&self) -> u32 {
        match self {
            Self::Functor(symbol) => symbol.level(),
            Self::ModuleType(symbol) => symbol.level(),
            Self::Module(symbol) => symbol.level(),
            Self::Effect(symbol) => symbol.level(),
            Self::Type(symbol) => symbol.level(),
            Self::Value(symbol) => symbol.level(),
        }
    }

    pub fn kind(&self) -> NamespaceKind {
        match self {
            Self::Functor(_) => NamespaceKind::Functor,
            Self::ModuleType(_) => NamespaceKind::ModuleType,
            Self::Module(_) => NamespaceKind::Module,
            Self::Effect(_) => NamespaceKind::Effect,
            Self::Type(_) => NamespaceKind::Type,
            Self::Value(_) => NamespaceKind::Value,
        }
    }
}

impl fmt::Display for AnySym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Functor(symbol) => write!(f, "{}", symbol),
            Self::ModuleType(symbol) => write!(f, "{}", symbol),
            Self::Module(symbol) => write!(f, "{}", symbol),
            Self::Effect(symbol) => write!(f, "{}", symbol),
            Self::Type(symbol) => write!(f, "{}", symbol),
            Self::Value(symbol) => write!(f, "{}", symbol),
        }
    }
}

pub fn merge2<A, B, DA, DB>(
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

pub fn merge3<A, B, C, DA, DB, DC>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
    c: Option<C>,
    default_c: DC,
) -> Option<(A, B, C)>
where
    DA: Fn() -> A,
    DB: Fn() -> B,
    DC: Fn() -> C,
{
    merge2(a, &default_a, b, &default_b).and_then(|(a, b)| {
        merge2(Some((a, b)), || (default_a(), default_b()), c, default_c)
            .map(|((a, b), c)| (a, b, c))
    })
}

pub fn merge4<A, B, C, D, DA, DB, DC, DD>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
    c: Option<C>,
    default_c: DC,
    d: Option<D>,
    default_d: DD,
) -> Option<(A, B, C, D)>
where
    DA: Fn() -> A,
    DB: Fn() -> B,
    DC: Fn() -> C,
    DD: Fn() -> D,
{
    merge3(a, &default_a, b, &default_b, c, &default_c).and_then(|(a, b, c)| {
        merge2(
            Some((a, b, c)),
            || (default_a(), default_b(), default_c()),
            d,
            default_d,
        )
        .map(|((a, b, c), d)| (a, b, c, d))
    })
}

pub fn merge5<A, B, C, D, E, DA, DB, DC, DD, DE>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
    c: Option<C>,
    default_c: DC,
    d: Option<D>,
    default_d: DD,
    e: Option<E>,
    default_e: DE,
) -> Option<(A, B, C, D, E)>
where
    DA: Fn() -> A,
    DB: Fn() -> B,
    DC: Fn() -> C,
    DD: Fn() -> D,
    DE: Fn() -> E,
{
    merge4(a, &default_a, b, &default_b, c, &default_c, d, &default_d).and_then(|(a, b, c, d)| {
        merge2(
            Some((a, b, c, d)),
            || (default_a(), default_b(), default_c(), default_d()),
            e,
            default_e,
        )
        .map(|((a, b, c, d), e)| (a, b, c, d, e))
    })
}

pub fn merge6<A, B, C, D, E, F, DA, DB, DC, DD, DE, DF>(
    a: Option<A>,
    default_a: DA,
    b: Option<B>,
    default_b: DB,
    c: Option<C>,
    default_c: DC,
    d: Option<D>,
    default_d: DD,
    e: Option<E>,
    default_e: DE,
    f: Option<F>,
    default_f: DF,
) -> Option<(A, B, C, D, E, F)>
where
    DA: Fn() -> A,
    DB: Fn() -> B,
    DC: Fn() -> C,
    DD: Fn() -> D,
    DE: Fn() -> E,
    DF: Fn() -> F,
{
    merge5(
        a, &default_a, b, &default_b, c, &default_c, d, &default_d, e, &default_e,
    )
    .and_then(|(a, b, c, d, e)| {
        merge2(
            Some((a, b, c, d, e)),
            || {
                (
                    default_a(),
                    default_b(),
                    default_c(),
                    default_d(),
                    default_e(),
                )
            },
            f,
            default_f,
        )
        .map(|((a, b, c, d, e), f)| (a, b, c, d, e, f))
    })
}

pub trait Substitute {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized;

    #[inline]
    fn subst_cow(&self, s: &HashMap<AnySym, AnySym>) -> Cow<'_, Self>
    where
        Self: Clone,
    {
        match self.try_subst(s) {
            Some(new_self) => Cow::Owned(new_self),
            None => Cow::Borrowed(self),
        }
    }

    #[inline]
    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        if let Some(new_self) = self.try_subst(s) {
            *self = new_self;
        }
    }

    #[inline]
    fn subst(mut self, s: &HashMap<AnySym, AnySym>) -> Self
    where
        Self: Sized,
    {
        self.subst_mut(s);
        self
    }
}

impl Substitute for FunctorSym {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Functor(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_functor().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitute for ModuleTypeSym {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::ModuleType(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_module_type().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitute for ModuleSym {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Module(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_module().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitute for EffectSym {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Effect(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_effect().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitute for TypeSym {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Type(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_type().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

impl Substitute for ValueSym {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let from = AnySym::Value(*self);
        if let Some(to) = s.get(&from) {
            let to = to.into_value().unwrap();
            Some(to)
        } else {
            None
        }
    }
}

// TODO kind of tedious to implement this for all symbols, but it's necessary for module sym substitution.
// For now just implement it for module namespace, if we need this for other namespaces create a macro.

impl Substitute for AnySym {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        s.get(self).copied()
    }
}
impl Substitute for ! {
    fn try_subst(&self, _s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        None
    }
}

impl<T> Substitute for Option<T>
where
    T: Substitute,
{
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        match self {
            Some(value) => value.try_subst(s).map(Some),
            None => Some(None),
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        if let Some(value) = self {
            value.subst_mut(s);
        }
    }
}

impl<T> Substitute for Vec<T>
where
    T: Substitute + Clone,
{
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = None;

        for (i, el) in self.iter().enumerate() {
            if let Some(el) = el.try_subst(s) {
                result.get_or_insert_with(|| self.clone())[i] = el;
            }
        }

        result
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        for el in self.iter_mut() {
            el.subst_mut(s);
        }
    }
}

impl<K, V> Substitute for HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Substitute + Clone,
{
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let mut result = None;

        for (key, value) in self {
            if let Some(value) = value.try_subst(s) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), value);
            }
        }

        result
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        for value in self.values_mut() {
            value.subst_mut(s);
        }
    }
}

macro_rules! impl_meta_substitute {
    ($($variant:ident),* $(,)?) => {
        impl<P> Substitute for kola_tree::meta::Meta<P>
        where
        P: kola_tree::meta::Phase<
            $($variant: Substitute),*
        >,
        {
            fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
                match self {
                    $(Self::$variant(t) => t.try_subst(s).map(Self::$variant)),*
                }
            }

        }
    };
}

impl_meta_substitute!(
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
    FunctorParam,
    FunctorBind,
    Bind,
    ModuleError,
    Module,
    ModulePath,
    ModuleImport,
    FunctorApp,
    ModuleExpr,
    ValueSpec,
    OpaqueTypeSpec,
    ModuleSpec,
    Spec,
    ConcreteModuleType,
    QualifiedModuleType,
    ModuleType,
);
