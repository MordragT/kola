use derive_more::From;
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

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

pub trait Substitute<N: Namespace> {
    fn try_substitute(&self, from: Sym<N>, to: Sym<N>) -> Option<Self>
    where
        Self: Sized;

    #[inline]
    fn substitute_cow(&self, from: Sym<N>, to: Sym<N>) -> Cow<'_, Self>
    where
        Self: Clone,
    {
        match self.try_substitute(from, to) {
            Some(new_self) => Cow::Owned(new_self),
            None => Cow::Borrowed(self),
        }
    }

    #[inline]
    fn substitute_mut(&mut self, from: Sym<N>, to: Sym<N>)
    where
        Self: Sized,
    {
        if let Some(new_self) = self.try_substitute(from, to) {
            *self = new_self;
        }
    }

    #[inline]
    fn substitute(mut self, from: Sym<N>, to: Sym<N>) -> Self
    where
        Self: Sized,
    {
        self.substitute_mut(from, to);
        self
    }
}

impl<N: Namespace> Substitute<N> for Sym<N> {
    fn try_substitute(&self, from: Sym<N>, to: Sym<N>) -> Option<Self> {
        if self == &from { Some(to) } else { None }
    }
}

// TODO kind of tedious to implement this for all symbols, but it's necessary for module sym substitution.
// For now just implement it for module namespace, if we need this for other namespaces create a macro.

impl Substitute<ModuleNamespace> for AnySym {
    fn try_substitute(&self, from: ModuleSym, to: ModuleSym) -> Option<Self> {
        match self {
            Self::Functor(symbol) => symbol.try_substitute(from, to).map(Self::Functor),
            Self::ModuleType(symbol) => symbol.try_substitute(from, to).map(Self::ModuleType),
            Self::Module(symbol) => symbol.try_substitute(from, to).map(Self::Module),
            Self::Effect(symbol) => symbol.try_substitute(from, to).map(Self::Effect),
            Self::Type(symbol) => symbol.try_substitute(from, to).map(Self::Type),
            Self::Value(symbol) => symbol.try_substitute(from, to).map(Self::Value),
        }
    }
}

impl Substitute<ModuleNamespace> for FunctorSym {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self> {
        None
    }
}

impl Substitute<ModuleNamespace> for ModuleTypeSym {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self> {
        None
    }
}

impl Substitute<ModuleNamespace> for EffectSym {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self> {
        None
    }
}

impl Substitute<ModuleNamespace> for TypeSym {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self> {
        None
    }
}

impl Substitute<ModuleNamespace> for ValueSym {
    fn try_substitute(&self, _from: ModuleSym, _to: ModuleSym) -> Option<Self> {
        None
    }
}

impl<N: Namespace> Substitute<N> for ! {
    fn try_substitute(&self, _from: Sym<N>, _to: Sym<N>) -> Option<Self> {
        None
    }
}

impl<T, N> Substitute<N> for Option<T>
where
    T: Substitute<N>,
    N: Namespace,
{
    fn try_substitute(&self, from: Sym<N>, to: Sym<N>) -> Option<Self> {
        match self {
            Some(value) => value.try_substitute(from, to).map(Some),
            None => Some(None),
        }
    }

    fn substitute_mut(&mut self, from: Sym<N>, to: Sym<N>) {
        if let Some(value) = self {
            value.substitute_mut(from, to);
        }
    }
}

impl<T, N> Substitute<N> for Vec<T>
where
    T: Substitute<N> + Clone,
    N: Namespace,
{
    fn try_substitute(&self, from: Sym<N>, to: Sym<N>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = None;

        for (i, el) in self.iter().enumerate() {
            if let Some(el) = el.try_substitute(from, to) {
                result.get_or_insert_with(|| self.clone())[i] = el;
            }
        }

        result
    }

    fn substitute_mut(&mut self, from: Sym<N>, to: Sym<N>) {
        for el in self.iter_mut() {
            el.substitute_mut(from, to);
        }
    }
}

impl<K, V, N> Substitute<N> for HashMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Substitute<N> + Clone,
    N: Namespace,
{
    fn try_substitute(&self, from: Sym<N>, to: Sym<N>) -> Option<Self> {
        let mut result = None;

        for (key, value) in self {
            if let Some(value) = value.try_substitute(from, to) {
                result
                    .get_or_insert_with(|| self.clone())
                    .insert(key.clone(), value);
            }
        }

        result
    }

    fn substitute_mut(&mut self, from: Sym<N>, to: Sym<N>) {
        for value in self.values_mut() {
            value.substitute_mut(from, to);
        }
    }
}

macro_rules! impl_meta_substitute {
    ($($variant:ident),* $(,)?) => {
        impl<P> Substitute<FunctorNamespace> for kola_tree::meta::Meta<P>
        where
        P: kola_tree::meta::Phase<
            $($variant: Substitute<FunctorNamespace>),*
        >,
        {
            fn try_substitute(&self, from: FunctorSym, to: FunctorSym) -> Option<Self> {
                match self {
                    $(Self::$variant(t) => t.try_substitute(from, to).map(Self::$variant)),*
                }
            }

        }

        impl<P> Substitute<ModuleTypeNamespace> for kola_tree::meta::Meta<P>
        where
        P: kola_tree::meta::Phase<
            $($variant: Substitute<ModuleTypeNamespace>),*
        >,
        {
            fn try_substitute(&self, from: ModuleTypeSym, to: ModuleTypeSym) -> Option<Self> {
                match self {
                    $(Self::$variant(t) => t.try_substitute(from, to).map(Self::$variant)),*
                }
            }

        }

        impl<P> Substitute<ModuleNamespace> for kola_tree::meta::Meta<P>
        where
        P: kola_tree::meta::Phase<
            $($variant: Substitute<ModuleNamespace>),*
        >,
        {
            fn try_substitute(&self, from: ModuleSym, to: ModuleSym) -> Option<Self> {
                match self {
                    $(Self::$variant(t) => t.try_substitute(from, to).map(Self::$variant)),*
                }
            }

        }

        impl<P> Substitute<TypeNamespace> for kola_tree::meta::Meta<P>
        where
        P: kola_tree::meta::Phase<
            $($variant: Substitute<TypeNamespace>),*
        >,
        {
            fn try_substitute(&self, from: TypeSym, to: TypeSym) -> Option<Self> {
                match self {
                    $(Self::$variant(t) => t.try_substitute(from, to).map(Self::$variant)),*
                }
            }

        }

        impl<P> Substitute<ValueNamespace> for kola_tree::meta::Meta<P>
        where
        P: kola_tree::meta::Phase<
            $($variant: Substitute<ValueNamespace>),*
        >,
        {
            fn try_substitute(&self, from: ValueSym, to: ValueSym) -> Option<Self> {
                match self {
                    $(Self::$variant(t) => t.try_substitute(from, to).map(Self::$variant)),*
                }
            }

        }

    };
}

impl_meta_substitute!(
    FunctorName,
    ModuleTypeName,
    ModuleName,
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
    TagExpr,
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
    SymbolExpr,
    TypeRepExpr,
    ExprError,
    Expr,
    // Types
    QualifiedEffectType,
    EffectOpType,
    EffectRowType,
    EffectType,
    QualifiedType,
    TypeVar,
    RecordFieldType,
    RecordType,
    TagType,
    VariantType,
    FuncType,
    TypeApplication,
    CompType,
    Type,
    TypeError,
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
