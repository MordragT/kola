use std::{fmt, hash::Hash, ops::Index};

use derive_more::From;
use kola_collections::{HashMap, hash_map};
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{
        self, EffectNamespace, FunctorNamespace, ModuleNamespace, ModuleTypeNamespace, Namespace,
        NamespaceKind, TypeNamespace, ValueNamespace, Vis,
    },
};

use crate::symbol::{
    AnySym, EffectSym, FunctorSym, ModuleSym, ModuleTypeSym, Substitute, Sym, TypeSym, ValueSym,
    merge6,
};

pub struct Def<T> {
    pub loc: Loc,
    pub vis: Vis,
    pub id: Option<Id<T>>,
}

impl<T> Def<T> {
    pub const fn bound(id: Id<T>, vis: Vis, loc: Loc) -> Self {
        Self {
            loc,
            vis,
            id: Some(id),
        }
    }

    pub const fn unbound(loc: Loc, vis: Vis) -> Self {
        Self { loc, vis, id: None }
    }

    pub const fn loc(&self) -> Loc {
        self.loc
    }

    pub const fn vis(&self) -> Vis {
        self.vis
    }

    pub fn id(&self) -> Id<T> {
        self.id.unwrap()
    }
}

impl<T> fmt::Debug for Def<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BindInfo")
            .field("id", &self.id)
            .field("loc", &self.loc)
            .field("vis", &self.vis)
            .finish()
    }
}

impl<T> Clone for Def<T> {
    fn clone(&self) -> Self {
        Self {
            loc: self.loc,
            vis: self.vis,
            id: self.id,
        }
    }
}

impl<T> Copy for Def<T> {}

impl<T> PartialEq for Def<T> {
    fn eq(&self, other: &Self) -> bool {
        self.loc == other.loc
    }
}

impl<T> Eq for Def<T> {}

impl<T> PartialOrd for Def<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Def<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.loc.cmp(&other.loc)
    }
}

impl<T> Hash for Def<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.loc.hash(state);
    }
}

pub type FunctorDef = Def<node::FunctorBind>;
pub type ModuleTypeDef = Def<node::ModuleTypeBind>;
pub type ModuleDef = Def<node::ModuleBind>;
pub type EffectTypeDef = Def<node::EffectTypeBind>;
pub type TypeDef = Def<node::TypeBind>;
pub type ValueDef = Def<node::ValueBind>;

#[derive(Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnyDef {
    Functor(FunctorDef),
    ModuleType(ModuleTypeDef),
    Module(ModuleDef),
    Effect(EffectTypeDef),
    Type(TypeDef),
    Value(ValueDef),
}

impl AnyDef {
    pub const fn kind(&self) -> NamespaceKind {
        match self {
            AnyDef::Functor(_) => NamespaceKind::Functor,
            AnyDef::ModuleType(_) => NamespaceKind::ModuleType,
            AnyDef::Module(_) => NamespaceKind::Module,
            AnyDef::Effect(_) => NamespaceKind::Effect,
            AnyDef::Type(_) => NamespaceKind::Type,
            AnyDef::Value(_) => NamespaceKind::Value,
        }
    }

    pub const fn location(&self) -> Loc {
        match self {
            AnyDef::Functor(info) => info.loc,
            AnyDef::ModuleType(info) => info.loc,
            AnyDef::Module(info) => info.loc,
            AnyDef::Effect(info) => info.loc,
            AnyDef::Type(info) => info.loc,
            AnyDef::Value(info) => info.loc,
        }
    }

    pub const fn visibility(&self) -> Vis {
        match self {
            AnyDef::Functor(info) => info.vis,
            AnyDef::ModuleType(info) => info.vis,
            AnyDef::Module(info) => info.vis,
            AnyDef::Effect(info) => info.vis,
            AnyDef::Type(info) => info.vis,
            AnyDef::Value(info) => info.vis,
        }
    }
}

#[derive(Debug)]
pub struct Defs<N: Namespace, T>(HashMap<Sym<N>, Def<T>>);

impl<N: Namespace, T> Defs<N, T> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert(&mut self, symbol: Sym<N>, info: Def<T>) {
        self.0.insert(symbol, info);
    }

    #[inline]
    pub fn get(&self, symbol: Sym<N>) -> Option<Def<T>>
    where
        Def<T>: Copy,
    {
        self.0.get(&symbol).copied()
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<'_, Sym<N>, Def<T>> {
        self.0.iter()
    }
}

impl<N: Namespace, T> Clone for Defs<N, T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<N: Namespace, T> Default for Defs<N, T> {
    #[inline]
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl<N: Namespace, T> IntoIterator for Defs<N, T> {
    type Item = (Sym<N>, Def<T>);
    type IntoIter = hash_map::IntoIter<Sym<N>, Def<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, N: Namespace, T> IntoIterator for &'a Defs<N, T> {
    type Item = (&'a Sym<N>, &'a Def<T>);
    type IntoIter = hash_map::Iter<'a, Sym<N>, Def<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, N: Namespace, T> IntoIterator for &'a mut Defs<N, T> {
    type Item = (&'a Sym<N>, &'a mut Def<T>);
    type IntoIter = hash_map::IterMut<'a, Sym<N>, Def<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<N: Namespace, T> Index<Sym<N>> for Defs<N, T> {
    type Output = Def<T>;

    fn index(&self, sym: Sym<N>) -> &Self::Output {
        self.0.get(&sym).expect("Bind not found")
    }
}

// impl<N: Namespace, T> Substitute for Defs<N, T>
// where
//     AnySym: From<Sym<N>>,{
//     fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
//     where
//         Self: Sized,
//     {

//         if let Some(def) = self.0.get(&from) {
//             let mut new_defs = self.clone();
//             new_defs.0.remove(&from);
//             new_defs.0.insert(to, *def);
//             Some(new_defs)
//         } else {
//             None
//         }
//     }

//     fn subst_mut(&mut self, from: Sym<N>, to: Sym<N>)
//     where
//         Self: Sized,
//     {
//         if let Some(def) = self.0.remove(&from) {
//             self.0.insert(to, def);
//         }
//     }
// }

impl Substitute for Defs<FunctorNamespace, node::FunctorBind> {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = None;

        for (from, to) in s {
            if let &AnySym::Functor(from) = from
                && let &AnySym::Functor(to) = to
                && let Some(value) = self.get(from)
            {
                result.get_or_insert_with(|| self.clone()).0.remove(&from);
                result.as_mut().unwrap().insert(to, value);
            }
        }

        result
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        for (from, to) in s {
            if let AnySym::Functor(from) = from
                && let AnySym::Functor(to) = to
                && let Some(value) = self.0.remove(from)
            {
                self.0.insert(*to, value);
            }
        }
    }
}

impl Substitute for Defs<ModuleTypeNamespace, node::ModuleTypeBind> {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = None;

        for (from, to) in s {
            if let &AnySym::ModuleType(from) = from
                && let &AnySym::ModuleType(to) = to
                && let Some(value) = self.get(from)
            {
                result.get_or_insert_with(|| self.clone()).0.remove(&from);
                result.as_mut().unwrap().insert(to, value);
            }
        }

        result
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        for (from, to) in s {
            if let AnySym::ModuleType(from) = from
                && let AnySym::ModuleType(to) = to
                && let Some(value) = self.0.remove(from)
            {
                self.0.insert(*to, value);
            }
        }
    }
}

impl Substitute for Defs<ModuleNamespace, node::ModuleBind> {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = None;

        for (from, to) in s {
            if let &AnySym::Module(from) = from
                && let &AnySym::Module(to) = to
                && let Some(value) = self.get(from)
            {
                result.get_or_insert_with(|| self.clone()).0.remove(&from);
                result.as_mut().unwrap().insert(to, value);
            }
        }

        result
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        for (from, to) in s {
            if let AnySym::Module(from) = from
                && let AnySym::Module(to) = to
                && let Some(value) = self.0.remove(from)
            {
                self.0.insert(*to, value);
            }
        }
    }
}

impl Substitute for Defs<EffectNamespace, node::EffectTypeBind> {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = None;

        for (from, to) in s {
            if let &AnySym::Effect(from) = from
                && let &AnySym::Effect(to) = to
                && let Some(value) = self.get(from)
            {
                result.get_or_insert_with(|| self.clone()).0.remove(&from);
                result.as_mut().unwrap().insert(to, value);
            }
        }

        result
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        for (from, to) in s {
            if let AnySym::Effect(from) = from
                && let AnySym::Effect(to) = to
                && let Some(value) = self.0.remove(from)
            {
                self.0.insert(*to, value);
            }
        }
    }
}

impl Substitute for Defs<TypeNamespace, node::TypeBind> {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = None;

        for (from, to) in s {
            if let &AnySym::Type(from) = from
                && let &AnySym::Type(to) = to
                && let Some(value) = self.get(from)
            {
                result.get_or_insert_with(|| self.clone()).0.remove(&from);
                result.as_mut().unwrap().insert(to, value);
            }
        }

        result
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        for (from, to) in s {
            if let AnySym::Type(from) = from
                && let AnySym::Type(to) = to
                && let Some(value) = self.0.remove(from)
            {
                self.0.insert(*to, value);
            }
        }
    }
}

impl Substitute for Defs<ValueNamespace, node::ValueBind> {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = None;

        for (from, to) in s {
            if let &AnySym::Value(from) = from
                && let &AnySym::Value(to) = to
                && let Some(value) = self.get(from)
            {
                result.get_or_insert_with(|| self.clone()).0.remove(&from);
                result.as_mut().unwrap().insert(to, value);
            }
        }

        result
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        for (from, to) in s {
            if let AnySym::Value(from) = from
                && let AnySym::Value(to) = to
                && let Some(value) = self.0.remove(from)
            {
                self.0.insert(*to, value);
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Definitions {
    functors: Defs<FunctorNamespace, node::FunctorBind>,
    module_types: Defs<ModuleTypeNamespace, node::ModuleTypeBind>,
    modules: Defs<ModuleNamespace, node::ModuleBind>,
    effects: Defs<EffectNamespace, node::EffectTypeBind>,
    types: Defs<TypeNamespace, node::TypeBind>,
    values: Defs<ValueNamespace, node::ValueBind>,
}

impl Definitions {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert_functor(&mut self, sym: FunctorSym, def: FunctorDef) {
        self.functors.insert(sym, def);
    }

    #[inline]
    pub fn insert_module_type(&mut self, sym: ModuleTypeSym, def: ModuleTypeDef) {
        self.module_types.insert(sym, def);
    }

    #[inline]
    pub fn insert_module(&mut self, sym: ModuleSym, def: ModuleDef) {
        self.modules.insert(sym, def);
    }

    #[inline]
    pub fn insert_effect(&mut self, sym: EffectSym, def: EffectTypeDef) {
        self.effects.insert(sym, def);
    }

    #[inline]
    pub fn insert_type(&mut self, sym: TypeSym, def: TypeDef) {
        self.types.insert(sym, def);
    }

    #[inline]
    pub fn insert_value(&mut self, sym: ValueSym, def: ValueDef) {
        self.values.insert(sym, def);
    }

    #[inline]
    pub fn get_functor(&self, sym: FunctorSym) -> Option<FunctorDef> {
        self.functors.get(sym)
    }

    #[inline]
    pub fn get_module_type(&self, sym: ModuleTypeSym) -> Option<ModuleTypeDef> {
        self.module_types.get(sym)
    }

    #[inline]
    pub fn get_module(&self, sym: ModuleSym) -> Option<ModuleDef> {
        self.modules.get(sym)
    }

    #[inline]
    pub fn get_effect(&self, sym: EffectSym) -> Option<EffectTypeDef> {
        self.effects.get(sym)
    }

    #[inline]
    pub fn get_type(&self, sym: TypeSym) -> Option<TypeDef> {
        self.types.get(sym)
    }

    #[inline]
    pub fn get_value(&self, sym: ValueSym) -> Option<ValueDef> {
        self.values.get(sym)
    }

    #[inline]
    pub fn get(&self, sym: impl Into<AnySym>) -> Option<AnyDef> {
        match sym.into() {
            AnySym::Functor(sym) => self.get_functor(sym).map(AnyDef::Functor),
            AnySym::ModuleType(sym) => self.get_module_type(sym).map(AnyDef::ModuleType),
            AnySym::Module(sym) => self.get_module(sym).map(AnyDef::Module),
            AnySym::Effect(sym) => self.get_effect(sym).map(AnyDef::Effect),
            AnySym::Type(sym) => self.get_type(sym).map(AnyDef::Type),
            AnySym::Value(sym) => self.get_value(sym).map(AnyDef::Value),
        }
    }

    #[inline]
    pub fn iter_functors(&self) -> impl Iterator<Item = (FunctorSym, FunctorDef)> {
        self.functors.iter().map(|(&sym, &def)| (sym, def))
    }

    #[inline]
    pub fn iter_module_types(&self) -> impl Iterator<Item = (ModuleTypeSym, ModuleTypeDef)> {
        self.module_types.iter().map(|(&sym, &def)| (sym, def))
    }

    #[inline]
    pub fn iter_modules(&self) -> impl Iterator<Item = (ModuleSym, ModuleDef)> {
        self.modules.iter().map(|(&sym, &def)| (sym, def))
    }

    #[inline]
    pub fn iter_effects(&self) -> impl Iterator<Item = (EffectSym, EffectTypeDef)> {
        self.effects.iter().map(|(&sym, &def)| (sym, def))
    }

    #[inline]
    pub fn iter_types(&self) -> impl Iterator<Item = (TypeSym, TypeDef)> {
        self.types.iter().map(|(&sym, &def)| (sym, def))
    }

    #[inline]
    pub fn iter_values(&self) -> impl Iterator<Item = (ValueSym, ValueDef)> {
        self.values.iter().map(|(&sym, &def)| (sym, def))
    }
}

impl Index<FunctorSym> for Definitions {
    type Output = FunctorDef;

    fn index(&self, index: FunctorSym) -> &Self::Output {
        &self.functors[index]
    }
}

impl Index<ModuleTypeSym> for Definitions {
    type Output = ModuleTypeDef;

    fn index(&self, index: ModuleTypeSym) -> &Self::Output {
        &self.module_types[index]
    }
}

impl Index<ModuleSym> for Definitions {
    type Output = ModuleDef;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        &self.modules[index]
    }
}

impl Index<EffectSym> for Definitions {
    type Output = EffectTypeDef;

    fn index(&self, index: EffectSym) -> &Self::Output {
        &self.effects[index]
    }
}

impl Index<TypeSym> for Definitions {
    type Output = TypeDef;

    fn index(&self, index: TypeSym) -> &Self::Output {
        &self.types[index]
    }
}

impl Index<ValueSym> for Definitions {
    type Output = ValueDef;

    fn index(&self, index: ValueSym) -> &Self::Output {
        &self.values[index]
    }
}

impl Substitute for Definitions {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        let functors = self.functors.try_subst(s);
        let module_types = self.module_types.try_subst(s);
        let modules = self.modules.try_subst(s);
        let effects = self.effects.try_subst(s);
        let types = self.types.try_subst(s);
        let values = self.values.try_subst(s);

        merge6(
            functors,
            || self.functors.clone(),
            module_types,
            || self.module_types.clone(),
            modules,
            || self.modules.clone(),
            effects,
            || self.effects.clone(),
            types,
            || self.types.clone(),
            values,
            || self.values.clone(),
        )
        .map(
            |(functors, module_types, modules, effects, types, values)| Self {
                functors,
                module_types,
                modules,
                effects,
                types,
                values,
            },
        )
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>)
    where
        Self: Sized,
    {
        self.functors.subst_mut(s);
        self.module_types.subst_mut(s);
        self.modules.subst_mut(s);
        self.effects.subst_mut(s);
        self.types.subst_mut(s);
        self.values.subst_mut(s);
    }
}
