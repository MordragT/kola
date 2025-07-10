use std::ops::Index;

use kola_collections::HashMap;
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{
        self, EffectName, FunctorName, ModuleName, ModuleNamespace, ModuleTypeName, TypeName,
        ValueName,
    },
};

use crate::symbol::{EffectSym, ModuleSym, ModuleTypeSym, Substitute, TypeSym, ValueSym};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleBindConst {
    Functor {
        id: Id<node::FunctorApp>,
        bind: ModuleSym,
        loc: Loc,
        functor: FunctorName,
        arg: ModuleSym,
    },
    Path {
        id: Id<node::ModulePath>,
        bind: ModuleSym,
        loc: Loc,
        path: Vec<ModuleName>,
    },
}

impl ModuleBindConst {
    pub fn functor(
        id: Id<node::FunctorApp>,
        bind: ModuleSym,
        loc: Loc,
        functor: FunctorName,
        arg: ModuleSym,
    ) -> Self {
        Self::Functor {
            id,
            bind,
            loc,
            functor,
            arg,
        }
    }

    pub fn path(
        id: Id<node::ModulePath>,
        bind: ModuleSym,
        loc: Loc,
        path: Vec<ModuleName>,
    ) -> Self {
        Self::Path {
            id,
            bind,
            loc,
            path,
        }
    }

    pub fn bind(&self) -> ModuleSym {
        match self {
            Self::Functor { bind, .. } => *bind,
            Self::Path { bind, .. } => *bind,
        }
    }

    pub fn loc(&self) -> Loc {
        match self {
            Self::Functor { loc, .. } => *loc,
            Self::Path { loc, .. } => *loc,
        }
    }
}

impl Substitute<ModuleNamespace> for ModuleBindConst {
    fn try_substitute(&self, from: ModuleSym, to: ModuleSym) -> Option<Self> {
        match self {
            Self::Functor {
                id,
                bind,
                loc,
                functor,
                arg,
            } => {
                if *bind == from {
                    // TODO if bind is equal to from then arg cannot be equal to from
                    // therefore no need to check arg, still this doesn't look that great
                    Some(Self::functor(*id, to, *loc, *functor, *arg))
                } else if *arg == from {
                    // Same as above
                    Some(Self::functor(*id, *bind, *loc, *functor, to))
                } else {
                    None
                }
            }
            Self::Path {
                id,
                bind,
                loc,
                path,
            } => {
                if *bind == from {
                    Some(Self::path(*id, to, *loc, path.clone()))
                } else {
                    None
                }
            }
        }
    }

    fn substitute_mut(&mut self, from: ModuleSym, to: ModuleSym) {
        match self {
            Self::Functor { bind, arg, .. } => {
                if *bind == from {
                    *bind = to;
                }
                if *arg == from {
                    *arg = to;
                }
            }
            Self::Path { bind, .. } => {
                if *bind == from {
                    *bind = to;
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleConst {
    /// The path that references some other module bind.
    pub path: Vec<ModuleName>,
    /// The global identifier of the module path that references some other module bind.
    pub id: Id<node::ModulePath>,
    /// The symbol of the module bind, this reference occured inside.
    pub source: ModuleSym,
    /// The location of the module reference in the source code.
    pub loc: Loc,
}

impl ModuleConst {
    pub fn new(
        path: Vec<ModuleName>,
        id: Id<node::ModulePath>,
        source: ModuleSym,
        loc: Loc,
    ) -> Self {
        Self {
            path,
            id,
            source,
            loc,
        }
    }
}

impl Substitute<ModuleNamespace> for ModuleConst {
    fn try_substitute(&self, from: ModuleSym, to: ModuleSym) -> Option<Self> {
        if self.source == from {
            Some(Self::new(self.path.clone(), self.id, to, self.loc))
        } else {
            None
        }
    }

    fn substitute_mut(&mut self, from: ModuleSym, to: ModuleSym) {
        if self.source == from {
            self.source = to;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueConst {
    /// The name of the value reference.
    pub name: ValueName,
    /// The identifier of the path expression that references some other value bind.
    pub id: Id<node::QualifiedExpr>,
    /// The symbol of the value bind, this reference occured inside.
    pub source: ValueSym,
    /// The location of the value reference in the source code.
    pub loc: Loc,
}

impl ValueConst {
    pub fn new(name: ValueName, id: Id<node::QualifiedExpr>, source: ValueSym, loc: Loc) -> Self {
        Self {
            name,
            id,
            source,
            loc,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeBindConst {
    /// The name of the type reference.
    pub name: TypeName,
    /// The identifier of the type path that references some other type bind.
    pub id: Id<node::QualifiedType>,
    /// The symbol of the type bind, this reference occured inside.
    pub source: TypeSym,
    /// The location of the type reference in the source code.
    pub loc: Loc,
}

impl TypeBindConst {
    pub fn new(name: TypeName, id: Id<node::QualifiedType>, source: TypeSym, loc: Loc) -> Self {
        Self {
            name,
            id,
            source,
            loc,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeConst {
    Qualified {
        /// The name of the type reference.
        name: TypeName,
        /// The identifier of the type path that references some other type bind.
        id: Id<node::QualifiedType>,
        /// The location of the type reference in the source code.
        loc: Loc,
    },
    TypeRep {
        /// The name of the type reference.
        name: TypeName,
        /// The identifier of the type path that references some other type bind.
        id: Id<node::TypeRepExpr>,
        /// The location of the type reference in the source code.
        loc: Loc,
    },
}

impl TypeConst {
    pub fn qualified(name: TypeName, id: Id<node::QualifiedType>, loc: Loc) -> Self {
        Self::Qualified { name, id, loc }
    }

    pub fn type_rep(name: TypeName, id: Id<node::TypeRepExpr>, loc: Loc) -> Self {
        Self::TypeRep { name, id, loc }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EffectBindConst {
    /// The name of the effect reference.
    pub name: EffectName,
    /// The identifier of the effect path that references some other effect bind.
    pub id: Id<node::QualifiedEffectType>,
    /// The symbol of the effect bind, this reference occured inside.
    pub source: EffectSym,
    /// The location of the effect reference in the source code.
    pub loc: Loc,
}

impl EffectBindConst {
    pub fn new(
        name: node::EffectName,
        id: Id<node::QualifiedEffectType>,
        source: EffectSym,
        loc: Loc,
    ) -> Self {
        Self {
            name,
            id,
            source,
            loc,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EffectConst {
    /// The name of the effect reference.
    pub name: EffectName,
    /// The identifier of the effect path that references some other effect bind.
    pub id: Id<node::QualifiedEffectType>,
    /// The location of the effect reference in the source code.
    pub loc: Loc,
}

impl EffectConst {
    pub fn new(name: EffectName, id: Id<node::QualifiedEffectType>, loc: Loc) -> Self {
        Self { name, id, loc }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleTypeBindConst {
    /// The name of the module type reference.
    pub name: ModuleTypeName,
    /// The identifier of the qualified module type that references some other module type bind.
    pub id: Id<node::QualifiedModuleType>,
    /// The symbol of the module type bind, this reference occured inside.
    pub source: ModuleTypeSym,
    /// The location of the module type reference in the source code.
    pub loc: Loc,
}

impl ModuleTypeBindConst {
    pub fn new(
        name: ModuleTypeName,
        id: Id<node::QualifiedModuleType>,
        source: ModuleTypeSym,
        loc: Loc,
    ) -> Self {
        Self {
            name,
            id,
            source,
            loc,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleTypeConst {
    /// The name of the module type reference.
    pub name: ModuleTypeName,
    /// The identifier of the qualified module type that references some other module type bind.
    pub id: Id<node::QualifiedModuleType>,
    /// The location of the type reference in the source code.
    pub loc: Loc,
}

impl ModuleTypeConst {
    pub fn new(name: ModuleTypeName, id: Id<node::QualifiedModuleType>, loc: Loc) -> Self {
        Self { name, id, loc }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Constraints {
    module_type_binds: Vec<ModuleTypeBindConst>,
    module_types: Vec<ModuleTypeConst>,
    module_binds: HashMap<ModuleSym, ModuleBindConst>,
    modules: Vec<ModuleConst>,
    effect_binds: Vec<EffectBindConst>,
    effects: Vec<EffectConst>,
    type_binds: Vec<TypeBindConst>,
    types: Vec<TypeConst>,
    values: Vec<ValueConst>,
}

impl Constraints {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert_module_type_bind(&mut self, type_ref: ModuleTypeBindConst) {
        self.module_type_binds.push(type_ref);
    }

    #[inline]
    pub fn insert_module_type(&mut self, type_ref: ModuleTypeConst) {
        self.module_types.push(type_ref);
    }

    #[inline]
    pub fn insert_module_bind(&mut self, sym: ModuleSym, constraint: ModuleBindConst) {
        self.module_binds.insert(sym, constraint);
    }

    #[inline]
    pub fn insert_module(&mut self, module_ref: ModuleConst) {
        self.modules.push(module_ref);
    }

    #[inline]
    pub fn insert_effect_bind(&mut self, effect_ref: EffectBindConst) {
        self.effect_binds.push(effect_ref);
    }

    #[inline]
    pub fn insert_effect(&mut self, effect_ref: EffectConst) {
        self.effects.push(effect_ref);
    }

    #[inline]
    pub fn insert_type_bind(&mut self, type_ref: TypeBindConst) {
        self.type_binds.push(type_ref);
    }

    #[inline]
    pub fn insert_type(&mut self, type_ref: TypeConst) {
        self.types.push(type_ref);
    }

    #[inline]
    pub fn insert_value(&mut self, value_ref: ValueConst) {
        self.values.push(value_ref);
    }

    #[inline]
    pub fn get_module_bind(&self, sym: ModuleSym) -> Option<&ModuleBindConst> {
        self.module_binds.get(&sym)
    }

    #[inline]
    pub fn module_type_binds(&self) -> &[ModuleTypeBindConst] {
        &self.module_type_binds
    }

    #[inline]
    pub fn module_types(&self) -> &[ModuleTypeConst] {
        &self.module_types
    }

    #[inline]
    pub fn module_binds(&self) -> &HashMap<ModuleSym, ModuleBindConst> {
        &self.module_binds
    }

    #[inline]
    pub fn modules(&self) -> &[ModuleConst] {
        &self.modules
    }

    #[inline]
    pub fn effect_binds(&self) -> &[EffectBindConst] {
        &self.effect_binds
    }

    #[inline]
    pub fn effects(&self) -> &[EffectConst] {
        &self.effects
    }

    #[inline]
    pub fn type_binds(&self) -> &[TypeBindConst] {
        &self.type_binds
    }

    #[inline]
    pub fn types(&self) -> &[TypeConst] {
        &self.types
    }

    #[inline]
    pub fn values(&self) -> &[ValueConst] {
        &self.values
    }
}

impl Index<ModuleSym> for Constraints {
    type Output = ModuleBindConst;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        self.module_binds
            .get(&index)
            .expect("ModuleSym not found in ModuleRefs")
    }
}

impl Substitute<ModuleNamespace> for Constraints {
    fn try_substitute(&self, from: ModuleSym, to: ModuleSym) -> Option<Self> {
        let mut result = None;

        if self.module_binds.contains_key(&from) {
            let new_constraints = result.get_or_insert_with(|| self.clone());
            let constraint = new_constraints.module_binds.remove(&from).unwrap(); // Safe to unwrap since we checked above
            new_constraints.insert_module_bind(to, constraint);
        }

        if let Some(modules) = self.modules.try_substitute(from, to) {
            result.get_or_insert_with(|| self.clone()).modules = modules;
        }

        result
    }

    fn substitute_mut(&mut self, from: ModuleSym, to: ModuleSym) {
        if let Some(constraint) = self.module_binds.remove(&from) {
            self.insert_module_bind(to, constraint);
        }

        for module_ref in &mut self.modules {
            module_ref.substitute_mut(from, to);
        }
    }
}
