use std::ops::Index;

use kola_collections::HashMap;
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{self, FunctorName, ModuleName, ModuleNamespace, ModuleTypeName, TypeName, ValueName},
};

use crate::symbol::{ModuleSym, ModuleTypeSym, Substitute, TypeSym, ValueSym};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleBindConstraint {
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

impl ModuleBindConstraint {
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

impl Substitute<ModuleNamespace> for ModuleBindConstraint {
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
pub struct ModuleRef {
    /// The path that references some other module bind.
    pub path: Vec<ModuleName>,
    /// The global identifier of the module path that references some other module bind.
    pub id: Id<node::ModulePath>,
    /// The symbol of the module bind, this reference occured inside.
    pub source: ModuleSym,
    /// The location of the module reference in the source code.
    pub loc: Loc,
}

impl ModuleRef {
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

impl Substitute<ModuleNamespace> for ModuleRef {
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
pub struct ValueRef {
    /// The name of the value reference.
    pub name: ValueName,
    /// The identifier of the path expression that references some other value bind.
    pub id: Id<node::QualifiedExpr>,
    /// The symbol of the value bind, this reference occured inside.
    pub source: ValueSym,
    /// The location of the value reference in the source code.
    pub loc: Loc,
}

impl ValueRef {
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
pub struct TypeBindRef {
    /// The name of the type reference.
    pub name: TypeName,
    /// The identifier of the type path that references some other type bind.
    pub id: Id<node::QualifiedType>,
    /// The symbol of the type bind, this reference occured inside.
    pub source: TypeSym,
    /// The location of the type reference in the source code.
    pub loc: Loc,
}

impl TypeBindRef {
    pub fn new(name: TypeName, id: Id<node::QualifiedType>, source: TypeSym, loc: Loc) -> Self {
        Self {
            name,
            id,
            source,
            loc,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeRef {
    /// The name of the type reference.
    pub name: TypeName,
    /// The identifier of the type path that references some other type bind.
    pub id: Id<node::QualifiedType>,
    /// The location of the type reference in the source code.
    pub loc: Loc,
}

impl TypeRef {
    pub fn new(name: TypeName, id: Id<node::QualifiedType>, loc: Loc) -> Self {
        Self { name, id, loc }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleTypeBindRef {
    /// The name of the module type reference.
    pub name: ModuleTypeName,
    /// The identifier of the qualified module type that references some other module type bind.
    pub id: Id<node::QualifiedModuleType>,
    /// The symbol of the module type bind, this reference occured inside.
    pub source: ModuleTypeSym,
    /// The location of the module type reference in the source code.
    pub loc: Loc,
}

impl ModuleTypeBindRef {
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
pub struct ModuleTypeRef {
    /// The name of the module type reference.
    pub name: ModuleTypeName,
    /// The identifier of the qualified module type that references some other module type bind.
    pub id: Id<node::QualifiedModuleType>,
    /// The location of the type reference in the source code.
    pub loc: Loc,
}

impl ModuleTypeRef {
    pub fn new(name: ModuleTypeName, id: Id<node::QualifiedModuleType>, loc: Loc) -> Self {
        Self { name, id, loc }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Constraints {
    module_type_binds: Vec<ModuleTypeBindRef>,
    module_types: Vec<ModuleTypeRef>,
    module_binds: HashMap<ModuleSym, ModuleBindConstraint>,
    modules: Vec<ModuleRef>,
    type_binds: Vec<TypeBindRef>,
    types: Vec<TypeRef>,
    values: Vec<ValueRef>,
    // constructors: Vec<ConstructorRef>,
}

impl Constraints {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn insert_module_type_bind(&mut self, type_ref: ModuleTypeBindRef) {
        self.module_type_binds.push(type_ref);
    }

    #[inline]
    pub fn insert_module_type(&mut self, type_ref: ModuleTypeRef) {
        self.module_types.push(type_ref);
    }

    #[inline]
    pub fn constrain_module_bind(&mut self, sym: ModuleSym, constraint: ModuleBindConstraint) {
        self.module_binds.insert(sym, constraint);
    }

    #[inline]
    pub fn insert_module(&mut self, module_ref: ModuleRef) {
        self.modules.push(module_ref);
    }

    #[inline]
    pub fn insert_type_bind(&mut self, type_ref: TypeBindRef) {
        self.type_binds.push(type_ref);
    }

    #[inline]
    pub fn insert_type(&mut self, type_ref: TypeRef) {
        self.types.push(type_ref);
    }

    #[inline]
    pub fn insert_value(&mut self, value_ref: ValueRef) {
        self.values.push(value_ref);
    }

    #[inline]
    pub fn get_module_bind(&self, sym: ModuleSym) -> Option<&ModuleBindConstraint> {
        self.module_binds.get(&sym)
    }

    #[inline]
    pub fn module_type_binds(&self) -> &[ModuleTypeBindRef] {
        &self.module_type_binds
    }

    #[inline]
    pub fn module_types(&self) -> &[ModuleTypeRef] {
        &self.module_types
    }

    #[inline]
    pub fn module_binds(&self) -> &HashMap<ModuleSym, ModuleBindConstraint> {
        &self.module_binds
    }

    #[inline]
    pub fn modules(&self) -> &[ModuleRef] {
        &self.modules
    }

    #[inline]
    pub fn type_binds(&self) -> &[TypeBindRef] {
        &self.type_binds
    }

    #[inline]
    pub fn types(&self) -> &[TypeRef] {
        &self.types
    }

    #[inline]
    pub fn values(&self) -> &[ValueRef] {
        &self.values
    }
}

impl Index<ModuleSym> for Constraints {
    type Output = ModuleBindConstraint;

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
            new_constraints.constrain_module_bind(to, constraint);
        }

        if let Some(modules) = self.modules.try_substitute(from, to) {
            result.get_or_insert_with(|| self.clone()).modules = modules;
        }

        result
    }

    fn substitute_mut(&mut self, from: ModuleSym, to: ModuleSym) {
        if let Some(constraint) = self.module_binds.remove(&from) {
            self.constrain_module_bind(to, constraint);
        }

        for module_ref in &mut self.modules {
            module_ref.substitute_mut(from, to);
        }
    }
}
