use std::collections::{HashMap, VecDeque};

use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{self, FunctorName, ModuleName, ModuleTypeName, TypeName, ValueName},
};

use crate::symbol::{
    AnySym, ModuleSym, ModuleTypeSym, Substitute, TypeSym, ValueSym, merge2, merge4, merge6,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleBindConst {
    Functor {
        id: Id<node::FunctorApp>,
        parent: ModuleSym,
        bind: ModuleSym,
        path: Option<ModuleSym>,
        loc: Loc,
        functor: FunctorName,
        args: Vec<ModuleSym>,
    },
    Path {
        id: Id<node::ModulePath>,
        parent: ModuleSym,
        bind: ModuleSym,
        loc: Loc,
        path: Vec<ModuleName>,
    },
}

impl ModuleBindConst {
    pub fn functor(
        id: Id<node::FunctorApp>,
        parent: ModuleSym,
        bind: ModuleSym,
        path: Option<ModuleSym>,
        loc: Loc,
        functor: FunctorName,
        args: Vec<ModuleSym>,
    ) -> Self {
        Self::Functor {
            id,
            parent,
            bind,
            path,
            loc,
            functor,
            args,
        }
    }

    pub fn path(
        id: Id<node::ModulePath>,
        parent: ModuleSym,
        bind: ModuleSym,
        loc: Loc,
        path: Vec<ModuleName>,
    ) -> Self {
        Self::Path {
            id,
            parent,
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

    pub fn parent(&self) -> ModuleSym {
        match self {
            Self::Functor { parent, .. } => *parent,
            Self::Path { parent, .. } => *parent,
        }
    }
}

impl Substitute for ModuleBindConst {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        match self {
            Self::Functor {
                id,
                parent,
                bind,
                path,
                loc,
                functor,
                args,
            } => {
                let parent_opt = parent.try_subst(s);
                let bind_opt = bind.try_subst(s);
                let path_opt = path.try_subst(s);
                let args_opt = args.try_subst(s);

                merge4(
                    parent_opt,
                    || *parent,
                    bind_opt,
                    || *bind,
                    path_opt,
                    || *path,
                    args_opt,
                    || args.clone(),
                )
                .map(|(parent, bind, path, args)| {
                    Self::functor(*id, parent, bind, path, *loc, *functor, args)
                })
            }
            Self::Path {
                id,
                parent,
                bind,
                loc,
                path,
            } => {
                let parent_opt = parent.try_subst(s);
                let bind_opt = bind.try_subst(s);

                merge2(parent_opt, || *parent, bind_opt, || *bind)
                    .map(|(parent, bind)| Self::path(*id, parent, bind, *loc, path.clone()))
            }
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        match self {
            Self::Functor {
                parent, bind, args, ..
            } => {
                parent.subst_mut(s);
                bind.subst_mut(s);
                args.subst_mut(s);
            }
            Self::Path { parent, bind, .. } => {
                parent.subst_mut(s);
                bind.subst_mut(s);
            }
        }
    }
}

pub type GlobalConstraints = VecDeque<ModuleBindConst>;

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

impl Substitute for ModuleConst {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Some(source) = self.source.try_subst(s) {
            Some(Self::new(self.path.clone(), self.id, source, self.loc))
        } else {
            None
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        self.source.subst_mut(s);
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

impl Substitute for ValueConst {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Some(source) = self.source.try_subst(s) {
            Some(Self::new(self.name, self.id, source, self.loc))
        } else {
            None
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        self.source.subst_mut(s);
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

impl Substitute for TypeBindConst {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Some(source) = self.source.try_subst(s) {
            Some(Self::new(self.name, self.id, source, self.loc))
        } else {
            None
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        self.source.subst_mut(s);
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
    // TypeRep {
    //     /// The name of the type reference.
    //     name: TypeName,
    //     /// The identifier of the type path that references some other type bind.
    //     id: Id<node::TypeRepExpr>,
    //     /// The location of the type reference in the source code.
    //     loc: Loc,
    // },
}

impl TypeConst {
    pub fn qualified(name: TypeName, id: Id<node::QualifiedType>, loc: Loc) -> Self {
        Self::Qualified { name, id, loc }
    }

    // pub fn type_rep(name: TypeName, id: Id<node::TypeRepExpr>, loc: Loc) -> Self {
    //     Self::TypeRep { name, id, loc }
    // }
}

impl Substitute for TypeConst {
    fn try_subst(&self, _s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        match self {
            Self::Qualified { name, id, loc } => Some(Self::qualified(*name, *id, *loc)),
            // Self::TypeRep { name, id, loc } => Some(Self::type_rep(*name, *id, *loc)),
        }
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

impl Substitute for ModuleTypeBindConst {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Some(source) = self.source.try_subst(s) {
            Some(Self::new(self.name, self.id, source, self.loc))
        } else {
            None
        }
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        self.source.subst_mut(s);
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

impl Substitute for ModuleTypeConst {
    fn try_subst(&self, _s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        Some(Self::new(self.name, self.id, self.loc))
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct LocalConstraints {
    module_type_binds: Vec<ModuleTypeBindConst>,
    module_types: Vec<ModuleTypeConst>,
    modules: Vec<ModuleConst>,
    type_binds: Vec<TypeBindConst>,
    types: Vec<TypeConst>,
    values: Vec<ValueConst>,
}

impl LocalConstraints {
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
    pub fn insert_module(&mut self, module_ref: ModuleConst) {
        self.modules.push(module_ref);
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
    pub fn module_type_binds(&self) -> &[ModuleTypeBindConst] {
        &self.module_type_binds
    }

    #[inline]
    pub fn module_types(&self) -> &[ModuleTypeConst] {
        &self.module_types
    }

    #[inline]
    pub fn modules(&self) -> &[ModuleConst] {
        &self.modules
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

impl Substitute for LocalConstraints {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
        let Self {
            module_type_binds,
            module_types,
            modules,
            type_binds,
            types,
            values,
        } = self;

        let module_type_binds_opt = module_type_binds.try_subst(s);
        let module_types_opt = module_types.try_subst(s);
        let modules_opt = modules.try_subst(s);
        let type_binds_opt = type_binds.try_subst(s);
        let types_opt = types.try_subst(s);
        let values_opt = values.try_subst(s);

        merge6(
            module_type_binds_opt,
            || module_type_binds.clone(),
            module_types_opt,
            || module_types.clone(),
            modules_opt,
            || modules.clone(),
            type_binds_opt,
            || type_binds.clone(),
            types_opt,
            || types.clone(),
            values_opt,
            || values.clone(),
        )
        .map(
            |(module_type_binds, module_types, modules, type_binds, types, values)| Self {
                module_type_binds,
                module_types,
                modules,
                type_binds,
                types,
                values,
            },
        )
    }

    fn subst_mut(&mut self, s: &HashMap<AnySym, AnySym>) {
        self.module_type_binds.subst_mut(s);
        self.module_types.subst_mut(s);
        self.modules.subst_mut(s);
        self.type_binds.subst_mut(s);
        self.types.subst_mut(s);
        self.values.subst_mut(s);
    }
}
