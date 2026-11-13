use std::fmt;

use kola_builtins::{BuiltinId, BuiltinType};
use kola_tree::prelude::*;
use kola_utils::as_variant;

use crate::symbol::{
    ModuleSym, ModuleTypeSym, TypeSym, ValueSym,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResolvedValue {
    Reference(ValueSym),
    Builtin(BuiltinId),
}

impl ResolvedValue {
    pub fn into_builtin(self) -> Option<BuiltinId> {
        as_variant!(self, Self::Builtin)
    }

    pub fn into_reference(self) -> Option<ValueSym> {
        as_variant!(self, Self::Reference)
    }
}

impl fmt::Display for ResolvedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolvedValue::Reference(sym) => sym.fmt(f),
            ResolvedValue::Builtin(id) => id.fmt(f),
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResolvedType {
    Reference(TypeSym),
    Builtin(BuiltinType),
}

impl ResolvedType {
    pub fn into_builtin(self) -> Option<BuiltinType> {
        as_variant!(self, Self::Builtin)
    }

    pub fn into_reference(self) -> Option<TypeSym> {
        as_variant!(self, Self::Reference)
    }
}

impl fmt::Display for ResolvedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolvedType::Reference(sym) => sym.fmt(f),
            ResolvedType::Builtin(ty) => ty.fmt(f),
        }
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedModule(pub ModuleSym);


impl fmt::Display for ResolvedModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedModuleType(pub ModuleTypeSym);


impl fmt::Display for ResolvedModuleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Resolution metadata for one module.
#[derive(Debug, Clone, Default, RelationStorage, Merge)]
pub struct NodeMap {
    #[relation]
    pub bind_pats: MaybeVec<node::BindPat, ValueSym>,
    #[relation]
    pub list_el_pats: MaybeVec<node::ListElPat, ValueSym>,
    #[relation]
    pub record_field_pats: MaybeVec<node::RecordFieldPat, ValueSym>,
    #[relation]
    pub qualified_exprs: MaybeVec<node::QualifiedExpr, ResolvedValue>,
    #[relation]
    pub let_exprs: MaybeVec<node::LetExpr, ValueSym>,
    #[relation]
    pub lambda_exprs: MaybeVec<node::LambdaExpr, ValueSym>,
    #[relation]
    pub handler_clauses: MaybeVec<node::HandlerClause, ValueSym>,
    #[relation]
    pub qualified_types: MaybeVec<node::QualifiedType, ResolvedType>,
    #[relation]
    pub type_vars: MaybeVec<node::TypeVar, TypeSym>,
    #[relation]
    pub type_var_binds: MaybeVec<node::TypeVarBind, TypeSym>,
    #[relation]
    pub value_binds: MaybeVec<node::ValueBind, ValueSym>,
    #[relation]
    pub type_binds: MaybeVec<node::TypeBind, TypeSym>,
    #[relation]
    pub module_binds: MaybeVec<node::ModuleBind, ModuleSym>,
    #[relation]
    pub module_type_binds: MaybeVec<node::ModuleTypeBind, ModuleTypeSym>,
    #[relation]
    pub module_bodies: MaybeVec<node::ModuleBody, ModuleSym>,
    #[relation]
    pub module_paths: MaybeVec<node::ModulePath, ResolvedModule>,
    #[relation]
    pub module_imports: MaybeVec<node::ModuleImport, ModuleSym>,
    #[relation]
    pub qualified_module_types: MaybeVec<node::QualifiedModuleType, ResolvedModuleType>,
}

impl NodeMap {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn get_opt<T: 'static, V: 'static>(&self, id: Id<T>) -> Option<&V>
    where
        Self: PartialRelation<T, V>,
    {
        PartialRelation::<T, V>::get_opt(self, id)
    }

    #[inline]
    pub fn set<T: 'static, V: 'static>(&mut self, id: Id<T>, value: V)
    where
        Self: PartialRelationSet<T, V>,
    {
        PartialRelationSet::<T, V>::set(self, id, value)
    }

    #[inline]
    #[track_caller]
    pub fn get_unchecked<T: 'static, V: 'static>(&self, id: Id<T>) -> &V
    where
        Self: PartialRelation<T, V>,
    {
        self.get_opt(id).expect("node metadata missing")
    }

    #[inline]
    #[track_caller]
    pub fn get_unchecked_mut<T: 'static, V: 'static>(&mut self, id: Id<T>) -> &mut V
    where
        Self: PartialRelationMut<T, V>,
    {
        PartialRelationMut::<T, V>::get_mut(self, id).expect("node metadata missing")
    }
}
