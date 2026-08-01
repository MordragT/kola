use std::{collections::HashMap, fmt};

use kola_builtins::{BuiltinId, BuiltinType};
use kola_subst::Substitutable;
use kola_tree::prelude::*;
use kola_utils::as_variant;

use crate::symbol::{
    AnySym, FunctorSym, ModuleSym, ModuleTypeSym, Substitution, TypeSym, ValueSym,
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

impl Substitutable<Substitution> for ResolvedValue {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Self::Reference(sym) = self
            && let Some(to) = sym.try_apply(s)
        {
            Some(Self::Reference(to))
        } else {
            None
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

impl Substitutable<Substitution> for ResolvedType {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        if let Self::Reference(sym) = self
            && let Some(to) = sym.try_apply(s)
        {
            Some(Self::Reference(to))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedModule(pub ModuleSym);

impl Substitutable<Substitution> for ResolvedModule {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}

impl fmt::Display for ResolvedModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedModuleType(pub ModuleTypeSym);

impl Substitutable<Substitution> for ResolvedModuleType {
    fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
        self.0.try_apply(s).map(Self)
    }
}

impl fmt::Display for ResolvedModuleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

kola_tree::define_side_table!(NodeMap {
    bind_pats: SideMap<node::BindPat, ValueSym>,
    list_el_pats: SideMap<node::ListElPat, ValueSym>,
    record_field_pats: SideMap<node::RecordFieldPat, ValueSym>,
    qualified_exprs: SideMap<node::QualifiedExpr, ResolvedValue>,
    let_exprs: SideMap<node::LetExpr, ValueSym>,
    lambda_exprs: SideMap<node::LambdaExpr, ValueSym>,
    handler_clauses: SideMap<node::HandlerClause, ValueSym>,
    qualified_types: SideMap<node::QualifiedType, ResolvedType>,
    type_vars: SideMap<node::TypeVar, TypeSym>,
    type_var_binds: SideMap<node::TypeVarBind, TypeSym>,
    value_binds: SideMap<node::ValueBind, ValueSym>,
    type_binds: SideMap<node::TypeBind, TypeSym>,
    module_binds: SideMap<node::ModuleBind, ModuleSym>,
    module_type_binds: SideMap<node::ModuleTypeBind, ModuleTypeSym>,
    functor_binds: SideMap<node::FunctorBind, FunctorSym>,
    module_bodies: SideMap<node::ModuleBody, ModuleSym>,
    module_paths: SideMap<node::ModulePath, ResolvedModule>,
    module_imports: SideMap<node::ModuleImport, ModuleSym>,
    qualified_module_types: SideMap<node::QualifiedModuleType, ResolvedModuleType>,
});
