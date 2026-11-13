use std::{collections::HashMap, fmt};

use kola_builtins::{BuiltinId, BuiltinType};
use kola_tree::prelude::*;
use kola_utils::as_variant;

use crate::symbol::{
    AnySym, ModuleSym, ModuleTypeSym, Substitution, TypeSym, ValueSym,
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

kola_tree::define_side_table!(
    /// Resolution metadata for one module.
    NodeMap {
        bind_pats: Self::node::BindPat => ValueSym,
        list_el_pats: Self::node::ListElPat => ValueSym,
        record_field_pats: Self::node::RecordFieldPat => ValueSym,
        qualified_exprs: Self::node::QualifiedExpr => ResolvedValue,
        let_exprs: Self::node::LetExpr => ValueSym,
        lambda_exprs: Self::node::LambdaExpr => ValueSym,
        handler_clauses: Self::node::HandlerClause => ValueSym,
        qualified_types: Self::node::QualifiedType => ResolvedType,
        type_vars: Self::node::TypeVar => TypeSym,
        type_var_binds: Self::node::TypeVarBind => TypeSym,
        value_binds: Self::node::ValueBind => ValueSym,
        type_binds: Self::node::TypeBind => TypeSym,
        module_binds: Self::node::ModuleBind => ModuleSym,
        module_type_binds: Self::node::ModuleTypeBind => ModuleTypeSym,
        module_bodies: Self::node::ModuleBody => ModuleSym,
        module_paths: Self::node::ModulePath => ResolvedModule,
        module_imports: Self::node::ModuleImport => ModuleSym,
        qualified_module_types: Self::node::QualifiedModuleType => ResolvedModuleType,
    }
);
