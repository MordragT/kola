use std::{collections::HashMap, fmt};

use kola_builtins::{BuiltinId, BuiltinType};
use kola_tree::prelude::*;
use kola_utils::as_variant;

use pastey::paste;

use crate::symbol::{AnySym, FunctorSym, ModuleSym, ModuleTypeSym, Substitute, TypeSym, ValueSym};

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

impl Substitute for ResolvedValue {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        if let Self::Reference(sym) = self
            && let Some(to) = sym.try_subst(s)
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

impl Substitute for ResolvedType {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        if let Self::Reference(sym) = self
            && let Some(to) = sym.try_subst(s)
        {
            Some(Self::Reference(to))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedModule(pub ModuleSym);

impl Substitute for ResolvedModule {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        self.0.try_subst(s).map(Self)
    }
}

impl fmt::Display for ResolvedModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedModuleType(pub ModuleTypeSym);

impl Substitute for ResolvedModuleType {
    fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self>
    where
        Self: Sized,
    {
        self.0.try_subst(s).map(Self)
    }
}

impl fmt::Display for ResolvedModuleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

macro_rules! define_node_map {
    (
        $(
            $field:ident : MetaVec<$node:ty, $value:ty>
        ),* $(,)?
    ) => {
        #[derive(Debug, Clone)]
        pub struct NodeMap {
            $(
                pub $field: MetaVec<$node, $value>,
            )*
        }

        $(
          impl Col<$node> for NodeMap {
              type Item = $value;

              #[inline]
              fn vec(&self) -> &Vec<Self::Item> {
                  self.$field.vec()
              }

              #[inline]
              fn vec_mut(&mut self) -> &mut Vec<Self::Item> {
                  self.$field.vec_mut()
              }
          }
        )*

        impl Substitute for NodeMap {
            fn try_subst(&self, s: &HashMap<AnySym, AnySym>) -> Option<Self> {
                let mut changed = false;

                $(
                    let $field = match self.$field.try_subst(s) {
                        Some(x) => {
                            changed = true;
                            x
                        }
                        None => self.$field.clone(),
                    };
                )*

                changed.then_some(Self {
                    $(
                        $field,
                    )*
                })
            }
        }

        paste!{
            impl NodeMap {
                pub fn new(cp: StorageCheckpoint) -> Self {
                    Self {
                        $(
                            $field: MetaVec::new(cp.$field),
                        )*
                    }
                }
            }
        }
    };
}

define_node_map! {
    bind_pats: MetaVec<node::BindPat, ValueSym>,
    list_el_pats: MetaVec<node::ListElPat, ValueSym>,
    record_field_pats: MetaVec<node::RecordFieldPat, ValueSym>,
    qualified_exprs: MetaVec<node::QualifiedExpr, ResolvedValue>,
    let_exprs: MetaVec<node::LetExpr, ValueSym>,
    lambda_exprs: MetaVec<node::LambdaExpr, ValueSym>,
    handler_clauses: MetaVec<node::HandlerClause, ValueSym>,
    qualified_types: MetaVec<node::QualifiedType, ResolvedType>,
    type_vars: MetaVec<node::TypeVar, TypeSym>,
    type_var_binds: MetaVec<node::TypeVarBind, TypeSym>,
    value_binds: MetaVec<node::ValueBind, ValueSym>,
    type_binds: MetaVec<node::TypeBind, TypeSym>,
    module_binds: MetaVec<node::ModuleBind, ModuleSym>,
    module_type_binds: MetaVec<node::ModuleTypeBind, ModuleTypeSym>,
    functor_binds: MetaVec<node::FunctorBind, FunctorSym>,
    module_bodies: MetaVec<node::ModuleBody, ModuleSym>,
    module_paths: MetaVec<node::ModulePath, ResolvedModule>,
    module_imports: MetaVec<node::ModuleImport, ModuleSym>,
    qualified_module_types: MetaVec<node::QualifiedModuleType, ResolvedModuleType>,
}
