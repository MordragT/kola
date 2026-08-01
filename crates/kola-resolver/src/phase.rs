use std::{collections::HashMap, fmt};

use kola_builtins::{BuiltinId, BuiltinType};
use kola_subst::Substitutable;
use kola_tree::prelude::*;
use kola_utils::as_variant;

use pastey::paste;

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

macro_rules! define_node_map {
    (
        $(
            $field:ident : MetaMap<$node:ty, $value:ty>
        ),* $(,)?
    ) => {
        #[derive(Debug, Clone)]
        pub struct NodeMap {
            $(
                pub $field: MetaMap<$node, $value>,
            )*
        }

        $(
          impl GetOpt<$node> for NodeMap {
              type Item = $value;

              fn get_opt(&self, id: Id<$node>) -> Option<&Self::Item> {
                  self.$field.get_opt(id)
              }

              fn get_opt_mut(&mut self, id: Id<$node>) -> Option<&mut Self::Item> {
                  self.$field.get_opt_mut(id)
              }

              fn set(&mut self, id: Id<$node>, value: Self::Item) -> Option<Self::Item> {
                  self.$field.set(id, value)
              }
          }

          impl Col<$node> for NodeMap {
              type Column = MetaMap<$node, $value>;
              type Ids<'a> = <MetaMap::<$node, $value> as Col<$node>>::Ids<'a>;


              #[inline]
              fn col(&self) -> &Self::Column {
                  &self.$field
              }

              #[inline]
              fn col_mut(&mut self) -> &mut Self::Column {
                  &mut self.$field
              }

              #[inline]
              fn ids<'a>(&'a self) -> Self::Ids<'a> {
                  self.$field.ids()
              }
          }
        )*

        impl Substitutable<Substitution> for NodeMap {
            fn try_apply(&self, s: &mut HashMap<AnySym, AnySym>) -> Option<Self> {
                let mut changed = false;

                $(
                    let $field = match self.$field.try_apply(s) {
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
            impl Default for NodeMap {
                fn default() -> Self {
                    Self {
                        $(
                            $field: MetaMap::new(),
                        )*
                    }
                }
            }
        }

        impl NodeMap {
            pub fn new() -> Self {
                Self::default()
            }
        }
    };
}

define_node_map! {
    bind_pats: MetaMap<node::BindPat, ValueSym>,
    list_el_pats: MetaMap<node::ListElPat, ValueSym>,
    record_field_pats: MetaMap<node::RecordFieldPat, ValueSym>,
    qualified_exprs: MetaMap<node::QualifiedExpr, ResolvedValue>,
    let_exprs: MetaMap<node::LetExpr, ValueSym>,
    lambda_exprs: MetaMap<node::LambdaExpr, ValueSym>,
    handler_clauses: MetaMap<node::HandlerClause, ValueSym>,
    qualified_types: MetaMap<node::QualifiedType, ResolvedType>,
    type_vars: MetaMap<node::TypeVar, TypeSym>,
    type_var_binds: MetaMap<node::TypeVarBind, TypeSym>,
    value_binds: MetaMap<node::ValueBind, ValueSym>,
    type_binds: MetaMap<node::TypeBind, TypeSym>,
    module_binds: MetaMap<node::ModuleBind, ModuleSym>,
    module_type_binds: MetaMap<node::ModuleTypeBind, ModuleTypeSym>,
    functor_binds: MetaMap<node::FunctorBind, FunctorSym>,
    module_bodies: MetaMap<node::ModuleBody, ModuleSym>,
    module_paths: MetaMap<node::ModulePath, ResolvedModule>,
    module_imports: MetaMap<node::ModuleImport, ModuleSym>,
    qualified_module_types: MetaMap<node::QualifiedModuleType, ResolvedModuleType>,
}
