use kola_collections::{BiMap, HashMap};
use kola_span::SourceId;
use kola_tree::{
    id::Id,
    node::{self, ModuleNamespace, Namespace, TypeNamespace, ValueNamespace},
};
use std::{hash::Hash, ops::Index};

use crate::{
    GlobalId,
    symbol::{ModuleSym, Sym, TypeSym, ValueSym},
};

#[derive(Debug)]
pub struct Bind<N: Namespace, T>(GlobalId<T>, Sym<N>);

impl<N: Namespace, T> Bind<N, T> {
    pub fn global_id(&self) -> GlobalId<T> {
        self.0
    }

    pub fn sym(&self) -> Sym<N> {
        self.1
    }

    pub fn source(&self) -> SourceId {
        self.0.source
    }

    pub fn node_id(&self) -> Id<T> {
        self.0.id
    }
}

impl<N: Namespace, T> Clone for Bind<N, T> {
    fn clone(&self) -> Self {
        Self(self.0, self.1)
    }
}

impl<N: Namespace, T> Copy for Bind<N, T> {}

impl<N: Namespace, T> PartialEq for Bind<N, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl<N: Namespace, T> Eq for Bind<N, T> {}

impl<N: Namespace, T> PartialOrd for Bind<N, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<N: Namespace, T> Ord for Bind<N, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0).then_with(|| self.1.cmp(&other.1))
    }
}

impl<N: Namespace, T> Hash for Bind<N, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}

pub type ModuleBind<T> = Bind<ModuleNamespace, T>;
pub type TypeBind<T> = Bind<TypeNamespace, T>;
pub type ValueBind<T> = Bind<ValueNamespace, T>;

pub type Lookup<T, S> = HashMap<GlobalId<T>, S>;

#[derive(Debug, Clone, Default)]
pub struct Bindings {
    pub module_binds: Lookup<node::ModuleBind, ModuleSym>,
    pub module_imports: Lookup<node::ModuleImport, ModuleSym>,
    pub modules: Lookup<node::Module, ModuleSym>,
    pub module_paths: Lookup<node::ModulePath, ModuleSym>,
    pub type_binds: Lookup<node::TypeBind, TypeSym>,
    // pub value_binds: Lookup<node::ValueBind, ValueSym>,
    pub value_binds: BiMap<GlobalId<node::ValueBind>, ValueSym>,
    pub let_exprs: Lookup<node::LetExpr, ValueSym>,
    pub lambda_exprs: Lookup<node::LambdaExpr, ValueSym>,
    pub path_exprs: Lookup<node::PathExpr, ValueSym>,
}

impl Bindings {
    #[inline]
    pub fn new() -> Self {
        Self {
            module_binds: Lookup::new(),
            module_imports: Lookup::new(),
            modules: Lookup::new(),
            module_paths: Lookup::new(),
            type_binds: Lookup::new(),
            // value_binds: Lookup::new(),
            value_binds: BiMap::new(),
            let_exprs: Lookup::new(),
            lambda_exprs: Lookup::new(),
            path_exprs: Lookup::new(),
        }
    }

    #[inline]
    pub fn insert_module_bind(
        &mut self,
        id: GlobalId<node::ModuleBind>,
        sym: ModuleSym,
    ) -> ModuleBind<node::ModuleBind> {
        self.module_binds.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn insert_module_import(
        &mut self,
        id: GlobalId<node::ModuleImport>,
        sym: ModuleSym,
    ) -> ModuleBind<node::ModuleImport> {
        self.module_imports.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn insert_module(
        &mut self,
        id: GlobalId<node::Module>,
        sym: ModuleSym,
    ) -> ModuleBind<node::Module> {
        self.modules.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn insert_module_path(
        &mut self,
        id: GlobalId<node::ModulePath>,
        sym: ModuleSym,
    ) -> ModuleBind<node::ModulePath> {
        self.module_paths.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn insert_type_bind(
        &mut self,
        id: GlobalId<node::TypeBind>,
        sym: TypeSym,
    ) -> TypeBind<node::TypeBind> {
        self.type_binds.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn insert_value_bind(
        &mut self,
        id: GlobalId<node::ValueBind>,
        sym: ValueSym,
    ) -> ValueBind<node::ValueBind> {
        self.value_binds.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn insert_let_expr(
        &mut self,
        id: GlobalId<node::LetExpr>,
        sym: ValueSym,
    ) -> ValueBind<node::LetExpr> {
        self.let_exprs.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn insert_lambda_expr(
        &mut self,
        id: GlobalId<node::LambdaExpr>,
        sym: ValueSym,
    ) -> ValueBind<node::LambdaExpr> {
        self.lambda_exprs.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn insert_path_expr(
        &mut self,
        id: GlobalId<node::PathExpr>,
        sym: ValueSym,
    ) -> ValueBind<node::PathExpr> {
        self.path_exprs.insert(id, sym);

        Bind(id, sym)
    }

    #[inline]
    pub fn lookup_module_bind(&self, id: GlobalId<node::ModuleBind>) -> Option<ModuleSym> {
        self.module_binds.get(&id).copied()
    }

    #[inline]
    pub fn lookup_module_import(&self, id: GlobalId<node::ModuleImport>) -> Option<ModuleSym> {
        self.module_imports.get(&id).copied()
    }

    #[inline]
    pub fn lookup_module(&self, id: GlobalId<node::Module>) -> Option<ModuleSym> {
        self.modules.get(&id).copied()
    }

    #[inline]
    pub fn lookup_module_path(&self, id: GlobalId<node::ModulePath>) -> Option<ModuleSym> {
        self.module_paths.get(&id).copied()
    }

    #[inline]
    pub fn lookup_type_bind(&self, id: GlobalId<node::TypeBind>) -> Option<TypeSym> {
        self.type_binds.get(&id).copied()
    }

    #[inline]
    pub fn lookup_value_bind(&self, id: GlobalId<node::ValueBind>) -> Option<ValueSym> {
        // self.value_binds.get(&id).copied()
        self.value_binds.get_by_key(&id).copied()
    }

    #[inline]
    pub fn lookup_value_bind_by_sym(&self, sym: ValueSym) -> Option<GlobalId<node::ValueBind>> {
        self.value_binds.get_by_value(&sym).copied()
    }

    #[inline]
    pub fn lookup_let_expr(&self, id: GlobalId<node::LetExpr>) -> Option<ValueSym> {
        self.let_exprs.get(&id).copied()
    }

    #[inline]
    pub fn lookup_lambda_expr(&self, id: GlobalId<node::LambdaExpr>) -> Option<ValueSym> {
        self.lambda_exprs.get(&id).copied()
    }

    #[inline]
    pub fn lookup_path_expr(&self, id: GlobalId<node::PathExpr>) -> Option<ValueSym> {
        self.path_exprs.get(&id).copied()
    }
}

impl Index<GlobalId<node::ModuleBind>> for Bindings {
    type Output = ModuleSym;

    fn index(&self, index: GlobalId<node::ModuleBind>) -> &Self::Output {
        self.module_binds
            .get(&index)
            .expect("Module symbol not found")
    }
}

impl Index<GlobalId<node::ModuleImport>> for Bindings {
    type Output = ModuleSym;

    fn index(&self, index: GlobalId<node::ModuleImport>) -> &Self::Output {
        self.module_imports
            .get(&index)
            .expect("Module import symbol not found")
    }
}

impl Index<GlobalId<node::Module>> for Bindings {
    type Output = ModuleSym;

    fn index(&self, index: GlobalId<node::Module>) -> &Self::Output {
        self.modules.get(&index).expect("Module symbol not found")
    }
}

impl Index<GlobalId<node::ModulePath>> for Bindings {
    type Output = ModuleSym;

    fn index(&self, index: GlobalId<node::ModulePath>) -> &Self::Output {
        self.module_paths
            .get(&index)
            .expect("Module path symbol not found")
    }
}

impl Index<GlobalId<node::TypeBind>> for Bindings {
    type Output = TypeSym;

    fn index(&self, index: GlobalId<node::TypeBind>) -> &Self::Output {
        self.type_binds.get(&index).expect("Type symbol not found")
    }
}

impl Index<GlobalId<node::ValueBind>> for Bindings {
    type Output = ValueSym;

    fn index(&self, index: GlobalId<node::ValueBind>) -> &Self::Output {
        self.value_binds
            // .get(&index)
            .get_by_key(&index)
            .expect("Value symbol not found")
    }
}

impl Index<GlobalId<node::LetExpr>> for Bindings {
    type Output = ValueSym;

    fn index(&self, index: GlobalId<node::LetExpr>) -> &Self::Output {
        self.let_exprs
            .get(&index)
            .expect("Let expression symbol not found")
    }
}

impl Index<GlobalId<node::LambdaExpr>> for Bindings {
    type Output = ValueSym;

    fn index(&self, index: GlobalId<node::LambdaExpr>) -> &Self::Output {
        self.lambda_exprs
            .get(&index)
            .expect("Lambda expression symbol not found")
    }
}

impl Index<GlobalId<node::PathExpr>> for Bindings {
    type Output = ValueSym;

    fn index(&self, index: GlobalId<node::PathExpr>) -> &Self::Output {
        self.path_exprs
            .get(&index)
            .expect("Path expression symbol not found")
    }
}
