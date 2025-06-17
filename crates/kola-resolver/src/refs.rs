use std::ops::Index;

use derive_more::From;
use kola_collections::HashMap;
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{self, ModuleName, TypeName, ValueName},
};

use crate::symbol::{ModuleSym, TypeSym, ValueSym};

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleBindRef(Vec<ModuleName>);

impl ModuleBindRef {
    pub fn new(path: Vec<ModuleName>) -> Self {
        Self(path)
    }

    pub fn path(&self) -> &[ModuleName] {
        &self.0
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueRef {
    /// The name of the value reference.
    pub name: ValueName,
    /// The identifier of the path expression that references some other value bind..
    pub id: Id<node::SelectExpr>,
    /// The symbol of the value bind, this reference occured inside.
    pub source: ValueSym,
    /// The location of the value reference in the source code.
    pub loc: Loc,
}

impl ValueRef {
    pub fn new(name: ValueName, id: Id<node::SelectExpr>, source: ValueSym, loc: Loc) -> Self {
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
    /// The name of the value reference.
    pub name: TypeName,
    /// The identifier of the type path that references some other type bind..
    pub id: Id<node::TypePath>,
    /// The symbol of the type bind, this reference occured inside.
    pub source: TypeSym,
    /// The location of the type reference in the source code.
    pub loc: Loc,
}

impl TypeBindRef {
    pub fn new(name: TypeName, id: Id<node::TypePath>, source: TypeSym, loc: Loc) -> Self {
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
    /// The name of the value reference.
    pub name: TypeName,
    /// The identifier of the type path that references some other type bind..
    pub id: Id<node::TypePath>,
    /// The location of the type reference in the source code.
    pub loc: Loc,
}

impl TypeRef {
    pub fn new(name: TypeName, id: Id<node::TypePath>, loc: Loc) -> Self {
        Self { name, id, loc }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct References {
    module_binds: HashMap<ModuleSym, ModuleBindRef>,
    modules: Vec<ModuleRef>,
    type_binds: Vec<TypeBindRef>,
    types: Vec<TypeRef>,
    values: Vec<ValueRef>,
}

impl References {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_module_bind(&mut self, sym: ModuleSym, path: Vec<ModuleName>) {
        self.module_binds.insert(sym, ModuleBindRef::new(path));
    }

    pub fn insert_module(&mut self, module_ref: ModuleRef) {
        self.modules.push(module_ref);
    }

    pub fn insert_type_bind(&mut self, type_ref: TypeBindRef) {
        self.type_binds.push(type_ref);
    }

    pub fn insert_type(&mut self, type_ref: TypeRef) {
        self.types.push(type_ref);
    }

    pub fn insert_value(&mut self, value_ref: ValueRef) {
        self.values.push(value_ref);
    }

    pub fn get_module_bind(&self, sym: ModuleSym) -> Option<&ModuleBindRef> {
        self.module_binds.get(&sym)
    }

    pub fn module_binds(&self) -> &HashMap<ModuleSym, ModuleBindRef> {
        &self.module_binds
    }

    pub fn modules(&self) -> &[ModuleRef] {
        &self.modules
    }

    pub fn type_binds(&self) -> &[TypeBindRef] {
        &self.type_binds
    }

    pub fn types(&self) -> &[TypeRef] {
        &self.types
    }

    pub fn values(&self) -> &[ValueRef] {
        &self.values
    }
}

impl Index<ModuleSym> for References {
    type Output = ModuleBindRef;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        self.module_binds
            .get(&index)
            .expect("ModuleSym not found in ModuleRefs")
    }
}
