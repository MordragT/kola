use std::ops::Index;

use derive_more::From;
use kola_collections::HashMap;
use kola_span::Loc;
use kola_tree::{
    id::Id,
    node::{self, ModuleName, ModuleTypeName, TypeName, ValueName},
};

use crate::symbol::{ModuleSym, ModuleTypeSym, TypeSym, ValueSym};

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

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct ConstructorRef {
//     /// The name of the type that this constructor belongs to.
//     pub variant: TypeName,
//     /// The name of the constructor reference.
//     pub name: ValueName,
//     /// The identifier of the expression that references some data constructor.
//     pub id: Id<node::QualifiedExpr>,
//     /// The symbol of the value bind, this reference occured inside.
//     pub source: ValueSym,
//     /// The location of the constructor reference in the source code.
//     pub loc: Loc,
// }

// impl ConstructorRef {
//     pub fn new(
//         variant: TypeName,
//         name: ValueName,
//         id: Id<node::QualifiedExpr>,
//         source: ValueSym,
//         loc: Loc,
//     ) -> Self {
//         Self {
//             variant,
//             name,
//             id,
//             source,
//             loc,
//         }
//     }
// }

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
pub struct References {
    module_type_binds: Vec<ModuleTypeBindRef>,
    module_types: Vec<ModuleTypeRef>,
    module_binds: HashMap<ModuleSym, ModuleBindRef>,
    modules: Vec<ModuleRef>,
    type_binds: Vec<TypeBindRef>,
    types: Vec<TypeRef>,
    values: Vec<ValueRef>,
    // constructors: Vec<ConstructorRef>,
}

impl References {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert_module_type_bind(&mut self, type_ref: ModuleTypeBindRef) {
        self.module_type_binds.push(type_ref);
    }

    pub fn insert_module_type(&mut self, type_ref: ModuleTypeRef) {
        self.module_types.push(type_ref);
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

    // pub fn insert_constructor(&mut self, constructor_ref: ConstructorRef) {
    //     self.constructors.push(constructor_ref);
    // }

    pub fn get_module_bind(&self, sym: ModuleSym) -> Option<&ModuleBindRef> {
        self.module_binds.get(&sym)
    }

    pub fn module_type_binds(&self) -> &[ModuleTypeBindRef] {
        &self.module_type_binds
    }

    pub fn module_types(&self) -> &[ModuleTypeRef] {
        &self.module_types
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

    // pub fn constructors(&self) -> &[ConstructorRef] {
    //     &self.constructors
    // }
}

impl Index<ModuleSym> for References {
    type Output = ModuleBindRef;

    fn index(&self, index: ModuleSym) -> &Self::Output {
        self.module_binds
            .get(&index)
            .expect("ModuleSym not found in ModuleRefs")
    }
}
