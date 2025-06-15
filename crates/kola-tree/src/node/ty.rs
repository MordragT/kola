use derive_more::From;
use serde::{Deserialize, Serialize};
use std::{borrow::Borrow, ops::Deref};

use kola_print::prelude::*;
use kola_utils::{as_variant, interner::StrKey};

use crate::{
    id::Id,
    node::{ModulePath, TypeName, ValueName},
    print::NodePrinter,
    tree::TreeView,
};

/*
type Option = forall a . [ Some : a, None ]
type OptionResult  = forall a e . [ Option a | +Error : e ]
type AlwaysSome = forall a . [ Option a | -None ]

type Person = { name : Str }
type Member = { Person | +id : Num }
type Id = { Member | -id }

map : forall a b . (a -> b) -> List a -> List b

TODO:

allow Open Variants and Open Records:

< Some : a, None | * >
{ name : Str, age : Num | * }

as well as (or just allow the latter, as it doesn't need any special handling)

forall a b . < Some : a, None | b >
forall a . { name : Str, age : Num | b }
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeError;

impl<'a> Notate<'a> for NodePrinter<'a, TypeError> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        "TypeError".red().display_in(arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypePath {
    pub path: Option<Id<ModulePath>>,
    pub ty: Id<TypeName>,
}

impl<'a> Notate<'a> for NodePrinter<'a, TypePath> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let TypePath { path, ty } = *self.value;

        let head = "TypePath".cyan().display_in(arena);

        let path = path.map(|p| self.to(p).notate(arena)).or_not(arena);
        let ty = self.to(ty).notate(arena);

        let single = path.clone().then(ty.clone(), arena).flatten(arena);
        let multi = path.then(ty, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct TypeVar(pub StrKey);

impl TypeVar {
    #[inline]
    pub fn as_str_key(&self) -> &StrKey {
        &self.0
    }
}

impl Deref for TypeVar {
    type Target = StrKey;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<StrKey> for TypeVar {
    #[inline]
    fn as_ref(&self) -> &StrKey {
        &self.0
    }
}

impl Borrow<StrKey> for TypeVar {
    #[inline]
    fn borrow(&self) -> &StrKey {
        &self.0
    }
}

impl PartialEq<StrKey> for TypeVar {
    #[inline]
    fn eq(&self, other: &StrKey) -> bool {
        self == other
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, TypeVar> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "TypeVar".magenta().display_in(arena);
        let value = self
            .interner
            .get(self.value.0)
            .expect("Symbol not found")
            .magenta()
            .display_in(arena);

        let single = arena.just(' ').then(value.clone(), arena);
        let multi = arena.newline().then(value, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordFieldType {
    pub name: Id<ValueName>,
    pub ty: Id<TypeExpr>,
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordFieldType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordFieldType { name, ty } = *self.value;

        let head = "RecordFieldType".blue().display_in(arena);

        let name = self.to_id(name).notate(arena);
        let ty = self.to_id(ty).notate(arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            arena.notate(", type = "),
            ty.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            arena.newline(),
            arena.notate("type = "),
            ty,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordType {
    pub fields: Vec<Id<RecordFieldType>>,
    pub extension: Option<Id<TypeVar>>,
}

impl RecordType {
    pub fn get(&self, index: usize, tree: &impl TreeView) -> RecordFieldType {
        *self.fields[index].get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordType { fields, extension } = self.value;

        let head = "RecordType".blue().display_in(arena);

        let fields = self.to_slice(fields).gather(arena);
        let extension = extension.map(|ext| self.to_id(ext).notate(arena));

        let single = [
            arena.notate(" fields = "),
            fields.clone().concat_map(
                |field| arena.just(' ').then(field, arena).flatten(arena),
                arena,
            ),
            extension
                .clone()
                .map(|ext| arena.notate(" extension = ").then(ext, arena))
                .or_not(arena)
                .flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("fields = "),
            fields.concat_by(arena.newline(), arena),
            extension
                .map(|ext| [arena.newline(), arena.notate("extension = "), ext].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// TODO better name it Tag ??
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct VariantCaseType {
    pub name: Id<ValueName>,
    pub ty: Option<Id<TypeExpr>>,
}

impl<'a> Notate<'a> for NodePrinter<'a, VariantCaseType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let VariantCaseType { name, ty } = *self.value;

        let head = "VariantCaseType".blue().display_in(arena);

        let name = self.to_id(name).notate(arena);
        let ty = ty.map(|ty| self.to_id(ty).notate(arena));

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            ty.clone()
                .map(|ty| arena.notate(", type = ").then(ty, arena))
                .or_not(arena)
                .flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            ty.map(|ty| [arena.newline(), arena.notate("type = "), ty].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct VariantType {
    pub cases: Vec<Id<VariantCaseType>>,
    pub extension: Option<Id<TypeVar>>,
}

impl VariantType {
    pub fn get(&self, index: usize, tree: &impl TreeView) -> VariantCaseType {
        *self.cases[index].get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, VariantType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let VariantType { cases, extension } = self.value;

        let head = "VariantType".blue().display_in(arena);

        let cases = self.to_slice(cases).gather(arena);
        let extension = extension.map(|ext| self.to_id(ext).notate(arena));

        let single = [
            arena.notate(" cases = "),
            cases.clone().concat_map(
                |field| arena.just(' ').then(field, arena).flatten(arena),
                arena,
            ),
            extension
                .clone()
                .map(|ext| arena.notate(" extension = ").then(ext, arena))
                .or_not(arena)
                .flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("cases = "),
            cases.concat_by(arena.newline(), arena),
            extension
                .map(|ext| [arena.newline(), arena.notate("extension = "), ext].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// TODO this needs to be disambiguated with parentheses if a function should be one argument
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FuncType {
    pub input: Id<TypeExpr>,
    pub output: Id<TypeExpr>,
}

impl<'a> Notate<'a> for NodePrinter<'a, FuncType> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let FuncType { input, output } = *self.value;

        let head = "FuncType".blue().display_in(arena);

        let input = self.to_id(input).notate(arena);
        let output = self.to_id(output).notate(arena);

        let single = [
            arena.notate(" input = "),
            input.clone().flatten(arena),
            arena.notate(", output = "),
            output.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("input = "),
            input,
            arena.newline(),
            arena.notate("output = "),
            output,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeApplication {
    pub constructor: Id<TypeExpr>,
    pub arg: Id<TypeExpr>,
}

impl<'a> Notate<'a> for NodePrinter<'a, TypeApplication> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let TypeApplication { constructor, arg } = *self.value;

        let head = "TypeApplication".blue().display_in(arena);

        let constructor = self.to_id(constructor).notate(arena);
        let arg = self.to_id(arg).notate(arena);

        let single = [
            arena.notate(" constructor = "),
            constructor.clone().flatten(arena),
            arena.notate(", arg = "),
            arg.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("constructor = "),
            constructor,
            arena.newline(),
            arena.notate("arg = "),
            arg,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum TypeExpr {
    Error(Id<TypeError>),
    Path(Id<TypePath>),
    Record(Id<RecordType>),
    Variant(Id<VariantType>),
    Func(Id<FuncType>),
    Application(Id<TypeApplication>),
}

impl<'a> Notate<'a> for NodePrinter<'a, TypeExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            TypeExpr::Error(e) => self.to_node(e.get(self.tree)).notate(arena),
            TypeExpr::Path(p) => self.to_node(p.get(self.tree)).notate(arena),
            TypeExpr::Record(r) => self.to_node(r.get(self.tree)).notate(arena),
            TypeExpr::Variant(v) => self.to_node(v.get(self.tree)).notate(arena),
            TypeExpr::Func(f) => self.to_node(f.get(self.tree)).notate(arena),
            TypeExpr::Application(a) => self.to_node(a.get(self.tree)).notate(arena),
        }
    }
}

impl TypeExpr {
    #[inline]
    pub fn to_error(self) -> Option<Id<TypeError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_type_path(self) -> Option<Id<TypePath>> {
        as_variant!(self, Self::Path)
    }

    #[inline]
    pub fn to_record_type(self) -> Option<Id<RecordType>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_variant_type(self) -> Option<Id<VariantType>> {
        as_variant!(self, Self::Variant)
    }

    #[inline]
    pub fn to_func_type(self) -> Option<Id<FuncType>> {
        as_variant!(self, Self::Func)
    }

    #[inline]
    pub fn to_type_application(self) -> Option<Id<TypeApplication>> {
        as_variant!(self, Self::Application)
    }

    #[inline]
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error(_))
    }

    #[inline]
    pub fn is_type_path(self) -> bool {
        matches!(self, Self::Path(_))
    }

    #[inline]
    pub fn is_record_type(self) -> bool {
        matches!(self, Self::Record(_))
    }

    #[inline]
    pub fn is_variant_type(self) -> bool {
        matches!(self, Self::Variant(_))
    }

    #[inline]
    pub fn is_func_type(self) -> bool {
        matches!(self, Self::Func(_))
    }

    #[inline]
    pub fn is_type_application(self) -> bool {
        matches!(self, Self::Application(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Type {
    pub vars: Vec<Id<TypeVar>>,
    pub ty: Id<TypeExpr>,
}

impl<'a> Notate<'a> for NodePrinter<'a, Type> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let Type { vars, ty } = self.value;

        let head = "Type".green().display_in(arena);

        let vars = self.to_slice(vars).gather(arena);
        let ty = self.to_id(*ty).notate(arena);

        let single = if vars.is_empty() {
            arena
                .notate(" type = ")
                .then(ty.clone(), arena)
                .flatten(arena)
        } else {
            [
                arena.notate(" vars = "),
                vars.clone()
                    .concat_by(arena.just(' '), arena)
                    .flatten(arena),
                arena.notate(", type = "),
                ty.clone().flatten(arena),
            ]
            .concat_in(arena)
        };

        let multi = if vars.is_empty() {
            [arena.newline(), arena.notate("type = "), ty]
                .concat_in(arena)
                .indent(arena)
        } else {
            [
                arena.newline(),
                arena.notate("vars = "),
                vars.concat_by(arena.newline(), arena),
                arena.newline(),
                arena.notate("type = "),
                ty,
            ]
            .concat_in(arena)
            .indent(arena)
        };

        head.then(single.or(multi, arena), arena)
    }
}
mod inspector {
    use std::hash::BuildHasher;

    use super::*;
    use crate::inspector::*;
    impl<'t, S: BuildHasher> NodeInspector<'t, Id<Type>, S> {
        pub fn as_type_path(self) -> Option<NodeInspector<'t, Id<TypePath>, S>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_type_path()
                .map(|path_id| NodeInspector::new(path_id, self.tree, self.interner))
        }

        pub fn as_function(self) -> Option<NodeInspector<'t, Id<FuncType>, S>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_func_type()
                .map(|fn_id| NodeInspector::new(fn_id, self.tree, self.interner))
        }

        pub fn as_record(self) -> Option<NodeInspector<'t, Id<RecordType>, S>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_record_type()
                .map(|record_id| NodeInspector::new(record_id, self.tree, self.interner))
        }

        pub fn as_variant(self) -> Option<NodeInspector<'t, Id<VariantType>, S>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_variant_type()
                .map(|variant_id| NodeInspector::new(variant_id, self.tree, self.interner))
        }

        pub fn as_type_application(self) -> Option<NodeInspector<'t, Id<TypeApplication>, S>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_type_application()
                .map(|app_id| NodeInspector::new(app_id, self.tree, self.interner))
        }

        pub fn has_type_vars(self, count: usize) -> Self {
            let vars_len = self.node.get(self.tree).vars.len();
            assert_eq!(
                vars_len, count,
                "Expected {} type variables but found {}",
                count, vars_len
            );
            self
        }

        pub fn type_var_at(self, index: usize) -> NodeInspector<'t, Id<TypeVar>, S> {
            let ty = self.node.get(self.tree);
            assert!(
                index < ty.vars.len(),
                "Type variable index {} out of bounds (max {})",
                index,
                ty.vars.len() - 1
            );
            let var_id = ty.vars[index];
            NodeInspector::new(var_id, self.tree, self.interner)
        }

        pub fn type_expr(self) -> NodeInspector<'t, Id<TypeExpr>, S> {
            let ty = self.node.get(self.tree);
            NodeInspector::new(ty.ty, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<TypePath>, S> {
        pub fn module_path(self) -> Option<NodeInspector<'t, Id<ModulePath>, S>> {
            let type_path = self.node.get(self.tree);
            type_path
                .path
                .map(|path_id| NodeInspector::new(path_id, self.tree, self.interner))
        }

        pub fn has_type_name(self, expected: &str) -> NodeInspector<'t, Id<TypePath>, S> {
            let type_name = self.node.get(self.tree).ty.get(self.tree).0;
            let name = self.interner.get(type_name).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected type variable name '{}' but found '{}'",
                expected, name
            );
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<TypeVar>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let type_var = self.node.get(self.tree);
            let name = self.interner.get(type_var.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected type variable name '{}' but found '{}'",
                expected, name
            );
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<FuncType>, S> {
        pub fn input(self) -> NodeInspector<'t, Id<TypeExpr>, S> {
            let function_type = self.node.get(self.tree);
            NodeInspector::new(function_type.input, self.tree, self.interner)
        }

        pub fn output(self) -> NodeInspector<'t, Id<TypeExpr>, S> {
            let function_type = self.node.get(self.tree);
            NodeInspector::new(function_type.output, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordType>, S> {
        pub fn has_fields(self, count: usize) -> Self {
            let fields_len = self.node.get(self.tree).fields.len();
            assert_eq!(
                fields_len, count,
                "Expected {} fields but found {}",
                count, fields_len
            );
            self
        }

        pub fn field_at(self, index: usize) -> NodeInspector<'t, Id<RecordFieldType>, S> {
            let record_type = self.node.get(self.tree);
            assert!(
                index < record_type.fields.len(),
                "Field index {} out of bounds (max {})",
                index,
                record_type.fields.len() - 1
            );
            let field_id = record_type.fields[index];
            NodeInspector::new(field_id, self.tree, self.interner)
        }

        pub fn extension(self) -> Option<NodeInspector<'t, Id<TypeVar>, S>> {
            let record_type = self.node.get(self.tree);
            record_type
                .extension
                .map(|ext_id| NodeInspector::new(ext_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordFieldType>, S> {
        pub fn has_field_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected field name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn type_expr(self) -> NodeInspector<'t, Id<TypeExpr>, S> {
            let field = self.node.get(self.tree);
            NodeInspector::new(field.ty, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<VariantType>, S> {
        pub fn has_cases(self, count: usize) -> Self {
            let cases_len = self.node.get(self.tree).cases.len();
            assert_eq!(
                cases_len, count,
                "Expected {} cases but found {}",
                count, cases_len
            );
            self
        }

        pub fn case_at(self, index: usize) -> NodeInspector<'t, Id<VariantCaseType>, S> {
            let variant_type = self.node.get(self.tree);
            assert!(
                index < variant_type.cases.len(),
                "Case index {} out of bounds (max {})",
                index,
                variant_type.cases.len() - 1
            );
            let case_id = variant_type.cases[index];
            NodeInspector::new(case_id, self.tree, self.interner)
        }

        pub fn extension(self) -> Option<NodeInspector<'t, Id<TypeVar>, S>> {
            let variant_type = self.node.get(self.tree);
            variant_type
                .extension
                .map(|ext_id| NodeInspector::new(ext_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<VariantCaseType>, S> {
        pub fn has_case_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            let name = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                name, expected,
                "Expected case name '{}' but found '{}'",
                expected, name
            );
            self
        }

        pub fn type_expr(self) -> Option<NodeInspector<'t, Id<TypeExpr>, S>> {
            let case = self.node.get(self.tree);
            case.ty
                .map(|ty_id| NodeInspector::new(ty_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<TypeExpr>, S> {
        pub fn as_error(self) -> Option<NodeInspector<'t, Id<TypeError>, S>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_error()
                .map(|err_id| NodeInspector::new(err_id, self.tree, self.interner))
        }

        pub fn as_path(self) -> Option<NodeInspector<'t, Id<TypePath>, S>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_type_path()
                .map(|path_id| NodeInspector::new(path_id, self.tree, self.interner))
        }

        pub fn as_record(self) -> Option<NodeInspector<'t, Id<RecordType>, S>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_record_type()
                .map(|rec_id| NodeInspector::new(rec_id, self.tree, self.interner))
        }

        pub fn as_variant(self) -> Option<NodeInspector<'t, Id<VariantType>, S>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_variant_type()
                .map(|var_id| NodeInspector::new(var_id, self.tree, self.interner))
        }

        pub fn as_function(self) -> Option<NodeInspector<'t, Id<FuncType>, S>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_func_type()
                .map(|fn_id| NodeInspector::new(fn_id, self.tree, self.interner))
        }

        pub fn as_application(self) -> Option<NodeInspector<'t, Id<TypeApplication>, S>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_type_application()
                .map(|app_id| NodeInspector::new(app_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<TypeApplication>, S> {
        pub fn constructor(self) -> NodeInspector<'t, Id<TypeExpr>, S> {
            let type_app = self.node.get(self.tree);
            NodeInspector::new(type_app.constructor, self.tree, self.interner)
        }

        pub fn arg(self) -> NodeInspector<'t, Id<TypeExpr>, S> {
            let type_app = self.node.get(self.tree);
            NodeInspector::new(type_app.arg, self.tree, self.interner)
        }
    }
}
