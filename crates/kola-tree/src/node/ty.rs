use derive_more::From;
use kola_print::prelude::*;
use kola_utils::as_variant;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use super::{Name, Symbol};
use crate::{id::NodeId, print::TreePrinter, tree::NodeContainer};

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

impl Printable<TreePrinter> for TypeError {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        "TypeError".red().display_in(arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypePath(pub Vec<NodeId<Name>>);

impl TypePath {
    pub fn get<'a>(&self, index: usize, tree: &'a impl NodeContainer) -> &'a Name {
        self.0[index].get(tree)
    }
}

impl Printable<TreePrinter> for TypePath {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "TypePath".cyan().display_in(arena);

        let path = self.0.gather(with, arena).concat_by(arena.just('.'), arena);

        let single = arena.just(' ').then(path.clone(), arena);
        let multi = arena.newline().then(path, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, From, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TypeVar(pub Symbol);

impl TypeVar {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl PartialEq<Symbol> for TypeVar {
    fn eq(&self, other: &Symbol) -> bool {
        &self.0 == other
    }
}

impl PartialEq<str> for TypeVar {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl Printable<TreePrinter> for TypeVar {
    fn notate<'a>(&'a self, _with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let head = "TypeVar".magenta().display_in(arena);
        let value = self.0.magenta().display_in(arena);

        let single = arena.just(' ').then(value.clone(), arena);
        let multi = arena.newline().then(value, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordFieldType {
    pub name: NodeId<Name>,
    pub ty: NodeId<TypeExpr>,
}

impl Printable<TreePrinter> for RecordFieldType {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty } = self;

        let head = "RecordFieldType".blue().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.notate(with, arena);

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
    pub fields: Vec<NodeId<RecordFieldType>>,
    pub extension: Option<NodeId<TypeVar>>,
}

impl RecordType {
    pub fn get(&self, index: usize, tree: &impl NodeContainer) -> RecordFieldType {
        *self.fields[index].get(tree)
    }
}

impl Printable<TreePrinter> for RecordType {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { fields, extension } = self;

        let head = "RecordType".blue().display_in(arena);

        let fields = fields.gather(with, arena);
        let extension = extension.as_ref().map(|ext| ext.notate(with, arena));

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
    pub name: NodeId<Name>,
    pub ty: Option<NodeId<TypeExpr>>,
}

impl Printable<TreePrinter> for VariantCaseType {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { name, ty } = self;

        let head = "VariantCaseType".blue().display_in(arena);

        let name = name.notate(with, arena);
        let ty = ty.as_ref().map(|ty| ty.notate(with, arena));

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
    pub cases: Vec<NodeId<VariantCaseType>>,
    pub extension: Option<NodeId<TypeVar>>,
}

impl VariantType {
    pub fn get(&self, index: usize, tree: &impl NodeContainer) -> VariantCaseType {
        *self.cases[index].get(tree)
    }
}

impl Printable<TreePrinter> for VariantType {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { cases, extension } = self;

        let head = "VariantType".blue().display_in(arena);

        let cases = cases.gather(with, arena);
        let extension = extension.as_ref().map(|ext| ext.notate(with, arena));

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
    pub input: NodeId<TypeExpr>,
    pub output: NodeId<TypeExpr>,
}

impl Printable<TreePrinter> for FuncType {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { input, output } = self;

        let head = "FuncType".blue().display_in(arena);

        let input = input.notate(with, arena);
        let output = output.notate(with, arena);

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
    pub constructor: NodeId<TypeExpr>,
    pub arg: NodeId<TypeExpr>,
}

impl Printable<TreePrinter> for TypeApplication {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { constructor, arg } = self;

        let head = "TypeApplication".blue().display_in(arena);

        let constructor = constructor.notate(with, arena);
        let arg = arg.notate(with, arena);

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
    Error(NodeId<TypeError>),
    Path(NodeId<TypePath>),
    Record(NodeId<RecordType>),
    Variant(NodeId<VariantType>),
    Func(NodeId<FuncType>),
    Application(NodeId<TypeApplication>),
}

impl Printable<TreePrinter> for TypeExpr {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        match self {
            Self::Error(e) => e.get(&with.tree).notate(with, arena),
            Self::Path(p) => p.get(&with.tree).notate(with, arena),
            Self::Record(r) => r.get(&with.tree).notate(with, arena),
            Self::Variant(v) => v.get(&with.tree).notate(with, arena),
            Self::Func(f) => f.get(&with.tree).notate(with, arena),
            Self::Application(a) => a.get(&with.tree).notate(with, arena),
        }
    }
}

impl TypeExpr {
    #[inline]
    pub fn to_error(self) -> Option<NodeId<TypeError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_type_path(self) -> Option<NodeId<TypePath>> {
        as_variant!(self, Self::Path)
    }

    #[inline]
    pub fn to_record_type(self) -> Option<NodeId<RecordType>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_variant_type(self) -> Option<NodeId<VariantType>> {
        as_variant!(self, Self::Variant)
    }

    #[inline]
    pub fn to_func_type(self) -> Option<NodeId<FuncType>> {
        as_variant!(self, Self::Func)
    }

    #[inline]
    pub fn to_type_application(self) -> Option<NodeId<TypeApplication>> {
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
    pub vars: Vec<NodeId<Name>>,
    pub ty: NodeId<TypeExpr>,
}

impl Printable<TreePrinter> for Type {
    fn notate<'a>(&'a self, with: &'a TreePrinter, arena: &'a Bump) -> Notation<'a> {
        let Self { vars, ty } = self;

        let head = "Type".green().display_in(arena);

        let vars = vars.gather(with, arena);
        let ty = ty.notate(with, arena);

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
    use super::*;
    use crate::inspector::*;

    impl<'t> NodeInspector<'t, NodeId<Type>> {
        /// Check if this type is a type path and return an inspector for it
        pub fn as_type_path(self) -> Option<NodeInspector<'t, NodeId<TypePath>>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_type_path()
                .map(|path_id| NodeInspector::new(path_id, self.tree))
        }

        /// Check if this type is a function type and return an inspector for it
        pub fn as_function(self) -> Option<NodeInspector<'t, NodeId<FuncType>>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_func_type()
                .map(|fn_id| NodeInspector::new(fn_id, self.tree))
        }

        /// Check if this type is a record type and return an inspector for it
        pub fn as_record(self) -> Option<NodeInspector<'t, NodeId<RecordType>>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_record_type()
                .map(|record_id| NodeInspector::new(record_id, self.tree))
        }

        /// Check if this type is a variant type and return an inspector for it
        pub fn as_variant(self) -> Option<NodeInspector<'t, NodeId<VariantType>>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_variant_type()
                .map(|variant_id| NodeInspector::new(variant_id, self.tree))
        }

        /// Check if this type is a type application and return an inspector for it
        pub fn as_type_application(self) -> Option<NodeInspector<'t, NodeId<TypeApplication>>> {
            let ty = self.node.get(self.tree);
            ty.ty
                .get(self.tree)
                .to_type_application()
                .map(|app_id| NodeInspector::new(app_id, self.tree))
        }

        /// Assert the type has the specified number of type variables
        pub fn has_type_vars(self, count: usize) -> Self {
            let vars_len = self.node.get(self.tree).vars.len();
            assert_eq!(
                vars_len, count,
                "Expected {} type variables but found {}",
                count, vars_len
            );
            self
        }

        /// Get an inspector for the type variable at the given index
        pub fn type_var_at(self, index: usize) -> NodeInspector<'t, NodeId<Name>> {
            let ty = self.node.get(self.tree);
            assert!(
                index < ty.vars.len(),
                "Type variable index {} out of bounds (max {})",
                index,
                ty.vars.len() - 1
            );
            let var_id = ty.vars[index];
            NodeInspector::new(var_id, self.tree)
        }

        /// Get an inspector for the type expression
        pub fn type_expr(self) -> NodeInspector<'t, NodeId<TypeExpr>> {
            let ty = self.node.get(self.tree);
            NodeInspector::new(ty.ty, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<TypePath>> {
        /// Assert the type path has the specified number of segments
        pub fn has_segments(self, count: usize) -> Self {
            let segments_len = self.node.get(self.tree).0.len();
            assert_eq!(
                segments_len, count,
                "Expected {} segments but found {}",
                count, segments_len
            );
            self
        }

        /// Assert the type path segment at the given index has the expected name
        pub fn segment_at_is(self, index: usize, expected: &str) -> Self {
            let type_path = self.node.get(self.tree);
            assert!(
                index < type_path.0.len(),
                "Segment index {} out of bounds (max {})",
                index,
                type_path.0.len() - 1
            );
            let segment = type_path.0[index].get(self.tree);
            assert_eq!(
                segment.as_str(),
                expected,
                "Expected segment '{}' but found '{}'",
                expected,
                segment.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<TypeVar>> {
        /// Assert the type variable has the specified name
        pub fn has_name(self, expected: &str) -> Self {
            let type_var = self.node.get(self.tree);
            assert_eq!(
                type_var.as_str(),
                expected,
                "Expected type variable name '{}' but found '{}'",
                expected,
                type_var.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<FuncType>> {
        /// Get an inspector for the function's parameter type
        pub fn input(self) -> NodeInspector<'t, NodeId<TypeExpr>> {
            let function_type = self.node.get(self.tree);
            NodeInspector::new(function_type.input, self.tree)
        }

        /// Get an inspector for the function's return type
        pub fn output(self) -> NodeInspector<'t, NodeId<TypeExpr>> {
            let function_type = self.node.get(self.tree);
            NodeInspector::new(function_type.output, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<RecordType>> {
        /// Assert the record type has the specified number of fields
        pub fn has_fields(self, count: usize) -> Self {
            let fields_len = self.node.get(self.tree).fields.len();
            assert_eq!(
                fields_len, count,
                "Expected {} fields but found {}",
                count, fields_len
            );
            self
        }

        /// Get an inspector for the record field at the given index
        pub fn field_at(self, index: usize) -> NodeInspector<'t, NodeId<RecordFieldType>> {
            let record_type = self.node.get(self.tree);
            assert!(
                index < record_type.fields.len(),
                "Field index {} out of bounds (max {})",
                index,
                record_type.fields.len() - 1
            );
            let field_id = record_type.fields[index];
            NodeInspector::new(field_id, self.tree)
        }

        /// Get an inspector for the record type extension if it has one
        pub fn extension(self) -> Option<NodeInspector<'t, NodeId<TypeVar>>> {
            let record_type = self.node.get(self.tree);
            record_type
                .extension
                .map(|ext_id| NodeInspector::new(ext_id, self.tree))
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<RecordFieldType>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<RecordFieldType>> {
        /// Assert the record field has the specified name
        pub fn has_field_name(self, expected: &str) -> Self {
            self.assert_name(expected, "field")
        }

        pub fn type_expr(self) -> NodeInspector<'t, NodeId<TypeExpr>> {
            let field = self.node.get(self.tree);
            NodeInspector::new(field.ty, self.tree)
        }
    }

    impl<'t> NodeInspector<'t, NodeId<VariantType>> {
        /// Assert the variant type has the specified number of cases
        pub fn has_cases(self, count: usize) -> Self {
            let cases_len = self.node.get(self.tree).cases.len();
            assert_eq!(
                cases_len, count,
                "Expected {} cases but found {}",
                count, cases_len
            );
            self
        }

        /// Get an inspector for the variant case at the given index
        pub fn case_at(self, index: usize) -> NodeInspector<'t, NodeId<VariantCaseType>> {
            let variant_type = self.node.get(self.tree);
            assert!(
                index < variant_type.cases.len(),
                "Case index {} out of bounds (max {})",
                index,
                variant_type.cases.len() - 1
            );
            let case_id = variant_type.cases[index];
            NodeInspector::new(case_id, self.tree)
        }

        /// Check if this variant type has an extension and return an inspector for it
        pub fn extension(self) -> Option<NodeInspector<'t, NodeId<TypeVar>>> {
            let variant_type = self.node.get(self.tree);
            variant_type
                .extension
                .map(|ext_id| NodeInspector::new(ext_id, self.tree))
        }
    }

    impl<'t> NamedNode for NodeInspector<'t, NodeId<VariantCaseType>> {
        fn assert_name(self, expected: &str, node_type: &str) -> Self {
            let name = self.node.get(self.tree).name.get(self.tree);
            assert_eq!(
                name.as_str(),
                expected,
                "Expected {} name '{}' but found '{}'",
                node_type,
                expected,
                name.0
            );
            self
        }
    }

    impl<'t> NodeInspector<'t, NodeId<VariantCaseType>> {
        /// Assert the variant case has the specified name
        pub fn has_case_name(self, expected: &str) -> Self {
            self.assert_name(expected, "case")
        }

        /// Get an inspector for the case's type if it has one
        pub fn type_expr(self) -> Option<NodeInspector<'t, NodeId<TypeExpr>>> {
            let case = self.node.get(self.tree);
            case.ty.map(|ty_id| NodeInspector::new(ty_id, self.tree))
        }
    }

    impl<'t> NodeInspector<'t, NodeId<TypeExpr>> {
        /// Check if this type expression is an error type
        pub fn as_error(self) -> Option<NodeInspector<'t, NodeId<TypeError>>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_error()
                .map(|err_id| NodeInspector::new(err_id, self.tree))
        }

        /// Check if this type expression is a type path
        pub fn as_path(self) -> Option<NodeInspector<'t, NodeId<TypePath>>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_type_path()
                .map(|path_id| NodeInspector::new(path_id, self.tree))
        }

        /// Check if this type expression is a record type
        pub fn as_record(self) -> Option<NodeInspector<'t, NodeId<RecordType>>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_record_type()
                .map(|rec_id| NodeInspector::new(rec_id, self.tree))
        }

        /// Check if this type expression is a variant type
        pub fn as_variant(self) -> Option<NodeInspector<'t, NodeId<VariantType>>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_variant_type()
                .map(|var_id| NodeInspector::new(var_id, self.tree))
        }

        /// Check if this type expression is a function type
        pub fn as_function(self) -> Option<NodeInspector<'t, NodeId<FuncType>>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_func_type()
                .map(|fn_id| NodeInspector::new(fn_id, self.tree))
        }

        /// Check if this type expression is a type application
        pub fn as_application(self) -> Option<NodeInspector<'t, NodeId<TypeApplication>>> {
            let type_expr = self.node.get(self.tree);
            type_expr
                .to_type_application()
                .map(|app_id| NodeInspector::new(app_id, self.tree))
        }
    }

    impl<'t> NodeInspector<'t, NodeId<TypeApplication>> {
        /// Get an inspector for the type constructor
        pub fn constructor(self) -> NodeInspector<'t, NodeId<TypeExpr>> {
            let type_app = self.node.get(self.tree);
            NodeInspector::new(type_app.constructor, self.tree)
        }

        /// Get an inspector for the type argument
        pub fn arg(self) -> NodeInspector<'t, NodeId<TypeExpr>> {
            let type_app = self.node.get(self.tree);
            NodeInspector::new(type_app.arg, self.tree)
        }
    }
}
