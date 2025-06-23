use derive_more::{Display, From, IntoIterator};
use kola_macros::Inspector;
use kola_print::prelude::*;
use kola_utils::{as_variant, interner::StrKey};
use serde::{Deserialize, Serialize};

use super::{ModulePath, Pat, Type, ValueName};
use crate::{
    id::Id,
    print::NodePrinter,
    tree::{TreeBuilder, TreeView},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ExprError;

impl ExprError {
    pub fn new_in(builder: &mut TreeBuilder) -> Id<Self> {
        builder.insert(Self)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, ExprError> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        "ExprError".red().display_in(arena)
    }
}

#[derive(Debug, From, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum LiteralExpr {
    Unit,
    Bool(bool),
    Num(f64),
    Char(char),
    Str(StrKey),
}

impl LiteralExpr {
    pub fn new_in(value: impl Into<Self>, builder: &mut TreeBuilder) -> Id<Self> {
        builder.insert(value.into())
    }

    pub fn to_bool(&self) -> Option<bool> {
        as_variant!(self, Self::Bool).copied()
    }

    pub fn to_num(&self) -> Option<f64> {
        as_variant!(self, Self::Num).copied()
    }

    pub fn to_char(&self) -> Option<char> {
        as_variant!(self, Self::Char).copied()
    }

    pub fn to_str(&self) -> Option<&StrKey> {
        as_variant!(self, Self::Str)
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    pub fn is_num(&self) -> bool {
        matches!(self, Self::Num(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Self::Char(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Self::Str(_))
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, LiteralExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "LiteralExpr".purple().display_in(arena);

        let lit = match *self.value {
            LiteralExpr::Unit => "Unit".yellow().display_in(arena),
            LiteralExpr::Bool(b) => b.yellow().display_in(arena),
            LiteralExpr::Num(n) => n.yellow().display_in(arena),
            LiteralExpr::Char(c) => c.yellow().display_in(arena),
            LiteralExpr::Str(s) => self
                .interner
                .get(s)
                .expect("Symbol not found")
                .yellow()
                .display_in(arena),
        }
        .enclose_by(arena.just('"'), arena);

        let single = arena.just(' ').then(lit.clone(), arena);
        let multi = arena.newline().then(lit, arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[into_iterator(owned, ref)]
pub struct ListExpr(pub Vec<Id<Expr>>);

impl ListExpr {
    pub fn empty_in(builder: &mut TreeBuilder) -> Id<Self> {
        builder.insert(Self(Vec::new()))
    }

    pub fn new_in<I>(elements: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator,
        I::Item: Into<Expr>,
    {
        let elements = elements
            .into_iter()
            .map(|e| builder.insert(e.into()))
            .collect();
        builder.insert(Self(elements))
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, ListExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "List".blue().display_in(arena);

        let values = self.to_slice(&self.value.0).gather(arena);

        let single = values.clone().concat_map(
            |expr| arena.just(' ').then(expr.flatten(arena), arena),
            arena,
        );
        let multi = values
            .concat_map(|expr| arena.newline().then(expr, arena), arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct RecordField {
    pub field: Id<ValueName>,
    pub type_: Option<Id<Type>>,
    pub value: Id<Expr>,
}

impl RecordField {
    pub fn new_in(
        field: impl Into<ValueName>,
        type_: Option<Type>,
        value: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let field = builder.insert(field.into());
        let type_ = type_.map(|t| builder.insert(t));
        let value = builder.insert(value.into());

        builder.insert(Self {
            field,
            type_,
            value,
        })
    }

    pub fn field(self, tree: &impl TreeView) -> ValueName {
        *self.field.get(tree)
    }

    pub fn type_(self, tree: &impl TreeView) -> Option<Type> {
        self.type_.map(|t| *t.get(tree))
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordField> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordField {
            field,
            type_,
            value,
        } = *self.value;

        let head = "RecordField".blue().display_in(arena);

        let field = self.to_id(field).notate(arena);
        let type_ = type_.map(|t| self.to_id(t).notate(arena));
        let value = self.to_id(value).notate(arena);

        let single = [
            arena.notate(" label = "),
            field.clone(),
            type_
                .clone()
                .map(|t| arena.notate(", type = ").then(t, arena))
                .or_not(arena),
            arena.notate(", value = "),
            value.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("label = "),
            field,
            type_
                .map(|t| [arena.newline(), arena.notate("type = "), t].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { x = 10, y = 20 }
#[derive(
    Debug,
    Inspector,
    From,
    IntoIterator,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[into_iterator(owned, ref)]
pub struct RecordExpr(pub Vec<Id<RecordField>>);

impl RecordExpr {
    pub fn new_in<I>(fields: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator<Item = Id<RecordField>>,
    {
        let fields = fields.into_iter().collect();
        builder.insert(Self(fields))
    }
}

// impl RecordExpr {
//     pub fn get(&self, name: impl AsRef<str>, tree: &impl TreeView) -> Option<RecordField> {
//         self.0.iter().find_map(|id| {
//             let field = id.get(tree);
//             (field.field(tree) == name.as_ref())
//                 .then_some(field)
//                 .copied()
//         })
//     }
// }

impl<'a> Notate<'a> for NodePrinter<'a, RecordExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "Record".blue().display_in(arena);

        let fields = self.to_slice(&self.value.0).gather(arena);

        let single = fields.clone().concat_map(
            |field| arena.just(' ').then(field.flatten(arena), arena),
            arena,
        );
        let multi = fields
            .concat_map(|field| arena.newline().then(field, arena), arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { y [: type] | +x [: type] = 10 }
#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct RecordExtendExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<Type>>,
    pub select: Id<FieldPath>,
    pub value: Id<Expr>,
    pub value_type: Option<Id<Type>>,
}

impl RecordExtendExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<Type>,
        select: impl Into<FieldPath>,
        value: impl Into<Expr>,
        value_type: Option<Type>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let source_type = source_type.map(|t| builder.insert(t));
        let select = builder.insert(select.into());
        let value = builder.insert(value.into());
        let value_type = value_type.map(|t| builder.insert(t));

        builder.insert(Self {
            source,
            source_type,
            select,
            value,
            value_type,
        })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }

    pub fn source_type(self, tree: &impl TreeView) -> Option<Type> {
        self.source_type.map(|t| *t.get(tree))
    }

    pub fn select(self, tree: &impl TreeView) -> &FieldPath {
        self.select.get(tree)
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }

    pub fn value_type(self, tree: &impl TreeView) -> Option<Type> {
        self.value_type.map(|t| *t.get(tree))
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordExtendExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordExtendExpr {
            source,
            source_type,
            select,
            value,
            value_type,
        } = self.value;

        let head = "RecordExtend".blue().display_in(arena);

        let source = self.to_id(*source).notate(arena);
        let source_type = source_type.map(|t| self.to(t).notate(arena));
        let field = self.to_id(*select).notate(arena);
        let value = self.to_id(*value).notate(arena);
        let value_type = value_type.map(|t| self.to(t).notate(arena));

        let single = [
            arena.notate(" source = "),
            source.clone(),
            source_type
                .clone()
                .map(|t| arena.notate(", source_type = ").then(t, arena))
                .or_not(arena),
            arena.notate(", field = "),
            field.clone(),
            arena.notate(", value = "),
            value.clone(),
            value_type
                .clone()
                .map(|t| arena.notate(", value_type = ").then(t, arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            source_type
                .map(|t| [arena.newline(), arena.notate("source_type = "), t].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("field = "),
            field,
            arena.newline(),
            arena.notate("value = "),
            value,
            value_type
                .map(|t| [arena.newline(), arena.notate("value_type = "), t].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { y [: type] | -x [: type] }
#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct RecordRestrictExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<Type>>,
    pub select: Id<FieldPath>,
    pub value_type: Option<Id<Type>>,
}

impl RecordRestrictExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<Type>,
        select: impl Into<FieldPath>,
        value_type: Option<Type>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let source_type = source_type.map(|t| builder.insert(t));
        let select = builder.insert(select.into());
        let value_type = value_type.map(|t| builder.insert(t));

        builder.insert(Self {
            source,
            source_type,
            select,
            value_type,
        })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }

    pub fn source_type(self, tree: &impl TreeView) -> Option<Type> {
        self.source_type.map(|t| *t.get(tree))
    }

    pub fn select(self, tree: &impl TreeView) -> &FieldPath {
        self.select.get(tree)
    }

    pub fn value_type(self, tree: &impl TreeView) -> Option<Type> {
        self.value_type.map(|t| *t.get(tree))
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordRestrictExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordRestrictExpr {
            source,
            source_type,
            select,
            value_type,
        } = self.value;

        let head = "RecordRestrict".blue().display_in(arena);

        let source = self.to_id(*source).notate(arena);
        let source_type = source_type.map(|t| self.to(t).notate(arena));
        let field = self.to_id(*select).notate(arena);
        let value_type = value_type.map(|t| self.to(t).notate(arena));

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            source_type
                .clone()
                .map(|t| arena.notate(", source_type = ").then(t, arena))
                .or_not(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            value_type
                .clone()
                .map(|t| arena.notate(", value_type = ").then(t, arena))
                .or_not(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            source_type
                .map(|t| [arena.newline(), arena.notate("source_type = "), t].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("field = "),
            field,
            value_type
                .map(|t| [arena.newline(), arena.notate("value_type = "), t].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum RecordUpdateOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordUpdateOp> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        self.value.red().display_in(arena)
    }
}

// { y | x = 10 }
#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct RecordUpdateExpr {
    pub source: Id<Expr>,
    pub source_type: Option<Id<Type>>,
    pub select: Id<FieldPath>,
    pub op: Id<RecordUpdateOp>,
    pub value: Id<Expr>,
    pub value_type: Option<Id<Type>>,
}

impl RecordUpdateExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        source_type: Option<Type>,
        select: impl Into<FieldPath>,
        op: RecordUpdateOp,
        value: impl Into<Expr>,
        value_type: Option<Type>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let source_type = source_type.map(|t| builder.insert(t));
        let select = builder.insert(select.into());
        let op = builder.insert(op);
        let value = builder.insert(value.into());
        let value_type = value_type.map(|t| builder.insert(t));

        builder.insert(Self {
            source,
            source_type,
            select,
            op,
            value,
            value_type,
        })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }

    pub fn source_type(self, tree: &impl TreeView) -> Option<Type> {
        self.source_type.map(|t| *t.get(tree))
    }

    pub fn select(self, tree: &impl TreeView) -> &FieldPath {
        self.select.get(tree)
    }

    pub fn op(self, tree: &impl TreeView) -> RecordUpdateOp {
        *self.op.get(tree)
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }

    pub fn value_type(self, tree: &impl TreeView) -> Option<Type> {
        self.value_type.map(|t| *t.get(tree))
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordUpdateExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordUpdateExpr {
            source,
            source_type,
            select,
            op,
            value,
            value_type,
        } = self.value;

        let head = "RecordUpdate".blue().display_in(arena);

        let source = self.to_id(*source).notate(arena);
        let source_type = source_type.map(|t| self.to(t).notate(arena));
        let field = self.to_id(*select).notate(arena);
        let op = self.to_id(*op).notate(arena);
        let value = self.to_id(*value).notate(arena);
        let value_type = value_type.map(|t| self.to(t).notate(arena));

        let single = [
            arena.notate(" source = "),
            source.clone(),
            source_type
                .clone()
                .map(|t| arena.notate(", source_type = ").then(t, arena))
                .or_not(arena),
            arena.notate(", field = "),
            field.clone(),
            arena.notate(", op = "),
            op.clone(),
            arena.notate(", value = "),
            value.clone(),
            value_type
                .clone()
                .map(|t| arena.notate(", value_type = ").then(t, arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            source_type
                .map(|t| [arena.newline(), arena.notate("source_type = "), t].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("field = "),
            field,
            arena.newline(),
            arena.notate("op = "),
            op,
            arena.newline(),
            arena.notate("value = "),
            value,
            value_type
                .map(|t| [arena.newline(), arena.notate("value_type = "), t].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// pub struct FieldPathIter<'a, T: TreeView> {
//     tree: &'a T,
//     current: Option<FieldPath>,
// }

// impl<'a, T: TreeView> FieldPathIter<'a, T> {
//     pub fn new(tree: &'a T, path: FieldPath) -> Self {
//         Self {
//             tree,
//             current: Some(path),
//         }
//     }
// }

// impl<T: TreeView> Iterator for FieldPathIter<'_, T> {
//     type Item = Id<ValueName>;

//     fn next(&mut self) -> Option<Self::Item> {
//         match self.current {
//             Some(FieldPath::Next(first, next)) => {
//                 self.current = Some(*first.get(self.tree));
//                 Some(next)
//             }
//             Some(FieldPath::First(name)) => {
//                 self.current = None;
//                 Some(name)
//             }
//             None => None,
//         }
//     }
// }

// #[derive(
//     Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
// )]
// pub enum FieldPath {
//     Next(Id<Self>, Id<ValueName>),
//     First(Id<ValueName>),
// }

// impl FieldPath {
//     pub fn new_in(name: impl Into<ValueName>, tree: &mut impl TreeView) -> Id<Self> {
//         let name = tree.insert(name.into());
//         tree.insert(FieldPath::First(name))
//     }

//     pub fn from_iter<T>(iter: impl IntoIterator<Item = ValueName>, tree: &mut T) -> Id<Self>
//     where
//         T: TreeView,
//     {
//         let mut iter = iter.into_iter();
//         let first = iter
//             .next()
//             .map(|name| tree.insert(name))
//             .expect("ModulePath must have at least one name");

//         let path = iter.fold(FieldPath::First(first), |path, name| {
//             let next = tree.insert(name);
//             FieldPath::Next(tree.insert(path), next)
//         });

//         tree.insert(path)
//     }

//     pub fn iter_rev<T>(self, tree: &T) -> FieldPathIter<'_, T>
//     where
//         T: TreeView,
//     {
//         FieldPathIter::new(tree, self)
//     }

//     pub fn get_from_back(self, index: usize, tree: &impl TreeView) -> Option<Id<ValueName>> {
//         self.iter_rev(tree).nth(index)
//     }

//     pub fn last(self, tree: &impl TreeView) -> Option<Id<ValueName>> {
//         self.iter_rev(tree).next()
//     }

//     pub fn len(self, tree: &impl TreeView) -> usize {
//         self.iter_rev(tree).count()
//     }
// }

// impl<'a> Notate<'a> for NodePrinter<'a, FieldPath> {
//     fn notate(&self, arena: &'a Bump) -> Notation<'a> {
//         match *self.value {
//             FieldPath::Next(first, next) => {
//                 let first = self.to(first).notate(arena);
//                 let next = self.to(next).notate(arena);

//                 let single = [first.clone(), arena.just(' '), next.clone()]
//                     .concat_in(arena)
//                     .flatten(arena);
//                 let multi = [first, arena.newline(), next]
//                     .concat_in(arena)
//                     .indent(arena);

//                 single.or(multi, arena)
//             }
//             FieldPath::First(name) => {
//                 let head = "FieldPath".cyan().display_in(arena);
//                 let name = self.to(name).notate(arena);

//                 [head, arena.just(' '), name]
//                     .concat_in(arena)
//                     .flatten(arena)
//             }
//         }
//     }
// }

#[derive(
    Debug,
    Inspector,
    From,
    IntoIterator,
    Default,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
#[from(forward)]
#[into_iterator(owned, ref)]
pub struct FieldPath(pub Vec<Id<ValueName>>);

impl FieldPath {
    pub fn new_in<I>(fields: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator,
        I::Item: Into<ValueName>,
    {
        let fields = fields
            .into_iter()
            .map(|f| builder.insert(f.into()))
            .collect();
        builder.insert(Self(fields))
    }

    pub fn get(&self, index: usize, tree: &impl TreeView) -> ValueName {
        *self.0[index].get(tree)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Id<ValueName>> {
        (&self).into_iter()
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, FieldPath> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let fields = &self.value.0;

        let head = "FieldPath".cyan().display_in(arena);

        let fields = self.to_slice(fields).gather(arena);

        let single = arena
            .just(' ')
            .then(fields.clone().concat_by(arena.just(' '), arena), arena)
            .flatten(arena);

        let multi = arena
            .newline()
            .then(fields.concat_by(arena.newline(), arena), arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct QualifiedExpr {
    pub path: Option<Id<ModulePath>>,
    pub source: Id<ValueName>,
    pub fields: Option<Id<FieldPath>>,
}

impl QualifiedExpr {
    pub fn new_in(
        path: Option<ModulePath>,
        source: impl Into<ValueName>,
        fields: Option<FieldPath>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let path = path.map(|p| builder.insert(p));
        let source = builder.insert(source.into());
        let fields = fields.map(|f| builder.insert(f));

        builder.insert(Self {
            path,
            source,
            fields,
        })
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, QualifiedExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let QualifiedExpr {
            path,
            source,
            fields,
        } = *self.value;

        let head = "QualifiedExpr".cyan().display_in(arena);

        let path = path.map(|p| self.to(p).notate(arena));
        let source = self.to(source).notate(arena);
        let fields = fields.map(|f| self.to(f).notate(arena));

        let single = [
            path.clone()
                .map(|p| [arena.notate(" path = "), p, arena.just(',')].concat_in(arena))
                .or_not(arena),
            arena.notate(" source = "),
            source.clone(),
            fields
                .clone()
                .map(|f| [arena.notate(", fields = "), f].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            path.map(|p| [arena.newline(), arena.notate("path = "), p].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("source = "),
            source,
            fields
                .map(|f| [arena.newline(), arena.notate("fields = "), f].concat_in(arena))
                .or_not(arena),
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl<'a> Notate<'a> for NodePrinter<'a, UnaryOp> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        self.value.red().display_in(arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct UnaryExpr {
    pub op: Id<UnaryOp>,
    pub operand: Id<Expr>,
}

impl UnaryExpr {
    pub fn new_in(op: UnaryOp, operand: impl Into<Expr>, builder: &mut TreeBuilder) -> Id<Self> {
        let op = builder.insert(op);
        let operand = builder.insert(operand.into());

        builder.insert(Self { op, operand })
    }

    pub fn op(self, tree: &impl TreeView) -> UnaryOp {
        *self.op.get(tree)
    }

    pub fn operand(self, tree: &impl TreeView) -> Expr {
        *self.operand.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, UnaryExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let UnaryExpr { op, operand } = self.value;

        let head = "Unary".blue().display_in(arena);

        let op = self.to_id(*op).notate(arena);
        let operand = self.to_id(*operand).notate(arena);

        let single = [
            arena.just(' '),
            op.clone(),
            arena.just(' '),
            operand.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [arena.newline(), op, arena.newline(), operand]
            .concat_in(arena)
            .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Display, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Less,
    Greater,
    LessEq,
    GreaterEq,
    // Logical
    And,
    Or,
    Xor,
    // Equality
    Eq,
    NotEq,
    // Record
    Merge,
}

impl<'a> Notate<'a> for NodePrinter<'a, BinaryOp> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        self.value.red().display_in(arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct BinaryExpr {
    pub op: Id<BinaryOp>,
    pub left: Id<Expr>,
    pub right: Id<Expr>,
}

impl BinaryExpr {
    pub fn new_in(
        op: BinaryOp,
        left: impl Into<Expr>,
        right: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let op = builder.insert(op);
        let left = builder.insert(left.into());
        let right = builder.insert(right.into());

        builder.insert(Self { op, left, right })
    }

    pub fn op(self, tree: &impl TreeView) -> BinaryOp {
        *self.op.get(tree)
    }

    pub fn left(self, tree: &impl TreeView) -> Expr {
        *self.left.get(tree)
    }

    pub fn right(self, tree: &impl TreeView) -> Expr {
        *self.right.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, BinaryExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let BinaryExpr { op, left, right } = self.value;

        let head = "Binary".blue().display_in(arena);

        let left = self.to_id(*left).notate(arena);
        let op = self.to_id(*op).notate(arena);
        let right = self.to_id(*right).notate(arena);

        let single = [
            arena.just(' '),
            left.clone().flatten(arena),
            arena.just(' '),
            op.clone().flatten(arena),
            arena.just(' '),
            right.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            left,
            arena.newline(),
            op,
            arena.newline(),
            right,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct LetExpr {
    pub name: Id<ValueName>,
    pub value_type: Option<Id<Type>>,
    pub value: Id<Expr>,
    pub inside: Id<Expr>,
}

impl LetExpr {
    pub fn new_in(
        name: impl Into<ValueName>,
        value_type: Option<Type>,
        value: impl Into<Expr>,
        inside: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let name = builder.insert(name.into());
        let value_type = value_type.map(|t| builder.insert(t));
        let value = builder.insert(value.into());
        let inside = builder.insert(inside.into());

        builder.insert(Self {
            name,
            value_type,
            value,
            inside,
        })
    }

    pub fn name(self, tree: &impl TreeView) -> ValueName {
        *self.name.get(tree)
    }

    pub fn value_type(self, tree: &impl TreeView) -> Option<&Type> {
        self.value_type.map(|t| t.get(tree))
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }

    pub fn inside(self, tree: &impl TreeView) -> Expr {
        *self.inside.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, LetExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let LetExpr {
            name,
            value_type,
            value,
            inside,
        } = self.value;

        let head = "Let".blue().display_in(arena);

        let name = self.to_id(*name).notate(arena);
        let value_type = value_type.map(|t| self.to(t).notate(arena));
        let value = self.to_id(*value).notate(arena);
        let inside = self.to_id(*inside).notate(arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
            value_type
                .clone()
                .map(|t| arena.notate(", value_type = ").then(t, arena))
                .or_not(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
            arena.notate(", inside = "),
            inside.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("name = "),
            name,
            value_type
                .map(|t| [arena.newline(), arena.notate("value_type = "), t].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("value = "),
            value,
            arena.newline(),
            arena.notate("inside = "),
            inside,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct IfExpr {
    pub predicate: Id<Expr>,
    pub then: Id<Expr>,
    pub or: Id<Expr>,
}

impl IfExpr {
    pub fn new_in(
        predicate: impl Into<Expr>,
        then: impl Into<Expr>,
        or: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let predicate = builder.insert(predicate.into());
        let then = builder.insert(then.into());
        let or = builder.insert(or.into());

        builder.insert(Self {
            predicate,
            then,
            or,
        })
    }

    pub fn predicate(self, tree: &impl TreeView) -> Expr {
        *self.predicate.get(tree)
    }

    pub fn then(self, tree: &impl TreeView) -> Expr {
        *self.then.get(tree)
    }

    pub fn or(self, tree: &impl TreeView) -> Expr {
        *self.or.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, IfExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IfExpr {
            predicate,
            then,
            or,
        } = self.value;

        let head = "If".blue().display_in(arena);

        let predicate = self.to_id(*predicate).notate(arena);
        let then = self.to_id(*then).notate(arena);
        let or = self.to_id(*or).notate(arena);

        let single = [
            arena.notate(" predicate = "),
            predicate.clone().flatten(arena),
            arena.notate(", then = "),
            then.clone().flatten(arena),
            arena.notate(", or = "),
            or.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("predicate = "),
            predicate,
            arena.newline(),
            arena.notate("then = "),
            then,
            arena.newline(),
            arena.notate("or = "),
            or,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct CaseBranch {
    pub pat: Id<Pat>,
    pub matches: Id<Expr>,
}

impl CaseBranch {
    pub fn new_in(
        pat: impl Into<Pat>,
        matches: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let pat = builder.insert(pat.into());
        let matches = builder.insert(matches.into());

        builder.insert(Self { pat, matches })
    }

    pub fn pat(self, tree: &impl TreeView) -> Pat {
        *self.pat.get(tree)
    }

    pub fn matches(self, tree: &impl TreeView) -> Expr {
        *self.matches.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, CaseBranch> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let CaseBranch { pat, matches } = self.value;

        let head = "Branch".blue().display_in(arena);

        let pat = self.to_id(*pat).notate(arena);
        let matches = self.to_id(*matches).notate(arena);

        let single = [
            arena.notate(" pat = "),
            pat.clone().flatten(arena),
            arena.notate(", matches = "),
            matches.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("pat = "),
            pat,
            arena.newline(),
            arena.notate("matches = "),
            matches,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(Debug, Inspector, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct CaseExpr {
    pub source: Id<Expr>,
    pub branches: Vec<Id<CaseBranch>>,
}

impl CaseExpr {
    pub fn new_in<I>(source: impl Into<Expr>, branches: I, builder: &mut TreeBuilder) -> Id<Self>
    where
        I: IntoIterator<Item = Id<CaseBranch>>,
    {
        let source = builder.insert(source.into());
        let branches = branches.into_iter().collect();

        builder.insert(Self { source, branches })
    }

    pub fn source(&self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, CaseExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let CaseExpr { source, branches } = self.value;

        let head = "Case".blue().display_in(arena);

        let source = self.to_id(*source).notate(arena);
        let branches = self.to_slice(branches).gather(arena).concat_in(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", branches = "),
            branches.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source.clone(),
            arena.newline(),
            arena.notate("branches = "),
            branches,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct CallExpr {
    pub func: Id<Expr>,
    pub arg: Id<Expr>,
}

impl CallExpr {
    pub fn new_in(
        func: impl Into<Expr>,
        arg: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let func = builder.insert(func.into());
        let arg = builder.insert(arg.into());

        builder.insert(Self { func, arg })
    }

    pub fn func(self, tree: &impl TreeView) -> Expr {
        *self.func.get(tree)
    }

    pub fn arg(self, tree: &impl TreeView) -> Expr {
        *self.arg.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, CallExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let CallExpr { func, arg } = self.value;

        let head = "Call".blue().display_in(arena);

        let func = self.to_id(*func).notate(arena);
        let arg = self.to_id(*arg).notate(arena);

        let single = [
            arena.notate(" func = "),
            func.clone().flatten(arena),
            arena.notate(", arg = "),
            arg.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("func = "),
            func,
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
    Debug, Inspector, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct LambdaExpr {
    pub param: Id<ValueName>, // TODO pattern
    pub param_type: Option<Id<Type>>,
    pub body: Id<Expr>,
}

impl LambdaExpr {
    pub fn new_in(
        param: impl Into<ValueName>,
        param_type: Option<Type>,
        body: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let param = builder.insert(param.into());
        let param_type = param_type.map(|t| builder.insert(t));
        let body = builder.insert(body.into());

        builder.insert(Self {
            param,
            param_type,
            body,
        })
    }

    pub fn param(self, tree: &impl TreeView) -> ValueName {
        *self.param.get(tree)
    }

    pub fn body(self, tree: &impl TreeView) -> Expr {
        *self.body.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, LambdaExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let LambdaExpr {
            param,
            param_type,
            body,
        } = self.value;

        let head = "Func".blue().display_in(arena);

        let param = self.to_id(*param).notate(arena);
        let param_type = param_type.map(|t| self.to_id(t).notate(arena));
        let body = self.to_id(*body).notate(arena);

        let single = [
            arena.notate(" param = "),
            param.clone(),
            param_type
                .clone()
                .map(|t| arena.notate(", param_type = ").then(t, arena))
                .or_not(arena),
            arena.notate(", body = "),
            body.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("param = "),
            param,
            param_type
                .clone()
                .map(|t| [arena.newline(), arena.notate("param_type = "), t].concat_in(arena))
                .or_not(arena),
            arena.newline(),
            arena.notate("body = "),
            body,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
    Inspector,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub struct TagExpr(pub Id<ValueName>);

impl TagExpr {
    pub fn new_in(tag: impl Into<ValueName>, builder: &mut TreeBuilder) -> Id<Self> {
        let tag = builder.insert(tag.into());

        builder.insert(Self(tag))
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, TagExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let head = "TagExpr".blue().display_in(arena);

        let tag = self.to_id(self.value.0).notate(arena);

        let single = arena.just(' ').then(tag.clone(), arena).flatten(arena);
        let multi = arena.newline().then(tag, arena).indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
    Inspector,
    From,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
)]
pub enum Expr {
    Error(Id<ExprError>),
    Literal(Id<LiteralExpr>),
    Qualified(Id<QualifiedExpr>),
    List(Id<ListExpr>),
    Record(Id<RecordExpr>),
    RecordExtend(Id<RecordExtendExpr>),
    RecordRestrict(Id<RecordRestrictExpr>),
    RecordUpdate(Id<RecordUpdateExpr>),
    Unary(Id<UnaryExpr>),
    Binary(Id<BinaryExpr>),
    Let(Id<LetExpr>),
    If(Id<IfExpr>),
    Case(Id<CaseExpr>),
    Call(Id<CallExpr>),
    Lambda(Id<LambdaExpr>),
    Tag(Id<TagExpr>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Expr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Expr::Error(e) => self.to_id(e).notate(arena),
            Expr::Literal(l) => self.to_id(l).notate(arena),
            Expr::Qualified(q) => self.to_id(q).notate(arena),
            Expr::List(l) => self.to_id(l).notate(arena),
            Expr::Record(r) => self.to_id(r).notate(arena),
            Expr::RecordExtend(r) => self.to_id(r).notate(arena),
            Expr::RecordRestrict(r) => self.to_id(r).notate(arena),
            Expr::RecordUpdate(r) => self.to_id(r).notate(arena),
            Expr::Unary(u) => self.to_id(u).notate(arena),
            Expr::Binary(b) => self.to_id(b).notate(arena),
            Expr::Let(l) => self.to_id(l).notate(arena),
            Expr::If(i) => self.to_id(i).notate(arena),
            Expr::Case(c) => self.to_id(c).notate(arena),
            Expr::Call(c) => self.to_id(c).notate(arena),
            Expr::Lambda(f) => self.to_id(f).notate(arena),
            Expr::Tag(t) => self.to_id(t).notate(arena),
        }
    }
}

impl Expr {
    #[inline]
    pub fn to_error(self) -> Option<Id<ExprError>> {
        as_variant!(self, Self::Error)
    }

    #[inline]
    pub fn to_literal(self) -> Option<Id<LiteralExpr>> {
        as_variant!(self, Self::Literal)
    }

    #[inline]
    pub fn to_qualified(self) -> Option<Id<QualifiedExpr>> {
        as_variant!(self, Self::Qualified)
    }

    #[inline]
    pub fn to_list(self) -> Option<Id<ListExpr>> {
        as_variant!(self, Self::List)
    }

    #[inline]
    pub fn to_record(self) -> Option<Id<RecordExpr>> {
        as_variant!(self, Self::Record)
    }

    #[inline]
    pub fn to_record_extend(self) -> Option<Id<RecordExtendExpr>> {
        as_variant!(self, Self::RecordExtend)
    }

    #[inline]
    pub fn to_record_restrict(self) -> Option<Id<RecordRestrictExpr>> {
        as_variant!(self, Self::RecordRestrict)
    }

    #[inline]
    pub fn to_record_update(self) -> Option<Id<RecordUpdateExpr>> {
        as_variant!(self, Self::RecordUpdate)
    }

    #[inline]
    pub fn to_unary(self) -> Option<Id<UnaryExpr>> {
        as_variant!(self, Self::Unary)
    }

    #[inline]
    pub fn to_binary(self) -> Option<Id<BinaryExpr>> {
        as_variant!(self, Self::Binary)
    }

    #[inline]
    pub fn to_let(self) -> Option<Id<LetExpr>> {
        as_variant!(self, Self::Let)
    }

    #[inline]
    pub fn to_if(self) -> Option<Id<IfExpr>> {
        as_variant!(self, Self::If)
    }

    #[inline]
    pub fn to_case(self) -> Option<Id<CaseExpr>> {
        as_variant!(self, Self::Case)
    }

    #[inline]
    pub fn to_call(self) -> Option<Id<CallExpr>> {
        as_variant!(self, Self::Call)
    }

    #[inline]
    pub fn to_lambda(self) -> Option<Id<LambdaExpr>> {
        as_variant!(self, Self::Lambda)
    }

    #[inline]
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error(_))
    }

    #[inline]
    pub fn is_literal(self) -> bool {
        matches!(self, Self::Literal(_))
    }

    #[inline]
    pub fn is_qualified(self) -> bool {
        matches!(self, Self::Qualified(_))
    }

    #[inline]
    pub fn is_list(self) -> bool {
        matches!(self, Self::List(_))
    }

    #[inline]
    pub fn is_record(self) -> bool {
        matches!(self, Self::Record(_))
    }

    #[inline]
    pub fn is_record_extend(self) -> bool {
        matches!(self, Self::RecordExtend(_))
    }

    #[inline]
    pub fn is_record_restrict(self) -> bool {
        matches!(self, Self::RecordRestrict(_))
    }

    #[inline]
    pub fn is_record_update(self) -> bool {
        matches!(self, Self::RecordUpdate(_))
    }

    #[inline]
    pub fn is_unary(self) -> bool {
        matches!(self, Self::Unary(_))
    }

    #[inline]
    pub fn is_binary(self) -> bool {
        matches!(self, Self::Binary(_))
    }

    #[inline]
    pub fn is_let(self) -> bool {
        matches!(self, Self::Let(_))
    }

    #[inline]
    pub fn is_if(self) -> bool {
        matches!(self, Self::If(_))
    }

    #[inline]
    pub fn is_case(self) -> bool {
        matches!(self, Self::Case(_))
    }

    #[inline]
    pub fn is_call(self) -> bool {
        matches!(self, Self::Call(_))
    }

    #[inline]
    pub fn is_lambda(self) -> bool {
        matches!(self, Self::Lambda(_))
    }
}
