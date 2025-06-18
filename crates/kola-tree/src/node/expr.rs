use derive_more::{Display, From, IntoIterator};
use kola_print::prelude::*;
use kola_utils::{as_variant, interner::StrKey};
use serde::{Deserialize, Serialize};

use super::{Pat, Type};
use crate::{
    id::Id,
    node::{ModulePath, ValueName},
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
    Debug, From, IntoIterator, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[into_iterator(owned, ref)]
pub struct ListExpr(pub Vec<Id<Expr>>);

impl ListExpr {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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
    Debug, From, IntoIterator, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
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

// { y | +x = 10 }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordExtendExpr {
    pub source: Id<Expr>,
    pub select: Id<FieldPath>,
    pub value: Id<Expr>,
}

impl RecordExtendExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        select: impl Into<FieldPath>,
        value: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let select = builder.insert(select.into());
        let value = builder.insert(value.into());

        builder.insert(Self {
            source,
            select,
            value,
        })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }

    pub fn select(self, tree: &impl TreeView) -> &FieldPath {
        self.select.get(tree)
    }

    pub fn value(self, tree: &impl TreeView) -> Expr {
        *self.value.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordExtendExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordExtendExpr {
            source,
            select,
            value,
        } = self.value;

        let head = "RecordExtend".blue().display_in(arena);

        let source = self.to_id(*source).notate(arena);
        let field = self.to_id(*select).notate(arena);
        let value = self.to_id(*value).notate(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

// { y | -x }
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordRestrictExpr {
    pub source: Id<Expr>,
    pub select: Id<FieldPath>,
}

impl RecordRestrictExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        select: impl Into<FieldPath>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let select = builder.insert(select.into());

        builder.insert(Self { source, select })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
    }

    pub fn select(self, tree: &impl TreeView) -> &FieldPath {
        self.select.get(tree)
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordRestrictExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordRestrictExpr { source, select } = self.value;

        let head = "RecordRestrict".blue().display_in(arena);

        let source = self.to_id(*source).notate(arena);
        let field = self.to_id(*select).notate(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct RecordUpdateExpr {
    pub source: Id<Expr>,
    pub select: Id<FieldPath>,
    pub op: Id<RecordUpdateOp>,
    pub value: Id<Expr>,
}

impl RecordUpdateExpr {
    pub fn new_in(
        source: impl Into<Expr>,
        select: impl Into<FieldPath>,
        op: RecordUpdateOp,
        value: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let source = builder.insert(source.into());
        let select = builder.insert(select.into());
        let op = builder.insert(op);
        let value = builder.insert(value.into());

        builder.insert(Self {
            source,
            select,
            op,
            value,
        })
    }

    pub fn source(self, tree: &impl TreeView) -> Expr {
        *self.source.get(tree)
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
}

impl<'a> Notate<'a> for NodePrinter<'a, RecordUpdateExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordUpdateExpr {
            source,
            select,
            op,
            value,
        } = self.value;

        let head = "RecordUpdate".blue().display_in(arena);

        let source = self.to_id(*source).notate(arena);
        let field = self.to_id(*select).notate(arena);
        let op = self.to_id(*op).notate(arena);
        let value = self.to_id(*value).notate(arena);

        let single = [
            arena.notate(" source = "),
            source.clone().flatten(arena),
            arena.notate(", field = "),
            field.clone().flatten(arena),
            arena.notate(", op = "),
            op.clone().flatten(arena),
            arena.notate(", value = "),
            value.clone().flatten(arena),
        ]
        .concat_in(arena);

        let multi = [
            arena.newline(),
            arena.notate("source = "),
            source,
            arena.newline(),
            arena.notate("field = "),
            field,
            arena.newline(),
            arena.notate("op = "),
            op,
            arena.newline(),
            arena.notate("value = "),
            value,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
}

#[derive(
    Debug,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PathExpr {
    pub path: Option<Id<ModulePath>>,
    pub source: Id<ValueName>,
    pub fields: Id<FieldPath>,
}

impl PathExpr {
    pub fn new_in(
        path: Option<ModulePath>,
        source: impl Into<ValueName>,
        fields: impl Into<FieldPath>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let path = path.map(|p| builder.insert(p));
        let source = builder.insert(source.into());
        let fields = builder.insert(fields.into());

        builder.insert(Self {
            path,
            source,
            fields,
        })
    }
}

impl<'a> Notate<'a> for NodePrinter<'a, PathExpr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let PathExpr {
            path,
            source,
            fields,
        } = *self.value;

        let head = "PathExpr".cyan().display_in(arena);

        let path = path.map(|p| self.to_id(p).notate(arena)).or_not(arena);
        let source = self.to(source).notate(arena);
        let fields = self.to(fields).notate(arena);

        let single = [
            arena.notate(" path = "),
            path.clone(),
            arena.notate(" source = "),
            source.clone(),
            arena.notate(", fields = "),
            fields.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("path = "),
            path,
            arena.newline(),
            arena.notate("source = "),
            arena.newline(),
            source,
            arena.notate("fields = "),
            fields,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct LetExpr {
    pub name: Id<ValueName>,
    pub value: Id<Expr>,
    pub inside: Id<Expr>,
}

impl LetExpr {
    pub fn new_in(
        name: impl Into<ValueName>,
        value: impl Into<Expr>,
        inside: impl Into<Expr>,
        builder: &mut TreeBuilder,
    ) -> Id<Self> {
        let name = builder.insert(name.into());
        let value = builder.insert(value.into());
        let inside = builder.insert(inside.into());

        builder.insert(Self {
            name,
            value,
            inside,
        })
    }

    pub fn name(self, tree: &impl TreeView) -> ValueName {
        *self.name.get(tree)
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
            value,
            inside,
        } = self.value;

        let head = "Let".blue().display_in(arena);

        let name = self.to_id(*name).notate(arena);
        let value = self.to_id(*value).notate(arena);
        let inside = self.to_id(*inside).notate(arena);

        let single = [
            arena.notate(" name = "),
            name.clone().flatten(arena),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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
    Debug, From, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Expr {
    Error(Id<ExprError>),
    Literal(Id<LiteralExpr>),
    Path(Id<PathExpr>),
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
    Lambda(Id<LambdaExpr>),
    Call(Id<CallExpr>),
}

impl<'a> Notate<'a> for NodePrinter<'a, Expr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        match *self.value {
            Expr::Error(e) => self.to_id(e).notate(arena),
            Expr::Literal(l) => self.to_id(l).notate(arena),
            Expr::Path(p) => self.to_id(p).notate(arena),
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
            Expr::Lambda(f) => self.to_id(f).notate(arena),
            Expr::Call(c) => self.to_id(c).notate(arena),
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
    pub fn to_path(self) -> Option<Id<PathExpr>> {
        as_variant!(self, Self::Path)
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
    pub fn to_lambda(self) -> Option<Id<LambdaExpr>> {
        as_variant!(self, Self::Lambda)
    }

    #[inline]
    pub fn to_call(self) -> Option<Id<CallExpr>> {
        as_variant!(self, Self::Call)
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
    pub fn is_path(self) -> bool {
        matches!(self, Self::Path(_))
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
    pub fn is_lambda(self) -> bool {
        matches!(self, Self::Lambda(_))
    }

    #[inline]
    pub fn is_call(self) -> bool {
        matches!(self, Self::Call(_))
    }
}
mod inspector {
    use std::hash::BuildHasher;

    use super::*;
    use crate::inspector::*;
    impl<'t, S: BuildHasher> NodeInspector<'t, Id<Expr>, S> {
        pub fn as_error(self) -> Option<NodeInspector<'t, Id<ExprError>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_error()
                .map(|err_id| NodeInspector::new(err_id, self.tree, self.interner))
        }

        pub fn as_literal(self) -> Option<NodeInspector<'t, Id<LiteralExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_literal()
                .map(|lit_id| NodeInspector::new(lit_id, self.tree, self.interner))
        }

        pub fn as_path(self) -> Option<NodeInspector<'t, Id<PathExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_path()
                .map(|path_id| NodeInspector::new(path_id, self.tree, self.interner))
        }

        pub fn as_list(self) -> Option<NodeInspector<'t, Id<ListExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_list()
                .map(|list_id| NodeInspector::new(list_id, self.tree, self.interner))
        }

        pub fn as_record(self) -> Option<NodeInspector<'t, Id<RecordExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_record()
                .map(|rec_id| NodeInspector::new(rec_id, self.tree, self.interner))
        }

        pub fn as_record_extend(self) -> Option<NodeInspector<'t, Id<RecordExtendExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_record_extend()
                .map(|ext_id| NodeInspector::new(ext_id, self.tree, self.interner))
        }

        pub fn as_record_restrict(self) -> Option<NodeInspector<'t, Id<RecordRestrictExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_record_restrict()
                .map(|res_id| NodeInspector::new(res_id, self.tree, self.interner))
        }

        pub fn as_record_update(self) -> Option<NodeInspector<'t, Id<RecordUpdateExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_record_update()
                .map(|upd_id| NodeInspector::new(upd_id, self.tree, self.interner))
        }

        pub fn as_unary(self) -> Option<NodeInspector<'t, Id<UnaryExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_unary()
                .map(|un_id| NodeInspector::new(un_id, self.tree, self.interner))
        }

        pub fn as_binary(self) -> Option<NodeInspector<'t, Id<BinaryExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_binary()
                .map(|bin_id| NodeInspector::new(bin_id, self.tree, self.interner))
        }

        pub fn as_let(self) -> Option<NodeInspector<'t, Id<LetExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_let()
                .map(|let_id| NodeInspector::new(let_id, self.tree, self.interner))
        }

        pub fn as_if(self) -> Option<NodeInspector<'t, Id<IfExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_if()
                .map(|if_id| NodeInspector::new(if_id, self.tree, self.interner))
        }

        pub fn as_case(self) -> Option<NodeInspector<'t, Id<CaseExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_case()
                .map(|case_id| NodeInspector::new(case_id, self.tree, self.interner))
        }

        pub fn as_lambda(self) -> Option<NodeInspector<'t, Id<LambdaExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_lambda()
                .map(|lambda_id| NodeInspector::new(lambda_id, self.tree, self.interner))
        }

        pub fn as_call(self) -> Option<NodeInspector<'t, Id<CallExpr>, S>> {
            let expr = self.node.get(self.tree);
            expr.to_call()
                .map(|call_id| NodeInspector::new(call_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<LiteralExpr>, S> {
        pub fn is_bool(self, expected: bool) -> Self {
            let lit = self.node.get(self.tree);
            match lit {
                LiteralExpr::Bool(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected bool {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected bool literal but found {:?}", lit),
            }
            self
        }

        pub fn is_num(self, expected: f64) -> Self {
            let lit = self.node.get(self.tree);
            match lit {
                LiteralExpr::Num(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected num {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected num literal but found {:?}", lit),
            }
            self
        }

        pub fn is_char(self, expected: char) -> Self {
            let lit = self.node.get(self.tree);
            match lit {
                LiteralExpr::Char(value) => {
                    assert_eq!(
                        *value, expected,
                        "Expected char {} but found {}",
                        expected, value
                    );
                }
                _ => panic!("Expected char literal but found {:?}", lit),
            }
            self
        }

        pub fn is_string(self, expected: &str) -> Self {
            let lit = self.node.get(self.tree);
            match lit {
                LiteralExpr::Str(value) => {
                    let value = self.interner.get(*value).expect("Symbol not found");

                    assert_eq!(
                        value, expected,
                        "Expected string \"{}\" but found \"{}\"",
                        expected, value
                    );
                }
                _ => panic!("Expected string literalbut found {:?}", lit),
            }
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<PathExpr>, S> {
        pub fn module_path(self) -> Option<NodeInspector<'t, Id<ModulePath>, S>> {
            let path = self.node.get(self.tree);
            path.path
                .map(|p| NodeInspector::new(p, self.tree, self.interner))
        }

        pub fn source(self) -> NodeInspector<'t, Id<ValueName>, S> {
            let path = self.node.get(self.tree);
            NodeInspector::new(path.source, self.tree, self.interner)
        }

        pub fn select(self) -> NodeInspector<'t, Id<FieldPath>, S> {
            let path = self.node.get(self.tree);
            NodeInspector::new(path.fields, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<FieldPath>, S> {
        pub fn has_fields(self, count: usize) -> Self {
            let fields_len = self.node.get(self.tree).0.len();
            assert_eq!(
                fields_len, count,
                "Expected {} fields but found {}",
                count, fields_len
            );
            self
        }

        pub fn field_at_is(self, index: usize, expected: &str) -> Self {
            let field_path = self.node.get(self.tree);
            assert!(
                index < field_path.0.len(),
                "Field index {} out of bounds (max {})",
                index,
                field_path.0.len() - 1
            );
            let field = field_path.get(index, self.tree);
            let s = self.interner.get(field.0).expect("Symbol not found");

            assert_eq!(
                s, expected,
                "Expected field '{}' but found '{}'",
                expected, s
            );
            self
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<UnaryExpr>, S> {
        pub fn op(self) -> UnaryOp {
            let unary = self.node.get(self.tree);
            unary.op(self.tree)
        }

        pub fn has_op(self, expected: UnaryOp) -> Self {
            let op = self.op();
            assert_eq!(
                op, expected,
                "Expected unary operator {:?} but found {:?}",
                expected, op
            );
            self
        }

        pub fn operand(self) -> NodeInspector<'t, Id<Expr>, S> {
            let unary = self.node.get(self.tree);
            NodeInspector::new(unary.operand, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<BinaryExpr>, S> {
        pub fn op(self) -> BinaryOp {
            let binary = self.node.get(self.tree);
            binary.op(self.tree)
        }

        pub fn has_op(self, expected: BinaryOp) -> Self {
            let op = self.op();
            assert_eq!(
                op, expected,
                "Expected binary operator {:?} but found {:?}",
                expected, op
            );
            self
        }

        pub fn left(self) -> NodeInspector<'t, Id<Expr>, S> {
            let binary = self.node.get(self.tree);
            NodeInspector::new(binary.left, self.tree, self.interner)
        }

        pub fn right(self) -> NodeInspector<'t, Id<Expr>, S> {
            let binary = self.node.get(self.tree);
            NodeInspector::new(binary.right, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<LetExpr>, S> {
        pub fn has_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).name(self.tree);
            let s = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                s, expected,
                "Expected let binding name '{}' but found '{}'",
                expected, s
            );
            self
        }

        pub fn value(self) -> NodeInspector<'t, Id<Expr>, S> {
            let let_expr = self.node.get(self.tree);
            NodeInspector::new(let_expr.value, self.tree, self.interner)
        }

        pub fn inside(self) -> NodeInspector<'t, Id<Expr>, S> {
            let let_expr = self.node.get(self.tree);
            NodeInspector::new(let_expr.inside, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<IfExpr>, S> {
        pub fn predicate(self) -> NodeInspector<'t, Id<Expr>, S> {
            let if_expr = self.node.get(self.tree);
            NodeInspector::new(if_expr.predicate, self.tree, self.interner)
        }

        pub fn then(self) -> NodeInspector<'t, Id<Expr>, S> {
            let if_expr = self.node.get(self.tree);
            NodeInspector::new(if_expr.then, self.tree, self.interner)
        }

        pub fn or(self) -> NodeInspector<'t, Id<Expr>, S> {
            let if_expr = self.node.get(self.tree);
            NodeInspector::new(if_expr.or, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<CaseExpr>, S> {
        pub fn source(self) -> NodeInspector<'t, Id<Expr>, S> {
            let case_expr = self.node.get(self.tree);
            NodeInspector::new(case_expr.source, self.tree, self.interner)
        }

        pub fn has_branches(self, count: usize) -> Self {
            let branches_len = self.node.get(self.tree).branches.len();
            assert_eq!(
                branches_len, count,
                "Expected {} branches but found {}",
                count, branches_len
            );
            self
        }

        pub fn branch_at(self, index: usize) -> NodeInspector<'t, Id<CaseBranch>, S> {
            let case_expr = self.node.get(self.tree);
            assert!(
                index < case_expr.branches.len(),
                "Branch index {} out of bounds (max {})",
                index,
                case_expr.branches.len() - 1
            );
            let branch_id = case_expr.branches[index];
            NodeInspector::new(branch_id, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<CaseBranch>, S> {
        pub fn pat(self) -> NodeInspector<'t, Id<Pat>, S> {
            let branch = self.node.get(self.tree);
            NodeInspector::new(branch.pat, self.tree, self.interner)
        }

        pub fn matches(self) -> NodeInspector<'t, Id<Expr>, S> {
            let branch = self.node.get(self.tree);
            NodeInspector::new(branch.matches, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<LambdaExpr>, S> {
        pub fn param_name(self) -> &'t str {
            let lambda = self.node.get(self.tree);
            let symbol = lambda.param(self.tree);
            self.interner.get(symbol.0).expect("Symbol not found")
        }

        pub fn has_param(self, expected: &str) -> Self {
            let param = self.param_name();
            assert_eq!(
                param, expected,
                "Expected parameter name '{}' but found '{}'",
                expected, param
            );
            self
        }

        pub fn body(self) -> NodeInspector<'t, Id<Expr>, S> {
            let lambda = self.node.get(self.tree);
            NodeInspector::new(lambda.body, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<CallExpr>, S> {
        pub fn func(self) -> NodeInspector<'t, Id<Expr>, S> {
            let call = self.node.get(self.tree);
            NodeInspector::new(call.func, self.tree, self.interner)
        }

        pub fn arg(self) -> NodeInspector<'t, Id<Expr>, S> {
            let call = self.node.get(self.tree);
            NodeInspector::new(call.arg, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<ListExpr>, S> {
        pub fn has_elements(self, count: usize) -> Self {
            let elements_len = self.node.get(self.tree).0.len();
            assert_eq!(
                elements_len, count,
                "Expected {} elements but found {}",
                count, elements_len
            );
            self
        }

        pub fn element_at(self, index: usize) -> NodeInspector<'t, Id<Expr>, S> {
            let list = self.node.get(self.tree);
            assert!(
                index < list.0.len(),
                "Element index {} out of bounds (max {})",
                index,
                list.0.len() - 1
            );
            let element_id = list.0[index];
            NodeInspector::new(element_id, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordExpr>, S> {
        pub fn has_fields(self, count: usize) -> Self {
            let fields_len = self.node.get(self.tree).0.len();
            assert_eq!(
                fields_len, count,
                "Expected {} fields but found {}",
                count, fields_len
            );
            self
        }

        pub fn field_at(self, index: usize) -> NodeInspector<'t, Id<RecordField>, S> {
            let record = self.node.get(self.tree);
            assert!(
                index < record.0.len(),
                "Field index {} out of bounds (max {})",
                index,
                record.0.len() - 1
            );
            let field_id = record.0[index];
            NodeInspector::new(field_id, self.tree, self.interner)
        }

        pub fn field_named(self, name: &str) -> Option<NodeInspector<'t, Id<RecordField>, S>> {
            let record = self.node.get(self.tree);
            record
                .0
                .iter()
                .find(|id| {
                    let field = id.get(self.tree).field(self.tree);
                    self.interner.get(field.0).expect("Symbol not found") == name
                })
                .map(|field_id| NodeInspector::new(*field_id, self.tree, self.interner))
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordField>, S> {
        pub fn has_field_name(self, expected: &str) -> Self {
            let name = self.node.get(self.tree).field(self.tree);
            let s = self.interner.get(name.0).expect("Symbol not found");

            assert_eq!(
                s, expected,
                "Expected field name '{}' but found '{}'",
                expected, s
            );
            self
        }

        pub fn value(self) -> NodeInspector<'t, Id<Expr>, S> {
            let field = self.node.get(self.tree);
            NodeInspector::new(field.value, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordExtendExpr>, S> {
        pub fn source(self) -> NodeInspector<'t, Id<Expr>, S> {
            let extend = self.node.get(self.tree);
            NodeInspector::new(extend.source, self.tree, self.interner)
        }

        pub fn select(self) -> NodeInspector<'t, Id<FieldPath>, S> {
            let extend = self.node.get(self.tree);
            NodeInspector::new(extend.select, self.tree, self.interner)
        }

        pub fn value(self) -> NodeInspector<'t, Id<Expr>, S> {
            let extend = self.node.get(self.tree);
            NodeInspector::new(extend.value, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordRestrictExpr>, S> {
        pub fn source(self) -> NodeInspector<'t, Id<Expr>, S> {
            let restrict = self.node.get(self.tree);
            NodeInspector::new(restrict.source, self.tree, self.interner)
        }

        pub fn select(self) -> NodeInspector<'t, Id<FieldPath>, S> {
            let restrict = self.node.get(self.tree);
            NodeInspector::new(restrict.select, self.tree, self.interner)
        }
    }

    impl<'t, S: BuildHasher> NodeInspector<'t, Id<RecordUpdateExpr>, S> {
        pub fn source(self) -> NodeInspector<'t, Id<Expr>, S> {
            let update = self.node.get(self.tree);
            NodeInspector::new(update.source, self.tree, self.interner)
        }

        pub fn select(self) -> NodeInspector<'t, Id<FieldPath>, S> {
            let update = self.node.get(self.tree);
            NodeInspector::new(update.select, self.tree, self.interner)
        }

        pub fn op(self) -> RecordUpdateOp {
            let update = self.node.get(self.tree);
            update.op(self.tree)
        }

        pub fn has_op(self, expected: RecordUpdateOp) -> Self {
            let op = self.op();
            assert_eq!(
                op, expected,
                "Expected record update operator {:?} but found {:?}",
                expected, op
            );
            self
        }

        pub fn value(self) -> NodeInspector<'t, Id<Expr>, S> {
            let update = self.node.get(self.tree);
            NodeInspector::new(update.value, self.tree, self.interner)
        }
    }
}
